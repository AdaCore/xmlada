with Unicode;           use Unicode;
with Unicode.CES;       use Unicode.CES;
with Sax.Attributes;    use Sax.Attributes;
with Sax.Exceptions;    use Sax.Exceptions;
with Sax.Locators;      use Sax.Locators;
with Sax.Encodings;     use Sax.Encodings;
with Schema.Validators; use Schema.Validators;
with Ada.Exceptions;    use Ada.Exceptions;
with Ada.Unchecked_Deallocation;
with Schema.Validators; use Schema.Validators;
with GNAT.IO;               use GNAT.IO;
with Input_Sources.File;    use Input_Sources.File;
with Schema.Schema_Readers; use Schema.Schema_Readers;

with GNAT.Directory_Operations; use GNAT.Directory_Operations;

package body Schema.Readers is

   Debug : Boolean := False;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Validator_List_Record, Validator_List);

   procedure Push  (List    : in out Validator_List;
                    Element : XML_Element;
                    Typ     : XML_Type;
                    G       : XML_Grammar_NS;
                    Data    : Validator_Data;
                    Is_Nil  : Boolean);
   procedure Pop   (List : in out Validator_List);
   procedure Clear (List : in out Validator_List);
   --  Push or remove validators from the list

   pragma Unreferenced (Clear);

   procedure Add_XML_Instance_Attributes
     (Handler   : in out Validating_Reader;
      Validator : access XML_Validator_Record'Class);
   --  Add the standard attributes from the XMLSchema-Instance namespace to
   --  Tmp.

   procedure Parse_Grammars
     (Handler  : in out Validating_Reader;
      Schema_Location : Byte_Sequence);
   --  Parse multiple grammars, as defined by the "schemaLocation" attribute

   procedure Internal_Characters
     (Handler : in out Validating_Reader;
      Ch      : Unicode.CES.Byte_Sequence;
      Empty_Element : Boolean);
   --  Internal version of Characters, that can deal with empty content for
   --  elements

   ----------------------
   -- Set_Debug_Output --
   ----------------------

   procedure Set_Debug_Output (Output : Boolean) is
   begin
      Debug := Output;
   end Set_Debug_Output;

   ----------------------------
   -- Set_Validating_Grammar --
   ----------------------------

   procedure Set_Validating_Grammar
     (Reader  : in out Validating_Reader;
      Grammar : Schema.Validators.XML_Grammar) is
   begin
      Reader.Grammar := Grammar;
   end Set_Validating_Grammar;

   ----------
   -- Push --
   ----------

   procedure Push
     (List    : in out Validator_List;
      Element : XML_Element;
      Typ     : XML_Type;
      G       : XML_Grammar_NS;
      Data    : Validator_Data;
      Is_Nil  : Boolean) is
   begin
      List := new Validator_List_Record'
        (Element => Element,
         Typ     => Typ,
         Grammar => G,
         Data    => Data,
         Is_Nil  => Is_Nil,
         Had_Character_Data => False,
         Next    => List);
   end Push;

   ---------
   -- Pop --
   ---------

   procedure Pop (List : in out Validator_List) is
      Tmp : Validator_List := List;
   begin
      if List /= null then
         Free (List.Data);
         List := List.Next;

         Unchecked_Free (Tmp);
      end if;
   end Pop;

   -----------
   -- Clear --
   -----------

   procedure Clear (List : in out Validator_List) is
   begin
      while List /= null loop
         Pop (List);
      end loop;
   end Clear;

   ---------------------------------
   -- Add_XML_Instance_Attributes --
   ---------------------------------

   procedure Add_XML_Instance_Attributes
     (Handler   : in out Validating_Reader;
      Validator : access XML_Validator_Record'Class)
   is
      XML_G, XML_IG : XML_Grammar_NS;
   begin
      Get_NS (Handler.Grammar, XML_Schema_URI, Result => XML_G);
      Get_NS (Handler.Grammar, XML_Instance_URI, Result => XML_IG);

      Add_Attribute
        (Validator,
         Create_Attribute ("type", XML_IG, Lookup (XML_G, "string")));
      Add_Attribute
        (Validator,
         Create_Attribute ("nil", XML_IG, Lookup (XML_G, "boolean")));
      Add_Attribute
        (Validator,
         Create_Attribute ("schemaLocation", XML_IG,
                           List_Of (Lookup (XML_G, "string"))));
      Add_Attribute
        (Validator,
         Create_Attribute ("noNamespaceSchemaLocation", XML_IG,
                           Lookup (XML_G, "string")));
   end Add_XML_Instance_Attributes;

   -------------------
   -- Parse_Grammar --
   -------------------

   procedure Parse_Grammar
     (Handler  : in out Validating_Reader;
      Xsd_File : Byte_Sequence;
      Add_To   : in out XML_Grammar)
   is
      File     : File_Input;
      Schema   : Schema_Reader;
   begin
      if Xsd_File (Xsd_File'First) /= '/'
        and then Xsd_File (Xsd_File'First) /= '\'
      then
         declare
            Current  : constant Byte_Sequence :=
              Get_System_Id (Handler.Locator.all);
            Xsd_File_Full : constant Byte_Sequence :=
              Dir_Name (Current) & Xsd_File;
         begin
            if Debug then
               Put_Line ("Parsing new grammar: " & Xsd_File_Full);
            end if;
            Open (Xsd_File_Full, File);
            Set_Public_Id (File, Xsd_File_Full);
            Set_System_Id (File, Xsd_File_Full);
         end;
      else
         if Debug then
            Put_Line ("Parsing new grammar: " & Xsd_File);
         end if;
         Open (Xsd_File, File);
         Set_Public_Id (File, Xsd_File);
         Set_System_Id (File, Xsd_File);
      end if;

      Set_Created_Grammar (Schema, Add_To);
      Parse (Schema, File);
      Close (File);
      Add_To := Get_Created_Grammar (Schema);

      if Debug then
         Put_Line ("Done parsing new grammar: " & Xsd_File);
      end if;
   end Parse_Grammar;

   --------------------
   -- Parse_Grammars --
   --------------------

   procedure Parse_Grammars
     (Handler  : in out Validating_Reader;
      Schema_Location : Byte_Sequence)
   is
      Start_NS, Last_NS, Index : Integer;
      Start_XSD, Last_XSD : Integer;
      C : Unicode_Char;
   begin
      Index    := Schema_Location'First;
      Start_NS := Schema_Location'First;
      while Index <= Schema_Location'Last loop
         while Index <= Schema_Location'Last loop
            Last_NS := Index;
            Encoding.Read (Schema_Location, Index, C);
            exit when Is_White_Space (C);
         end loop;

         while Index <= Schema_Location'Last loop
            Start_XSD := Index;
            Encoding.Read (Schema_Location, Index, C);
            exit when not Is_White_Space (C);
         end loop;

         while Index <= Schema_Location'Last loop
            Last_XSD := Index;
            Encoding.Read (Schema_Location, Index, C);
            exit when Is_White_Space (C);
         end loop;

         if Index > Schema_Location'Last then
            Last_XSD := Schema_Location'Last + 1;
         end if;

         if Debug then
            Put_Line ("NS=" & Schema_Location (Start_NS .. Last_NS - 1)
                      & ASCII.LF
                      & "XSD=" & Schema_Location (Start_XSD .. Last_XSD - 1));
         end if;

         Parse_Grammar (Handler, Schema_Location (Start_XSD .. Last_XSD - 1),
                        Add_To => Handler.Grammar);

         while Index <= Schema_Location'Last loop
            Start_NS := Index;
            Encoding.Read (Schema_Location, Index, C);
            exit when not Is_White_Space (C);
         end loop;
      end loop;
   end Parse_Grammars;

   -------------------
   -- Start_Element --
   -------------------

   procedure Start_Element
     (Handler       : in out Validating_Reader;
      Namespace_URI : Unicode.CES.Byte_Sequence := "";
      Local_Name    : Unicode.CES.Byte_Sequence := "";
      Qname         : Unicode.CES.Byte_Sequence := "";
      Atts          : Sax.Attributes.Attributes'Class)
   is
      pragma Unreferenced (Qname);
      Element   : XML_Element := No_Element;
      Data      : Validator_Data;
      Typ       : XML_Type;
      Is_Nil    : Boolean;

      procedure Get_Grammar_From_Attributes;
      --  Parse the grammar, reading its name from the attributes

      procedure Compute_Type (G : XML_Grammar_NS);
      --  Compute the type to use, depending on whether the xsi:type attribute
      --  was specified

      ---------------------------------
      -- Get_Grammar_From_Attributes --
      ---------------------------------

      procedure Get_Grammar_From_Attributes is
         No_Index : constant Integer := Get_Index
           (Atts, URI => XML_Instance_URI,
            Local_Name => "noNamespaceSchemaLocation");
         Location_Index : constant Integer := Get_Index
           (Atts, URI => XML_Instance_URI,
            Local_Name => "schemaLocation");
      begin
         if No_Index /= -1 then
            Parse_Grammar (Handler, Get_Value (Atts, No_Index),
                           Add_To => Handler.Grammar);
         elsif Location_Index /= -1 then
            Parse_Grammars (Handler, Get_Value (Atts, Location_Index));
         end if;
      end Get_Grammar_From_Attributes;

      ------------------
      -- Compute_Type --
      ------------------

      procedure Compute_Type (G : XML_Grammar_NS) is
         Type_Index : constant Integer := Get_Index
           (Atts, URI => XML_Instance_URI, Local_Name => "type");
         Derives_By_Extension, Derives_By_Restriction : Boolean;
      begin
         Typ := Get_Type (Element);

         if Type_Index /= -1 then
            if Debug then
               Put_Line ("Getting element definition from type attribute: "
                         & Get_Value (Atts, Type_Index));
            end if;

            --  ??? Should check with namespaces
            Typ := Lookup (G, Get_Value (Atts, Type_Index));

            Derives_By_Extension :=
              Is_Extension_Of (Get_Validator (Typ), Get_Type (Element));
            Derives_By_Restriction :=
              Is_Restriction_Of (Get_Validator (Typ), Get_Type (Element));

            if not Derives_By_Extension
              and then not Derives_By_Restriction
              and then Typ /= Get_Type (Element)
              and then Get_Local_Name (Get_Type (Element)) /= "ur-Type"
            then
               Validation_Error
                 ("The type mentionned in the ""type"" attribute must"
                  & " derive from the element's type in the schema");
            end if;

            if Derives_By_Restriction
              and then Get_Block_On_Restriction (Element)
            then
               Validation_Error
                 ("Cannot use restriction of element's type in this context");

            elsif Derives_By_Extension
              and then Get_Block_On_Extension (Element)
            then
               Validation_Error
                 ("Cannot use extension of element's type in this context");
            end if;
         end if;
      end Compute_Type;

      G : XML_Grammar_NS;
   begin
      if Debug then
         Put_Line (ASCII.ESC & "[33m"
                   & "Start_Element: " & Local_Name
                   & ASCII.ESC & "[39m");
      end if;

      --  Get the name of the grammar to use from the element's attributes

      if Handler.Grammar = No_Grammar then
         Get_Grammar_From_Attributes;

         if Handler.Grammar = No_Grammar then
            return;  --  Always valid
         end if;
      end if;

      --  Whether this element is valid in the current context

      Get_NS (Handler.Grammar, Namespace_URI, Result => G);

      if Handler.Validators /= null then
         Validate_Start_Element
           (Get_Validator (Handler.Validators.Typ),
            Local_Name, Namespace_URI,
            Handler.Validators.Data,
            Handler.Grammar, Element);
      else
         if Debug then
            Put_Line ("Getting element definition from grammar: "
                      & Namespace_URI & " " & Local_Name);
         end if;
         Element := Lookup_Element (G, Local_Name);

         Add_XML_Instance_Attributes
           (Handler, Get_Validator (Get_Type (Element)));
      end if;

      --  If not: this is a validation error

      if Element = No_Element then
         Validation_Error
           ("No data type definition for element " & String (Local_Name));
      end if;

      Compute_Type (G);
      Data := Create_Validator_Data (Get_Validator (Typ));
      Validate_Attributes
        (Get_Validator (Typ), Atts, Handler.Ids,
         Is_Nillable (Element), Is_Nil);
      Push (Handler.Validators, Element, Typ, G, Data, Is_Nil);
   end Start_Element;

   -----------------
   -- End_Element --
   -----------------

   procedure End_Element
     (Handler       : in out Validating_Reader;
      Namespace_URI : Unicode.CES.Byte_Sequence := "";
      Local_Name    : Unicode.CES.Byte_Sequence := "";
      Qname         : Unicode.CES.Byte_Sequence := "")
   is
      pragma Unreferenced (Namespace_URI, Local_Name);
   begin
      if Handler.Validators /= null then
         --  No character data => behave as an empty element, but we need to
         --  test explicitely. For instance, the "minLength" facet might or
         --  the "fixed" attribute need to test whether the empty string is
         --  valid.
         if not Handler.Validators.Had_Character_Data
           and then not Handler.Validators.Is_Nil
         then
            Internal_Characters (Handler, "", Empty_Element => True);
         end if;

         Validate_End_Element
           (Get_Validator (Handler.Validators.Typ),
            Qname,
            Handler.Validators.Data);
      end if;
      Pop (Handler.Validators);
   end End_Element;

   -------------------------
   -- Internal_Characters --
   -------------------------

   procedure Internal_Characters
     (Handler : in out Validating_Reader;
      Ch      : Unicode.CES.Byte_Sequence;
      Empty_Element : Boolean) is
   begin
      if Handler.Validators /= null then
         Handler.Validators.Had_Character_Data := True;

         if Handler.Validators.Is_Nil then
            if Ch /= "" then
               Validation_Error
                 ("Element has character data, but is declared as nil");
            end if;

         elsif Has_Fixed (Handler.Validators.Element) then
            if Ch /= Get_Fixed (Handler.Validators.Element).all then
               Validation_Error
                 ("Element's value must be """
                  & Get_Fixed (Handler.Validators.Element).all & """");
            end if;

         else
            Validate_Characters
              (Get_Validator (Get_Type (Handler.Validators.Element)), Ch,
               Empty_Element => Empty_Element);
         end if;
      end if;
   end Internal_Characters;

   ----------------
   -- Characters --
   ----------------

   procedure Characters
     (Handler : in out Validating_Reader;
      Ch      : Unicode.CES.Byte_Sequence) is
   begin
      Internal_Characters (Handler, Ch, Empty_Element => False);
   end Characters;

   --------------------------
   -- Ignorable_Whitespace --
   --------------------------

   procedure Ignorable_Whitespace
     (Handler : in out Validating_Reader;
      Ch      : Unicode.CES.Byte_Sequence) is
   begin
      if Handler.Validators /= null
        and then Is_Simple_Type
          (Get_Validator (Get_Type (Handler.Validators.Element)))
      then
         Internal_Characters (Handler, Ch, Empty_Element => False);
      end if;
   end Ignorable_Whitespace;

   -----------
   -- Parse --
   -----------

   procedure Parse
     (Parser : in out Validating_Reader;
      Input  : in out Input_Sources.Input_Source'Class)
   is
      Loc : aliased Locator_Impl;
   begin
      Sax.Readers.Parse (Sax.Readers.Reader (Parser), Input);
      Unref (Parser.Locator.all);

   exception
      when E : XML_Validation_Error =>
         Copy (Loc, Parser.Locator.all);
         Validation_Error
           (Parser, Create (Byte_Sequence (Exception_Message (E)),
                            Loc'Unchecked_Access));
         Unref (Parser.Locator.all);
   end Parse;

   ----------------------
   -- Validation_Error --
   ----------------------

   procedure Validation_Error
     (Reader : in out Validating_Reader;
      Except  : Sax.Exceptions.Sax_Parse_Exception'Class)
   is
      pragma Unreferenced (Reader);
   begin
      Validation_Error
        (To_String (Get_Locator (Except)) & ": "
         & String (Get_Message (Except)));
   end Validation_Error;

   --------------------------
   -- Set_Document_Locator --
   --------------------------

   procedure Set_Document_Locator
     (Handler : in out Validating_Reader;
      Loc     : access Sax.Locators.Locator'Class) is
   begin
      Handler.Locator := Locator_Access (Loc);
      Ref (Handler.Locator.all);
   end Set_Document_Locator;

end Schema.Readers;
