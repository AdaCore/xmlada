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

   procedure Parse_Grammar
     (Handler  : in out Validating_Reader;
      Xsd_File : Byte_Sequence);
   --  Parse the grammar to use from an XSD file

   procedure Parse_Grammars
     (Handler  : in out Validating_Reader;
      Schema_Location : Byte_Sequence);
   --  Parse multiple grammars, as defined by the "schemaLocation" attribute

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
      Data    : Validator_Data;
      Is_Nil  : Boolean) is
   begin
      List := new Validator_List_Record'
        (Element => Element,
         Typ     => Typ,
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
      Xsd_File : Byte_Sequence)
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

      Set_Created_Grammar (Schema, Handler.Grammar);
      Parse (Schema, File);
      Close (File);
      Handler.Grammar := Get_Created_Grammar (Schema);

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
      G : XML_Grammar_NS;
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

         Parse_Grammar (Handler, Schema_Location (Start_XSD .. Last_XSD - 1));

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
      G         : XML_Grammar_NS;
      Typ       : XML_Type;
      Type_Index : Integer;
      Index     : Integer;
      Is_Nil    : Boolean;
   begin
      if Debug then
         Put_Line (ASCII.ESC & "[33m"
                   & "Start_Element: " & Local_Name
                   & ASCII.ESC & "[39m");
      end if;

      --  Get the name of the grammar to use from the element's attributes

      if Handler.Grammar = No_Grammar then
         Index := Get_Index (Atts, URI => XML_Instance_URI,
                             Local_Name => "noNamespaceSchemaLocation");
         if Index /= -1 then
            Parse_Grammar (Handler, Get_Value (Atts, Index));
         end if;

         Index := Get_Index (Atts, URI => XML_Instance_URI,
                             Local_Name => "schemaLocation");
         if Index /= -1 then
            Parse_Grammars (Handler, Get_Value (Atts, Index));
         end if;

         if Handler.Grammar = No_Grammar then
            --  Always valid
            return;
         end if;
      end if;

      --  Whether this element is valid in the current context

      if Handler.Validators /= null then
         Validate_Start_Element
           (Get_Validator (Handler.Validators.Typ),
            Local_Name, Handler.Validators.Data, Element);
      else
         if Debug then
            Put_Line ("Getting element definition from grammar: "
                      & Namespace_URI & " " & Local_Name);
         end if;
         Get_NS (Handler.Grammar, Namespace_URI, Result => G);
         Element := Lookup_Element (G, Local_Name);

         Add_XML_Instance_Attributes
           (Handler, Get_Validator (Get_Type (Element)));
      end if;

      --  If not: this is a validation error

      if Element = No_Element then
         Raise_Exception
           (XML_Validation_Error'Identity,
            "No data type definition for element " & String (Local_Name));
      end if;

      --  Whether the element specifies a different type to use than the one
      --  defined in the grammar

      Typ := Get_Type (Element);

      Type_Index := Get_Index
        (Atts, URI => XML_Instance_URI, Local_Name => "type");
      if Type_Index /= -1 then
         if Debug then
            Put_Line ("Getting element definition from type attribute: "
                      & Get_Value (Atts, Type_Index));
         end if;
         --  ??? Should check with namespaces
         Get_NS (Handler.Grammar, Namespace_URI, Result => G);
         Typ := Lookup (G, Get_Value (Atts, Type_Index));

         declare
            Derives_By_Extension : constant Boolean :=
              Is_Extension_Of (Get_Validator (Typ), Get_Type (Element));
            Derives_By_Restriction : constant Boolean :=
              Is_Restriction_Of (Get_Validator (Typ), Get_Type (Element));
         begin
            if not Derives_By_Extension
              and then not Derives_By_Restriction
              and then Typ /= Get_Type (Element)
              and then Get_Local_Name (Get_Type (Element)) /= "ur-Type"
            then
               Raise_Exception
                 (XML_Validation_Error'Identity,
                  "The type mentionned in the ""type"" attribute must derive"
                  & " from the element's type in the schema");
            end if;

            if Derives_By_Restriction
              and then Get_Block_On_Restriction (Element)
            then
               Raise_Exception
                 (XML_Validation_Error'Identity,
                  "Cannot use restriction of element's type in this context");

            elsif Derives_By_Extension
              and then Get_Block_On_Extension (Element)
            then
               Raise_Exception
                 (XML_Validation_Error'Identity,
                  "Cannot use extension of element's type in this context");
            end if;
         end;
      end if;

      --  Check the element's attributes

      Data := Create_Validator_Data (Get_Validator (Typ));
      Validate_Attributes
        (Get_Validator (Typ), Atts, Handler.Ids,
         Is_Nillable (Element), Is_Nil);
      Push (Handler.Validators, Element, Typ, Data, Is_Nil);
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
         if Has_Fixed (Handler.Validators.Element)
           and then not Handler.Validators.Had_Character_Data
           and then Get_Fixed (Handler.Validators.Element).all /= ""
         then
            Raise_Exception
              (XML_Validation_Error'Identity,
               "Element's value must be """
               & Get_Fixed (Handler.Validators.Element).all & """");
         end if;

         Validate_End_Element
           (Get_Validator (Handler.Validators.Typ),
            Qname,
            Handler.Validators.Data);
      end if;
      Pop (Handler.Validators);
   end End_Element;

   ----------------
   -- Characters --
   ----------------

   procedure Characters
     (Handler : in out Validating_Reader;
      Ch      : Unicode.CES.Byte_Sequence) is
   begin
      if Handler.Validators /= null then
         Handler.Validators.Had_Character_Data := True;

         if Handler.Validators.Is_Nil then
            if Ch /= "" then
               Raise_Exception
                 (XML_Validation_Error'Identity,
                  "Element has character data, but is declared as nil");
            end if;

         elsif Has_Fixed (Handler.Validators.Element) then
            if Ch /= Get_Fixed (Handler.Validators.Element).all then
               Raise_Exception
                 (XML_Validation_Error'Identity,
                  "Element's value must be """
                  & Get_Fixed (Handler.Validators.Element).all & """");
            end if;
         else
            Validate_Characters
                 (Get_Validator (Get_Type (Handler.Validators.Element)), Ch);
         end if;
      end if;
   end Characters;

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
      Except  : Sax.Exceptions.Sax_Parse_Exception'Class) is
   begin
      Raise_Exception
        (XML_Validation_Error'Identity,
         To_String (Get_Locator (Except)) & ": "
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
