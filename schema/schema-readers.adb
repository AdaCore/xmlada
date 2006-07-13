with Unicode;           use Unicode;
with Unicode.CES;       use Unicode.CES;
with Sax.Attributes;    use Sax.Attributes;
with Sax.Exceptions;    use Sax.Exceptions;
with Sax.Locators;      use Sax.Locators;
with Sax.Encodings;     use Sax.Encodings;
with Sax.Utils;         use Sax.Utils;
with Sax.Readers;       use Sax.Readers;
with Schema.Validators; use Schema.Validators;
with Ada.Exceptions;    use Ada.Exceptions;
with Ada.IO_Exceptions;
with Ada.Unchecked_Deallocation;
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
      Ch      : Unicode.CES.Byte_Sequence);
   --  Store Ch in the current sequence of characters. This is needed to
   --  collapse multiple calls to Characters and Ignorable_Whitespace into a
   --  single string, for validation purposes.

   procedure Validate_Current_Characters (Handler : Validating_Reader'Class);
   --  Validate the current set of characters

   procedure Free (Mapping : in out Prefix_Mapping_Access);
   --  Free the memory occupied by Mapping

   procedure Reset (Parser : in out Validating_Reader);
   --  Reset the state of the parser so that we can parse other documents

   procedure Hook_Start_Element
     (Handler       : in out Reader'Class;
      Namespace_URI : Unicode.CES.Byte_Sequence := "";
      Local_Name    : Unicode.CES.Byte_Sequence := "";
      Qname         : Unicode.CES.Byte_Sequence := "";
      Atts          : in out Sax.Attributes.Attributes'Class);
   procedure Hook_End_Element
     (Handler       : in out Reader'Class;
      Namespace_URI : Unicode.CES.Byte_Sequence := "";
      Local_Name    : Unicode.CES.Byte_Sequence := "";
      Qname         : Unicode.CES.Byte_Sequence := "");
   procedure Hook_Characters
     (Handler : in out Reader'Class; Ch : Unicode.CES.Byte_Sequence);
   procedure Hook_Ignorable_Whitespace
     (Handler : in out Reader'Class;
      Ch      : Unicode.CES.Byte_Sequence);
   procedure Hook_Start_Prefix_Mapping
     (Handler : in out Reader'Class;
      Prefix  : Unicode.CES.Byte_Sequence;
      URI     : Unicode.CES.Byte_Sequence);
   procedure Hook_End_Prefix_Mapping
     (Handler : in out Reader'Class;
      Prefix  : Unicode.CES.Byte_Sequence);
   procedure Hook_Set_Document_Locator
     (Handler : in out Reader'Class;
      Loc     : access Sax.Locators.Locator'Class);
   --  See for the corresponding primitive operations. These provide the
   --  necessary validation hooks.

   ----------
   -- Free --
   ----------

   procedure Free (Mapping : in out Prefix_Mapping_Access) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Prefix_Mapping, Prefix_Mapping_Access);
   begin
      if Mapping /= null then
         Free (Mapping.Prefix);
         Free (Mapping.Namespace);
         Unchecked_Free (Mapping);
      end if;
   end Free;

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

   ----------------------------
   -- Get_Validating_Grammar --
   ----------------------------

   function Get_Validating_Grammar
     (Reader : Validating_Reader) return Schema.Validators.XML_Grammar is
   begin
      return Reader.Grammar;
   end Get_Validating_Grammar;

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
         Start_Loc  => null,
         Characters => null,
         Next    => List);
   end Push;

   ---------
   -- Pop --
   ---------

   procedure Pop (List : in out Validator_List) is
      Tmp : Validator_List := List;
   begin
      if List /= null then
         if List.Start_Loc /= null then
            Unref (List.Start_Loc);
         end if;

         if List.Characters /= null then
            Free (List.Characters);
         end if;

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
         Create_Local_Attribute ("type", XML_IG, Lookup (XML_G, "string")));
      Add_Attribute
        (Validator,
         Create_Local_Attribute ("nil", XML_IG, Lookup (XML_G, "boolean")));
      Add_Attribute
        (Validator,
         Create_Local_Attribute ("schemaLocation", XML_IG,
                                 List_Of (Lookup (XML_G, "string"))));
      Add_Attribute
        (Validator,
         Create_Local_Attribute ("noNamespaceSchemaLocation", XML_IG,
                                 Lookup (XML_G, "string")));
   end Add_XML_Instance_Attributes;

   ---------------------
   -- To_Absolute_URI --
   ---------------------

   function To_Absolute_URI
     (Handler : Validating_Reader;
      URI     : Byte_Sequence) return Byte_Sequence is
   begin
      if URI = "" then
         return URI;
      elsif URI (URI'First) /= '/'
        and then URI (URI'First) /= '\'
      then
         return Dir_Name (Get_System_Id (Handler.Locator.all)) & URI;
      else
         return URI;
      end if;
   end To_Absolute_URI;

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
      Xsd_File_Full : constant Byte_Sequence :=
        To_Absolute_URI (Handler, Xsd_File);
   begin
      if Debug then
         Put_Line ("Parsing grammar: " & Xsd_File_Full);
         Debug_Dump (Add_To);
      end if;
      Open (Xsd_File_Full, File);
      Set_Public_Id (File, Xsd_File_Full);
      Set_System_Id (File, Xsd_File_Full);

      --  MANU ? More efficient: Add_To will likely already contain the grammar
      --  for the schema-for-schema, and we won't have to recreate it in most
      --  cases.
      Set_Validating_Grammar (Schema, Add_To);
      Set_Created_Grammar (Schema, Add_To);
      Parse (Schema, File);
      Close (File);
      Add_To := Get_Created_Grammar (Schema);

      if Debug then
         Put_Line ("Done parsing new grammar: " & Xsd_File);
      end if;
   exception
      when Ada.IO_Exceptions.Name_Error =>
         --  According to XML Schema Primer 0, section 5.6, this is not an
         --  error when we do not find the schema, since this attribute is only
         --  a hint.
         Warning
           (Handler,
            Create (Message => "Could not open file " & Xsd_File_Full,
                    Loc     => Locator_Impl_Access (Handler.Locator)));
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
      Local_Grammar : XML_Grammar_NS;
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

         --  Do not reparse the grammar if we already know about it
         Get_NS
           (Handler.Grammar,
            Namespace_URI    => Schema_Location (Start_NS .. Last_NS - 1),
            Result           => Local_Grammar,
            Create_If_Needed => False);
         if Local_Grammar = null then
            Parse_Grammar
              (Handler, Schema_Location (Start_XSD .. Last_XSD - 1),
               Add_To => Handler.Grammar);
         end if;

         while Index <= Schema_Location'Last loop
            Start_NS := Index;
            Encoding.Read (Schema_Location, Index, C);
            exit when not Is_White_Space (C);
         end loop;
      end loop;
   end Parse_Grammars;

   ---------------------------------
   -- Validate_Current_Characters --
   ---------------------------------

   procedure Validate_Current_Characters (Handler : Validating_Reader'Class) is
      Empty_Element : Boolean := False;
   begin
      --  If we were in the middle of a series of Characters callback, we need
      --  to process them now

      if Handler.Validators /= null then
         if Handler.Validators.Characters = null
           and then not Handler.Validators.Is_Nil
         then
            --  No character data => behave as an empty element, but we need to
            --  test explicitely. For instance, the "minLength" facet might or
            --  the "fixed" attribute need to test whether the empty string is
            --  valid.
            Handler.Validators.Characters := new Byte_Sequence'("");
            Empty_Element := True;
         end if;

         if Handler.Validators.Characters /= null then
            if Handler.Validators.Is_Nil then
               if not Empty_Element then
                  Free (Handler.Validators.Characters);
                  Validation_Error
                    ("Element has character data, but is declared as nil");
               end if;

            elsif Has_Fixed (Handler.Validators.Element) then
               if Handler.Validators.Characters.all /=
                 Get_Fixed (Handler.Validators.Element).all
               then
                  Free (Handler.Validators.Characters);
                  Validation_Error
                    ("Element's value must be """
                     & Get_Fixed (Handler.Validators.Element).all & """");
               end if;

            else
               Validate_Characters
                 (Get_Validator (Handler.Validators.Typ),
                  Handler.Validators.Characters.all,
                  Empty_Element => Empty_Element);
            end if;

            Free (Handler.Validators.Characters);
         end if;

         if Handler.Validators.Start_Loc /= null then
            Unref (Handler.Validators.Start_Loc);
         end if;
      end if;
   end Validate_Current_Characters;

   ------------------------
   -- Hook_Start_Element --
   ------------------------

   procedure Hook_Start_Element
     (Handler       : in out Reader'Class;
      Namespace_URI : Unicode.CES.Byte_Sequence := "";
      Local_Name    : Unicode.CES.Byte_Sequence := "";
      Qname         : Unicode.CES.Byte_Sequence := "";
      Atts          : in out Sax.Attributes.Attributes'Class)
   is
      pragma Unreferenced (Qname);
      Element   : XML_Element := No_Element;
      Data      : Validator_Data;
      Typ       : XML_Type;
      Is_Nil    : Boolean;

      procedure Get_Grammar_From_Attributes;
      --  Parse the grammar, reading its name from the attributes

      procedure Compute_Type;
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
            Parse_Grammar (Validating_Reader (Handler),
                           Get_Value (Atts, No_Index),
                           Add_To => Validating_Reader (Handler).Grammar);
         end if;

         if Location_Index /= -1 then
            Parse_Grammars (Validating_Reader (Handler),
                            Get_Value (Atts, Location_Index));
         end if;
      end Get_Grammar_From_Attributes;

      ------------------
      -- Compute_Type --
      ------------------

      procedure Compute_Type is
         Type_Index : constant Integer := Get_Index
           (Atts, URI => XML_Instance_URI, Local_Name => "type");
         G : XML_Grammar_NS;
         Had_Restriction, Had_Extension : Boolean := False;
      begin
         Typ := Get_Type (Element);

         if Type_Index /= -1 then
            if Debug then
               Put_Line ("Getting element definition from type attribute: "
                         & Get_Value (Atts, Type_Index));
            end if;

            declare
               Qname : constant Byte_Sequence := Get_Value (Atts, Type_Index);
               Separator : constant Integer := Split_Qname (Qname);
               Namespace : Byte_Sequence_Access;
            begin
               Namespace := Get_Namespace_From_Prefix
                 (Validating_Reader (Handler),
                  Qname (Qname'First .. Separator - 1));
               if Namespace /= null then
                  Get_NS
                    (Validating_Reader (Handler).Grammar, Namespace.all, G);
               else
                  Get_NS
                    (Validating_Reader (Handler).Grammar, Namespace_URI, G);
               end if;

               Typ := Lookup (G, Qname (Separator + 1 .. Qname'Last),
                              Create_If_Needed => False);
            end;

            if Typ = No_Type then
               Validation_Error
                 ("Unknown type """ & Get_Value (Atts, Type_Index) & '"');
            end if;

            if Get_Validator (Typ) /= Get_Validator (Get_Type (Element)) then
               Check_Replacement
                 (Get_Validator (Typ), Get_Type (Element),
                  Had_Restriction => Had_Restriction,
                  Had_Extension   => Had_Extension);

               if Had_Restriction
                 and then Get_Block_On_Restriction (Element)
               then
                  Validation_Error
                    ("Element """ & Get_Local_Name (Element)
                       & """ blocks the use of restrictions of the type");
               end if;

               if Had_Extension
                 and then Get_Block_On_Extension (Element)
               then
                  Validation_Error
                    ("Element """ & Get_Local_Name (Element)
                       & """ blocks the use of extensions of the type");
               end if;
            end if;
         end if;
      end Compute_Type;

      G : XML_Grammar_NS;
   begin
      if Debug then
         Put_Line (ASCII.ESC & "[33m"
                   & "Start_Element: " & Namespace_URI & ':' & Local_Name
                   & ASCII.ESC & "[39m");
      end if;

      Validate_Current_Characters (Validating_Reader (Handler));

      --  Get the name of the grammar to use from the element's attributes

      Get_Grammar_From_Attributes;

      if Validating_Reader (Handler).Grammar = No_Grammar then
         return;  --  Always valid, since we have no grammar anyway
      end if;

      --  Find out the namespace to use for the current element. This namespace
      --  can be guessed from the parent's element for validation purposes if
      --  we have an unqualified item. Whether qualification should be
      --  mandatory or not is tested later on

      if Namespace_URI = ""
        and then Validating_Reader (Handler).Validators /= null
      then
         G := Validating_Reader (Handler).Validators.Grammar;
      else
         Get_NS (Validating_Reader (Handler).Grammar,
                 Namespace_URI, Result => G);
      end if;

      --  Whether this element is valid in the current context

      if Validating_Reader (Handler).Validators /= null then
         Validate_Start_Element
           (Get_Validator (Validating_Reader (Handler).Validators.Typ),
            Local_Name, Namespace_URI, G,
            Validating_Reader (Handler).Validators.Data,
            Get_Target_NS (Validating_Reader (Handler).Grammar), Element);
      else
         if Debug then
            Put_Line ("Getting element definition from grammar: "
                      & Namespace_URI & " " & Local_Name);
         end if;
         Element := Lookup_Element (G, Local_Name, False);

         if Element = No_Element then
            Validation_Error
              ("Element """ & To_QName (Namespace_URI, Local_Name)
               & """: No matching declaration available");
         end if;

         Add_XML_Instance_Attributes
           (Validating_Reader (Handler), Get_Validator (Get_Type (Element)));
      end if;

      --  If not: this is a validation error

      if Element = No_Element then
         Validation_Error
           ("Unexpected element """ &
            To_QName (Namespace_URI, Local_Name) & """");
      end if;

      Compute_Type;
      Data := Create_Validator_Data (Get_Validator (Typ));

      Validate_Attributes
        (Get_Validator (Typ), Atts, Validating_Reader (Handler).Ids,
         Is_Nillable (Element), Is_Nil,
         Validating_Reader (Handler).Grammar);

      if Validating_Reader (Handler).Validators /= null
        and then Validating_Reader (Handler).Validators.Is_Nil
      then
         Validation_Error
           ("Element is set as nil, and doesn't accept any child element");
      end if;

      Push (Validating_Reader (Handler).Validators, Element,
            Typ, G, Data, Is_Nil);
   end Hook_Start_Element;

   ----------------------
   -- Hook_End_Element --
   ----------------------

   procedure Hook_End_Element
     (Handler       : in out Reader'Class;
      Namespace_URI : Unicode.CES.Byte_Sequence := "";
      Local_Name    : Unicode.CES.Byte_Sequence := "";
      Qname         : Unicode.CES.Byte_Sequence := "")
   is
      pragma Unreferenced (Namespace_URI);
   begin
      if Debug then
         Put_Line (ASCII.ESC & "[33m"
                   & "End_Element: " & Local_Name
                   & ASCII.ESC & "[39m");
      end if;

      if Validating_Reader (Handler).Validators /= null then
         Validate_Current_Characters (Validating_Reader (Handler));

         --  Do not check if the element is nil, since no child is expected
         --  anyway, and some validators (sequence,...) will complain
         if not Validating_Reader (Handler).Validators.Is_Nil then
            Validate_End_Element
              (Get_Validator (Validating_Reader (Handler).Validators.Typ),
               Qname,
               Validating_Reader (Handler).Validators.Data);
         end if;
      end if;
      Pop (Validating_Reader (Handler).Validators);
   end Hook_End_Element;

   -------------------------
   -- Internal_Characters --
   -------------------------

   procedure Internal_Characters
     (Handler : in out Validating_Reader;
      Ch      : Unicode.CES.Byte_Sequence)
   is
      Tmp : Byte_Sequence_Access;
   begin
      if Handler.Validators /= null then
         if Debug then
            Put_Line ("Appending characters --" & Ch & "--");
         end if;
         if Handler.Validators.Characters = null then
            Handler.Validators.Characters := new String'(Ch);
            Handler.Validators.Start_Loc := new Locator_Impl;
            Copy (Handler.Validators.Start_Loc.all, Handler.Locator.all);
         else
            Tmp := new String'(Handler.Validators.Characters.all & Ch);
            Free (Handler.Validators.Characters);
            Handler.Validators.Characters := Tmp;
         end if;
      end if;
   end Internal_Characters;

   ---------------------
   -- Hook_Characters --
   ---------------------

   procedure Hook_Characters
     (Handler : in out Reader'Class;
      Ch      : Unicode.CES.Byte_Sequence) is
   begin
      Internal_Characters (Validating_Reader (Handler), Ch);
   end Hook_Characters;

   -------------------------------
   -- Hook_Ignorable_Whitespace --
   -------------------------------

   procedure Hook_Ignorable_Whitespace
     (Handler : in out Reader'Class;
      Ch      : Unicode.CES.Byte_Sequence) is
   begin
      if Validating_Reader (Handler).Validators /= null
        and then Is_Simple_Type
          (Get_Type (Validating_Reader (Handler).Validators.Element))
        and then not Validating_Reader (Handler).Validators.Is_Nil
      then
         Internal_Characters (Validating_Reader (Handler), Ch);
      end if;
   end Hook_Ignorable_Whitespace;

   -----------
   -- Reset --
   -----------

   procedure Reset (Parser : in out Validating_Reader) is
   begin
      if Parser.Locator /= null then
         Unref (Parser.Locator.all);
      end if;

      Free  (Parser.Ids);
      Clear (Parser.Validators);
      Free  (Parser.Prefixes);
   end Reset;

   -----------
   -- Parse --
   -----------

   procedure Parse
     (Parser : in out Validating_Reader;
      Input  : in out Input_Sources.Input_Source'Class)
   is
      Loc : aliased Locator_Impl;
   begin
      if Get_Feature (Parser, Schema_Validation_Feature) then
         Set_Hooks (Parser,
                    Start_Element => Hook_Start_Element'Access,
                    End_Element   => Hook_End_Element'Access,
                    Characters    => Hook_Characters'Access,
                    Whitespace    => Hook_Ignorable_Whitespace'Access,
                    Start_Prefix  => Hook_Start_Prefix_Mapping'Access,
                    End_Prefix    => Hook_End_Prefix_Mapping'Access,
                    Doc_Locator   => Hook_Set_Document_Locator'Access);
      else
         Set_Hooks (Parser,
                    Start_Element => null,
                    End_Element   => null,
                    Characters    => null,
                    Whitespace    => null,
                    Start_Prefix  => null,
                    End_Prefix    => null,
                    Doc_Locator   => null);
      end if;

      if Parser.Grammar = No_Grammar then
         --  Make sure predefined types are known
         Initialize (Parser.Grammar);
      end if;

      Sax.Readers.Parse (Sax.Readers.Reader (Parser), Input);

      Reset (Parser);

   exception
      when E : XML_Validation_Error =>
         if Parser.Validators /= null
           and then Parser.Validators.Start_Loc /= null
         then
            Copy (Loc, Parser.Validators.Start_Loc.all);
         else
            Copy (Loc, Parser.Locator.all);
         end if;

         Reset (Parser);

         Validation_Error
           (Parser, Create (Byte_Sequence (Exception_Message (E)),
            Loc'Unchecked_Access));

      when others =>
         Reset (Parser);
         raise;
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

   -------------------------------
   -- Hook_Set_Document_Locator --
   -------------------------------

   procedure Hook_Set_Document_Locator
     (Handler : in out Reader'Class;
      Loc     : access Sax.Locators.Locator'Class) is
   begin
      Validating_Reader (Handler).Locator := Locator_Access (Loc);
      Ref (Validating_Reader (Handler).Locator.all);
   end Hook_Set_Document_Locator;

   -------------------------------
   -- Hook_Start_Prefix_Mapping --
   -------------------------------

   procedure Hook_Start_Prefix_Mapping
     (Handler : in out Reader'Class;
      Prefix  : Unicode.CES.Byte_Sequence;
      URI     : Unicode.CES.Byte_Sequence) is
   begin
      Validating_Reader (Handler).Prefixes := new Prefix_Mapping'
        (Prefix    => new Byte_Sequence'(Prefix),
         Namespace => new Byte_Sequence'(URI),
         Next      => Validating_Reader (Handler).Prefixes);
   end Hook_Start_Prefix_Mapping;

   -----------------------------
   -- Hook_End_Prefix_Mapping --
   -----------------------------

   procedure Hook_End_Prefix_Mapping
     (Handler : in out Reader'Class;
      Prefix  : Unicode.CES.Byte_Sequence)
   is
      Tmp  : Prefix_Mapping_Access := Validating_Reader (Handler).Prefixes;
      Tmp2 : Prefix_Mapping_Access;
   begin
      if Validating_Reader (Handler).Prefixes /= null then
         if Validating_Reader (Handler).Prefixes.Prefix.all = Prefix then
            Validating_Reader (Handler).Prefixes :=
              Validating_Reader (Handler).Prefixes.Next;
            Free (Tmp);
         else
            while Tmp.Next /= null
              and then Tmp.Next.Prefix.all /= Prefix
            loop
               Tmp := Tmp.Next;
            end loop;

            if Tmp.Next /= null then
               Tmp2 := Tmp.Next;
               Tmp.Next := Tmp2.Next;
               Free (Tmp2);
            end if;
         end if;
      end if;
   end Hook_End_Prefix_Mapping;

   -------------------------------
   -- Get_Namespace_From_Prefix --
   -------------------------------

   function Get_Namespace_From_Prefix
     (Handler  : Validating_Reader;
      Prefix   : Unicode.CES.Byte_Sequence)
      return Unicode.CES.Byte_Sequence_Access
   is
      Tmp : Prefix_Mapping_Access := Handler.Prefixes;
   begin
      while Tmp /= null and then Tmp.Prefix.all /= Prefix loop
         Tmp := Tmp.Next;
      end loop;

      if Tmp = null then
         return null;
      else
         return Tmp.Namespace;
      end if;
   end Get_Namespace_From_Prefix;

end Schema.Readers;
