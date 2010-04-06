-----------------------------------------------------------------------
--                XML/Ada - An XML suite for Ada95                   --
--                                                                   --
--                Copyright (C) 2003-2010, AdaCore                   --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

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

   procedure Validate_Current_Characters
     (Handler : in out Validating_Reader'Class);
   --  Validate the current set of characters

   procedure Reset (Parser : in out Validating_Reader);
   --  Reset the state of the parser so that we can parse other documents.
   --  This doesn't reset the grammar

   procedure Hook_Start_Element
     (Handler       : in out Reader'Class;
      Namespace_URI : Unicode.CES.Byte_Sequence := "";
      Local_Name    : Unicode.CES.Byte_Sequence := "";
      Qname         : Unicode.CES.Byte_Sequence := "";
      Elem          : Element_Access;
      Atts          : in out Sax.Attributes.Attributes'Class);
   procedure Hook_End_Element
     (Handler       : in out Reader'Class;
      Namespace_URI : Unicode.CES.Byte_Sequence := "";
      Local_Name    : Unicode.CES.Byte_Sequence := "";
      Qname         : Unicode.CES.Byte_Sequence := "";
      Elem          : Element_Access);
   procedure Hook_Characters
     (Handler : in out Reader'Class; Ch : Unicode.CES.Byte_Sequence);
   procedure Hook_Ignorable_Whitespace
     (Handler : in out Reader'Class;
      Ch      : Unicode.CES.Byte_Sequence);
   procedure Hook_Set_Document_Locator
     (Handler : in out Reader'Class;
      Loc     : in out Sax.Locators.Locator);
   --  See for the corresponding primitive operations. These provide the
   --  necessary validation hooks.

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
      Reader.Context.Grammar := Grammar;
   end Set_Validating_Grammar;

   ----------------------------
   -- Get_Validating_Grammar --
   ----------------------------

   function Get_Validating_Grammar
     (Reader : Validating_Reader) return Schema.Validators.XML_Grammar is
   begin
      return Reader.Context.Grammar;
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
         Start_Loc  => No_Locator,
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
      Get_NS (Handler.Context.Grammar, XML_Instance_URI, Result => XML_IG);

      if not Has_Attribute (Validator, XML_IG, "type") then
         Get_NS (Handler.Context.Grammar, XML_Schema_URI, Result => XML_G);

         Add_Attribute
           (Validator,
            Create_Local_Attribute ("type", XML_IG, Lookup (XML_G, "string")),
            Is_Local => True);
         Add_Attribute
           (Validator,
            Create_Local_Attribute ("nil", XML_IG, Lookup (XML_G, "boolean")),
            Is_Local => True);
         Add_Attribute
           (Validator,
            Create_Local_Attribute ("schemaLocation", XML_IG,
              List_Of (XML_G, Lookup (XML_G, "string"))),
            Is_Local => True);
         Add_Attribute
           (Validator,
            Create_Local_Attribute ("noNamespaceSchemaLocation", XML_IG,
              Lookup (XML_G, "string")),
            Is_Local => True);
      end if;
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
         return Dir_Name (Get_System_Id (Handler.Locator)) & URI;
      else
         return URI;
      end if;
   end To_Absolute_URI;

   -------------------
   -- Parse_Grammar --
   -------------------

   procedure Parse_Grammar
     (Handler  : in out Validating_Reader;
      URI      : Byte_Sequence;
      Xsd_File : Byte_Sequence;
      Add_To   : in out XML_Grammar)
   is
      File     : File_Input;
      Schema   : Schema_Reader;
      Xsd_File_Full : constant Byte_Sequence :=
        To_Absolute_URI (Handler, Xsd_File);
      Local_Grammar : XML_Grammar_NS;
   begin
      if Debug then
         Put_Line ("NS=" & URI & ASCII.LF & "XSD=" & Xsd_File);
      end if;

      if URI /= "-" then
         Get_NS
           (Handler.Context.Grammar,
            Namespace_URI    => URI,
            Result           => Local_Grammar,
            Create_If_Needed => False);

         if Get_XSD_Version (Handler.Context.Grammar) = XSD_1_0 then
            --  Must check that no element of the same namespace was seen
            --  already (as per 4.3.2 (4) in the XSD 1.0 norm, which was
            --  changed in XSD 1.1).

            declare
               NS : XML_NS;
            begin
               Find_NS_From_URI
                 (Handler,
                  Context => Handler.Context.Context,
                  URI     => URI,
                  NS      => NS);

               if NS /= No_XML_NS
                 and then Element_Count (NS) > 0
                 and then Xsd_File_Full /= Get_System_Id (Local_Grammar)
               then
                  Validation_Error
                    ("schemaLocation for """
                     & URI & """ cannot occur after the first"
                     & " element of that namespace in XSD 1.0");
               end if;
            end;
         end if;
      end if;

      --  Do not reparse the grammar if we already know about it

      if Local_Grammar = null then
         if Debug then
            Put_Line ("Parsing grammar: " & Xsd_File_Full);
            Debug_Dump (Add_To);
         end if;
         Open (Xsd_File_Full, File);
         Set_Public_Id (File, Xsd_File_Full);
         Set_System_Id (File, Xsd_File_Full);

         --  MANU ? More efficient: Add_To will likely already contain the
         --  grammar for the schema-for-schema, and we won't have to recreate
         --  it in most
         --  cases.
         Set_Validating_Grammar (Schema, Add_To);
         Set_Created_Grammar (Schema, Add_To);
         Use_Basename_In_Error_Messages
           (Schema, Use_Basename_In_Error_Messages (Handler));
         Parse (Schema, File);
         Close (File);
         Add_To := Get_Created_Grammar (Schema);

         if Debug then
            Put_Line ("Done parsing new grammar: " & Xsd_File);
         end if;
      end if;

   exception
      when Ada.IO_Exceptions.Name_Error =>
         Close (File);

         --  According to XML Schema Primer 0, section 5.6, this is not an
         --  error when we do not find the schema, since this attribute is only
         --  a hint.
         Warning
           (Handler,
            Create (Message => "Could not open file " & Xsd_File_Full,
                    Loc     => Handler.Locator));
      when others =>
         Close (File);
         raise;
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

         Parse_Grammar
           (Handler,
            URI      => Schema_Location (Start_NS .. Last_NS - 1),
            Xsd_File => Schema_Location (Start_XSD .. Last_XSD - 1),
            Add_To   => Handler.Context.Grammar);

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

   procedure Validate_Current_Characters
     (Handler : in out Validating_Reader'Class)
   is
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

            if Has_Default (Handler.Validators.Element) then
               Handler.Validators.Characters := new Byte_Sequence'
                 (Get_Default (Handler.Validators.Element).all);
               Characters
                 (Handler, Get_Default (Handler.Validators.Element).all);
            else
               Handler.Validators.Characters := new Byte_Sequence'("");
               Empty_Element := True;
            end if;
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
                  Empty_Element => Empty_Element,
                  Context       => Validating_Reader (Handler).Context);
            end if;

            Free (Handler.Validators.Characters);
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
      Elem          : Element_Access;
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
            Parse_Grammar
              (Validating_Reader (Handler),
               URI      => "",
               Xsd_File => Get_Value (Atts, No_Index),
               Add_To   => Validating_Reader (Handler).Context.Grammar);
            Global_Check (Validating_Reader (Handler).Context.Grammar);
         end if;

         if Location_Index /= -1 then
            Parse_Grammars (Validating_Reader (Handler),
                            Get_Value (Atts, Location_Index));
            Global_Check (Validating_Reader (Handler).Context.Grammar);
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
               NS        : XML_NS;
            begin
               Get_Namespace_From_Prefix
                 (Validating_Reader (Handler),
                  Qname (Qname'First .. Separator - 1), NS);
               Get_NS (Validating_Reader (Handler).Context.Grammar,
                       Get_URI (NS), G);
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

      Validating_Reader (Handler).Context.Context := Elem;

      Validate_Current_Characters (Validating_Reader (Handler));

      --  Get the name of the grammar to use from the element's attributes

      Get_Grammar_From_Attributes;

      if Validating_Reader (Handler).Context.Grammar = No_Grammar then
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
         Get_NS (Validating_Reader (Handler).Context.Grammar,
                 Namespace_URI, Result => G);
      end if;

      --  Whether this element is valid in the current context

      if Validating_Reader (Handler).Validators /= null then
         if Debug then
            Put_Line ("Using parent's validator to validate the element");
         end if;

         Validate_Start_Element
           (Get_Validator (Validating_Reader (Handler).Validators.Typ),
            Local_Name, Namespace_URI, G,
            Validating_Reader (Handler).Validators.Data,
            Validating_Reader (Handler).Context.Grammar, Element);

         --  If not: this is a validation error

         if Element = No_Element then
            Validation_Error
              ("Unexpected element """ &
               To_QName (Namespace_URI, Local_Name) & """");
         end if;

      else
         if Debug then
            Put_Line ("Parent node defines no validator, lookup in grammar: "
                      & Namespace_URI & " " & Local_Name);
         end if;
         Element := Lookup_Element (G, Local_Name, False);

         if Element = No_Element then
            Validation_Error
              ("Element """ & To_QName (Namespace_URI, Local_Name)
               & """: No matching declaration available");
         end if;

         --  The toplevel elements has special attributes to point to the XSD
         --  grammar for instance. This cannot be done automatically when
         --  parsing the grammar, since there is no reference there as to which
         --  node is the root.
         Add_XML_Instance_Attributes
           (Validating_Reader (Handler), Get_Validator (Get_Type (Element)));
      end if;

      Compute_Type;
      Data := Create_Validator_Data (Get_Validator (Typ));

      Validate_Attributes
        (Get_Validator (Typ), Atts,
         Is_Nillable (Element), Is_Nil,
         Validating_Reader (Handler).Context);

      if Validating_Reader (Handler).Validators /= null
        and then Validating_Reader (Handler).Validators.Is_Nil
      then
         Validation_Error
           ("Element is set as nil, and doesn't accept any child element");
      end if;

      Push (Validating_Reader (Handler).Validators, Element,
            Typ, G, Data, Is_Nil);

   exception
      when others =>
         Free (Data);
         raise;
   end Hook_Start_Element;

   ----------------------
   -- Hook_End_Element --
   ----------------------

   procedure Hook_End_Element
     (Handler       : in out Reader'Class;
      Namespace_URI : Unicode.CES.Byte_Sequence := "";
      Local_Name    : Unicode.CES.Byte_Sequence := "";
      Qname         : Unicode.CES.Byte_Sequence := "";
      Elem          : Element_Access)
   is
      pragma Unreferenced (Namespace_URI);
   begin
      if Debug then
         Put_Line (ASCII.ESC & "[33m"
                   & "End_Element: " & Local_Name
                   & ASCII.ESC & "[39m");
      end if;

      Validating_Reader (Handler).Context.Context := Elem;

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
            Copy (Handler.Validators.Start_Loc, Handler.Locator);
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
      Parser.Locator := No_Locator;
      Free (Parser.Context.Id_Table);
      Clear (Parser.Validators);
   end Reset;

   -----------
   -- Parse --
   -----------

   procedure Parse
     (Parser : in out Validating_Reader;
      Input  : in out Input_Sources.Input_Source'Class)
   is
      Loc : aliased Locator;
   begin
      if Get_Feature (Parser, Schema_Validation_Feature) then
         Set_Hooks (Parser,
                    Start_Element => Hook_Start_Element'Access,
                    End_Element   => Hook_End_Element'Access,
                    Characters    => Hook_Characters'Access,
                    Whitespace    => Hook_Ignorable_Whitespace'Access,
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

      Parser.Context.Parser := Parser'Unrestricted_Access;

      if Parser.Context.Grammar = No_Grammar then
         --  Make sure predefined types are known
         Initialize (Parser.Context.Grammar);
      end if;

      Sax.Readers.Parse (Sax.Readers.Reader (Parser), Input);

      Reset (Parser);

   exception
      when E : XML_Validation_Error =>
         if Parser.Validators /= null
           and then Parser.Validators.Start_Loc /= No_Locator
         then
            Copy (Loc, Parser.Validators.Start_Loc);
         else
            Copy (Loc, Parser.Locator);
         end if;

         Reset (Parser);

         Validation_Error
           (Parser, Create (Byte_Sequence (Exception_Message (E)),
            Loc));

      when others =>
         Reset (Parser);
         raise;
   end Parse;

   ----------------------
   -- Validation_Error --
   ----------------------

   procedure Validation_Error
     (Reader : in out Validating_Reader;
      Except : Sax.Exceptions.Sax_Parse_Exception'Class)
   is
      pragma Unmodified (Reader);
   begin
      Validation_Error
        (To_String (Get_Locator (Except),
         Use_Basename_In_Error_Messages (Reader))
         & ": " & String (Get_Message (Except)));
   end Validation_Error;

   -------------------------------
   -- Hook_Set_Document_Locator --
   -------------------------------

   procedure Hook_Set_Document_Locator
     (Handler : in out Reader'Class;
      Loc     : in out Sax.Locators.Locator) is
   begin
      Validating_Reader (Handler).Locator := Loc;
   end Hook_Set_Document_Locator;

   -----------------
   -- Get_Context --
   -----------------

   function Get_Context
     (Handler : access Validating_Reader)
      return Schema.Validators.Validation_Context_Access is
   begin
      --  ??? This is dangerous
      return Handler.Context'Unrestricted_Access;
   end Get_Context;

   -------------------------------
   -- Get_Namespace_From_Prefix --
   -------------------------------

   procedure Get_Namespace_From_Prefix
     (Handler  : in out Validating_Reader;
      Prefix   : Unicode.CES.Byte_Sequence;
      NS       : out Sax.Readers.XML_NS) is
   begin
      Find_NS
        (Parser  => Handler,
         Context => Handler.Context.Context,
         Prefix  => Prefix,
         NS      => NS);
      if Get_URI (NS) = "" then
         NS := No_XML_NS;
      end if;
   end Get_Namespace_From_Prefix;
end Schema.Readers;
