with Unicode.CES;       use Unicode.CES;
with Sax.Attributes;    use Sax.Attributes;
with Sax.Exceptions;    use Sax.Exceptions;
with Sax.Locators;      use Sax.Locators;
with Schema.Validators; use Schema.Validators;
with Ada.Exceptions;    use Ada.Exceptions;
with Ada.Unchecked_Deallocation;
with Schema.Validators; use Schema.Validators;
with GNAT.IO; use GNAT.IO;

package body Schema.Readers is

   Schema_URI : constant Byte_Sequence := "http://www.w3.org/2001/XMLSchema";
   Str_Id                   : constant Byte_Sequence := "id";
   Str_Name                 : constant Byte_Sequence := "name";

   Str_List                 : constant Byte_Sequence := "list";
   Str_Restriction          : constant Byte_Sequence := "restriction";
   Str_Union                : constant Byte_Sequence := "union";

   Str_Include              : constant Byte_Sequence := "include";
   Str_Import               : constant Byte_Sequence := "import";
   Str_Redefine             : constant Byte_Sequence := "redefine";
   Str_Annotation           : constant Byte_Sequence := "annotation";
   Str_Attribute            : constant Byte_Sequence := "attribute";
   Str_Attribute_Group      : constant Byte_Sequence := "attributeGroup";
   Str_Complex_Type         : constant Byte_Sequence := "complexType";
   Str_Element              : constant Byte_Sequence := "element";
   Str_Group                : constant Byte_Sequence := "group";
   Str_Notation             : constant Byte_Sequence := "notation";
   Str_Simple_Type          : constant Byte_Sequence := "simpleType";

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Validator_List_Record, Validator_List);

   procedure Push  (List      : in out Validator_List;
                    Validator : Type_Validator;
                    Data      : Validator_Data);
   procedure Pop   (List : in out Validator_List);
   procedure Clear (List : in out Validator_List);
   --  Push or remove validators from the list

   type Schema_Node_Validator is new Type_Validator_Record with null record;
--     function Validate_Attributes
--       (Validator : access Schema_Node_Validator;
--        Atts      : Sax.Attributes.Attributes'Class) return Validator_Status;
   procedure Validate_Start_Element
     (Validator         : access Schema_Node_Validator;
      Local_Name        : Unicode.CES.Byte_Sequence;
      Data              : Validator_Data;
      Element_Validator : out Type_Validator);
   --  Validate the <schema> node contents

   function Create_Schema_Node_Validator
     (Grammar : XML_Grammar) return Type_Validator;
   --  Create a new validator for <schema> node

   type Simple_Type_Validator is new Type_Validator_Record with null record;
   procedure Validate_Attributes
     (Validator : access Simple_Type_Validator;
      Atts      : Sax.Attributes.Attributes'Class;
      Data      : Validator_Data);
   procedure Validate_Start_Element
     (Validator         : access Simple_Type_Validator;
      Local_Name        : Unicode.CES.Byte_Sequence;
      Data              : Validator_Data;
      Element_Validator : out Type_Validator);
   --  Validate the <simpleType> node contents

   ------------
   -- Create --
   ------------

   function Create (Grammar : XML_Grammar) return Validating_Reader is
   begin
      return Validating_Reader'
        (Sax.Readers.Reader with
         Grammar    => Grammar,
         Locator    => null,
         Validators => null);
   end Create;

   ----------
   -- Push --
   ----------

   procedure Push
     (List      : in out Validator_List;
      Validator : Type_Validator;
      Data      : Validator_Data) is
   begin
      List := new Validator_List_Record'
        (Validator => Validator,
         Data      => Data,
         Next      => List);
   end Push;

   ---------
   -- Pop --
   ---------

   procedure Pop (List : in out Validator_List) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Validator_Data_Record'Class, Validator_Data);
      Tmp : Validator_List := List;
   begin
      if List /= null then
         if List.Data /= null then
            Free (List.Data.all);
            Unchecked_Free (List.Data);
         end if;

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
      pragma Unreferenced (Namespace_URI, Qname);
      Validator : Type_Validator;
      Data      : Validator_Data;
   begin
      if Handler.Validators /= null then
         Validate_Start_Element
           (Handler.Validators.Validator, Local_Name,
            Handler.Validators.Data, Validator);
      end if;

      if Validator = null then
         Validator := Lookup_Element (Handler.Grammar, Local_Name);
      end if;

      if Validator = null then
         Raise_Exception
           (XML_Validation_Error'Identity,
            "No data type definition for elements " & String (Local_Name));
      end if;

      Data := Create_Validator_Data (Validator);
      Validate_Attributes (Validator, Atts, Data);
      Push (Handler.Validators, Clone (Validator.all), Data);
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
         Validate_End_Element
           (Handler.Validators.Validator,
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
         Validate_Characters
           (Handler.Validators.Validator, Ch, Handler.Validators.Data);
      end if;
   end Characters;

   -----------------
   -- Get_Grammar --
   -----------------

   function Get_Grammar
     (Reader : Schema_Reader) return Schema.Validators.XML_Grammar is
   begin
      return Reader.Grammar;
   end Get_Grammar;

   ----------------------------------
   -- Create_Schema_Node_Validator --
   ----------------------------------

   function Create_Schema_Node_Validator
     (Grammar : XML_Grammar) return Type_Validator
   is
      formChoice                 : Type_Validator;
      blockSet                   : Type_Validator;
      All_Validator              : Type_Validator;
      Derivation_Control         : Type_Validator;
      reducedDerivationControl   : Type_Validator;
      derivationSet              : Type_Validator;
      Validator  : constant Type_Validator := new Schema_Node_Validator;
   begin
      --  The "formChoice" type of schema.xsd
      formChoice := Lookup_Type (Grammar, "NMTOKEN");
      Add_Restriction (formChoice, "enumeration", "qualified");
      Add_Restriction (formChoice, "enumeration", "unqualified");

      Derivation_Control := Lookup_Type (Grammar, "NMTOKEN");
      Add_Restriction (Derivation_Control, "enumeration", "substitution");
      Add_Restriction (Derivation_Control, "enumeration", "extension");
      Add_Restriction (Derivation_Control, "enumeration", "restriction");

      All_Validator := Lookup_Type (Grammar, "token");
      Add_Restriction (All_Validator, "enumeration", "#all");

      --  The "blockSet" type of schema.xsd
      blockSet := Lookup_Type (Grammar, "anyType");
      Add_Union (blockSet, All_Validator);
      Add_Union (blockSet, List_Of (Derivation_Control));

      --  The "reducedDerivationControl" type of schema.xsd
      reducedDerivationControl := Lookup_Type (Grammar, "NMTOKEN");
      Add_Restriction (reducedDerivationControl, "enumeration", "extension");
      Add_Restriction
        (reducedDerivationControl, "enumeration", "restriction");

      --  The "derivationSet" type of schema.xsd
      derivationSet := Lookup_Type (Grammar, "anyType");
      Add_Union (derivationSet, All_Validator);
      Add_Union (derivationSet, reducedDerivationControl);

      Add_Attribute
        (Validator, Create_Attribute_Validator
           (Name           => "attributeFormDefault",
            Attribute_Type => formChoice,
            Attribute_Use  => Default,
            Value          => "unqualified"));
      Add_Attribute
        (Validator, Create_Attribute_Validator
           (Name              => "blockDefault",
            Attribute_Type    => blockSet,
            Attribute_Use     => Default,
            Value             => ""));
      Add_Attribute
        (Validator, Create_Attribute_Validator
           (Name              => "elementFormDefault",
            Attribute_Type    => formChoice,
            Attribute_Use     => Default,
            Value             => "unqualified"));
      Add_Attribute
        (Validator, Create_Attribute_Validator
           (Name              => "finalDefault",
            Attribute_Type    => derivationSet,
            Attribute_Use     => Default,
            Value             => ""));
      Add_Attribute
        (Validator, Create_Attribute_Validator
           (Name              => "id",
            Attribute_Type    => Lookup_Type (Grammar, "ID")));
      Add_Attribute
        (Validator, Create_Attribute_Validator
           (Name              => "targetNamespace",
            Attribute_Type    => Lookup_Type (Grammar, "uriReference")));
      Add_Attribute
        (Validator, Create_Attribute_Validator
           (Name              => "version",
            Attribute_Type    => Lookup_Type (Grammar, "token")));

      return Validator;
   end Create_Schema_Node_Validator;

   -------------------------
   -- Validate_Attributes --
   -------------------------

--     function Validate_Attributes
--       (Validator : access Schema_Node_Validator;
--        Atts      : Sax.Attributes.Attributes'Class) return Validator_Status
--     is
--        pragma Unreferenced (Validator);
--        Length : constant Natural := Get_Length (Atts);
--     begin
--        for A in 0 .. Length - 1 loop
--           declare
--              Name : constant Byte_Sequence := Get_Local_Name (Atts, A);
--              URI  : constant Byte_Sequence := Get_URI (Atts, A);
--           begin
--              if URI = ""
--                or else URI = Schema_URI
--              then
--                 if Name = Str_Attribute_Form_Default_Attribute then
--                  Put_Line ("No handling of attributeFormDefault attribute");
--
--                 elsif Name = Str_Block_Default then
--                    Put_Line ("No handling of blockDefault attribute");
--
--                 elsif Name = Str_Element_Form_Default then
--                    Put_Line ("No handling of elementFormDefault attribute");
--
--                 elsif Name = Str_Final_Default then
--                    Put_Line ("No handling of finalDefault attribute");
--
--                 elsif Name = Str_Id then
--                    Put_Line ("No handling of id attribute");
--
--                 elsif Name = Str_Target_Namespace then
--                    Put_Line ("No handling of targetNamespace attribute");
--
--                 elsif Name = Str_Version then
--                    Put_Line ("No handling of version attribute");
--
--                 else
--                    Put_Line ("Invalid attribut for <schema> element: "
--                              & URI & ':' & Name);
--                    return Undefined_Error;
--                 end if;
--              end if;
--           end;
--        end loop;
--
--        return No_Error;
--     end Validate_Attributes;

   ----------------------------
   -- Validate_Start_Element --
   ----------------------------

   procedure Validate_Start_Element
     (Validator         : access Schema_Node_Validator;
      Local_Name        : Unicode.CES.Byte_Sequence;
      Data              : Validator_Data;
      Element_Validator : out Type_Validator)
   is
      pragma Unreferenced (Validator, Data);
   begin
      if Local_Name = Str_Simple_Type then
         null;

      elsif Local_Name = Str_Include
        or else Local_Name = Str_Import
        or else Local_Name = Str_Redefine
        or else Local_Name = Str_Annotation
        or else Local_Name = Str_Attribute
        or else Local_Name = Str_Attribute_Group
        or else Local_Name = Str_Complex_Type
        or else Local_Name = Str_Element
        or else Local_Name = Str_Group
        or else Local_Name = Str_Notation
      then
         Raise_Exception
           (XML_Validation_Error'Identity,
            "No handling for <" & Local_Name & "> yet");

      else
         Raise_Exception
           (XML_Validation_Error'Identity,
            "Unexpected element: " & Local_Name);
      end if;

      Element_Validator := null;
   end Validate_Start_Element;

   -------------------------
   -- Validate_Attributes --
   -------------------------

   procedure Validate_Attributes
     (Validator : access Simple_Type_Validator;
      Atts      : Sax.Attributes.Attributes'Class;
      Data      : Validator_Data)
   is
      pragma Unreferenced (Validator, Data);
      Length : constant Natural := Get_Length (Atts);
   begin
      for A in 0 .. Length - 1 loop
         declare
            Name : constant Byte_Sequence := Get_Local_Name (Atts, A);
            URI  : constant Byte_Sequence := Get_URI (Atts, A);
         begin
            if URI = ""
              or else URI = Schema_URI
            then
               if Name = Str_Id then
                  Put_Line ("No handling for ID attribute yet");
               elsif Name = Str_Name then
                  Put_Line ("No handling for Name attribute yet");
               else
                  Raise_Exception
                    (XML_Validation_Error'Identity,
                     "Invalid attribut for <simpleType> element: "
                     & URI & ':' & Name);
               end if;
            end if;
         end;
      end loop;
   end Validate_Attributes;

   ----------------------------
   -- Validate_Start_Element --
   ----------------------------

   procedure Validate_Start_Element
     (Validator         : access Simple_Type_Validator;
      Local_Name        : Unicode.CES.Byte_Sequence;
      Data              : Validator_Data;
      Element_Validator : out Type_Validator)
   is
      pragma Unreferenced (Validator, Data);
   begin
      if Local_Name = Str_Annotation
        or else Local_Name = Str_List
        or else Local_Name = Str_Restriction
        or else Local_Name = Str_Union
      then
         Raise_Exception
           (XML_Validation_Error'Identity,
            "No handling for <" & Local_Name & "> yet");
      else
         Raise_Exception
           (XML_Validation_Error'Identity, "Undefined error");
      end if;
      Element_Validator := null;
   end Validate_Start_Element;

   --------------------
   -- Start_Document --
   --------------------

   procedure Start_Document (Handler : in out Schema_Reader) is
   begin
      Clear (Handler.Current_Element);
      Free (Handler.Grammar);
      Initialize (Handler.Grammar);

      Handler.Current_Element := new Validator_List_Record'
        (Validator => Create_Schema_Node_Validator (Handler.Grammar),
         Data      => null,
         Next      => null);
   end Start_Document;

   -------------------
   -- Start_Element --
   -------------------

   procedure Start_Element
     (Handler       : in out Schema_Reader;
      Namespace_URI : Unicode.CES.Byte_Sequence := "";
      Local_Name    : Unicode.CES.Byte_Sequence := "";
      Qname         : Unicode.CES.Byte_Sequence := "";
      Atts          : Sax.Attributes.Attributes'Class)
   is
      pragma Unreferenced (Qname);
      Next_Element : Type_Validator;
   begin
      if Namespace_URI /= Schema_URI then
         Raise_Exception
           (XML_Validation_Error'Identity,
            "All elements must be in the schema namespace");

      elsif Handler.Current_Element = null then
         if Local_Name /= "schema" then
            Raise_Exception
              (XML_Validation_Error'Identity,
               "Root element must be <schema>");
         end if;

         Push (Handler.Current_Element, new Schema_Node_Validator, null);

      else
         Validate_Start_Element
           (Handler.Current_Element.Validator, Local_Name,
            Handler.Current_Element.Data, Next_Element);

         if Next_Element = null
           and then Local_Name = Str_Simple_Type
         then
            Next_Element := new Simple_Type_Validator;
         end if;

         Push (Handler.Current_Element, Next_Element,
               Create_Validator_Data (Next_Element));
      end if;

      Validate_Attributes (Handler.Current_Element.Validator, Atts,
                           Handler.Current_Element.Data);
   end Start_Element;

   -----------------
   -- End_Element --
   -----------------

   procedure End_Element
     (Handler       : in out Schema_Reader;
      Namespace_URI : Unicode.CES.Byte_Sequence := "";
      Local_Name    : Unicode.CES.Byte_Sequence := "";
      Qname         : Unicode.CES.Byte_Sequence := "")
   is
      pragma Unreferenced (Qname, Namespace_URI, Local_Name);
   begin
      Pop (Handler.Current_Element);
   end End_Element;

   ----------------
   -- Characters --
   ----------------

   procedure Characters
     (Handler : in out Schema_Reader; Ch : Unicode.CES.Byte_Sequence)
   is
      pragma Unreferenced (Handler, Ch);
   begin
      null;
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
