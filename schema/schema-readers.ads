
with Sax.Attributes;
with Sax.Exceptions;
with Sax.Locators;
with Input_Sources;
with Sax.Readers;
with Unicode.CES;
with Schema.Validators;

package Schema.Readers is

   type Validating_Reader is new Sax.Readers.Reader with private;

   procedure Set_Validating_Grammar
     (Reader  : in out Validating_Reader;
      Grammar : Schema.Validators.XML_Grammar);
   --  Create an XML reader that will validate its input file. The grammar
   --  must have been parsed first.
   --  If other schema files need to be parsed because of the presence of a
   --  "targetNamespace" attribute, their corresponding grammars will be added
   --  to grammar, in their own namespace of course.

   procedure Validation_Error
     (Reader : in out Validating_Reader;
      Except  : Sax.Exceptions.Sax_Parse_Exception'Class);
   --  Called when a validation error occurs.
   --  By default, this raises XML_Validation_Error

   procedure Set_Debug_Output (Output : Boolean);
   --  Whether we should output debug traces

   procedure Parse_Grammar
     (Handler  : in out Validating_Reader;
      Xsd_File : Unicode.CES.Byte_Sequence;
      Add_To   : in out Schema.Validators.XML_Grammar);
   --  Parse the grammar to use from an XSD file, and add it to the grammar
   --  currently defined by Handled.

private
   type Validator_List_Record;
   type Validator_List is access Validator_List_Record;
   type Validator_List_Record is record
      Element   : Schema.Validators.XML_Element;
      Typ       : Schema.Validators.XML_Type;
      --  Typ will most often be the type of Element, but if the instance has
      --  specified a xsi:type attribute, this could point to some other type.

      Grammar   : Schema.Validators.XML_Grammar_NS;
      --  The grammar to which Element belongs

      Data      : Schema.Validators.Validator_Data;
      Is_Nil    : Boolean;          --  Whether the element has xsi:nil="true"
      Had_Character_Data : Boolean; --  Whether some character data was given
      Next      : Validator_List;
   end record;

   type Validating_Reader is new Sax.Readers.Reader with record
      Grammar  : Schema.Validators.XML_Grammar := Schema.Validators.No_Grammar;
      Validators : Validator_List;
      Locator    : Sax.Locators.Locator_Access;

      Id_Validator : Schema.Validators.XML_Validator;
      Ids          : Schema.Validators.Id_Htable_Access;
   end record;

   procedure Parse
     (Parser : in out Validating_Reader;
      Input  : in out Input_Sources.Input_Source'Class);
   procedure Set_Document_Locator
     (Handler : in out Validating_Reader;
      Loc     : access Sax.Locators.Locator'Class);
   procedure Start_Element
     (Handler       : in out Validating_Reader;
      Namespace_URI : Unicode.CES.Byte_Sequence := "";
      Local_Name    : Unicode.CES.Byte_Sequence := "";
      Qname         : Unicode.CES.Byte_Sequence := "";
      Atts          : Sax.Attributes.Attributes'Class);
   procedure End_Element
     (Handler       : in out Validating_Reader;
      Namespace_URI : Unicode.CES.Byte_Sequence := "";
      Local_Name    : Unicode.CES.Byte_Sequence := "";
      Qname         : Unicode.CES.Byte_Sequence := "");
   procedure Characters
     (Handler : in out Validating_Reader; Ch : Unicode.CES.Byte_Sequence);
   procedure Ignorable_Whitespace
     (Handler : in out Validating_Reader;
      Ch      : Unicode.CES.Byte_Sequence);
end Schema.Readers;
