
with Sax.Attributes;
with Sax.Exceptions;
with Sax.Locators;
with Input_Sources;
with Sax.Readers;
with Unicode.CES;
with Schema.Validators;

package Schema.Readers is

   type Validating_Reader is new Sax.Readers.Reader with private;

   procedure Set_Grammar
     (Reader  : in out Validating_Reader;
      Grammar : Schema.Validators.XML_Grammar);
   --  Create an XML reader that will validate its input file. The grammar
   --  must have been parsed first.

   procedure Validation_Error
     (Reader : in out Validating_Reader;
      Except  : Sax.Exceptions.Sax_Parse_Exception'Class);
   --  Called when a validation error occurs.
   --  By default, this raises XML_Validation_Error

private
   type Validator_List_Record;
   type Validator_List is access Validator_List_Record;
   type Validator_List_Record is record
      Validator : Schema.Validators.XML_Type;
      Data      : Schema.Validators.Validator_Data;
      Next      : Validator_List;
   end record;

   type Validating_Reader is new Sax.Readers.Reader with record
      Grammar    : Schema.Validators.XML_Grammar;
      Validators : Validator_List;
      Locator    : Sax.Locators.Locator_Access;
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

end Schema.Readers;
