
with Sax.Exceptions;
with Sax.Locators;
with Input_Sources;
with Sax.Readers;
with Unicode.CES;
with Schema.Validators;

package Schema.Readers is

   type Validating_Reader is new Sax.Readers.Reader with private;
   --  To get full validation of an XML document, you must derive from this
   --  type. You must also enable the Validation_Feature feature, through a
   --  call to Set_Feature.
   --  If you override the Parse method in your code, you must call
   --     Parse (Validating_Reader (Your_Reader), Input);
   --  and not  Parse (Reader (Your_Reader), Input) to get validation.
   --
   --  In most cases, the reader will find by itself what variable should be
   --  used, from the contents of the XML file:
   --  It uses the attribute of the nodes to find out what grammar, for
   --  instance from the following XML extract:
   --     <root xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
   --           xsi:noNamespaceSchemaLocation="my_file.xsd" />
   --  or
   --     <root xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
   --           xsi:schemaLocation="ns1 my_file.xsd  ns2 my_file2.xsd" />
   --
   --  The second variant associates a specific grammar with each of the
   --  namespaces found in the document.

   procedure Set_Validating_Grammar
     (Reader  : in out Validating_Reader;
      Grammar : Schema.Validators.XML_Grammar);
   function Get_Validating_Grammar
     (Reader : Validating_Reader) return Schema.Validators.XML_Grammar;
   --  Create an XML reader that will validate its input file. The grammar
   --  must have been parsed first (most likely through a call to
   --  Schema.Schema_Readers.Schema_Reader or a call to Parse_Grammar below).
   --  If other schema files need to be parsed because of the presence of a
   --  "targetNamespace" attribute, their corresponding grammars will be added
   --  to grammar, in their own namespace of course.
   --  If this is not called, no validation will take place.

   procedure Validation_Error
     (Reader : in out Validating_Reader;
      Except : Sax.Exceptions.Sax_Parse_Exception'Class);
   --  Called when a validation error occurs.
   --  By default, this raises XML_Validation_Error

   procedure Set_Debug_Output (Output : Boolean);
   --  Whether we should output debug traces

   function To_Absolute_URI
     (Handler : Validating_Reader;
      URI     : Unicode.CES.Byte_Sequence) return Unicode.CES.Byte_Sequence;
   --  Convert a URI read in the input stream of Handler to an absolute URI.
   --  This is used for instance to find the location of a schema file,...

   procedure Parse_Grammar
     (Handler  : in out Validating_Reader;
      Xsd_File : Unicode.CES.Byte_Sequence;
      Add_To   : in out Schema.Validators.XML_Grammar);
   --  Parse the grammar to use from an XSD file, and add it to Add_To.
   --  The resulting grammar can be passed to Set_Validating_Grammar.

   function Get_Namespace_From_Prefix
     (Handler  : Validating_Reader;
      Prefix   : Unicode.CES.Byte_Sequence)
      return Unicode.CES.Byte_Sequence_Access;
   --  Get the namespace associated with a given prefix, in the current
   --  context.
   --  The caller must not modify the return value.

private
   type Prefix_Mapping;
   type Prefix_Mapping_Access is access Prefix_Mapping;
   type Prefix_Mapping is record
      Prefix    : Unicode.CES.Byte_Sequence_Access;
      Namespace : Unicode.CES.Byte_Sequence_Access;
      Next      : Prefix_Mapping_Access;
   end record;
   --  Usee a list to store the prefixes, since there aren't that many of them,
   --  and a local prefix can override a more global one (so a hash table needs
   --  some special handling in any case).

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

      Start_Loc  : Sax.Locators.Locator_Impl_Access;
      --  Set when starting a sequence of Characters call from the SAX parser
      Characters : Unicode.CES.Byte_Sequence_Access;
      --  The current stream of characters we have seen. We need to collapse
      --  adjacent characters, so that we can validate the full contents of a
      --  tag at once, and not by parts.

      Next      : Validator_List;
   end record;

   type Validating_Reader is new Sax.Readers.Reader with record
      Grammar  : Schema.Validators.XML_Grammar := Schema.Validators.No_Grammar;
      Validators : Validator_List;
      Locator    : Sax.Locators.Locator_Access;
      Prefixes   : Prefix_Mapping_Access;
      Ids          : Schema.Validators.Id_Htable_Access;
   end record;

   procedure Parse
     (Parser : in out Validating_Reader;
      Input  : in out Input_Sources.Input_Source'Class);
   --  Override inherited method.

end Schema.Readers;
