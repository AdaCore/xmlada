
--  This package provides a SAX Reader that parses an XML Schema file, and
--  creates the appropri

with Sax.Attributes;
with Schema.Readers;
with Unicode.CES;
with Schema.Validators;

package Schema.Schema_Readers is

   type Schema_Reader is new Schema.Readers.Validating_Reader with private;
   --  An XML reader that parses an XML schema, and store the information in
   --  a grammar

   function Get_Grammar
     (Reader : Schema_Reader) return Schema.Validators.XML_Grammar;
   --  Return the grammar parsed

private
   type Schema_Reader is new Schema.Readers.Validating_Reader with record
      Grammar         : Schema.Validators.XML_Grammar;
   end record;

   procedure Start_Document (Handler : in out Schema_Reader);
   procedure Start_Element
     (Handler       : in out Schema_Reader;
      Namespace_URI : Unicode.CES.Byte_Sequence := "";
      Local_Name    : Unicode.CES.Byte_Sequence := "";
      Qname         : Unicode.CES.Byte_Sequence := "";
      Atts          : Sax.Attributes.Attributes'Class);
   procedure Characters
     (Handler : in out Schema_Reader; Ch : Unicode.CES.Byte_Sequence);

end Schema.Schema_Readers;
