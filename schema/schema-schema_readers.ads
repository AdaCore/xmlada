
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

   procedure Set_Debug_Output (Output : Boolean);
   --  Whether extra debug output should be displayed

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

   type Context_Type is (Context_Complex_Type,
                         Context_Element,
                         Context_Sequence,
                         Context_Choice,
                         Context_Schema,
                         Context_Attribute);

   type Context (Typ : Context_Type);
   type Context_Access is access all Context;
   type Context (Typ : Context_Type) is record
      Next  : Context_Access;
      Level : Integer;
      case Typ is
         when Context_Complex_Type =>
            Complex_Type_Name      : Unicode.CES.Byte_Sequence_Access;
            Complex_Type_Validator : Schema.Validators.XML_Validator;
         when Context_Element =>
            Element : Schema.Validators.XML_Element;
         when Context_Sequence =>
            Seq       : Schema.Validators.Sequence;
         when Context_Choice =>
            C       : Schema.Validators.Choice;
         when Context_Schema =>
            null;
         when Context_Attribute =>
            Attribute : Schema.Validators.Attribute_Validator;
      end case;
   end record;

   type Schema_Reader is new Schema.Readers.Validating_Reader with record
      Grammar         : Schema.Validators.XML_Grammar;

      Target_NS       : Schema.Validators.XML_Grammar_NS;
      Prefixes        : Prefix_Mapping_Access;
      Contexts        : Context_Access;
   end record;

   procedure Start_Document (Handler : in out Schema_Reader);
   procedure Start_Element
     (Handler       : in out Schema_Reader;
      Namespace_URI : Unicode.CES.Byte_Sequence := "";
      Local_Name    : Unicode.CES.Byte_Sequence := "";
      Qname         : Unicode.CES.Byte_Sequence := "";
      Atts          : Sax.Attributes.Attributes'Class);
   procedure End_Element
     (Handler       : in out Schema_Reader;
      Namespace_URI : Unicode.CES.Byte_Sequence := "";
      Local_Name    : Unicode.CES.Byte_Sequence := "";
      Qname         : Unicode.CES.Byte_Sequence := "");
   procedure Characters
     (Handler : in out Schema_Reader; Ch : Unicode.CES.Byte_Sequence);
   procedure Start_Prefix_Mapping
     (Handler : in out Schema_Reader;
      Prefix  : Unicode.CES.Byte_Sequence;
      URI     : Unicode.CES.Byte_Sequence);
   procedure End_Prefix_Mapping
     (Handler : in out Schema_Reader;
      Prefix  : Unicode.CES.Byte_Sequence);

end Schema.Schema_Readers;
