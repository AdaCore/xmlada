with Sax.Exceptions;
with Sax.Locators;
with Sax.Readers;
with Sax.Attributes;
with Unicode.CES;

package Debug_Readers is
   type Debug_Reader is new Sax.Readers.Reader with private;

   procedure Warning
     (Handler : in out Debug_Reader;
      Except : Sax.Exceptions.Sax_Parse_Exception'Class);
   procedure Error
     (Handler : in out Debug_Reader;
      Except  : Sax.Exceptions.Sax_Parse_Exception'Class);
   procedure Fatal_Error
     (Handler : in out Debug_Reader;
      Except  : Sax.Exceptions.Sax_Parse_Exception'Class);
   procedure Set_Document_Locator
     (Handler : in out Debug_Reader;
      Loc     : access Sax.Locators.Locator'Class);
   procedure Start_Document (Handler : in out Debug_Reader);
   procedure End_Document (Handler : in out Debug_Reader);
   procedure Start_Prefix_Mapping
     (Handler : in out Debug_Reader;
      Prefix  : Unicode.CES.Byte_Sequence;
      URI     : Unicode.CES.Byte_Sequence);
   procedure End_Prefix_Mapping
     (Handler : in out Debug_Reader;
      Prefix  : Unicode.CES.Byte_Sequence);
   procedure Start_Element
     (Handler       : in out Debug_Reader;
      Namespace_URI : Unicode.CES.Byte_Sequence := "";
      Local_Name    : Unicode.CES.Byte_Sequence := "";
      Qname         : Unicode.CES.Byte_Sequence := "";
      Atts          : Sax.Attributes.Attributes'Class);
   procedure End_Element
     (Handler : in out Debug_Reader;
      Namespace_URI : Unicode.CES.Byte_Sequence := "";
      Local_Name    : Unicode.CES.Byte_Sequence := "";
      Qname         : Unicode.CES.Byte_Sequence := "");
   procedure Characters
     (Handler : in out Debug_Reader; Ch : Unicode.CES.Byte_Sequence);
   procedure Ignorable_Whitespace
     (Handler : in out Debug_Reader; Ch : Unicode.CES.Byte_Sequence);
   procedure Processing_Instruction
     (Handler : in out Debug_Reader;
      Target  : Unicode.CES.Byte_Sequence;
      Data    : Unicode.CES.Byte_Sequence);
   procedure Skipped_Entity
     (Handler : in out Debug_Reader; Name : Unicode.CES.Byte_Sequence);
   procedure Comment
     (Handler : in out Debug_Reader; Ch : Unicode.CES.Byte_Sequence);
   procedure Start_Cdata (Handler : in out Debug_Reader);
   procedure End_Cdata (Handler : in out Debug_Reader);
   procedure Start_Entity
     (Handler : in out Debug_Reader; Name : Unicode.CES.Byte_Sequence);
   procedure End_Entity
     (Handler : in out Debug_Reader; Name : Unicode.CES.Byte_Sequence);
   procedure Start_DTD
     (Handler   : in out Debug_Reader;
      Name      : Unicode.CES.Byte_Sequence;
      Public_Id : Unicode.CES.Byte_Sequence := "";
      System_Id : Unicode.CES.Byte_Sequence := "");
   procedure End_DTD (Handler : in out Debug_Reader);
   procedure Internal_Entity_Decl
     (Handler : in out Debug_Reader;
      Name    : Unicode.CES.Byte_Sequence;
      Value   : Unicode.CES.Byte_Sequence);
   procedure External_Entity_Decl
     (Handler   : in out Debug_Reader;
      Name      : Unicode.CES.Byte_Sequence;
      Public_Id : Unicode.CES.Byte_Sequence;
      System_Id : Unicode.CES.Byte_Sequence);
   procedure Unparsed_Entity_Decl
     (Handler       : in out Debug_Reader;
      Name          : Unicode.CES.Byte_Sequence;
      System_Id     : Unicode.CES.Byte_Sequence;
      Notation_Name : Unicode.CES.Byte_Sequence);
   procedure Element_Decl
     (Handler : in out Debug_Reader;
      Name    : Unicode.CES.Byte_Sequence;
      Model   : Unicode.CES.Byte_Sequence;
      Parsed_Model : Sax.Readers.Element_Model_Ptr);
   procedure Notation_Decl
     (Handler       : in out Debug_Reader;
      Name          : Unicode.CES.Byte_Sequence;
      Public_Id     : Unicode.CES.Byte_Sequence;
      System_Id     : Unicode.CES.Byte_Sequence);
   procedure Attribute_Decl
     (Handler : in out Debug_Reader;
      Ename   : Unicode.CES.Byte_Sequence;
      Aname   : Unicode.CES.Byte_Sequence;
      Typ     : Unicode.CES.Byte_Sequence;
      Value_Default : Unicode.CES.Byte_Sequence;
      Value   : Unicode.CES.Byte_Sequence);

private
   type Debug_Reader is new Sax.Readers.Reader with record
      Locator : Sax.Locators.Locator_Access;
   end record;
end Debug_Readers;
