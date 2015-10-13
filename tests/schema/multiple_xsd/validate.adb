--  This file demonstrates how to preload multiple independent XSD files, and
--  then reuse them to validate multiple XML files.
--  Which XSD file to use for a given XML file is not known in advance, but
--  given via attributes in the XML files themselves.
--  In the XSD files were using namespaces, we would not need to do any
--  special setup. Just load them all in the same XML_Grammar, and use that
--  common grammar to validate the XML files.
--  In this case, though, the XML files do not use namespaces, so the XSD
--  have to be loaded in their own XML_Grammar, or we would end up with
--  conflicts.

with Ada.Text_IO;           use Ada.Text_IO;
with Sax.Readers;           use Sax.Readers;
with Sax.Symbols;           use Sax.Symbols;
with Sax.Utils;             use Sax.Utils;
with Schema.Validators;     use Schema.Validators;
with Schema.Readers;        use Schema.Readers;
with Schema.Schema_Readers; use Schema.Schema_Readers;
with Input_Sources.File;    use Input_Sources.File;

procedure Validate is
   type Reader_With_Preloaded_XSD is new Validating_Reader with record
      G1, G2 : XML_Grammar;
   end record;
   overriding procedure Parse_Grammar
      (Self          : not null access Reader_With_Preloaded_XSD;
       URI, Xsd_File : Sax.Symbols.Symbol;
       Do_Create_NFA : Boolean := True);

   overriding procedure Parse_Grammar
      (Self          : not null access Reader_With_Preloaded_XSD;
       URI, Xsd_File : Sax.Symbols.Symbol;
       Do_Create_NFA : Boolean := True) is
   begin
      if Xsd_File = "algo1.xsd" then
         Self.Set_Grammar (Self.G1);
      elsif Xsd_File = "algo2.xsd" then
         Self.Set_Grammar (Self.G2);
      else
         --  Call the inherited procedure
         Validating_Reader (Self.all).Parse_Grammar
            (URI, Xsd_File, Do_Create_NFA);
      end if;
   end Parse_Grammar;

   Symbols : Symbol_Table;
   F       : File_Input;
   S       : Schema_Reader;
   R       : Reader_With_Preloaded_XSD;
begin
   --  We need to share the symbol table between all involved parsers and
   --  grammars. These symbol tables are used to efficiently store internal
   --  strings.

   Symbols := Allocate;
   S.Set_Symbol_Table (Symbols);
   R.Set_Symbol_Table (Symbols);

   --  Load multiple grammars. We need to reset the grammar before parsing
   --  each new file, or the files will be merged and might conflict with
   --  each other.

   Open ("algo1.xsd", F);
   S.Parse (F);
   F.Close;
   R.G1 := S.Get_Grammar;

   Open ("algo2.xsd", F);
   S.Set_Grammar (No_Grammar);
   S.Parse (F);
   F.Close;
   R.G2 := S.Get_Grammar;

   --  Now parse all the XML files, and let the reader chose which grammar
   --  to use via the overriding of Parse_Grammar

   R.Set_Feature (Schema_Validation_Feature, True);

   Open ("test1.xml", F);
   R.Parse (F);
   F.Close;

   Open ("test2.xml", F);
   R.Parse (F);
   F.Close;

exception
   when Xml_Validation_Error =>
      Put_Line ("Error: " & R.Get_Error_Message & S.Get_Error_Message);
end Validate;
