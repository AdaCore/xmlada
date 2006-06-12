--  This is a small example showing how to parse one or more XML schemas
--  and then validate one XML document. To use this tool, do a
--  "make test" at the root of the XML/Ada distribution, then run
--      ./testschema -xsd schema1.xsd -xsd schema2.xsd file.xml
--  where schema1.xsd, schema2.xsd, schema3.xsd,... are our schema files
--  to parse, and file.xml the XML document to validate

with Schema.Readers;        use Schema.Readers;
with Schema.Schema_Grammar; use Schema.Schema_Grammar;
with Schema.Schema_Readers; use Schema.Schema_Readers;
with Schema.Validators;     use Schema.Validators;
with Input_Sources.File;    use Input_Sources.File;
with Ada.Exceptions;        use Ada.Exceptions;
with GNAT.IO;               use GNAT.IO;
with Sax.Readers;           use Sax.Readers;
with GNAT.Command_Line;     use GNAT.Command_Line;
with GNAT.OS_Lib;           use GNAT.OS_Lib;

procedure TestSchema is
   Read      : File_Input;
   My_Reader : Validating_Reader;
   Schema    : Schema_Reader;
   Grammar   : XML_Grammar := No_Grammar;
   Xml_File  : String_Access := null;

begin
   loop
      case Getopt ("xsd: debug") is
         when 'x' =>
            Open (Parameter, Read);

            --  We want to validate the XSD file itself
            Set_Validating_Grammar (Schema, Create_Schema_For_Schema);

            --  When we are parsing several XSD files (one per namespace
            --  in general), we need to store all of them in the same
            --  Grammar object, so that we validate the document only
            --  once afterward
            Set_Created_Grammar (Schema, Grammar);

            --  Parse and validate the current grammar
            Parse (Schema, Read);
            Close (Read);

            --  Grammar now contains all the XSD files we have parsed so far
            Grammar := Get_Created_Grammar (Schema);

         when 'd' =>
            Standard.Schema.Readers.Set_Debug_Output (True);
            Standard.Schema.Validators.Set_Debug_Output (True);
            Standard.Schema.Schema_Readers.Set_Debug_Output (True);

         when others =>
            exit;
      end case;
   end loop;

   Xml_File := new String'(Get_Argument);

   if Xml_File.all /= "" then
      Set_Validating_Grammar (My_Reader, Grammar);
      Set_Feature (My_Reader, Schema_Validation_Feature, True);
      Open (Xml_File.all, Read);
      Parse (My_Reader, Read);
      Close (Read);
   end if;

   Free (Grammar);

exception
   when E : XML_Validation_Error | XML_Fatal_Error =>
      Put_Line (Exception_Message (E));
      Close (Read);
end TestSchema;
