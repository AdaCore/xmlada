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
   Xsd_File  : String_Access := null;
   Xml_File  : String_Access := null;

begin
   loop
      case Getopt ("xsd: debug") is
         when 'x' =>
            Free (Xsd_File);
            Xsd_File := new String'(Parameter);
         when 'd' =>
            Standard.Schema.Readers.Set_Debug_Output (True);
            Standard.Schema.Validators.Set_Debug_Output (True);
            Standard.Schema.Schema_Readers.Set_Debug_Output (True);
         when others =>
            exit;
      end case;
   end loop;

   Xml_File := new String'(Get_Argument);

   if Xsd_File /= null then
      Open (Xsd_File.all, Read);
      Set_Public_Id (Read, Xsd_File.all);
      Set_System_Id (Read, Xsd_File.all);
      Set_Validating_Grammar (Schema, Create_Schema_For_Schema);
      Parse (Schema, Read);
      Close (Read);
      Set_Validating_Grammar (My_Reader, Get_Created_Grammar (Schema));
   end if;

   if Xml_File.all /= "" then
      Open (Xml_File.all, Read);
      Set_Public_Id (Read, Xml_File.all);
      Set_System_Id (Read, Xml_File.all);
      Parse (My_Reader, Read);
      Close (Read);
   end if;

exception
   when E : XML_Validation_Error | XML_Fatal_Error =>
      Put_Line (Exception_Message (E));
      Close (Read);
end TestSchema;
