with Schema.Readers;        use Schema.Readers;
with Schema.Schema_Grammar; use Schema.Schema_Grammar;
with Schema.Schema_Readers; use Schema.Schema_Readers;
with Schema.Validators;     use Schema.Validators;
with Schema.Dom_Readers;    use Schema.Dom_Readers;
with Input_Sources.File;    use Input_Sources.File;
with Ada.Exceptions;        use Ada.Exceptions;
with GNAT.IO;               use GNAT.IO;
with Sax.Readers;           use Sax.Readers;
with GNAT.Command_Line;     use GNAT.Command_Line;
with GNAT.OS_Lib;           use GNAT.OS_Lib;
with DOM.Core;              use DOM.Core;
with DOM.Core.Nodes;        use DOM.Core.Nodes;

procedure TestDomSchema is
   Read      : File_Input;
   My_Reader : Schema.Dom_Readers.Tree_Reader;
   Schema    : Schema_Reader;
   Grammar   : XML_Grammar := No_Grammar;
   Xsd_File  : String_Access := null;
   Xml_File  : String_Access := null;
   Doc       : Document;
   Silent    : Boolean := False;

begin
   loop
      case Getopt ("xsd: debug silent") is
         when 'x' =>
            Free (Xsd_File);
            Xsd_File := new String'(Parameter);
         when 'd' =>
            Standard.Schema.Readers.Set_Debug_Output (True);
            Standard.Schema.Validators.Set_Debug_Output (True);
            Standard.Schema.Schema_Readers.Set_Debug_Output (True);
         when 's' =>
            Silent := True;
         when others =>
            exit;
      end case;
   end loop;

   Xml_File := new String'(Get_Argument);

   if Xsd_File /= null then
      Open (Xsd_File.all, Read);
      Set_Validating_Grammar (Schema, Create_Schema_For_Schema);
      Parse (Schema, Read);
      Close (Read);
      Grammar := Get_Created_Grammar (Schema);
   end if;

   if Xml_File.all /= "" then
      Set_Validating_Grammar (My_Reader, Grammar);
      Set_Feature (My_Reader, Schema_Validation_Feature, True);
      Open (Xml_File.all, Read);
      Parse (My_Reader, Read);
      Doc := Get_Tree (My_Reader);

      if not Silent then
         Print (Doc);
      end if;

      Close (Read);
   end if;

   Free (Grammar);

exception
   when E : XML_Validation_Error | XML_Fatal_Error =>
      Put_Line (Exception_Message (E));
      Close (Read);
end TestDomSchema;
