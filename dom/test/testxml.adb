with GNAT.Command_Line;  use GNAT.Command_Line;
with Input_Sources.File; use Input_Sources.File;
with Tree_Readers;       use Tree_Readers;
with Sax.Readers;        use Sax.Readers;
with DOM.Core.Nodes;     use DOM.Core.Nodes;
with Ada.Exceptions;     use Ada.Exceptions;
with Ada.Text_IO;        use Ada.Text_IO;

procedure Testxml is
   Silent : Boolean := False;
   With_URI : Boolean := False;
   Read : File_Input;
   My_Tree_Reader : Tree_Reader;
   Name_Start : Natural;
   Validate : Boolean := False;

begin
   --  Parse the command line
   loop
      case Getopt ("silent uri validate") is
         when ASCII.Nul => exit;

         when 's' => Silent := True;
         when 'u' => With_URI := True;
         when 'v' => Validate := True;

         when others => null;
      end case;
   end loop;

   declare
      S : constant String := Get_Argument;
   begin
      if S'Length > 0 then
         --  Base file name should be used as the public Id
         Name_Start := S'Last;
         while Name_Start >= S'First  and then S (Name_Start) /= '/' loop
            Name_Start := Name_Start - 1;
         end loop;
         Set_Public_Id (Read, S (Name_Start + 1 .. S'Last));
         Open (S, Read);

         --  Full name is used as the system id
         Set_System_Id (Read, S);
      else
         Set_Public_Id (Read, "test.xml");
         Set_System_Id (Read, "test.xml");
         Open ("test.xml", Read);
      end if;
   end;

   Set_Feature (My_Tree_Reader, Validation_Feature, Validate);

   Parse (My_Tree_Reader, Read);
   Close (Read);

   if not Silent then
      Print (Get_Tree (My_Tree_Reader),
             Print_Comments => False,
             Print_XML_PI => False,
             With_URI => With_URI);
   end if;

exception
   when E : XML_Fatal_Error =>
      Put_Line (Exception_Message (E));
end Testxml;
