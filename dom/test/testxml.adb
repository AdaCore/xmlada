with GNAT.Command_Line;  use GNAT.Command_Line;
with Input_Sources;      use Input_Sources;
with Input_Sources.File; use Input_Sources.File;
with Input_Sources.Http; use Input_Sources.Http;
with DOM.Readers;        use DOM.Readers;
with Sax.Readers;        use Sax.Readers;
with DOM.Core.Nodes;     use DOM.Core.Nodes;
with Ada.Exceptions;     use Ada.Exceptions;
with Ada.Text_IO;        use Ada.Text_IO;
with Ada.Unchecked_Deallocation;

--  Try also
--     ./testxml http://java.sun.com/j2ee/1.4/docs/tutorial/examples/jaxp
--     /dom/samples/slideSample01.xml

procedure Testxml is
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Input_Source'Class, Input_Source_Access);
   Silent : Boolean := False;
   With_URI : Boolean := False;
   Dump : Boolean := False;
   Read : Input_Source_Access;
   My_Tree_Reader : Tree_Reader;
   Name_Start : Natural;
   Validate : Boolean := False;
   Valid_Chars : Boolean := False;
   Must_Normalize : Boolean := False;

begin
   --  Parse the command line
   loop
      case Getopt ("silent uri normalize validate dump valid_chars") is
         when ASCII.Nul => exit;

         when 's' => Silent := True;
         when 'u' => With_URI := True;
         when 'v' =>
            if Full_Switch = "validate" then
               Validate := True;
            elsif Full_Switch = "valid_chars" then
               Valid_Chars := True;
            end if;
         when 'd' => Dump := True;
         when 'n' => Must_Normalize := True;

         when others => null;
      end case;
   end loop;

   declare
      S : constant String := Get_Argument;
   begin
      if S'Length > 0 then
         if S'Length > 6 and then S (S'First .. S'First + 6) = "http://" then
            Read := new Http_Input;
            Open (S, Http_Input (Read.all));
         else
            Read := new File_Input;
            Open (S, File_Input (Read.all));
         end if;

         --  Base file name should be used as the public Id
         Name_Start := S'Last;
         while Name_Start >= S'First  and then S (Name_Start) /= '/' loop
            Name_Start := Name_Start - 1;
         end loop;

         Set_Public_Id (Read.all, S (Name_Start + 1 .. S'Last));

         --  Full name is used as the system id
         Set_System_Id (Read.all, S);
      else
         Read := new File_Input;
         Set_Public_Id (Read.all, "test.xml");
         Set_System_Id (Read.all, "test.xml");
         Open ("test.xml", File_Input (Read.all));
      end if;
   end;

   Set_Feature (My_Tree_Reader, Validation_Feature, Validate);
   Set_Feature (My_Tree_Reader, Test_Valid_Chars_Feature, Valid_Chars);

   Parse (My_Tree_Reader, Read.all);
   Close (Read.all);
   Unchecked_Free (Read);

   if Must_Normalize then
      Normalize (Get_Tree (My_Tree_Reader));
   end if;

   if not Silent then
      if Dump then
         DOM.Core.Nodes.Dump (Get_Tree (My_Tree_Reader), With_URI => With_URI);
      else
         Print (Get_Tree (My_Tree_Reader),
                Print_Comments => False,
                Print_XML_PI => False,
                With_URI => With_URI);
      end if;
   end if;

   Free (My_Tree_Reader);

exception
   when E : XML_Fatal_Error =>
      Close (Read.all);
      Put_Line (Exception_Message (E));
      Free (My_Tree_Reader);
end Testxml;
