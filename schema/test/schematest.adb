-----------------------------------------------------------------------
--                XML/Ada - An XML suite for Ada95                   --
--                                                                   --
--                       Copyright (C) 2007, AdaCore                 --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

--  Run the automatic testsuite for XML Schema from www.w3c.org
--  You can download these from the web
--      http://www.w3.org/XML/Schema
--  in the "Test Collection" part for a link to the latest .tar.gz package.
--  Also:
--   http://www.w3.org/XML/2004/xml-schema-test-suite/index.html

with Ada.Exceptions;            use Ada.Exceptions;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with Ada.Text_IO;               use Ada.Text_IO;
with DOM.Core.Documents;        use DOM.Core.Documents;
with DOM.Core.Nodes;            use DOM.Core, DOM.Core.Nodes;
with DOM.Readers;               use DOM.Readers;
with GNAT.Command_Line;         use GNAT.Command_Line;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with Input_Sources.File;        use Input_Sources, Input_Sources.File;
with Schema.Readers;            use Schema.Readers;
with Schema.Schema_Readers;     use Schema.Schema_Readers;
with Schema.Validators;         use Schema.Validators;
with Sax.Readers;           use Sax.Readers;

procedure Schematest is

   Testdir : constant String := "xmlschema2006-11-06";

   Verbose       : Boolean := False;
   Debug         : Boolean := False;
   Test_XML      : Boolean := True;
   Test_Schemas  : Boolean := True;
   --  Whether to test the validity of XML or Schema files. If both are false,
   --  the only output will be for unexpected internal errors

   Show_Expected : Boolean := True;
   --  When a test is expected to be invalid, should we compare the error
   --  messages as well ?

   Accepted_Only      : constant Boolean := True;
   --  If true, then only tests that are marked as "accepted" are run. Some
   --  tests might be under discussion, and have a status of "queried". Such
   --  tests are not run.

   Total_Parsed_Schema : Natural := 0;
   Total_Parsed_XML    : Natural := 0;
   Total_Error         : Natural := 0;

   Xlink : constant String := "http://www.w3.org/1999/xlink";

   procedure Run_Testsuite  (Filename : String);
   procedure Run_Testset    (Filename : String);
   procedure Run_Test_Group
     (Testset : String; Group : Node; Base_Dir : String);
   procedure Parse_Schema_Test
     (Testset, Group : String;
      Schema         : Node;
      Base_Dir       : String;
      Grammar        : out XML_Grammar;
      Schema_Files   : out Unbounded_String);
   procedure Parse_Instance_Test
     (Testset, Group, Schema : String;
      Test           : Node;
      Base_Dir       : String;
      Grammar        : XML_Grammar);
   --  Run the testsuite whose description is in Filename

   function Get_Attribute (N : Node; Attribute : String) return String;
   function Get_Attribute_NS (N : Node; URI, Local : String) return String;
   --  Query an attribute from N. The empty string is returned if the attribute
   --  does not exists

   procedure Test_Header (Testset, Group, Schema, Test : String);
   procedure Error (Testset, Group, Schema, Test, Msg : String);
   procedure Expected (Testset, Group, Schema, Test, Msg : String);
   --  Print an error message

   type Outcome_Value is (Valid, Invalid, NotKnown);
   function Get_Expected (N : Node) return Outcome_Value;
   --  Whether the test is expected to be valid or invalid

   type Status_Value is (Accepted, Queried);
   function Get_Status
     (Testset, Group, Schema, Test : String; N : Node) return Status_Value;
   --  Get the status of the test

   Last_Set    : Unbounded_String;
   Last_Grp    : Unbounded_String;
   Last_Schema : Unbounded_String;
   --  Keep track of what was already output, to limit the amount of
   --  duplication

   -------------------
   -- Get_Attribute --
   -------------------

   function Get_Attribute (N : Node; Attribute : String) return String is
      Attr : constant Node := Get_Named_Item (Attributes (N), Attribute);
   begin
      if Attr = null then
         return "";
      else
         return Node_Value (Attr);
      end if;
   end Get_Attribute;

   ----------------------
   -- Get_Attribute_NS --
   ----------------------

   function Get_Attribute_NS (N : Node; URI, Local : String) return String is
      Attr : constant Node := Get_Named_Item_NS
        (Attributes (N), URI, Local);
   begin
      if Attr = null then
         return "";
      else
         return Node_Value (Attr);
      end if;
   end Get_Attribute_NS;

   -----------------
   -- Test_Header --
   -----------------

   procedure Test_Header (Testset, Group, Schema, Test : String) is
   begin
      if Testset /= To_String (Last_Set) then
         Last_Set    := To_Unbounded_String (Testset);
         Last_Grp    := Null_Unbounded_String;
         Put_Line ("Set: " & Testset);
      end if;

      if To_String (Last_Grp) /= Group then
         Last_Grp    := To_Unbounded_String (Group);
         Last_Schema := Null_Unbounded_String;
         Put_Line ("Grp: " & Group);
      end if;

      if Schema /= To_String (Last_Schema) then
         Last_Schema := To_Unbounded_String (Schema);
         if Schema /= "" then
            Put_Line (Schema);
         end if;
      end if;

      if Test /= "" then
         Put_Line (Test);
      end if;
   end Test_Header;

   -----------
   -- Error --
   -----------

   procedure Error (Testset, Group, Schema, Test, Msg : String) is
   begin
      Total_Error := Total_Error + 1;
      Test_Header (Testset, Group, Schema, Test);
      Put_Line (Msg);
      New_Line;
   end Error;

   --------------
   -- Expected --
   --------------

   procedure Expected (Testset, Group, Schema, Test, Msg : String) is
   begin
      if Show_Expected then
         Test_Header (Testset, Group, Schema, Test);
         Put_Line ("OK: " & Msg);
         New_Line;
      end if;
   end Expected;

   ------------------
   -- Get_Expected --
   ------------------

   function Get_Expected (N : Node) return Outcome_Value is
      N2 : Node := First_Child (N);
   begin
      while N2 /= null loop
         if Local_Name (N2) = "expected" then
            if Get_Attribute (N2, "validity") = "valid" then
               return Valid;
            elsif Get_Attribute (N2, "validity") = "invalid" then
               return Invalid;
            end if;

         end if;
         N2 := Next_Sibling (N2);
      end loop;
      return NotKnown;
   end Get_Expected;

   ----------------
   -- Get_Status --
   ----------------

   function Get_Status
     (Testset, Group, Schema, Test : String; N : Node) return Status_Value
   is
      N2 : Node := First_Child (N);
   begin
      while N2 /= null loop
         if Local_Name (N2) = "current" then
            if Get_Attribute (N2, "status") = "accepted" then
               return Accepted;
            elsif Get_Attribute (N2, "status") = "queried" then
               if Verbose then
                  Test_Header (Testset, Group, Schema, Test);
                  Put_Line ("Test not accepted, see "
                            & Get_Attribute (N2, "bugzilla"));
                  New_Line;
               end if;
               return Queried;
            else
               Put_Line ("Invalid status: " & Get_Attribute (N2, "status"));
               raise Program_Error;
            end if;

         end if;
         N2 := Next_Sibling (N2);
      end loop;

      return Accepted;
   end Get_Status;

   -----------------------
   -- Parse_Schema_Test --
   -----------------------

   procedure Parse_Schema_Test
     (Testset, Group : String;
      Schema         : Node;
      Base_Dir       : String;
      Grammar        : out XML_Grammar;
      Schema_Files   : out Unbounded_String)
   is
      Name   : constant String := Get_Attribute (Schema, "name");
      Document : Unbounded_String;
      Reader : Schema_Reader;
      Input  : File_Input;
      N      : Node := First_Child (Schema);
      Outcome : constant Outcome_Value := Get_Expected (Schema);
   begin
      Grammar      := No_Grammar;
      Schema_Files := Null_Unbounded_String;

      if Accepted_Only
        and then Get_Status (Testset, Group, Name, "", Schema) /= Accepted
      then
         return;
      end if;

      begin
         Set_Feature (Reader, Schema_Validation_Feature, True);
         Set_Created_Grammar (Reader, No_Grammar);
         Use_Basename_In_Error_Messages (Reader, True);

         while N /= null loop
            if Local_Name (N) = "schemaDocument" then
               Document := To_Unbounded_String
                 (Normalize_Pathname
                    (Get_Attribute_NS (N, Xlink, "href"),
                     Base_Dir, Resolve_Links => False));
               if Verbose then
                  Put_Line ("Parsing " & To_String (Document));
               end if;
               if Schema_Files /= Null_Unbounded_String then
                  Append (Schema_Files, " - ");
               end if;
               Append (Schema_Files, Document);

               Total_Parsed_Schema := Total_Parsed_Schema + 1;
               Open (To_String (Document), Input);
               Parse (Reader, Input);
               Close (Input);
            end if;
            N := Next_Sibling (N);
         end loop;

         Grammar := Get_Created_Grammar (Reader);
         Global_Check (Grammar);

         if Test_Schemas and then Outcome = Invalid then
            Error (Testset, Group,
                   To_String (Document), "",
                   "(i)");
         end if;

      exception
         when E : XML_Validation_Error | XML_Fatal_Error =>
            if Test_Schemas and then Outcome = Valid then
               Error (Testset, Group,
                      To_String (Document),
                      "", "(v) " & Exception_Message (E));
            else
               --  The error message already includes the name of the
               --  document, so we do not repeat it
               Expected
                 (Testset, Group, To_String (Document), "",
                  Exception_Message (E));
            end if;

         when E : others =>
            Error (Testset, Group,
                   To_String (Document), "",
                   Exception_Information (E));
      end;
   end Parse_Schema_Test;

   -------------------------
   -- Parse_Instance_Test --
   -------------------------

   procedure Parse_Instance_Test
     (Testset, Group, Schema : String;
      Test           : Node;
      Base_Dir       : String;
      Grammar        : XML_Grammar)
   is
      Name     : constant String := Get_Attribute (Test, "name");
      Outcome  : constant Outcome_Value := Get_Expected (Test);
      N        : Node := First_Child (Test);
      Reader   : Validating_Reader;
      Input    : File_Input;
      Document : Unbounded_String;
   begin
      if Accepted_Only
        and then Get_Status (Testset, Group, Schema, Name, Test) /= Accepted
      then
         return;
      end if;

      Use_Basename_In_Error_Messages (Reader, True);
      Set_Validating_Grammar (Reader, Grammar);
      Set_Feature (Reader, Schema_Validation_Feature, True);

      while N /= null loop
         if Local_Name (N) = "instanceDocument" then
            begin
               Document := To_Unbounded_String
                 (Normalize_Pathname
                    (Get_Attribute_NS (N, Xlink, "href"),
                     Base_Dir, Resolve_Links => False));
               if Verbose then
                  Put_Line ("Parsing " & To_String (Document));
               end if;

               Total_Parsed_XML := Total_Parsed_XML + 1;
               Open (To_String (Document), Input);
               Parse (Reader, Input);
               Close (Input);

               if Test_XML and then Outcome = Invalid then
                  Error (Testset, Group, Schema,
                         To_String (Document),
                         "(i)");
               end if;

            exception
               when E : XML_Validation_Error | XML_Fatal_Error =>
                  if Test_XML and then Outcome = Valid then
                     Error (Testset, Group, Schema,
                            To_String (Document),
                            "(v) " & Exception_Message (E));
                  else
                     --  The error message already includes the name of the
                     --  document, so we do not repeat it
                     Expected (Testset, Group, Schema,
                               To_String (Document),
                               Exception_Message (E));
                  end if;

               when E : others =>
                  Error (Testset, Group, Schema,
                         To_String (Document),
                         Exception_Information (E));
            end;
         end if;
         N := Next_Sibling (N);
      end loop;
   end Parse_Instance_Test;

   --------------------
   -- Run_Test_Group --
   --------------------

   procedure Run_Test_Group
     (Testset    : String;
      Group      : Node;
      Base_Dir   : String)
   is
      Name   : constant String := Get_Attribute (Group, "name");
      N      : Node := First_Child (Group);
      Schema : XML_Grammar;
      Schema_Files : Unbounded_String;
   begin
      while N /= null loop
         if Local_Name (N) = "schemaTest" then
            Parse_Schema_Test
              (Testset, Name, N, Base_Dir,
               Grammar      => Schema,
               Schema_Files => Schema_Files);
            if Schema = No_Grammar then
               --  We couldn't parse the grammar, or the tests isn't "accepted"
               --  so nothing else to do
               exit;
            end if;

         elsif Local_Name (N) = "instanceTest" then
            Parse_Instance_Test
              (Testset, Name, To_String (Schema_Files), N, Base_Dir, Schema);
         end if;

         N := Next_Sibling (N);
      end loop;
   end Run_Test_Group;

   -----------------
   -- Run_Testset --
   -----------------

   procedure Run_Testset (Filename : String) is
      Input  : File_Input;
      Reader : Tree_Reader;
      N      : Node;
      Name   : Unbounded_String;
   begin
      Open (Filename, Input);
      Parse (Reader, Input);
      Close (Input);

      N := Get_Element (Get_Tree (Reader));
      Name := To_Unbounded_String (Get_Attribute (N, "name"));

      if Verbose then
         Put_Line ("Testset: " & To_String (Name));
      end if;

      N := First_Child (N);
      while N /= null loop
         if Local_Name (N) = "testGroup" then
            Run_Test_Group
              (Testset    => To_String (Name),
               Group      => N,
               Base_Dir   => Dir_Name (Filename));
         end if;

         N := Next_Sibling (N);
      end loop;

      Free (Reader);
   end Run_Testset;

   -------------------
   -- Run_Testsuite --
   -------------------

   procedure Run_Testsuite (Filename : String) is
      Input  : File_Input;
      Reader : Tree_Reader;
      N      : Node;
   begin
      Open (Filename, Input);
      Parse (Reader, Input);
      Close (Input);

      N := Get_Element (Get_Tree (Reader));
      Put_Line ("Version: " & Get_Attribute (N, "schemaVersion"));
      Put_Line ("Release: " & Get_Attribute (N, "releaseDate"));

      N := First_Child (N);
      while N /= null loop
         if Local_Name (N) = "testSetRef" then
            Run_Testset
              (Normalize_Pathname
                 (Get_Attribute_NS (N, Xlink, "href"),
                  Dir_Name (Filename),
                  Resolve_Links => False));
         end if;

         N := Next_Sibling (N);
      end loop;

      Free (Reader);
   end Run_Testsuite;

begin
   loop
      case Getopt ("v d x s e") is
         when 'v'    => Verbose := True;
         when 'd'    => Debug   := True;
         when 'x'    => Test_XML := False;
         when 's'    => Test_Schemas := False;
         when 'e'    => Show_Expected := False;
         when others => exit;
      end case;
   end loop;

   if Debug then
      Schema.Readers.Set_Debug_Output (True);
      Schema.Validators.Set_Debug_Output (True);
      Schema.Schema_Readers.Set_Debug_Output (True);
   end if;

   Change_Dir (Testdir);
   Run_Testsuite ("suite.xml");

   Put_Line ("Schemas:" & Total_Parsed_Schema'Img
             & " XML:" & Total_Parsed_XML'Img
             & " Errors:" & Total_Error'Img);
end Schematest;
