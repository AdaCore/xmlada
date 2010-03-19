-----------------------------------------------------------------------
--                XML/Ada - An XML suite for Ada95                   --
--                                                                   --
--                       Copyright (C) 2007-2010, AdaCore            --
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
--
--  Some tests are disabled through the "disable" file

with Ada.Command_Line;          use Ada.Command_Line;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Exceptions;            use Ada.Exceptions;
with Ada.Float_Text_IO;         use Ada.Float_Text_IO;
with Ada.Strings.Hash;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with Ada.Text_IO;               use Ada.Text_IO;
with DOM.Core.Documents;        use DOM.Core.Documents;
with DOM.Core.Nodes;            use DOM.Core, DOM.Core.Nodes;
with DOM.Readers;               use DOM.Readers;
with GNAT.Command_Line;         use GNAT.Command_Line;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with Input_Sources.File;        use Input_Sources, Input_Sources.File;
with Sax.Readers;               use Sax.Readers;
with Schema.Readers;            use Schema.Readers;
with Schema.Schema_Readers;     use Schema.Schema_Readers;
with Schema.Validators;         use Schema.Validators;

procedure Schematest is

   Testdir : constant String := "xmlschema2006-11-06";
   Disable_File_List : constant String := "disable";

   Verbose       : Boolean := False;
   Debug         : Boolean := False;
   Test_XML      : Boolean := True;
   Test_Schemas  : Boolean := True;
   --  Whether to test the validity of XML or Schema files. If both are false,
   --  the only output will be for unexpected internal errors

   Show_Passed : Boolean := True;
   Show_Failed : Boolean := True;
   --  What tests output should be displayed ?

   Stats : Boolean := False;
   Stats_Hide_OK : Boolean := False;
   --  If True, print statistics of passed and failed tests per chapter

   Accepted_Only      : Boolean := True;
   --  If true, then only tests that are marked as "accepted" are run. Some
   --  tests might be under discussion, and have a status of "queried". Such
   --  tests are not run.

   Xlink : constant String := "http://www.w3.org/1999/xlink";

   type Error_Kind is (XSD_Should_Pass,
                       XSD_Should_Fail,
                       XML_Should_Pass,
                       XML_Should_Fail,
                       Internal_Error);
   type Errors_Count is array (Error_Kind) of Natural;
   --  The various categories of errors:
   --  Either the XSD was valid, but rejected by XML/Ada.
   --  Or the XSD was invalid, but accepted by XML/Ada
   --  Or the XML was valid, but validation failed in XML/Ada
   --  Or the XML was invalid, but validation passed in XML/Ada
   --  Or an internal unknown error.

   type Group_Result is record
      Name          : Ada.Strings.Unbounded.Unbounded_String;
      Descr         : Ada.Strings.Unbounded.Unbounded_String;
      Disabled      : Boolean := False;
      Test_Count    : Natural := 0;
      Errors        : Errors_Count := (others => 0);
      Parsed_XSD    : Natural := 0;
      Parsed_XML    : Natural := 0;
   end record;

   procedure Run_Testsuite  (Filename : String);
   procedure Run_Testset    (Filename : String);
   procedure Run_Test_Group
     (Testset : String; Group : Node; Base_Dir : String);
   procedure Parse_Schema_Test
     (Group          : in out Group_Result;
      Schema         : Node;
      Base_Dir       : String;
      Grammar        : out XML_Grammar;
      Schema_Files   : out Unbounded_String);
   procedure Parse_Instance_Test
     (Group          : in out Group_Result;
      Schema         : String;
      Test           : Node;
      Base_Dir       : String;
      Grammar        : XML_Grammar);
   --  Run the testsuite whose description is in Filename

   function Get_Attribute (N : Node; Attribute : String) return String;
   function Get_Attribute_NS (N : Node; URI, Local : String) return String;
   --  Query an attribute from N. The empty string is returned if the attribute
   --  does not exists

   procedure Parse_Disabled;
   --  Parse the list of disabled tests

   procedure Test_Header (Group : Group_Result; Schema, Test : String);
   procedure Expected (Group : Group_Result; Schema, Test, Msg : String);
   --  Print an error message

   package Group_Hash is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => Group_Result,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => "=");
   use Group_Hash;

   Groups : Group_Hash.Map;

   procedure Error
     (Kind  : Error_Kind;
      Group : in out Group_Result;
      Schema, Test, Msg : String);
   --  Print an error message.

   type Outcome_Value is (Valid, Invalid, NotKnown);
   function Get_Expected (N : Node) return Outcome_Value;
   --  Whether the test is expected to be valid or invalid

   type Status_Value is (Accepted, Queried);
   function Get_Status
     (Group : Group_Result;
      Schema, Test : String;
      N     : Node) return Status_Value;
   --  Get the status of the test

   procedure Print_Results;
   --  Print overview of results

   Last_Grp    : Unbounded_String;
   Last_Schema : Unbounded_String;
   --  Keep track of what was already output, to limit the amount of
   --  duplication

   --------------------
   -- Parse_Disabled --
   --------------------

   procedure Parse_Disabled is
      File : File_Type;
      Line : String (1 .. 1024);
      Last : Natural;
   begin
      Open (File, Mode => In_File, Name => Disable_File_List);

      while not End_Of_File (File) loop
         Get_Line (File, Line, Last);
         if Line (1) /= '-' and then Line (1) /= ' ' then
            Groups.Include
              (Key => Line (1 .. Last),
               New_Item => Group_Result'
                 (Name     => To_Unbounded_String (Line (1 .. Last)),
                  Disabled => True,
                  others   => <>));
         end if;
      end loop;

      Close (File);

   exception
      when Name_Error =>
         null;
   end Parse_Disabled;

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

   procedure Test_Header (Group : Group_Result; Schema, Test : String) is
   begin
      if Last_Grp /= Group.Name then
         Last_Grp    := Group.Name;
         Last_Schema := Null_Unbounded_String;
         Put_Line ("Grp: " & To_String (Group.Name));
      end if;

      if Schema /= To_String (Last_Schema) then
         Last_Schema := To_Unbounded_String (Schema);
         if Schema /= "" then
            Put_Line (Format_Pathname (Schema, UNIX));
         end if;
      end if;

      if Test /= "" then
         Put_Line (Format_Pathname (Test, UNIX));
      end if;
   end Test_Header;

   -----------
   -- Error --
   -----------

   procedure Error
     (Kind  : Error_Kind;
      Group : in out Group_Result;
      Schema, Test, Msg : String) is
   begin
      Group.Errors (Kind) := Group.Errors (Kind) + 1;

      if Show_Failed then
         Test_Header (Group, Schema, Test);
         Put_Line (Msg);
         New_Line;
      end if;
   end Error;

   --------------
   -- Expected --
   --------------

   procedure Expected (Group : Group_Result; Schema, Test, Msg : String) is
   begin
      if Show_Passed then
         Test_Header (Group, Schema, Test);
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
     (Group : Group_Result;
      Schema, Test : String;
      N     : Node) return Status_Value
   is
      N2 : Node := First_Child (N);
   begin
      while N2 /= null loop
         if Local_Name (N2) = "current" then
            if Get_Attribute (N2, "status") = "accepted" then
               return Accepted;
            elsif Get_Attribute (N2, "status") = "queried" then
               if Verbose then
                  Test_Header (Group, Schema, Test);
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
     (Group          : in out Group_Result;
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
        and then Get_Status (Group, Name, "", Schema) /= Accepted
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

               Group.Parsed_XSD := Group.Parsed_XSD + 1;
               Open (To_String (Document), Input);
               Parse (Reader, Input);
               Close (Input);
            end if;
            N := Next_Sibling (N);
         end loop;

         Grammar := Get_Created_Grammar (Reader);
         Global_Check (Grammar);

         if Test_Schemas and then Outcome = Invalid then
            Error (Kind    => XSD_Should_Fail,
                   Group   => Group,
                   Schema  => To_String (Document),
                   Test    => "",
                   Msg     => "(i)");
         end if;

      exception
         when E : XML_Validation_Error | XML_Fatal_Error =>
            Close (Input);
            if Test_Schemas and then Outcome = Valid then
               Error (Kind    => XSD_Should_Pass,
                      Group   => Group,
                      Schema  => To_String (Document),
                      Test    => "",
                      Msg     => "(v) " & Exception_Message (E));
            else
               --  The error message already includes the name of the
               --  document, so we do not repeat it
               Expected
                 (Group, To_String (Document), "",
                  Exception_Message (E));
            end if;

         when E : others =>
            Close (Input);
            Error (Kind    => Internal_Error,
                   Group   => Group,
                   Schema  => To_String (Document),
                   Test    => "",
                   Msg     => Exception_Information (E));
      end;
   end Parse_Schema_Test;

   -------------------------
   -- Parse_Instance_Test --
   -------------------------

   procedure Parse_Instance_Test
     (Group     : in out Group_Result;
      Schema    : String;
      Test      : Node;
      Base_Dir  : String;
      Grammar   : XML_Grammar)
   is
      Name     : constant String := Get_Attribute (Test, "name");
      Outcome  : constant Outcome_Value := Get_Expected (Test);
      N        : Node := First_Child (Test);
      Reader   : Validating_Reader;
      Input    : File_Input;
      Document : Unbounded_String;
   begin
      if Accepted_Only
        and then Get_Status (Group, Schema, Name, Test) /= Accepted
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

               Group.Parsed_XML := Group.Parsed_XML + 1;
               Open (To_String (Document), Input);
               Parse (Reader, Input);
               Close (Input);

               if Test_XML and then Outcome = Invalid then
                  Error (Kind    => XML_Should_Fail,
                         Group   => Group,
                         Schema  => Schema,
                         Test    => To_String (Document),
                         Msg     => "(i)");
               end if;

            exception
               when E : XML_Validation_Error | XML_Fatal_Error =>
                  Close (Input);
                  if Test_XML and then Outcome = Valid then
                     Error (Kind    => XML_Should_Pass,
                            Group   => Group,
                            Schema  => Schema,
                            Test    => To_String (Document),
                            Msg     => "(v) " & Exception_Message (E));
                  else
                     --  The error message already includes the name of the
                     --  document, so we do not repeat it
                     Expected (Group, Schema,
                               To_String (Document),
                               Exception_Message (E));
                  end if;

               when E : others =>
                  Close (Input);
                  Error (Kind    => Internal_Error,
                         Group   => Group,
                         Schema  => Schema,
                         Test    => To_String (Document),
                         Msg     => Exception_Message (E));
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
      Result : Group_Result;
   begin
      if Find (Groups, Name) /= Group_Hash.No_Element then
         Result := Group_Hash.Element (Groups, Name);
         if Result.Disabled then
            Put_Line ("Grp: " & Name & " (disabled)");
            New_Line;
            return;
         end if;
      else
         Result.Name := To_Unbounded_String (Testset & " / " & Name);
      end if;

      while N /= null loop
         if Local_Name (N) = "description" then
            Result.Descr := To_Unbounded_String
              (Node_Value (First_Child (N)));

         elsif Local_Name (N) = "schemaTest" then
            Result.Test_Count := Result.Test_Count + 1;
            Parse_Schema_Test
              (Result, N, Base_Dir,
               Grammar      => Schema,
               Schema_Files => Schema_Files);
            if Schema = No_Grammar then
               --  We couldn't parse the grammar, or the tests isn't "accepted"
               --  so nothing else to do
               exit;
            end if;

         elsif Local_Name (N) = "instanceTest" then
            Result.Test_Count := Result.Test_Count + 1;
            Parse_Instance_Test
              (Result, To_String (Schema_Files), N, Base_Dir, Schema);
         end if;

         N := Next_Sibling (N);
      end loop;

      Group_Hash.Include (Groups, Name, Result);
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

   procedure Print_Results is
      type Group_Kind is (Fully_Passed, Partially_Passed, Fully_Failed);

      Total_Error : Natural := 0;
      Total_Tests : Natural := 0;
      Total_XML   : Natural := 0;
      Total_XSD   : Natural := 0;
      Group_Total : Natural;
      Errors      : array (Group_Kind) of Errors_Count :=
        (others => (others => 0));
      Group       : Group_Hash.Cursor := Group_Hash.First (Groups);
      Kind        : Group_Kind;
      Group_Should_Fail : Natural;
      Gr          : Group_Result;
   begin
      Put_Line (Base_Name (Command_Name, ".exe"));

      if Accepted_Only then
         Put_Line ("Tests marked by W3C as non-accepted were not run");
      end if;

      while Has_Element (Group) loop
         Gr := Group_Hash.Element (Group);

         Total_Tests := Total_Tests + Gr.Test_Count;
         Total_XML   := Total_XML   + Gr.Parsed_XML;
         Total_XSD   := Total_XSD   + Gr.Parsed_XSD;

         Group_Total := 0;
         Group_Should_Fail :=
           Gr.Errors (XSD_Should_Fail) + Gr.Errors (XML_Should_Fail);

         for K in Gr.Errors'Range loop
            Group_Total := Group_Total + Gr.Errors (K);
         end loop;

         Total_Error := Total_Error + Group_Total;

         if Group_Total = 0 then
            Kind := Fully_Passed;
         elsif Group_Total = Gr.Test_Count then
            Kind := Fully_Failed;
         else
            Kind := Partially_Passed;
         end if;

         for K in Gr.Errors'Range loop
            Errors (Kind)(K)  := Errors (Kind)(K) + Gr.Errors (K);
         end loop;

         if Stats then
            if Gr.Disabled then
               Put_Line ("    --disabled--");

            elsif Group_Total /= 0 or not Stats_Hide_OK then
               New_Line;
               Put_Line (To_String (Gr.Name));

               if Gr.Descr /= Null_Unbounded_String then
                  Put_Line (To_String (Gr.Descr));
               end if;

               Put_Line ("   tests=" & Gr.Test_Count'Img
                         & " including xsd=" & Gr.Parsed_XSD'Img
                         & " xml=" & Gr.Parsed_XML'Img);
               Put ("   OK ="
                    & Integer'Image (Gr.Test_Count - Group_Total)
                    & "  FAILED ="
                    & Integer'Image (Group_Total) & " ie ");
               Put (100.0 * Float (Group_Total) / Float (Gr.Test_Count),
                    Aft => 0, Exp => 0);
               Put_Line (" %");

               if Group_Should_Fail /= 0 then
                  Put_Line
                    ("   accepted but should have failed:"
                     & Integer'Image (Group_Should_Fail)
                     & " (xsd=" & Gr.Errors (XSD_Should_Fail)'Img
                     & " xml="  & Gr.Errors (XML_Should_Fail)'Img & ")");
               end if;

               if Gr.Errors
                 (XSD_Should_Pass) + Gr.Errors (XML_Should_Pass) /= 0
               then
                  Put_Line
                    ("   rejected but should have passed:"
                     & Integer'Image
                       (Gr.Errors (XSD_Should_Pass)
                        + Gr.Errors (XML_Should_Pass))
                     & " (xsd=" & Gr.Errors (XSD_Should_Pass)'Img
                     & " xml="  & Gr.Errors (XML_Should_Pass)'Img & ")");
               end if;

               if Gr.Errors (Internal_Error) /= 0 then
                  Put_Line
                    ("  internal errors:"
                     & Gr.Errors (Internal_Error)'Img);
               end if;
            end if;
         end if;

         Next (Group);
      end loop;

      if Stats then
         New_Line;
      end if;

      Put_Line ("Total number of tests:" & Total_Tests'Img);
      Put_Line ("  " & Total_XSD'Img
                & " XSD files (not including those parsed"
                & " automatically)");
      Put_Line ("  " & Total_XML'Img & " XML files");
      New_Line;

      for K in Error_Kind'Range loop
         case K is
            when XSD_Should_Pass =>
               Put ("XSD KO, should be OK:");
            when XSD_Should_Fail =>
               Put ("XSD OK, should be KO:");
            when XML_Should_Pass =>
               Put ("XML KO, should be OK:");
            when XML_Should_Fail =>
               Put ("XML OK, should be KO:");
            when Internal_Error =>
               Put ("Internal error:");
         end case;

         for Kind in Group_Kind'Range loop
            case Kind is
               when Fully_Passed =>
                  Put (" in_full_pass=");
               when Fully_Failed =>
                  Put (" in_full_fail=");
               when Partially_Passed =>
                  Put (" in_partial=");
            end case;
            Put (Errors (Kind)(K)'Img);
         end loop;

         New_Line;
      end loop;

      Put ("Total errors:" & Total_Error'Img & " (");
      Put (100.0 * Float (Total_Error) / Float (Total_Tests),
           Aft => 0, Exp => 0);
      Put_Line (" %)");
   end Print_Results;

begin
   if not Is_Directory (Testdir) then
      Put_Line (Standard_Error, "No such directory: " & Testdir);
      return;
   end if;

   loop
      case Getopt ("v d x s e f h a -stats -statsKO") is
         when 'h'    =>
            Put_Line ("-v   Verbose mode");
            Put_Line ("-d   Debug mode");
            Put_Line ("-x   Disable testing of XML file validity");
            Put_Line ("-s   Disable testing of XSD file validity");
            Put_Line ("-a   Also run ambiguous tests under discussion");
            New_Line;
            Put_Line ("-e   Hide PASSED tests");
            Put_Line ("-f   Hide FAILED tests in the output");
            New_Line;
            Put_Line ("--stats    Print statistics (per chapter)");
            Put_Line ("--statsKO  Print stats only for failed groups");
            return;

         when 'v'    => Verbose := True;
         when 'd'    => Debug   := True;
         when 'x'    => Test_XML := False;
         when 's'    => Test_Schemas := False;
         when 'e'    => Show_Passed := False;
         when 'f'    => Show_Failed := False;
         when 'a'    => Accepted_Only := False;
         when '-'    =>
            Stats := True;
            if Full_Switch = "-statsKO" then
               Stats_Hide_OK := True;
            end if;
         when others => exit;
      end case;
   end loop;

   Parse_Disabled;

   if Debug then
      Schema.Readers.Set_Debug_Output (True);
      Schema.Validators.Set_Debug_Output (True);
      Schema.Schema_Readers.Set_Debug_Output (True);
   end if;

   Change_Dir (Testdir);
   Run_Testsuite ("suite.xml");

   Print_Results;
end Schematest;
