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
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Exceptions;            use Ada.Exceptions;
with Ada.Float_Text_IO;         use Ada.Float_Text_IO;
with Ada.Strings.Hash;
with Ada.Strings.Maps;          use Ada.Strings.Maps;
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

   Alternative_Dir : constant String :=
     "XML/xml-schema-test-suite/2004-01-14/xmlschema2006-11-06";
   --  Where we might find the CVS checkout of W3C, which contains more
   --  up-to-date metadata. Whenever possible, we use files from this
   --  directory

   Disable_File_List : constant String := "disable";

   Check_Alternative_Dir : Boolean := False;

   Verbose       : Boolean := False;
   Debug         : Boolean := False;

   Show_Files : Boolean := False;
   --  Whether to show the XML and XSD file names in test results

   Show_Descr : Boolean := False;
   --  Whether to show group descriptions

   Hide_Fully_Failed_Groups : Boolean := False;
   --  If True, fully failed groups are not displayed, assuming this is an
   --  unimplemented feature.

   Accepted_Only      : Boolean := True;
   --  If true, then only tests that are marked as "accepted" are run. Some
   --  tests might be under discussion, and have a status of "queried". Such
   --  tests are not run.

   Features : Supported_Features := All_Features;

   Xlink : constant String := "http://www.w3.org/1999/xlink";

   type Result_Kind is (Passed,
                        Not_Accepted,
                        Not_Implemented,
                        XSD_Should_Pass,
                        XSD_Should_Fail,
                        XML_Should_Pass,
                        XML_Should_Fail,
                        Internal_Error);
   subtype Error_Kind is Result_Kind range Not_Implemented .. Internal_Error;
   type Result_Count is array (Result_Kind) of Natural;
   --  The various categories of errors:
   --  Either the XSD was valid, but rejected by XML/Ada.
   --  Or the XSD was invalid, but accepted by XML/Ada
   --  Or the XML was valid, but validation failed in XML/Ada
   --  Or the XML was invalid, but validation passed in XML/Ada
   --  Or an internal unknown error.

   type Test_Result is record
      Name   : Ada.Strings.Unbounded.Unbounded_String;
      Msg    : Ada.Strings.Unbounded.Unbounded_String;
      XSD    : Ada.Strings.Unbounded.Unbounded_String;
      XML    : Ada.Strings.Unbounded.Unbounded_String;
      Kind   : Result_Kind;
   end record;

   package Test_Result_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Test_Result);
   use Test_Result_Lists;

   type Group_Kind is (Fully_Passed, Partially_Passed, Fully_Failed);

   type Group_Result is record
      Name          : Ada.Strings.Unbounded.Unbounded_String;
      Descr         : Ada.Strings.Unbounded.Unbounded_String;
      Tests         : Test_Result_Lists.List;
      Kind          : Group_Kind;
      Disabled      : Boolean := False;
      Test_Count    : Natural := 0;
      Counts        : Result_Count := (others => 0);
      Parsed_XSD    : Natural := 0;
      Parsed_XML    : Natural := 0;
   end record;

   Filter : array (Result_Kind) of Boolean := (others => True);

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
      Schema         : Unbounded_String;
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

   package Group_Hash is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => Group_Result,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => "=");
   use Group_Hash;

   Groups : Group_Hash.Map;

   type Outcome_Value is (Valid, Invalid, NotKnown);
   function Get_Expected (N : Node) return Outcome_Value;
   --  Whether the test is expected to be valid or invalid

   type Status_Value is (Accepted, Queried);
   function Get_Status (N : Node) return Status_Value;
   --  Get the status of the test

   procedure Print_Group_Results (Group : Group_Result);
   --  Print the results for the specific group

   procedure Print_Results;
   --  Print overview of results

   procedure Set_Description
     (Result : in out Group_Result;
      Annotation : Node);
   --  Set the description of the group

   procedure Load (File : String; Input : in out File_Input'Class);
   --  Open File, loading from the alternative directory if the file is
   --  found, or from Testdir otherwise

   ----------
   -- Load --
   ----------

   procedure Load (File : String; Input : in out File_Input'Class) is
   begin
      if Check_Alternative_Dir then
         if Is_Regular_File (Alternative_Dir & Directory_Separator & File) then
            if Verbose then
               Put_Line
                 ("Load " & Alternative_Dir & Directory_Separator & File);
            end if;

            Open (Alternative_Dir & Directory_Separator & File, Input);
            return;
         end if;
      end if;

      if Verbose then
         Put_Line ("Load " & Testdir & Directory_Separator & File);
      end if;

      Open (Testdir & Directory_Separator & File, Input);
   end Load;

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
                  Kind     => Fully_Passed,
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

   function Get_Status (N : Node) return Status_Value is
      N2 : Node := First_Child (N);
   begin
      while N2 /= null loop
         if Local_Name (N2) = "current" then
            if Get_Attribute (N2, "status") = "accepted"
              or else Get_Attribute (N2, "status") = "stable"
            then
               return Accepted;
            elsif Get_Attribute (N2, "status") = "queried"
              or else Get_Attribute (N2, "status") = "disputed-spec"
              or else Get_Attribute (N2, "status") = "disputed-test"
              or else Get_Attribute (N2, "status") = "disputedTest"
            then
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
      Result   : Test_Result;
      Name     : constant String := Get_Attribute (Schema, "name");
      Reader   : Schema_Reader;
      Input    : File_Input;
      N        : Node := First_Child (Schema);
      Outcome  : constant Outcome_Value := Get_Expected (Schema);
   begin
      Result.Name   := To_Unbounded_String (Name);
      Result.Kind   := Passed;
      Grammar       := No_Grammar;
      Schema_Files  := Null_Unbounded_String;

      if Accepted_Only and then Get_Status (Schema) /= Accepted then
         --  Do not increment Group.Test_Count
         Result.Kind      := Not_Accepted;

      else
         begin
            Set_Feature (Reader, Schema_Validation_Feature, True);
            Set_Created_Grammar (Reader, No_Grammar);
            Use_Basename_In_Error_Messages (Reader, True);
            Set_Supported_Features (Reader, Features);

            Group.Test_Count := Group.Test_Count + 1;

            while N /= null loop
               if Local_Name (N) = "schemaDocument" then
                  Group.Parsed_XSD := Group.Parsed_XSD + 1;

                  if Schema_Files /= Null_Unbounded_String then
                     Append (Schema_Files, " - ");
                  end if;

                  Load (Normalize_Pathname
                        (Get_Attribute_NS (N, Xlink, "href"),
                         Base_Dir, Resolve_Links => False),
                    Input);
                  Result.XSD := To_Unbounded_String (Get_System_Id (Input));
                  Append (Schema_Files, Result.XSD);

                  Parse (Reader, Input);
                  Close (Input);
               end if;

               N := Next_Sibling (N);
            end loop;

            Grammar := Get_Created_Grammar (Reader);
            Global_Check (Grammar);

            if Outcome = Invalid then
               Result.Kind  := XSD_Should_Fail;
               Grammar := No_Grammar;
            end if;

         exception
            when E : XML_Not_Implemented =>
               Close (Input);
               Result.Kind := Not_Implemented;
               Result.Msg  := To_Unbounded_String (Exception_Message (E));
               Grammar     := No_Grammar;

            when E : XML_Validation_Error | XML_Fatal_Error =>
               Close (Input);
               if Outcome = Valid then
                  Result.Kind := XSD_Should_Pass;
                  Result.Msg  := To_Unbounded_String (Exception_Message (E));
                  Grammar     := No_Grammar;
               else
                  Result.Kind := Passed;
                  Result.Msg  := To_Unbounded_String (Exception_Message (E));
               end if;

            when E : others =>
               Close (Input);
               Result.Kind := Internal_Error;
               Result.Msg  := To_Unbounded_String (Exception_Message (E));
               Grammar     := No_Grammar;
         end;
      end if;

      Append (Group.Tests, Result);
   end Parse_Schema_Test;

   -------------------------
   -- Parse_Instance_Test --
   -------------------------

   procedure Parse_Instance_Test
     (Group     : in out Group_Result;
      Schema    : Unbounded_String;
      Test      : Node;
      Base_Dir  : String;
      Grammar   : XML_Grammar)
   is
      Result   : Test_Result;
      Name     : constant String := Get_Attribute (Test, "name");
      Outcome  : constant Outcome_Value := Get_Expected (Test);
      N        : Node := First_Child (Test);
      Reader   : Validating_Reader;
      Input    : File_Input;
      Tmp_Gr   : Group_Result;
   begin
      if Find (Groups, To_String (Group.Name) & " / " & Name) /=
        Group_Hash.No_Element
      then
         Tmp_Gr := Group_Hash.Element
           (Groups, To_String (Group.Name) & " / " & Name);
         if Tmp_Gr.Disabled then
            Put_Line ("Test: " & To_String (Tmp_Gr.Name) & " (disabled)");
            New_Line;
            return;
         end if;
      end if;

      Result.Name := To_Unbounded_String (Name);
      Result.Kind := Passed;
      Result.XSD  := Schema;

      if Accepted_Only and then Get_Status (Test) /= Accepted then
         --  Do not increment Group.Test_Count
         Result.Kind := Not_Accepted;
         Append (Group.Tests, Result);
         return;
      end if;

      Use_Basename_In_Error_Messages (Reader, True);
      Set_Validating_Grammar (Reader, Grammar);
      Set_Feature (Reader, Schema_Validation_Feature, True);

      while N /= null loop
         if Local_Name (N) = "instanceDocument" then
            begin
               Group.Parsed_XML := Group.Parsed_XML + 1;

               Result.Kind := Passed;
               Load (Normalize_Pathname
                     (Get_Attribute_NS (N, Xlink, "href"),
                      Base_Dir, Resolve_Links => False),
                     Input);
               Result.XML := To_Unbounded_String (Get_System_Id (Input));

               Parse (Reader, Input);
               Close (Input);

               if Outcome = Invalid then
                  Result.Kind := XML_Should_Fail;
               end if;

            exception
               when E : XML_Not_Implemented =>
                  Close (Input);
                  Result.Kind := Not_Implemented;
                  Result.Msg  := To_Unbounded_String (Exception_Message (E));

               when E : XML_Validation_Error | XML_Fatal_Error =>
                  Close (Input);
                  if Outcome = Valid then
                     Result.Kind := XML_Should_Pass;
                     Result.Msg  :=
                       To_Unbounded_String (Exception_Message (E));
                  else
                     Result.Kind := Passed;
                     Result.Msg  :=
                       To_Unbounded_String (Exception_Message (E));
                  end if;

               when E : others =>
                  Close (Input);
                  Result.Kind := Internal_Error;
                  Result.Msg  := To_Unbounded_String (Exception_Message (E));
            end;

            Group.Test_Count := Group.Test_Count + 1;
            Append (Group.Tests, Result);  --  A copy of Result
         end if;
         N := Next_Sibling (N);
      end loop;
   end Parse_Instance_Test;

   ---------------------
   -- Set_Description --
   ---------------------

   procedure Set_Description
     (Result : in out Group_Result;
      Annotation : Node)
   is
      N  : Node := First_Child (Annotation);
      N2, N3 : Node;
   begin
      while N /= null loop
         if Local_Name (N) = "documentation" then
            N2 := First_Child (N);
            while N2 /= null loop
               if Local_Name (N2) = "Description" then
                  N3 := First_Child (N2);
                  while N3 /= null loop
                     Append (Result.Descr, Node_Value (N3));
                     N3 := Next_Sibling (N3);
                  end loop;

               elsif Node_Type (N2) = Text_Node then
                  Append (Result.Descr, Node_Value (N2));
               end if;

               N2 := Next_Sibling (N2);
            end loop;
         end if;

         N := Next_Sibling (N);
      end loop;

      Trim (Result.Descr,
            To_Set (" " & ASCII.HT & ASCII.LF),
            To_Set (" " & ASCII.HT & ASCII.LF));
   end Set_Description;

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
      Total_Errors : Natural := 0;
      Cursor : Test_Result_Lists.Cursor;
      Kind   : Result_Kind;
   begin
      Result.Name := To_Unbounded_String (Testset & " / " & Name);
      Result.Counts := (others => 0);

      if Find (Groups, To_String (Result.Name)) /= Group_Hash.No_Element then
         Result := Group_Hash.Element (Groups, To_String (Result.Name));
         if Result.Disabled then
            Put_Line ("Grp: " & To_String (Result.Name) & " (disabled)");
            New_Line;
            return;
         else
            Put_Line ("Reusing existing group for "
                      & To_String (Result.Name));
         end if;
      end if;

      while N /= null loop
         if Local_Name (N) = "annotation" then
            Set_Description (Result, N);

         elsif Local_Name (N) = "schemaTest" then
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
            Parse_Instance_Test (Result, Schema_Files, N, Base_Dir, Schema);
         end if;

         N := Next_Sibling (N);
      end loop;

      Cursor := First (Result.Tests);
      while Has_Element (Cursor) loop
         Kind := Test_Result_Lists.Element (Cursor).Kind;
         Result.Counts (Kind) := Result.Counts (Kind) + 1;
         if Kind in Error_Kind then
            Total_Errors := Total_Errors + 1;
         end if;

         Next (Cursor);
      end loop;

      if Total_Errors = 0 then
         Result.Kind := Fully_Passed;
      elsif Total_Errors = Result.Test_Count then
         Result.Kind := Fully_Failed;
      else
         Result.Kind := Partially_Passed;
      end if;

      Print_Group_Results (Result);
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
      Load (Filename, Input);
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
      Load (Filename, Input);
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

   -------------------------
   -- Print_Group_Results --
   -------------------------

   procedure Print_Group_Results (Group : Group_Result) is
      Total  : Natural := 0;
      Cursor : Test_Result_Lists.Cursor := First (Group.Tests);
      Test   : Test_Result;
      Show_Group : Boolean := False;
   begin
      if Group.Kind = Fully_Failed and then Hide_Fully_Failed_Groups then
         return;
      end if;

      while Has_Element (Cursor) loop
         if Filter (Test_Result_Lists.Element (Cursor).Kind) then
            Show_Group := True;
            exit;
         end if;
         Next (Cursor);
      end loop;

      if not Show_Group then
         return;
      end if;

      for K in Error_Kind loop
         Total := Total + Group.Counts (K);
      end loop;

      Put_Line ("Grp: " & To_String (Group.Name));

      if Show_Descr and then Group.Descr /= "" then
         Put_Line ("  " & To_String (Group.Descr));
      end if;

      if Group.Disabled then
         Put_Line ("  --disabled--");

      else
         Put ("  ");
         Put (100.0 * Float (Group.Counts (Passed)) / Float (Group.Test_Count),
              Aft => 0, Exp => 0);
         Put_Line ("% tests=" & Group.Test_Count'Img
                   & " (xsd=" & Group.Parsed_XSD'Img
                   & " xml="  & Group.Parsed_XML'Img
                   & ") OK=" & Group.Counts (Passed)'Img
                   & " FAILED=" & Integer'Image (Total));

         Cursor := First (Group.Tests);
         while Has_Element (Cursor) loop
            Test := Test_Result_Lists.Element (Cursor);

            if Filter (Test.Kind) then
               case Test.Kind is
                  when Passed          => Put ("  OK ");
                  when Not_Accepted    => Put ("  NA ");
                  when XSD_Should_Fail => Put ("  SF ");
                  when XSD_Should_Pass => Put ("  SP ");
                  when XML_Should_Fail => Put ("  XF ");
                  when XML_Should_Pass => Put ("  XP ");
                  when Internal_Error  => Put ("  IE ");
                  when Not_Implemented => Put ("  NI ");
               end case;

               Put_Line (To_String (Test.Name));

               if Show_Files then
                  Put ("    ./testschema");
                  if Test.XSD /= "" then
                     Put (" -xsd " & To_String (Test.XSD));
                  end if;

                  if Test.XML /= "" then
                     Put (" " & To_String (Test.XML));
                  end if;

                  New_Line;
               end if;

               if Test.Msg /= "" then
                  Put_Line ("    " & To_String (Test.Msg));
               end if;
            end if;

            Next (Cursor);
         end loop;

         New_Line;
      end if;
   end Print_Group_Results;

   -------------------
   -- Print_Results --
   -------------------

   procedure Print_Results is
      Total_Error : Natural := 0;
      Total_Tests : Natural := 0;
      Total_XML   : Natural := 0;
      Total_XSD   : Natural := 0;
      Errors      : array (Group_Kind) of Result_Count :=
        (others => (others => 0));
      Group       : Group_Hash.Cursor := Group_Hash.First (Groups);
      Gr          : Group_Result;
   begin
      while Has_Element (Group) loop
         Gr := Group_Hash.Element (Group);

         Total_Tests := Total_Tests + Gr.Test_Count;
         Total_XML   := Total_XML   + Gr.Parsed_XML;
         Total_XSD   := Total_XSD   + Gr.Parsed_XSD;

         for K in Error_Kind loop
            if K /= Not_Implemented then
               Total_Error := Total_Error + Gr.Counts (K);
            end if;

            Errors (Gr.Kind)(K) := Errors (Gr.Kind)(K) + Gr.Counts (K);
         end loop;

         Next (Group);
      end loop;

      Put_Line ("Total number of tests:" & Total_Tests'Img
                & " (omitting non-accepted tests)");
      Put_Line ("  " & Total_XSD'Img
                & " XSD files (not including those parsed"
                & " automatically)");
      Put_Line ("  " & Total_XML'Img & " XML files");

      for K in Error_Kind loop
         case K is
            when XSD_Should_Pass =>
               Put ("SP: XSD KO, should be OK:");
            when XSD_Should_Fail =>
               Put ("SF: XSD OK, should be KO:");
            when XML_Should_Pass =>
               Put ("XP: XML KO, should be OK:");
            when XML_Should_Fail =>
               Put ("XF: XML OK, should be KO:");
            when Not_Implemented =>
               Put ("NI: Features not implemented:");
            when Internal_Error =>
               Put ("IE: Internal error:");
         end case;

         for Kind in Group_Kind'Range loop
            if Errors (Kind)(K) /= 0 then
               case Kind is
                  when Fully_Passed =>
                     Put (" in_full_pass=");
                  when Fully_Failed =>
                     Put (" in_full_fail=");
                  when Partially_Passed =>
                     Put (" in_partial=");
               end case;
               Put (Errors (Kind)(K)'Img);
            end if;
         end loop;

         New_Line;
      end loop;

      Put ("Total errors:" & Total_Error'Img & " (");
      Put (100.0 * Float (Total_Error) / Float (Total_Tests),
           Aft => 0, Exp => 0);
      Put_Line (" %)");
   end Print_Results;

   Setting  : Boolean;
begin
   if not Is_Directory (Testdir) then
      Put_Line (Standard_Error, "No such directory: " & Testdir);
      return;
   end if;

   loop
      case Getopt ("v d a h f -filter: -descr -group -hide: -feature:"
                   & " -cvs") is
         when 'h'    =>
            Put_Line ("-v   Verbose mode");
            Put_Line ("-d   Debug mode");
            Put_Line ("-f   Show XSD and XML file names in results");
            Put_Line ("-a   Also run ambiguous tests under discussion");
            Put_Line
              ("--filter [PA,NA,SP,SF,XP,XF,IE] only show those tests.");
            Put_Line ("     Separate categories with commas.");
            Put_Line ("     This will also only matching groups.");
            Put_Line
              ("--hide [PA,NA,SP,SF,XP,XF,IE] only show those tests.");
            Put_Line ("     Opposite of --filter, cannot be combined");
            Put_Line ("--descr Show group descriptions");
            Put_Line ("--cvs   Check the CVS checkout of W3C (see README file)"
                      & " for more up-to-date data");
            Put_Line ("--group Hide fully failed groups");
            Put_Line ("     These likely show unimplemented features");
            Put_Line ("--feature name    Disable support for a feature");
            Put_Line
              ("     Valid names are: subgroup");
            Put_Line
              ("     xsd_1_0");
            return;

         when 'v' => Verbose := True;
         when 'd' => Debug   := True;
         when 'f' => Show_Files := True;
         when '-' =>
            if Full_Switch = "-group" then
               Hide_Fully_Failed_Groups := True;

            elsif Full_Switch = "-cvs" then
               Check_Alternative_Dir := True;

            elsif Full_Switch = "-feature" then
               if Parameter = "subgroup" then
                  Features.Substitution_Group := False;
               elsif Parameter = "xsd_1_0" then
                  Features.XSD_Version := XSD_1_0;
               else
                  Put_Line ("Invalid feature name");
                  return;
               end if;

            elsif Full_Switch = "-filter"
              or else Full_Switch = "-hide"
            then
               Setting := Full_Switch = "-filter";

               Filter := (others => not Setting);
               declare
                  F : constant String := Parameter;
                  Prev : Integer := F'First;
                  Pos  : Integer := F'First - 1;
               begin
                  loop
                     Pos := Pos + 1;
                     if Pos > F'Last or else F (Pos) = ',' then
                        if F (Prev .. Pos - 1) = "SF" then
                           Filter (XSD_Should_Fail) := Setting;
                        elsif F (Prev .. Pos - 1) = "SP" then
                           Filter (XSD_Should_Pass) := Setting;
                        elsif F (Prev .. Pos - 1) = "XF" then
                           Filter (XML_Should_Fail) := Setting;
                        elsif F (Prev .. Pos - 1) = "XP" then
                           Filter (XML_Should_Pass) := Setting;
                        elsif F (Prev .. Pos - 1) = "PA" then
                           Filter (Passed) := Setting;
                        elsif F (Prev .. Pos - 1) = "NA" then
                           Filter (Not_Accepted) := Setting;
                        elsif F (Prev .. Pos - 1) = "IE" then
                           Filter (Internal_Error) := Setting;
                        elsif F (Prev .. Pos - 1) = "NI" then
                           Filter (Not_Implemented) := Setting;
                        else
                           Put_Line ("Invalid filter: " & F (Prev .. Pos - 1));
                           return;
                        end if;

                        Prev := Pos + 1;
                        exit when Pos > F'Last;
                     end if;
                  end loop;
               end;

            elsif Full_Switch = "-descr" then
               Show_Descr := True;

            else
               Put_Line ("Invalid switch: -" & Full_Switch);
            end if;
         when 'a'    => Accepted_Only := False;
         when others => exit;
      end case;
   end loop;

   Parse_Disabled;

   if Debug then
      Schema.Readers.Set_Debug_Output (True);
      Schema.Validators.Set_Debug_Output (True);
      Schema.Schema_Readers.Set_Debug_Output (True);
   end if;

   Put_Line (Base_Name (Command_Name, ".exe"));

   if Accepted_Only then
      Put_Line ("Tests marked by W3C as non-accepted were not run");
   end if;

   Run_Testsuite ("suite.xml");

   Print_Results;
end Schematest;
