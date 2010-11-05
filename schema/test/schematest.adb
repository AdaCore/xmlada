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
with Sax.Symbols;               use Sax.Symbols;
with Sax.Utils;                 use Sax.Utils;
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

   Symbols : Symbol_Table;
   Test_Set_Ref : Symbol;
   Test_Group   : Symbol;
   S_Annotation : Symbol;
   S_Schema_Test, S_Instance_Test, S_Documentation, S_Description : Symbol;
   S_Instance_Document, S_Schema_Document, S_Current, S_Expected : Symbol;

   S_Validity, S_Status, S_Name, S_Href, S_Schema_Version : Symbol;
   S_Release_Date, S_Xlink : Symbol;

   --  Shared symbol table (optional, this would be created automatically by
   --  each parser otherwise, it is just more efficient in the number of calls
   --  to malloc this way)

   Accepted_Only      : Boolean := True;
   --  If true, then only tests that are marked as "accepted" are run. Some
   --  tests might be under discussion, and have a status of "queried". Such
   --  tests are not run.

   XSD_Version : XSD_Versions := XSD_1_1;

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

   type Group_Result is record
      Name          : Ada.Strings.Unbounded.Unbounded_String;
      Descr         : Ada.Strings.Unbounded.Unbounded_String;
      Tests         : Test_Result_Lists.List;
      Disabled      : Boolean := False;
      Test_Count    : Result_Count := (others => 0);
      Counts        : Result_Count := (others => 0);
      Parsed_XSD    : Natural := 0;
      Parsed_XML    : Natural := 0;
   end record;

   Filter : array (Result_Kind) of Boolean := (others => True);

   procedure Run_Testsuite  (Filename : String);
   procedure Run_Testset (Filename : String; Grammar : in out XML_Grammar);
   procedure Run_Test_Group
     (Testset  : String;
      Group    : Node;
      Base_Dir : String;
      Grammar  : in out XML_Grammar);
   procedure Parse_Schema_Test
     (Group          : in out Group_Result;
      Schema         : Node;
      Base_Dir       : String;
      Failed_Grammar : out Boolean;
      Grammar        : in out XML_Grammar;
      Schema_Files   : out Unbounded_String);
   procedure Parse_Instance_Test
     (Group          : in out Group_Result;
      Schema         : Unbounded_String;
      Test           : Node;
      Base_Dir       : String;
      Grammar        : XML_Grammar;
      Failed_Grammar : Boolean);
   --  Run the testsuite whose description is in Filename

   function Get_Attribute (N : Node; Attribute : Symbol) return String;
   function Get_Attribute_NS (N : Node; URI, Local : Symbol) return String;
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

   function Get_Attribute (N : Node; Attribute : Symbol) return String is
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

   function Get_Attribute_NS (N : Node; URI, Local : Symbol) return String is
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
         if Local_Name (N2) = S_Expected then
            if Get_Attribute (N2, S_Validity) = "valid" then
               return Valid;
            elsif Get_Attribute (N2, S_Validity) = "invalid" then
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
         if Local_Name (N2) = S_Current then
            if Get_Attribute (N2, S_Status) = "accepted"
              or else Get_Attribute (N2, S_Status) = "stable"
            then
               return Accepted;
            elsif Get_Attribute (N2, S_Status) = "queried"
              or else Get_Attribute (N2, S_Status) = "disputed-spec"
              or else Get_Attribute (N2, S_Status) = "disputed-test"
              or else Get_Attribute (N2, S_Status) = "disputedTest"
            then
               return Queried;
            else
               Put_Line ("Invalid status: " & Get_Attribute (N2, S_Status));
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
      Failed_Grammar : out Boolean;
      Grammar        : in out XML_Grammar;
      Schema_Files   : out Unbounded_String)
   is
      Result   : Test_Result;
      Name     : constant String := Get_Attribute (Schema, S_Name);
      XSD_Reader   : Schema_Reader;
      Input    : File_Input;
      N        : Node := First_Child (Schema);
      Outcome  : constant Outcome_Value := Get_Expected (Schema);
      Kind     : Result_Kind;
   begin
      if Verbose then
         Put_Line ("Parse_Schema_Test: " & Name);
      end if;

      Failed_Grammar := False;
      Result.Name    := To_Unbounded_String (Name);
      Result.Kind    := Passed;
      Schema_Files   := Null_Unbounded_String;

      if Accepted_Only and then Get_Status (Schema) /= Accepted then
         --  Do not increment Group.Test_Count
         Result.Kind    := Not_Accepted;
         Failed_Grammar := True;

      else
         begin
            Set_Symbol_Table (XSD_Reader, Symbols);  --  optional (efficiency)
            Set_Grammar (XSD_Reader, Grammar);
            Use_Basename_In_Error_Messages (XSD_Reader, True);

            if Outcome = Invalid then
               Kind := XSD_Should_Fail;
            else
               Kind := XSD_Should_Pass;
            end if;

            Group.Test_Count (Kind) := Group.Test_Count (Kind) + 1;

            while N /= null loop
               if Local_Name (N) = S_Schema_Document then
                  Group.Parsed_XSD := Group.Parsed_XSD + 1;

                  if Schema_Files /= Null_Unbounded_String then
                     Append (Schema_Files, " - ");
                  end if;

                  Load (Normalize_Pathname
                        (Get_Attribute_NS (N, S_Xlink, S_Href),
                         Base_Dir, Resolve_Links => False),
                    Input);

                  if Verbose then
                     Put_Line ("  Will parse: " & Get_System_Id (Input));
                  end if;

                  Result.XSD := To_Unbounded_String (Get_System_Id (Input));
                  Append (Schema_Files, Result.XSD);

                  Parse (XSD_Reader, Input);
                  Close (Input);
               end if;

               N := Next_Sibling (N);
            end loop;

            Grammar := Get_Grammar (XSD_Reader);

            if Outcome = Invalid then
               Result.Kind  := Kind;
               Failed_Grammar := True;
            end if;

         exception
            when Standard.Schema.XML_Not_Implemented =>
               Close (Input);
               Result.Kind := Not_Implemented;
               Result.Msg  := To_Unbounded_String
                  (Get_Error_Message (XSD_Reader));
               Failed_Grammar := True;

            when XML_Validation_Error =>
               Close (Input);
               Result.Msg  :=
                 To_Unbounded_String (Get_Error_Message (XSD_Reader));
               Failed_Grammar := True;
               if Outcome = Valid then
                  Result.Kind    := XSD_Should_Pass;
               else
                  Result.Kind := Passed;
               end if;

            when E : XML_Fatal_Error =>
               Close (Input);
               Result.Msg  := To_Unbounded_String (Exception_Message (E));
               if Outcome = Valid then
                  Result.Kind := XSD_Should_Pass;
               else
                  Result.Kind := Passed;
               end if;

            when E : others =>
               Close (Input);
               Result.Kind := Internal_Error;
               Result.Msg  := To_Unbounded_String (Exception_Information (E));
               Failed_Grammar := True;
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
      Grammar   : XML_Grammar;
      Failed_Grammar : Boolean)
   is
      Result   : Test_Result;
      Name     : constant String := Get_Attribute (Test, S_Name);
      Outcome  : constant Outcome_Value := Get_Expected (Test);
      N        : Node := First_Child (Test);
      Inst_Reader   : Validating_Reader;
      Input    : File_Input;
      Tmp_Gr   : Group_Result;
   begin
      if Verbose then
         Put_Line ("Parse_Instance_Test: " & Name);
      end if;

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

      Set_Symbol_Table (Inst_Reader, Symbols);  --  optional, for efficiency
      Use_Basename_In_Error_Messages (Inst_Reader, True);
      Set_Grammar (Inst_Reader, Grammar);
      Set_Feature (Inst_Reader, Schema_Validation_Feature, True);

      while N /= null loop
         if Local_Name (N) = S_Instance_Document then
            begin
               Group.Parsed_XML := Group.Parsed_XML + 1;

               if Outcome = Valid then
                  Group.Test_Count (XML_Should_Pass) :=
                    Group.Test_Count (XML_Should_Pass) + 1;
               else
                  Group.Test_Count (XML_Should_Fail) :=
                    Group.Test_Count (XML_Should_Fail) + 1;
               end if;

               Result.Kind := Passed;
               Load (Normalize_Pathname
                     (Get_Attribute_NS (N, S_Xlink, S_Href),
                      Base_Dir, Resolve_Links => False),
                     Input);
               Result.XML := To_Unbounded_String (Get_System_Id (Input));

               if Failed_Grammar then
                  if Outcome = Valid then
                     Result.Kind := XML_Should_Pass;
                     Result.Msg  :=
                       To_Unbounded_String ("XSD file could not be parsed");

                  else
                     --  We did expect to fail anyway. The error message might
                     --  not be correct though
                     null;
                  end if;

               else
                  Parse (Inst_Reader, Input);
                  Close (Input);

                  if Outcome = Invalid then
                     Result.Kind := XML_Should_Fail;
                  end if;
               end if;

            exception
               when Standard.Schema.XML_Not_Implemented =>
                  Close (Input);
                  Result.Kind := Not_Implemented;
                  Result.Msg  := To_Unbounded_String
                     (Get_Error_Message (Inst_Reader));

               when XML_Validation_Error =>
                  Close (Input);
                  Result.Msg  :=
                    To_Unbounded_String (Get_Error_Message (Inst_Reader));
                  if Outcome = Valid then
                     Result.Kind := XML_Should_Pass;
                  else
                     Result.Kind := Passed;
                  end if;

               when E : XML_Fatal_Error =>
                  Close (Input);
                  Result.Msg  := To_Unbounded_String (Exception_Message (E));
                  if Outcome = Valid then
                     Result.Kind := XML_Should_Pass;
                  else
                     Result.Kind := Passed;
                  end if;

               when E : others =>
                  Close (Input);
                  Result.Kind := Internal_Error;
                  Result.Msg  :=
                     To_Unbounded_String (Exception_Information (E));
            end;

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
         if Local_Name (N) = S_Documentation then
            N2 := First_Child (N);
            while N2 /= null loop
               if Local_Name (N2) = S_Description then
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
      Base_Dir   : String;
      Grammar    : in out XML_Grammar)
   is
      Name           : constant String := Get_Attribute (Group, S_Name);
      N              : Node := First_Child (Group);
      Schema_Files   : Unbounded_String;
      Result         : Group_Result;
      Total_Errors   : Natural := 0;
      Cursor         : Test_Result_Lists.Cursor;
      Kind           : Result_Kind;
      Failed_Grammar : Boolean := False;
   begin
      Reset (Grammar);  --  Optional optimization, keep the metaschema
      Result.Name := To_Unbounded_String (Testset & " / " & Name);
      Result.Counts := (others => 0);

      Set_XSD_Version (Grammar, XSD_Version);

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
         if Local_Name (N) = S_Annotation then
            Set_Description (Result, N);

         elsif Local_Name (N) = S_Schema_Test then
            Parse_Schema_Test
              (Result, N, Base_Dir,
               Failed_Grammar => Failed_Grammar,
               Grammar        => Grammar,
               Schema_Files   => Schema_Files);

            --  If we failed to parse the grammar, that might be accepted, so
            --  we'll still run each test, marking them all as "can't parse"
            --  (which might be the expected result)
            --  ??? For now, we simply do not run any of the tests. But there
            --  are situations where XML/Ada report an error on the XSD rather
            --  than on the XML (for instance disallowedSubst00503m4_n where
            --  we restrict a type that has block="restriction").

            exit when Failed_Grammar;

         elsif Local_Name (N) = S_Instance_Test then
            Parse_Instance_Test (Result, Schema_Files, N, Base_Dir, Grammar,
                                 Failed_Grammar);
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

      Print_Group_Results (Result);
      Group_Hash.Include (Groups, Name, Result);
   end Run_Test_Group;

   -----------------
   -- Run_Testset --
   -----------------

   procedure Run_Testset (Filename : String; Grammar : in out XML_Grammar) is
      Input  : File_Input;
      Reader : Tree_Reader;
      N      : Node;
      Name   : Unbounded_String;
   begin
      Set_Symbol_Table (Reader, Symbols);  --  optional, for efficiency

      Load (Filename, Input);
      Parse (Reader, Input);
      Close (Input);

      N := Get_Element (Get_Tree (Reader));
      Name := To_Unbounded_String (Get_Attribute (N, S_Name));

      if Verbose then
         Put_Line ("Testset: " & To_String (Name));
      end if;

      N := First_Child (N);
      while N /= null loop
         if Local_Name (N) = Test_Group then
            Run_Test_Group
              (Testset    => To_String (Name),
               Group      => N,
               Base_Dir   => Dir_Name (Filename),
               Grammar    => Grammar);
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
      Grammar : XML_Grammar := No_Grammar;
   begin
      Set_Symbol_Table (Reader, Symbols);  --  optional, for efficiency

      Load (Filename, Input);
      Parse (Reader, Input);
      Close (Input);

      N := Get_Element (Get_Tree (Reader));
      Put_Line ("Version: " & Get_Attribute (N, S_Schema_Version));
      Put_Line ("Release: " & Get_Attribute (N, S_Release_Date));

      N := First_Child (N);
      while N /= null loop
         if Local_Name (N) = Test_Set_Ref then
            Run_Testset
              (Normalize_Pathname
                 (Get_Attribute_NS (N, S_Xlink, S_Href),
                  Dir_Name (Filename),
                  Resolve_Links => False),
               Grammar => Grammar);
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
      Count      : Integer;
   begin
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

         Count := 0;
         for K in Group.Test_Count'Range loop
            Count := Count + Group.Test_Count (K);
         end loop;

         Put (100.0 * Float (Group.Counts (Passed)) / Float (Count),
              Aft => 0, Exp => 0);
         Put ("% tests=(");

         if Group.Test_Count (XSD_Should_Fail) /= 0 then
            Put (" sf=" & Group.Test_Count (XSD_Should_Fail)'Img);
         end if;
         if Group.Test_Count (XSD_Should_Pass) /= 0 then
            Put (" sp=" & Group.Test_Count (XSD_Should_Pass)'Img);
         end if;
         if Group.Test_Count (XML_Should_Fail) /= 0 then
            Put (" xf=" & Group.Test_Count (XML_Should_Fail)'Img);
         end if;
         if Group.Test_Count (XML_Should_Pass) /= 0 then
            Put (" xp=" & Group.Test_Count (XML_Should_Pass)'Img);
         end if;

         Put_Line ("} (xsd=" & Group.Parsed_XSD'Img
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
      Total       : Result_Count := (others => 0);
      Errors      : Result_Count := (others => 0);
      Group       : Group_Hash.Cursor := Group_Hash.First (Groups);
      Gr          : Group_Result;
   begin
      while Has_Element (Group) loop
         Gr := Group_Hash.Element (Group);

         for K in Gr.Test_Count'Range loop
            Total (K) := Total (K) + Gr.Test_Count (K);
            Total_Tests := Total_Tests + Gr.Test_Count (K);
         end loop;

         Total_XML   := Total_XML   + Gr.Parsed_XML;
         Total_XSD   := Total_XSD   + Gr.Parsed_XSD;

         for K in Error_Kind loop
            if K /= Not_Implemented then
               Total_Error := Total_Error + Gr.Counts (K);
            end if;

            Errors (K) := Errors (K) + Gr.Counts (K);
         end loop;

         Next (Group);
      end loop;

      Put_Line ("Total number of tests:" & Total_Tests'Img
                & " (omitting non-accepted tests)");
      Put_Line ("  " & Total_XSD'Img
                & " XSD files (not including those parsed"
                & " automatically)");
      Put_Line ("  " & Total_XML'Img & " XML files");

      if Check_Alternative_Dir then
         Put_Line ("Comparing with latest CVS baselines from W3C");
      end if;

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

         if Errors (K) /= 0 then
            case K is
               when Not_Implemented | Internal_Error =>
                  Put (Errors (K)'Img);
               when others =>
                  Put (" failed=" & Errors (K)'Img & " /" & Total (K)'Img);
            end case;
         end if;

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

   --  Since we are going to create multiple parsers, we will share the symbol
   --  table, which saves on the number of calls to malloc().
   --  This is however optional, since a parser would create its own symbol
   --  table when appropriate

   declare
      S : constant Symbol_Table_Access := new Symbol_Table_Record;
   begin
      Symbols := Symbol_Table_Pointers.Allocate (S);

      Test_Set_Ref := Find (S, "testSetRef");
      Test_Group   := Find (S, "testGroup");
      S_Annotation := Find (S, "annotation");
      S_Schema_Test := Find (S, "schemaTest");
      S_Instance_Test := Find (S, "instanceTest");
      S_Documentation := Find (S, "documentation");
      S_Description   := Find (S, "Description");
      S_Instance_Document := Find (S, "instanceDocument");
      S_Schema_Document := Find (S, "schemaDocument");
      S_Current := Find (S, "current");
      S_Expected := Find (S, "expected");
      S_Validity := Find (S, "validity");
      S_Status   := Find (S, "status");
      S_Name     := Find (S, "name");
      S_Href     := Find (S, "href");
      S_Schema_Version := Find (S, "schemaVersion");
      S_Release_Date   := Find (S, "releaseDate");
      S_Xlink := Find (S, "http://www.w3.org/1999/xlink");
   end;

   loop
      case Getopt ("v d a h f -filter: -descr -group -hide: -xsd10"
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
            Put_Line ("--xsd10 Support for version XSD 1.0");
            return;

         when 'v' => Verbose := True;
         when 'd' => Debug   := True;
         when 'f' => Show_Files := True;

         when '-' =>
            if Full_Switch = "-cvs" then
               Check_Alternative_Dir := True;

            elsif Full_Switch = "-xsd10" then
               XSD_Version := XSD_1_0;

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
      Schema.Set_Debug_Output (True);
   end if;

   Put_Line (Base_Name (Command_Name, ".exe"));

   if Accepted_Only then
      Put_Line ("Tests marked by W3C as non-accepted were not run");
   end if;

   Run_Testsuite ("suite.xml");

   Print_Results;
end Schematest;
