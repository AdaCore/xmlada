with Ada.Command_Line;   use Ada.Command_Line;
with Ada.Exceptions;     use Ada.Exceptions;
with Ada.Text_IO;        use Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with DOM.Core.Documents; use DOM.Core, DOM.Core.Documents;
with DOM.Core.Nodes;     use DOM.Core.Nodes;
with DOM.Readers;        use DOM.Readers;
with GNAT.Command_Line;  use GNAT.Command_Line;
with GNAT.OS_Lib;        use GNAT.OS_Lib;
with Input_Sources.File; use Input_Sources.File;
with Input_Sources.Http; use Input_Sources.Http;
with Input_Sources;      use Input_Sources;
with Sax.Encodings;      use Sax.Encodings;
with Sax.Readers;        use Sax.Readers;
with Testxml_Support;    use Testxml_Support;
with Unicode.CES;        use Unicode.CES;
with Unicode.Encodings;  use Unicode.Encodings;

--  Try also
--     ./testxml http://java.sun.com/j2ee/1.4/docs/tutorial/examples/jaxp
--     /dom/samples/slideSample01.xml

procedure Testxml is
   Show_Not_Found_Tests    : constant Boolean := False;
   --  If True, an error message is printed for tests that are not found.
   --  Otherwise, the test is simply ignored.

   Show_Invalid_Encoding   : constant Boolean := False;
   --  If True, an unsupported encoding reported by the XML parser is
   --  considered as a fatal error for the testsuite. If False, the test is
   --  simply ignored

   Run_XML_1_1_Tests       : constant Boolean := False;
   --  Whether we should run XML 1.1 tests. If False, only XML 1.0 tests are
   --  run

   Run_Unsupported_Tests   : constant Boolean := False;
   --  If True, then the tests that are listed as unsupported in
   --  xmlconf_expected.xml will be run anyway, although they are expected to
   --  fail.

   Run_Ambiguous_Tests     : constant Boolean := False;
   --  If True, ambiguous tests are run. These tests conflict with some other
   --  tests, and are expected to fail, pending clarification from W3C

   Compare_Error_Messages  : constant Boolean := True;
   --  Compare the error message read from XML/Ada with a baseline stored in
   --  xmlconf_expected.xml, to detect changes in the output. This is not
   --  strictly part of the XML Conformance Testsuite

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Input_Source'Class, Input_Source_Access);

   Silent : Boolean := False;
   With_URI : Boolean := False;
   Dump : Boolean := False;
   Name_Start : Natural;
   Validate : Boolean := False;
   Valid_Chars : Boolean := False;
   Must_Normalize : Boolean := False;
   Support_Namespaces : Boolean := True;
   Encoding_Out : Unicode.Encodings.Unicode_Encoding := Get_By_Name ("utf-8");
   EOL : Byte_Sequence_Access := new Byte_Sequence'(Sax.Encodings.Lf_Sequence);
   Print_Comments : Boolean := False;
   Print_XML_PI   : Boolean := False;
   Collapse_Empty_Nodes : Boolean := False;
   Auto_Run : Boolean := False;
   Verbose : Boolean := False;

   type Testcases_Result is record
      Success_Count   : Natural := 0;
      Failure_Count   : Natural := 0;
      Ignore_Count    : Natural := 0;
      Not_Found_Count : Natural := 0;
   end record;
   No_Result        : constant Testcases_Result := (0, 0, 0, 0);
   Single_Success   : constant Testcases_Result := (1, 0, 0, 0);
   Single_Failure   : constant Testcases_Result := (0, 1, 0, 0);
   Single_Ignore    : constant Testcases_Result := (0, 0, 1, 0);
   Single_Not_Found : constant Testcases_Result := (0, 0, 0, 1);

   function "+" (R1, R2 : Testcases_Result) return Testcases_Result;
   --  Add two results

   function Open_Input (XML_File : String) return Input_Source_Access;
   --  Open a given input_source. According to the file prefix, various types
   --  of sources can be open

   procedure Run_Single_Test (XML_File : String);
   --  Parse XML_File, and print the output according to the command-line
   --  parameters

   procedure Run_Testsuite;
   --  Parse the W3C's testsuite description file, and run every test in it.
   --  Return True if all tests succeeded

   function Run_Testcases
     (N : Node; Base : String; Expected : Document) return Testcases_Result;
   --  Parse a <TESTCASES> node from tests/xmlconf.xml to drive the automatic
   --  testsuite.
   --  Expected is the tree describing the expected result for all trees
   --  Return True if all tests succeeded

   function Run_Test
     (Base, Test_Type, Entities, ID : String;
      URI, Sections, Description, Output : String;
      Namespaces : Boolean;
      Unsupported : Boolean;
      Reference  : Node)
      return Testcases_Result;
   --  Run a single test from the W3C testsuite

   function Get_Attribute (N : Node; Attribute : String) return String;
   --  Query an attribute from N. The empty string is returned if the attribute
   --  does not exists

   procedure Diff_Output
     (Reader   : in out My_Tree_Reader'Class;
      Expected : String;
      Result   : in out Testcases_Result);
   --  Compare the output of a test with the expected output

   procedure Run_Error_Test
     (Reader    : in out My_Tree_Reader'Class;
      Input     : in out Input_Source'Class;
      Result    : out Testcases_Result;
      Msg       : out String_Access);
   procedure Run_Not_WF_Test
     (Reader    : in out My_Tree_Reader'Class;
      Input     : in out Input_Source'Class;
      Result    : out Testcases_Result;
      Msg       : out String_Access);
   procedure Run_Valid_Test
     (Reader    : in out My_Tree_Reader'Class;
      Input     : in out Input_Source'Class;
      Result    : out Testcases_Result;
      Msg       : out String_Access);
   procedure Run_Invalid_Test
     (Reader    : in out My_Tree_Reader'Class;
      Input     : in out Input_Source'Class;
      Result    : out Testcases_Result;
      Msg       : out String_Access);
   --  Run a single test, for each of the possible test category

   function Trim (Str : String) return String;
   --  Remove all leading white space characters in Str

   function Test_URI (Base, ID, URI : String) return String;
   --  Compute the URI for the test

   ----------
   -- Trim --
   ----------

   function Trim (Str : String) return String is
      S : Integer := Str'First;
   begin
      while S <= Str'Last
        and then (Str (S) = ' ' or else Str (S) = ASCII.LF)
      loop
         S := S + 1;
      end loop;

      if S <= Str'Last then
         return Str (S .. Str'Last);
      else
         return "";
      end if;
   end Trim;

   ---------
   -- "+" --
   ---------

   function "+" (R1, R2 : Testcases_Result) return Testcases_Result is
   begin
      return (Success_Count   => R1.Success_Count   + R2.Success_Count,
              Failure_Count   => R1.Failure_Count   + R2.Failure_Count,
              Ignore_Count    => R1.Ignore_Count    + R2.Ignore_Count,
              Not_Found_Count => R1.Not_Found_Count + R2.Not_Found_Count);
   end "+";

   ----------------
   -- Open_Input --
   ----------------

   function Open_Input (XML_File : String) return Input_Source_Access is
      Read : Input_Source_Access;
   begin
      if XML_File'Length > 0 then
         if XML_File'Length > 6
           and then XML_File (XML_File'First .. XML_File'First + 6) = "http://"
         then
            Read := new Http_Input;
            Open (XML_File, Http_Input (Read.all));
         else
            Read := new File_Input;
            Open (XML_File, File_Input (Read.all));
         end if;

         --  Base file name should be used as the public Id
         Name_Start := XML_File'Last;
         while Name_Start >= XML_File'First
           and then XML_File (Name_Start) /= '/'
         loop
            Name_Start := Name_Start - 1;
         end loop;

         Set_Public_Id (Read.all, XML_File (Name_Start + 1 .. XML_File'Last));

         --  Full name is used as the system id
         Set_System_Id (Read.all, XML_File);
      else
         Read := new File_Input;
         Open ("test.xml", File_Input (Read.all));
      end if;
      return Read;

   exception
      when Name_Error =>
         Put_Line (Standard_Error, "Cannot open " & XML_File);
         return null;
   end Open_Input;

   ---------------------
   -- Run_Single_Test --
   ---------------------

   procedure Run_Single_Test (XML_File : String) is
      Read : Input_Source_Access;
      Reader : My_Tree_Reader;
   begin
      Read := Open_Input (XML_File);
      if Read = null then
         return;
      end if;

      Set_Feature (Reader, Namespace_Feature, Support_Namespaces);
      Set_Feature (Reader, Namespace_Prefixes_Feature, Support_Namespaces);
      Set_Feature (Reader, Validation_Feature, Validate);
      Set_Feature (Reader, Test_Valid_Chars_Feature, Valid_Chars);

      Parse (Reader, Read.all);

      if Reader.Had_Error then
         Put_Line (Reader.Error_Msg.all);
      end if;

      Close (Read.all);
      Free (Reader.Error_Msg);
      Unchecked_Free (Read);

      if Must_Normalize then
         Normalize (Get_Tree (Reader));
      end if;

      if not Silent then
         if Dump then
            DOM.Core.Nodes.Dump (Get_Tree (Reader), With_URI => With_URI);
         else
            Print (Get_Tree (Reader),
                   Print_Comments => Print_Comments,
                   Print_XML_PI   => Print_XML_PI,
                   With_URI       => With_URI,
                   EOL_Sequence   => EOL.all,
                   Encoding       => Encoding_Out,
                   Collapse_Empty_Nodes => Collapse_Empty_Nodes);
         end if;
      end if;

      Free (Reader);

   exception
      when E : XML_Fatal_Error =>
         if Reader.Had_Error then
            Put_Line (Reader.Error_Msg.all);
         end if;
         Put_Line (Exception_Message (E));
   end Run_Single_Test;

   -------------------
   -- Run_Testsuite --
   -------------------

   procedure Run_Testsuite is
      Input : File_Input;
      Tests : Tree_Reader;
      N     : Node;
      Success : Testcases_Result := No_Result;

      Expected : Tree_Reader;

   begin
      Open ("xmlconf_expected.xml", Input);
      Set_Feature (Expected, Validation_Feature, True);
      Parse (Expected, Input);
      Close (Input);

      Open ("tests/xmlconf.xml", Input);
      Parse (Tests, Input);
      Close (Input);

      N := First_Child (Get_Element (Get_Tree (Tests)));
      while N /= null loop
         if Node_Name (N) = "TESTCASES" then
            Success := Success + Run_Testcases
              (N, Get_Attribute (N, "xml:base"), Get_Tree (Expected));

         elsif Node_Type (N) = Element_Node then
            Put_Line ("Unknown node in xmlconf.xml: " & Node_Name (N));
            Success := Success + Single_Failure;
         end if;

         N := Next_Sibling (N);
      end loop;

      Free (Tests);

      Put_Line ("Success:  " & Integer'Image (Success.Success_Count));
      Put_Line ("Failure:  " & Integer'Image (Success.Failure_Count));
      Put_Line ("Ignored:  " & Integer'Image (Success.Ignore_Count));
      Put_Line ("Not found:" & Integer'Image (Success.Not_Found_Count));

      if Success.Failure_Count = 0 then
         Set_Exit_Status (Ada.Command_Line.Success);
      else
         Set_Exit_Status (Failure);
      end if;
   end Run_Testsuite;

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

   -------------------
   -- Run_Testcases --
   -------------------

   function Run_Testcases
     (N : Node; Base : String; Expected : Document) return Testcases_Result
   is
      Test : Node := First_Child (N);
      Success : Testcases_Result := No_Result;
   begin
      if Verbose then
         Put_Line ("--- Profile: " & Get_Attribute (N, "PROFILE"));
      end if;
      while Test /= null loop
         if Node_Name (Test) = "TEST" then
            if Run_XML_1_1_Tests
              or else Get_Attribute (Test, "VERSION") /= "1.1"
            then
               declare
                  ID     : constant String := Get_Attribute (Test, "ID");
                  Expect : constant Node :=
                    Get_Element_By_Id (Expected, ID);
               begin
                  if Run_Ambiguous_Tests
                    or else Expect = null
                    or else Get_Attribute (Expect, "ambiguous") /= "yes"
                  then
                     Success := Success + Run_Test
                       (Base        => Base,
                        Test_Type   => Get_Attribute (Test, "TYPE"),
                        Entities    => Get_Attribute (Test, "ENTITIES"),
                        ID          => ID,
                        URI         => Get_Attribute (Test, "URI"),
                        Sections    => Get_Attribute (Test, "SECTIONS"),
                        Output      => Get_Attribute (Test, "OUTPUT"),
                        Reference   => Expect,
                        Unsupported => Expect /= null
                        and then Get_Attribute (Expect, "unsupported") = "yes",
                        Namespaces  =>
                          Get_Attribute (Test, "NAMESPACE") /= "no",
                        Description => Node_Value (First_Child (Test)));
                  else
                     Success := Success + Single_Ignore;
                  end if;
               end;
            else
               Success := Success + Single_Ignore;
            end if;

         elsif Node_Name (Test) = "TESTCASES" then
            Success := Success + Run_Testcases (Test, Base, Expected);

         elsif Node_Type (Test) = Element_Node then
            Put_Line
              (Standard_Error, "Unknown child of TEST: " & Node_Name (Test));
            Success := Success + Single_Failure;
         end if;

         Test := Next_Sibling (Test);
      end loop;
      return Success;
   end Run_Testcases;

   --------------
   -- Test_URI --
   --------------

   function Test_URI (Base, ID, URI : String) return String is
   begin
      if Base'Length = 0 then
         if ID'Length > 8
           and then ID (ID'First .. ID'First + 7) = "rmt-ns11"
         then
            return Normalize_Pathname
              (Name          => URI,
               Directory     => "tests/eduni/namespaces/1.1",
               Resolve_Links => False);
         elsif ID'Length > 8
           and then ID (ID'First .. ID'First + 7) = "rmt-ns10"
         then
            return Normalize_Pathname
              (Name          => URI,
               Directory     => "tests/eduni/namespaces/1.0",
               Resolve_Links => False);
         elsif ID'Length > 6
           and then ID (ID'First .. ID'First + 6) = "rmt-e2e"
         then
            return Normalize_Pathname
              (Name          => URI,
               Directory     => "tests/eduni/errata-2e",
               Resolve_Links => False);
         elsif ID'Length > 3
           and then ID (ID'First .. ID'First + 3) = "rmt-"
         then
            return Normalize_Pathname
              (Name          => URI,
               Directory     => "tests/eduni/xml-1.1",
               Resolve_Links => False);
         else
            return "tests/" & URI;
         end if;

      elsif Base (Base'Last) = '/' then
         return Normalize_Pathname
           (Name          => URI,
            Directory     => "tests/" & Base,
            Resolve_Links => False);

      else
         return Normalize_Pathname
           (Name          => URI,
            Directory     => "tests/" & Base & '/',
            Resolve_Links => False);
      end if;
   end Test_URI;

   --------------
   -- Run_Test --
   --------------

   function Run_Test
     (Base, Test_Type, Entities, ID : String;
      URI, Sections, Description, Output : String;
      Namespaces : Boolean;
      Unsupported : Boolean;
      Reference  : Node)
      return Testcases_Result
   is
      Path     : constant String := Test_URI (Base, ID, URI);
      Input    : Input_Source_Access := Open_Input (Path);
      Reader   : My_Tree_Reader;
      Result   : Testcases_Result := No_Result;
      Result2  : Testcases_Result := No_Result;
      Msg, Tmp : String_Access;
   begin
      if Verbose then
         Put_Line ("Running " & Base & " " & URI);
      end if;

      if not Namespaces then
         Set_Feature (Reader, Namespace_Feature, False);
         Set_Feature (Reader, Namespace_Prefixes_Feature, False);
      end if;

      Set_Feature (Reader, Test_Valid_Chars_Feature, True);

      if Test_Type = "valid" then
         if not Unsupported or else Run_Unsupported_Tests then
            Set_Feature (Reader, Validation_Feature, True);
            Run_Valid_Test (Reader, Input.all, Result, Msg);
         else
            Result := Single_Ignore;
         end if;

      elsif Test_Type = "wf" then
         if not Unsupported or else Run_Unsupported_Tests then
            Set_Feature (Reader, Validation_Feature, False);
            Run_Valid_Test (Reader, Input.all, Result, Msg);
         else
            Result := Single_Ignore;
         end if;

      elsif Test_Type = "not-wf" then
         if not Unsupported or else Run_Unsupported_Tests then
            Set_Feature (Reader, Validation_Feature, False);
            Run_Not_WF_Test (Reader, Input.all, Result, Msg);
         else
            Result := Single_Ignore;
         end if;

      elsif Test_Type = "invalid" then
         --  Run the test twice (once with validation, once without).
         --  Even if the test is unsupported, we still check that XML/Ada find
         --  the document as well-formed
         Result2 := Run_Test
           (Base, "wf", Entities, ID, URI, Sections, Description, Output,
            Namespaces, Unsupported => False,
            Reference => Reference);

         if not Unsupported or else Run_Unsupported_Tests then
            Set_Feature (Reader, Validation_Feature, True);
            Run_Invalid_Test (Reader, Input.all, Result, Msg);
         else
            Result := Single_Ignore;
         end if;

      elsif Test_Type = "error" then
         if not Unsupported or else Run_Unsupported_Tests then
            Set_Feature (Reader, Validation_Feature, False);
            Run_Error_Test (Reader, Input.all, Result, Msg);
         else
            Result := Single_Ignore;
         end if;

      else
         Put_Line ("Unknown test_type: " & Test_Type);
         Result := Single_Failure;
      end if;

      if Output /= "" then
         Diff_Output (Reader, Test_URI (Base, ID, Output), Result);
      end if;

      if Compare_Error_Messages
        and then Msg /= null
      then
         if Reference = null
           or else First_Child (Reference) = null
         then
            Tmp := new String'("No expected error message defined"
                               & ASCII.LF & Msg.all);
            Free (Msg);
            Msg := Tmp;
            Result := Single_Failure;

         elsif Msg.all /= Node_Value (First_Child (Reference)) then
            Result := Single_Failure;
         end if;
      end if;

      if Result.Failure_Count > 0 then
         New_Line;
         Put_Line ('[' & ID & "] " & Test_Type & " FAILURE " & Path);
         if Msg /= null then
            Put_Line ("  Got:      " & Msg.all);
         end if;

         if Reference /= null
           and then First_Child (Reference) /= null
         then
            Put_Line ("  Expected: " & Node_Value (First_Child (Reference)));
         end if;

         Put_Line ("  Description: [" & Sections & "] " & Trim (Description));
      end if;

      Free (Msg);
      Close (Input.all);
      Unchecked_Free (Input);
      Free (Reader.Error_Msg);
      Free (Reader);
      return Result + Result2;

   exception
      when E : Invalid_Encoding =>
         if Show_Invalid_Encoding then
            New_Line;
            Put_Line ('[' & ID & "] Invalid encoding " & Path);
            Put_Line ("   [" & Sections & "] " & Trim (Description));
            Put_Line (Exception_Message (E));
            return Single_Failure;
         else
            return Single_Ignore;
         end if;

      when Name_Error =>
         if Show_Not_Found_Tests then
            New_Line;
            Put_Line ('[' & ID & "] File not found: " & Path);
            return Single_Failure;
         else
            return Single_Not_Found;
         end if;

      when E : others =>
         New_Line;
         Put_Line (Standard_Error, '[' & ID & "] Unexpected error for " & URI);
         Put_Line (Standard_Error,
                   "   [" & Sections & "] " & Trim (Description));
         Put_Line (Standard_Error, Exception_Information (E));
         return Single_Failure;
   end Run_Test;

   -----------------
   -- Diff_Output --
   -----------------

   procedure Diff_Output
     (Reader   : in out My_Tree_Reader'Class;
      Expected : String;
      Result   : in out Testcases_Result)
   is
      function System (Str : String) return Integer;
      pragma Import (C, System, "system");

      File  : File_Type;
      File2 : File_Type;
   begin
      if Result.Success_Count > 0 then
         Create (File, Out_File);
         Set_Output (File);
         Print (Get_Tree (Reader),
                Print_Comments       => Print_Comments,
                Print_XML_PI         => Print_XML_PI,
                With_URI             => With_URI,
                EOL_Sequence         => EOL.all,
                Encoding             => Encoding_Out,
                Collapse_Empty_Nodes => Collapse_Empty_Nodes);
         Set_Output (Standard_Output);
         Flush (File);

         Create (File2, Out_File);

         declare
            Name  : constant String := Ada.Text_IO.Name (File);
            Name2 : constant String := Ada.Text_IO.Name (File2);
         begin
            --  Process the expected output by removing the DTD, which
            --  is not stored in the DOM tree, and thus cannot be output
            if System
              ("sed -e '/<!DOCTYPE/,/]>/d' " & Expected & " > " & Name2
               & ASCII.NUL) /= 0
            then
               Result := Single_Failure;

            elsif System
              ("diff -u " & Name2 & " " & Name & ASCII.NUL) /= 0
            then
               Result := Single_Failure;
            end if;

            Delete (File2);
            Delete (File);
         end;
      end if;
   end Diff_Output;

   --------------------
   -- Run_Error_Test --
   --------------------

   procedure Run_Error_Test
     (Reader    : in out My_Tree_Reader'Class;
      Input     : in out Input_Source'Class;
      Result    : out Testcases_Result;
      Msg       : out String_Access) is
   begin
      Parse (Reader, Input);

      if Reader.Had_Error then
         Msg := new String'(Reader.Error_Msg.all);
         Result := Single_Success;
      else
         Result := Single_Failure;
      end if;

   exception
      when E : XML_Fatal_Error =>
         Msg    := new String'
           ("Unexpected Fatal_Error, must have Error" & ASCII.LF
            & Exception_Message (E));
         Result := Single_Failure;
   end Run_Error_Test;

   ---------------------
   -- Run_Not_WF_Test --
   ---------------------

   procedure Run_Not_WF_Test
     (Reader    : in out My_Tree_Reader'Class;
      Input     : in out Input_Source'Class;
      Result    : out Testcases_Result;
      Msg       : out String_Access) is
   begin
      Parse (Reader, Input);
      Result := Single_Failure;
   exception
      when E : XML_Fatal_Error =>
         Msg := new String'(Exception_Message (E));
         Result := Single_Success;
   end Run_Not_WF_Test;

   --------------------
   -- Run_Valid_Test --
   --------------------

   procedure Run_Valid_Test
     (Reader    : in out My_Tree_Reader'Class;
      Input     : in out Input_Source'Class;
      Result    : out Testcases_Result;
      Msg       : out String_Access) is
   begin
      Parse (Reader, Input);
      Result := Single_Success;
   exception
      when E : XML_Fatal_Error =>
         Msg    := new String'(Exception_Message (E));
         Result := Single_Failure;
   end Run_Valid_Test;

   ----------------------
   -- Run_Invalid_Test --
   ----------------------

   procedure Run_Invalid_Test
     (Reader    : in out My_Tree_Reader'Class;
      Input     : in out Input_Source'Class;
      Result    : out Testcases_Result;
      Msg       : out String_Access) is
   begin
      Parse (Reader, Input);

      if Reader.Had_Error then
         Msg := new String'(Reader.Error_Msg.all);
         Result := Single_Success;
      else
         Result := Single_Failure;
      end if;

   exception
      when E : XML_Fatal_Error =>
         Msg := new String'
           ("Unexpected Fatal_Error, must have Error" & ASCII.LF
            & Exception_Message (E));
         Result := Single_Failure;
   end Run_Invalid_Test;

begin
   --  Parse the command line
   loop
      case Getopt
        ("silent uri normalize validate dump valid_chars encoding-out: eol:"
         & " comments xmlpi collapse nonamespaces auto verbose")
      is
         when ASCII.Nul => exit;
         when 'e' =>
            if Full_Switch = "eol" then
               Free (EOL);
               if Parameter = "\n" then
                  EOL := new String'("" & ASCII.LF);
               else
                  EOL := new String'(Parameter);
               end if;
            elsif Full_Switch = "encoding-out" then
               Encoding_Out := Get_By_Name (Parameter);
            end if;
         when 'x' => Print_XML_PI := True;
         when 'c' =>
            if Full_Switch = "comments" then
               Print_Comments := True;
            else
               Collapse_Empty_Nodes := True;
            end if;
         when 's' => Silent := True;
         when 'u' => With_URI := True;
         when 'v' =>
            if Full_Switch = "validate" then
               Validate := True;
            elsif Full_Switch = "valid_chars" then
               Valid_Chars := True;
            elsif Full_Switch = "verbose" then
               Verbose := True;
            end if;
         when 'd' => Dump := True;
         when 'n' =>
            if Full_Switch = "normalize" then
               Must_Normalize := True;
            else
               Support_Namespaces := False;
            end if;
         when 'a' => Auto_Run := True;

         when others => null;
      end case;
   end loop;

   if Auto_Run then
      Run_Testsuite;
   else
      Run_Single_Test (Get_Argument);
   end if;
end Testxml;
