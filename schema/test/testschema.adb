-----------------------------------------------------------------------
--                XML/Ada - An XML suite for Ada95                   --
--                                                                   --
--                       Copyright (C) 2004-2008, AdaCore            --
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

--  This is a small example showing how to parse one or more XML schemas
--  and then validate one XML document. To use this tool, do a
--  "make test" at the root of the XML/Ada distribution, then run
--      ./testschema -xsd schema1.xsd -xsd schema2.xsd file1.xml file2.xml
--  where schema1.xsd, schema2.xsd, schema3.xsd,... are our schema files
--  to parse, and file.xml the XML document to validate

with Ada.Text_IO;
with Ada.Text_IO.Text_Streams; use Ada.Text_IO.Text_Streams;
with Schema.Readers;        use Schema.Readers;
with Schema.Dom_Readers;    use Schema.Dom_Readers;
with Schema.Schema_Readers; use Schema.Schema_Readers;
with Schema.Validators;     use Schema.Validators;
with DOM.Core.Nodes;        use DOM.Core.Nodes;
with Input_Sources.File;    use Input_Sources.File;
with Ada.Exceptions;        use Ada.Exceptions;
with GNAT.IO;               use GNAT.IO;
with Sax.Readers;           use Sax.Readers;
with GNAT.Command_Line;     use GNAT.Command_Line;

procedure TestSchema is
   Debug     : Boolean := False;
   Read      : File_Input;
   My_Reader : Validating_Reader_Access;
   Schema    : Schema_Reader;
   Grammar   : XML_Grammar := No_Grammar;
   Explicit_XSD : Boolean := False;
   Switches  : constant String := "xsd: debug base dom";
   DOM       : boolean := False;

begin
   --  Special case: check if we want debug output, before doing anything else
   loop
      case Getopt (Switches) is
         when 'd' =>
            if Full_Switch = "debug" then
               Debug := True;
               Standard.Schema.Readers.Set_Debug_Output (True);
               Standard.Schema.Validators.Set_Debug_Output (True);
               Standard.Schema.Schema_Readers.Set_Debug_Output (True);
            elsif Full_Switch = "dom" then
               DOM := True;
            end if;
         when ASCII.NUL =>
            exit;
         when others =>
            null;  --  Handled later
      end case;
   end loop;

   --  Create the parser

   if DOM then
      My_Reader := new Standard.Schema.Dom_Readers.Tree_Reader;
   else
      My_Reader := new Standard.Schema.Readers.Validating_Reader;
   end if;

   --  We want to validate with possibly several schemas to parse first. This
   --  is slightly more complex than a single grammar, since some checks can
   --  only be done at the end, and we need to let XML/Ada know about that.

   Set_Created_Grammar (Schema, No_Grammar);
   Initialize_Option_Scan;

   loop
      case Getopt (Switches) is
         when 'x' =>
            if Debug then
               New_Line;
               Put_Line ("Parsing schema: " & Parameter);
            end if;

            begin
               Open (Parameter, Read);
               Parse (Schema, Read);
               Close (Read);
               Explicit_XSD := True;
            exception
               when XML_Validation_Error =>
                  Close (Read);
                  raise;
            end;

         when 'b' =>
            Use_Basename_In_Error_Messages (Schema, True);
            Use_Basename_In_Error_Messages (My_Reader.all, True);

         when 'd' =>
            null; --  Already handled

         when others =>
            exit;
      end case;
   end loop;

   --  If we have at least one schema, we need to perform the final checks
   --  to make sure they are correct and leave no undefined entity.

   if Explicit_XSD then
      Grammar  := Get_Created_Grammar (Schema);
      Global_Check (Grammar);

      --  Validate the documents with the schemas we have just parsed.
      Set_Validating_Grammar (My_Reader.all, Grammar);
   end if;

   --  Activate validation. Even though we have a validating reader, we can
   --  still choose to disable validation if we know the document is correct.
   --  This makes loading the document faster

   Set_Feature (My_Reader.all, Schema_Validation_Feature, True);

   --  Now valid all XML files given as input

   loop
      declare
         Xml_File : constant String := Get_Argument;
      begin
         exit when Xml_File'Length = 0;

         if Debug then
            New_Line;
            Put_Line ("Parsing XML file: " & Xml_File);
         end if;

         Open (Xml_File, Read);
         Parse (My_Reader.all, Read);
         Close (Read);

         if DOM then
            Write
              (Stream => Stream (Ada.Text_IO.Current_Output),
               N      => Get_Tree (Tree_Reader (My_Reader.all)),
               Print_XML_Declaration => False,
               EOL_Sequence => "");
         end if;
      end;
   end loop;

exception
   when E : XML_Validation_Error | XML_Fatal_Error =>
      Put_Line (Exception_Message (E));
      Close (Read);
end TestSchema;
