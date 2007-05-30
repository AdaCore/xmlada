-----------------------------------------------------------------------
--                XML/Ada - An XML suite for Ada95                   --
--                                                                   --
--                       Copyright (C) 2004-2007, AdaCore            --
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

with Schema.Readers;        use Schema.Readers;
--  with Schema.Schema_Grammar; use Schema.Schema_Grammar;
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

exception
   when E : XML_Validation_Error | XML_Fatal_Error =>
      Put_Line (Exception_Message (E));
      Close (Read);
end TestDomSchema;
