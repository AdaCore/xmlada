-----------------------------------------------------------------------
--                XML/Ada - An XML suite for Ada95                   --
--                                                                   --
--                       Copyright (C) 2010, AdaCore                 --
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

pragma Ada_05;

with Ada.Text_IO;        use Ada.Text_IO;
with Input_Sources.File; use Input_Sources.File;
with Input_Sources;      use Input_Sources;
with Sax.Exceptions;     use Sax.Exceptions;
with Sax.Readers;        use Sax.Readers;
with Sax.Symbols;        use Sax.Symbols;
with Sax.Utils;          use Sax.Utils;
with Unicode.CES;        use Unicode.CES;

procedure Cleanup_Schema is

   type My_Reader is new Sax.Readers.Reader with record
      In_Annotation : Natural := 0;

      Omit_Schema_Node : Boolean := False;
      --  If True, will not display the schema node

      Open_Tag_Was_Closed  : Boolean := True;
      --  Whether the <open> tag of the current node was closed. This is so
      --  that we can have empty nodes printed as <open/>.
   end record;
   overriding procedure Error
     (Handler : in out My_Reader;
      Except  : Sax.Exceptions.Sax_Parse_Exception'Class) is null;
   overriding procedure Start_Element
     (Handler       : in out My_Reader;
      NS            : Sax.Utils.XML_NS;
      Local_Name    : Sax.Symbols.Symbol;
      Atts          : Sax_Attribute_List);
   overriding procedure End_Element
     (Handler       : in out My_Reader;
      NS            : Sax.Utils.XML_NS;
      Local_Name    : Sax.Symbols.Symbol);
   overriding procedure Characters
     (Handler : in out My_Reader;
      Ch      : Unicode.CES.Byte_Sequence);
   overriding procedure Ignorable_Whitespace
     (Handler : in out My_Reader;
      Ch      : Unicode.CES.Byte_Sequence) is null;

   procedure Put_Prefix (P : Symbol);
   --  Print "P:" if [P] is defined

   ----------------
   -- Put_Prefix --
   ----------------

   procedure Put_Prefix (P : Symbol) is
   begin
      if P /= No_Symbol and then P /= Empty_String then
         Put (Get (P).all & ":");
      end if;
   end Put_Prefix;

   -------------------
   -- Start_Element --
   -------------------

   overriding procedure Start_Element
     (Handler       : in out My_Reader;
      NS            : Sax.Utils.XML_NS;
      Local_Name    : Sax.Symbols.Symbol;
      Atts          : Sax_Attribute_List)
   is
      Att_Length : constant Natural := Get_Length (Atts);
   begin
      if Handler.Omit_Schema_Node and then Local_Name = "schema" then
         return;
      end if;

      if not Handler.Open_Tag_Was_Closed then
         Put (">");
         Handler.Open_Tag_Was_Closed := True;
      end if;

      if Local_Name = "annotation" then
         Handler.In_Annotation := Handler.In_Annotation + 1;
      end if;

      if Local_Name = "include" then
         Handler.In_Annotation := Handler.In_Annotation + 1;

         for A in 1 .. Att_Length loop
            if Get_Name (Atts, A).Local = "schemaLocation" then
               declare
                  R      : My_Reader;
                  Input2 : File_Input;
               begin
                  R.Omit_Schema_Node := True;
                  Open (Get (Get_Value (Atts, A)).all, Input2);
                  Parse (R, Input2);
                  Close (Input2);
               end;
            end if;
         end loop;
      end if;

      if Handler.In_Annotation /= 0 then
         return;
      end if;

      Put ("<");
      Put_Prefix (Get_Prefix (NS));
      Put (Get (Local_Name).all);

      Handler.Open_Tag_Was_Closed := False;

      for A in 1 .. Att_Length loop
         Put (" " & Get_Qname (Atts, A) & "='"
              & Get (Get_Value (Atts, A)).all & "'");
      end loop;
   end Start_Element;

   -----------------
   -- End_Element --
   -----------------

   overriding procedure End_Element
     (Handler       : in out My_Reader;
      NS            : Sax.Utils.XML_NS;
      Local_Name    : Sax.Symbols.Symbol)
   is
   begin
      if Handler.Omit_Schema_Node and then Local_Name = "schema" then
         return;
      end if;

      if Local_Name = "annotation" or else Local_Name = "include" then
         Handler.In_Annotation := Handler.In_Annotation - 1;
         return;
      end if;

      if Handler.In_Annotation /= 0 then
         return;
      end if;

      if not Handler.Open_Tag_Was_Closed then
         Put ("/>");
         Handler.Open_Tag_Was_Closed := True;
      else
         Put ("</");
         Put_Prefix (Get_Prefix (NS));
         Put (Get (Local_Name).all & ">");
      end if;
   end End_Element;

   ----------------
   -- Characters --
   ----------------

   overriding procedure Characters
     (Handler : in out My_Reader;
      Ch      : Unicode.CES.Byte_Sequence) is
   begin
      if Handler.In_Annotation /= 0 then
         return;
      end if;

      if not Handler.Open_Tag_Was_Closed then
         Put (">");
         Handler.Open_Tag_Was_Closed := True;
      end if;

      Put (Ch);
   end Characters;

   Input  : File_Input;
   Reader : My_Reader;
begin
   Set_Feature (Reader, Validation_Feature, False);

   --  Include xmlns: attributes
   Set_Feature (Reader, Namespace_Prefixes_Feature, True);

   Put ("<?xml version='1.0'?>");
   Open ("schema.xsd", Input);
   Parse (Reader, Input);
   Close (Input);
end Cleanup_Schema;
