-----------------------------------------------------------------------
--                XML/Ada - An XML suite for Ada95                   --
--                                                                   --
--                       Copyright (C) 2001-2007, AdaCore            --
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

--  This example shows how an XML tree can be converted to a string
--  in memory, without going through a temporary file on the disk

with Ada.Streams;           use Ada.Streams;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;
with DOM.Core.Nodes;        use DOM.Core.Nodes;
with DOM.Readers;           use DOM.Readers;
with Input_Sources.File;    use Input_Sources.File;

procedure ToString is

   type String_Stream_Type is new Root_Stream_Type with record
      Str        : Unbounded_String;
      Read_Index : Natural := 1;
   end record;
   procedure Read
     (Stream : in out String_Stream_Type;
      Item   :    out Stream_Element_Array;
      Last   :    out Stream_Element_Offset);
   procedure Write
      (Stream : in out String_Stream_Type;
       Item   : Stream_Element_Array);
   procedure Open
      (Stream : in out String_Stream_Type'Class;
       Str    : String);
   --  Declare a new stream type to write strings in memory

   -----------
   -- Write --
   -----------

   procedure Write
     (Stream : in out String_Stream_Type;
      Item   : Stream_Element_Array)
   is
      Str : String (1 .. Integer (Item'Length));
      S   : Integer := Str'First;
   begin
      for J in Item'Range loop
         Str (S) := Character'Val (Item (J));
         S := S + 1;
      end loop;
      Append (Stream.Str, Str);
   end Write;

   ----------
   -- Open --
   ----------

   procedure Open
     (Stream : in out String_Stream_Type'Class;
      Str    : in String) is
   begin
      Stream.Str        := To_Unbounded_String (Str);
      Stream.Read_Index := 1;
   end Open;

   ----------
   -- Read --
   ----------

   procedure Read
     (Stream : in out String_Stream_Type;
      Item   :    out Stream_Element_Array;
      Last   :    out Stream_Element_Offset)
   is
      Str : constant String := Slice
        (Stream.Str, Stream.Read_Index, Stream.Read_Index + Item'Length - 1);
      J   : Stream_Element_Offset := Item'First;
   begin
      for S in Str'Range loop
         Item (J) := Stream_Element (Character'Pos (Str (S)));
         J := J + 1;
      end loop;
      Last := Item'First + Str'Length - 1;
      Stream.Read_Index := Stream.Read_Index + Item'Length;
   end Read;

   Input  : File_Input;
   Reader : Tree_Reader;
   Output : aliased String_Stream_Type;
begin
   Open ("test.xml", Input);
   Parse (Reader, Input);
   Close (Input);

   Open (Output, "");
   Write (Output'Access, Get_Tree (Reader));
   Put_Line (To_String (Output.Str));
end ToString;