-----------------------------------------------------------------------
--                XML/Ada - An XML suite for Ada95                   --
--                                                                   --
--                       Copyright (C) 2001-2002                     --
--                            ACT-Europe                             --
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

package body Unicode.CES is

   --------------
   -- Read_Bom --
   --------------

   procedure Read_Bom
     (Str : String;
      Len : out Natural;
      BOM : out Bom_Type;
      XML_Support : Boolean := True) is
   begin
      if Str'Length >= 2
        and then Str (Str'First) = Character'Val (16#FE#)
        and then Str (Str'First + 1) = Character'Val (16#FF#)
      then
         Len := 2;
         BOM := Utf16_BE;

      elsif Str'Length >= 2
        and then Str (Str'First) = Character'Val (16#FF#)
        and then Str (Str'First + 1) = Character'Val (16#FE#)
      then
         Len := 2;
         BOM := Utf16_LE;

      elsif Str'Length >= 4
        and then Str (Str'First)     = Character'Val (16#00#)
        and then Str (Str'First + 1) = Character'Val (16#00#)
        and then Str (Str'First + 2) = Character'Val (16#FE#)
        and then Str (Str'First + 3) = Character'Val (16#FF#)
      then
         Len := 4;
         BOM := Utf32_BE;

      elsif Str'Length >= 4
        and then Str (Str'First)     = Character'Val (16#FF#)
        and then Str (Str'First + 1) = Character'Val (16#FE#)
        and then Str (Str'First + 2) = Character'Val (16#00#)
        and then Str (Str'First + 3) = Character'Val (16#00#)
      then
         Len := 4;
         BOM := Utf32_BE;

      elsif Str'Length >= 3
        and then Str (Str'First)     = Character'Val (16#EF#)
        and then Str (Str'First + 1) = Character'Val (16#BB#)
        and then Str (Str'First + 2) = Character'Val (16#BF#)
      then
         Len := 3;
         BOM := Utf8_All;

      elsif XML_Support
        and then Str'Length >= 4
        and then Str (Str'First)     = Character'Val (16#00#)
        and then Str (Str'First + 1) = Character'Val (16#00#)
        and then Str (Str'First + 2) = Character'Val (16#00#)
        and then Str (Str'First + 3) = Character'Val (16#3C#)
      then
         Len := 0;
         BOM := Ucs4_BE;

      elsif XML_Support
        and then Str'Length >= 4
        and then Str (Str'First)     = Character'Val (16#3C#)
        and then Str (Str'First + 1) = Character'Val (16#00#)
        and then Str (Str'First + 2) = Character'Val (16#00#)
        and then Str (Str'First + 3) = Character'Val (16#00#)
      then
         Len := 0;
         BOM := Ucs4_LE;

      elsif XML_Support
        and then Str'Length >= 4
        and then Str (Str'First)     = Character'Val (16#00#)
        and then Str (Str'First + 1) = Character'Val (16#00#)
        and then Str (Str'First + 2) = Character'Val (16#3C#)
        and then Str (Str'First + 3) = Character'Val (16#00#)
      then
         Len := 0;
         BOM := Ucs4_2143;

      elsif XML_Support
        and then Str'Length >= 4
        and then Str (Str'First)     = Character'Val (16#00#)
        and then Str (Str'First + 1) = Character'Val (16#3C#)
        and then Str (Str'First + 2) = Character'Val (16#00#)
        and then Str (Str'First + 3) = Character'Val (16#00#)
      then
         Len := 0;
         BOM := Ucs4_3412;

      elsif XML_Support
        and then Str'Length >= 4
        and then Str (Str'First)     = Character'Val (16#00#)
        and then Str (Str'First + 1) = Character'Val (16#3C#)
        and then Str (Str'First + 2) = Character'Val (16#00#)
        and then Str (Str'First + 3) = Character'Val (16#3F#)
      then
         Len := 0;
         BOM := Utf16_BE;

      elsif XML_Support
        and then Str'Length >= 4
        and then Str (Str'First)     = Character'Val (16#3C#)
        and then Str (Str'First + 1) = Character'Val (16#00#)
        and then Str (Str'First + 2) = Character'Val (16#3F#)
        and then Str (Str'First + 3) = Character'Val (16#00#)
      then
         Len := 0;
         BOM := Utf16_LE;

      elsif XML_Support
        and then Str'Length >= 4
        and then Str (Str'First)     = Character'Val (16#3C#)
        and then Str (Str'First + 1) = Character'Val (16#3F#)
        and then Str (Str'First + 2) = Character'Val (16#78#)
        and then Str (Str'First + 3) = Character'Val (16#6D#)
      then
         --  Utf8, ASCII, some part of ISO8859, Shift-JIS, EUC,...
         Len := 0;
         BOM := Unknown;

      else
         Len := 0;
         BOM := Unknown;
      end if;
   end Read_Bom;

   -----------------------
   -- Index_From_Offset --
   -----------------------

   function Index_From_Offset
     (Str : Byte_Sequence; Offset : Natural; Encoding : Encoding_Scheme)
      return Integer
   is
      Pos  : Natural := Str'First;
      Offs : Integer := Offset;
      C    : Unicode_Char;
   begin
      while Pos <= Str'Last loop
         if Offs <= 0 then
            return Pos;
         end if;
         Encoding.Read (Str, Pos, C);
         Offs := Offs - 1;
      end loop;
      return -1;
   end Index_From_Offset;

end Unicode.CES;
