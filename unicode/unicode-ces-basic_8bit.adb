-----------------------------------------------------------------------
--                XML/Ada - An XML suite for Ada95                   --
--                                                                   --
--                       Copyright (C) 2001                          --
--                            ACT-Europe                             --
--                       Author: Emmanuel Briot                      --
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

--  This package implements a basic 8bit encoding.
--  Only code points from 16#00# to 16#FF# can be encoded in such strings.
--
--  However, then can be used to read files that contain accented characters,
--  in combination with Unicode.CCS.Iso_8859_1 for instance

with Unicode.CES.Utf32;    use Unicode.CES.Utf32;
with Unicode.CCS;          use Unicode.CCS;

package body Unicode.CES.Basic_8bit is

   function Convert
     (Str     : Basic_8bit_String;
      Convert : Unicode.CCS.Conversion_Function := Identity'Access;
      Order   : Byte_Order := Default_Byte_Order) return Basic_8bit_String;
   --  Internal conversion function

   ------------
   -- Encode --
   ------------

   function Encode (Char : Unicode_Char) return Basic_8bit_String is
   begin
      if Char > 16#FF# then
         raise Invalid_Encoding;
      end if;
      return "" & Character'Val (Char);
   end Encode;

   ----------
   -- Read --
   ----------

   function Read (Str : Basic_8bit_String; Index : Positive)
      return Unicode_Char is
   begin
      return Character'Pos (Str (Index));
   end Read;

   -----------
   -- Width --
   -----------

   function Width (Char : Unicode_Char) return Natural is
   begin
      return 1;
   end Width;

   ------------
   -- Length --
   ------------

   function Length (Str : Basic_8bit_String) return Natural is
   begin
      return Str'Length;
   end Length;

   ----------------
   -- From_Utf32 --
   ----------------

   function From_Utf32
     (Str : Unicode.CES.Utf32.Utf32_LE_String)
      return Basic_8bit_String
   is
      Result : Basic_8bit_String (1 .. Str'Length / Utf32_Char_Width);
      R_Index : Positive := Result'First;
      C : Unicode_Char;
      J : Positive := Str'First;
   begin
      while J <= Str'Last loop
         C := Unicode.CES.Utf32.Read (Str, J);
         Result (R_Index .. R_Index) :=  Encode (C);
         J := J + Unicode.CES.Utf32.Width (C);
         R_Index := R_Index + 1;
      end loop;
      return Result;
   end From_Utf32;

   --------------
   -- To_Utf32 --
   --------------

   function To_Utf32
     (Str : Basic_8bit_String)
      return Unicode.CES.Utf32.Utf32_LE_String
   is
      Result : Utf32_LE_String (1 .. Str'Length * Utf32_Char_Width);
      R_Index : Positive := Result'First;
   begin
      for J in Str'Range loop
         Result (R_Index .. R_Index + 3) :=
           Unicode.CES.Utf32.Encode (Read (Str, J));
         R_Index := R_Index + 4;
      end loop;
      return Result;
   end To_Utf32;

   -------------
   -- Convert --
   -------------

   function Convert
     (Str     : Basic_8bit_String;
      Convert : Unicode.CCS.Conversion_Function := Identity'Access;
      Order   : Byte_Order := Default_Byte_Order) return Basic_8bit_String
   is
      S   : String (Str'Range);
   begin
      if Convert = Identity'Access then
         return Str;
      else
         for J in Str'Range loop
            S (J .. J) := Encode (Convert (Read (Str, J)));
         end loop;
         return S;
      end if;
   end Convert;

   -------------------
   -- To_Unicode_LE --
   -------------------

   function To_Unicode_LE
     (Str   : Basic_8bit_String;
      Cs    : Unicode.CCS.Character_Set := Unicode.CCS.Unicode_Character_Set;
      Order : Byte_Order := Default_Byte_Order) return Basic_8bit_String is
   begin
      return Convert (Str, Cs.To_Unicode, Order);
   end To_Unicode_LE;

   -----------
   -- To_CS --
   -----------

   function To_CS
     (Str   : Basic_8bit_String;
      Cs    : Unicode.CCS.Character_Set := Unicode.CCS.Unicode_Character_Set;
      Order : Byte_Order := Default_Byte_Order) return Basic_8bit_String is
   begin
      return Convert (Str, Cs.To_CS, Order);
   end To_CS;

end Unicode.CES.Basic_8bit;
