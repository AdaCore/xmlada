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

with Unicode.CES.Utf32; use Unicode.CES.Utf32;
with Unicode.CCS;       use Unicode.CCS;

package body Unicode.CES.Utf8 is

   function Compute_Mask (Char : Unicode_Char) return Unicode_Char;
   pragma Inline (Compute_Mask);
   --  Return the mask to be used for the encoding of the first byte in the
   --  sequence representing Char.

   function Internal_Convert
     (Str     : Utf8_String;
      Convert : Unicode.CCS.Conversion_Function := Identity'Access;
      Order   : Byte_Order := Default_Byte_Order) return Utf8_String;
   --  Internal function used to convert character sets

   ------------
   -- Encode --
   ------------

   function Encode (Char : Unicode_Char) return String is
      Len    : constant Natural := Width (Char);
      Mask   : constant Unicode_Char := Compute_Mask (Char);
      Val    : Unicode_Char := Char;
      Output : String (1 .. Len);
   begin
      for J in reverse 2 .. Len loop
         Output (J) := Character'Val ((Val and 16#3f#) or 16#80#);
         Val := Val / (2 ** 6);
      end loop;
      Output (1) := Character'Val (Val or Mask);
      return Output;
   end Encode;

   ----------
   -- Read --
   ----------

   function Read (Str : Utf8_String; Index : Positive) return Unicode_Char is
      Mask : Unicode_Char;
      Len  : Natural;
      Val  : Unicode_Char;
      C    : Unicode_Char := Character'Pos (Str (Index));
   begin
      --  Compute the length of the encoding given what was in the first byte
      if C < 128 then
         Len := 1;
         Mask := 16#7f#;
      elsif (C and 16#E0#) = 16#C0# then
         Len := 2;
         Mask := 16#1f#;
      elsif (C and 16#F0#) = 16#E0# then
         Len := 3;
         Mask := 16#0f#;
      elsif (C and 16#F8#) = 16#F0# then
         Len := 4;
         Mask := 16#07#;
      elsif (C and 16#FC#) = 16#F8# then
         Len := 5;
         Mask := 16#03#;
      elsif (C and 16#FE#) = 16#FC# then
         Len := 6;
         Mask := 16#01#;
      else
         raise Invalid_Encoding;
      end if;

      Val := C and Mask;
      for Count in 1 .. Len - 1 loop
         C := Character'Pos (Str (Index + Count));
         if (C and 16#C0#) /= 16#80# then
            raise Invalid_Encoding;
         end if;
         Val := Val * (2 ** 6);
         Val := Val or (C and 16#3f#);
      end loop;
      return Val;
   end Read;

   ------------
   -- Length --
   ------------

   function Length (Str : Utf8_String) return Natural is
      Pos : Natural := Str'First;
      Length : Natural := 0;
   begin
      while Pos <= Str'Last loop
         Pos := Pos + Width (Read (Str, Pos));
         Length := Length + 1;
      end loop;
      return Length;
   end Length;

   ------------------
   -- Compute_Mask --
   ------------------

   function Compute_Mask (Char : Unicode_Char) return Unicode_Char is
   begin
      if Char < 16#80# then
         return 16#0#;
      elsif Char < 16#800# then
         return 16#C0#;
      elsif Char < 16#10000# then
         return 16#E0#;
      elsif Char < 16#200000# then
         return 16#F0#;
      elsif Char < 16#4000000# then
         return 16#F8#;
      else
         return 16#FC#;
      end if;
   end Compute_Mask;

   -----------
   -- Width --
   -----------

   function Width (Char : Unicode_Char) return Natural is
   begin
      if Char < 16#80# then
         return 1;
      elsif Char < 16#800# then
         return 2;
      elsif Char < 16#10000# then
         return 3;
      elsif Char < 16#200000# then
         return 4;
      elsif Char < 16#4000000# then
         return 5;
      else
         return 6;
      end if;
   end Width;

   ----------------
   -- From_Utf32 --
   ----------------

   function From_Utf32 (Str : Unicode.CES.Utf32.Utf32_String)
      return Utf8_String
   is
      --  Allocate worst case
      Result : Utf8_String (1 .. (Str'Length / Utf32_Char_Width) * 6);
      J : Positive := Str'First;
      R_Index : Positive := Result'First;
      C : Unicode_Char;
   begin
      while J <= Str'Last loop
         C := Unicode.CES.Utf32.Read (Str, J);
         declare
            Tmp : constant String := Encode (C);
         begin
            Result (R_Index .. R_Index + Tmp'Length - 1) := Tmp;
            R_Index := R_Index + Tmp'Length;
         end;
         J := J + Unicode.CES.Utf32.Width (C);
      end loop;

      return Result (1 .. R_Index - 1);
   end From_Utf32;

   --------------
   -- To_Utf32 --
   --------------

   function To_Utf32 (Str : Utf8_String)
      return Unicode.CES.Utf32.Utf32_LE_String
   is
      --  Allocate worst case
      Result : Utf32_LE_String (1 .. Str'Length * Utf32_Char_Width);
      J : Positive := Str'First;
      R_Index : Positive := 1;
      C : Unicode_Char;
   begin
      while J <= Str'Last loop
         C := Read (Str, J);
         Result (R_Index .. R_Index + Utf32_Char_Width - 1) :=
           Unicode.CES.Utf32.Encode (C);
         R_Index := R_Index + Utf32_Char_Width;
         J := J + Width (C);
      end loop;
      return Result (1 .. R_Index - 1);
   end To_Utf32;

   ----------------------
   -- Internal_Convert --
   ----------------------

   function Internal_Convert
     (Str     : Utf8_String;
      Convert : Unicode.CCS.Conversion_Function := Identity'Access;
      Order   : Byte_Order := Default_Byte_Order) return Utf8_String
   is
      Offset : Natural := 0;
      BOM : Bom_Type;
      C : Unicode_Char;
      J : Natural;
   begin
      Read_Bom (Str, Offset, BOM);

      case BOM is
         when Utf8_All | Unknown => null;
         when others             => raise Invalid_Encoding;
      end case;

      if Convert = Identity'Access then
         return Str (Str'First + Offset .. Str'Last);
      else
         declare
            --  Allocate worst case for the string
            Result : Utf8_String (1 .. Str'Length * 6);
            R_Index : Natural := Result'First;
         begin
            J := Str'First + Offset;
            while J <= Str'Last loop
               C := Read (Str, J);
               declare
                  Tmp : constant String := Encode (Convert (C));
               begin
                  Result (R_Index .. R_Index + Tmp'Length - 1) := Tmp;
                  R_Index := R_Index + Tmp'Length;
               end;
               J := J + Width (C);
            end loop;
            return Result (1 .. R_Index - 1);
         end;
      end if;
   end Internal_Convert;

   -------------------
   -- To_Unicode_LE --
   -------------------

   function To_Unicode_LE
     (Str   : Utf8_String;
      Cs    : Unicode.CCS.Character_Set := Unicode.CCS.Unicode_Character_Set;
      Order : Byte_Order := Default_Byte_Order) return Utf8_String is
   begin
      return Internal_Convert (Str, Cs.To_Unicode, Order);
   end To_Unicode_LE;

   -----------
   -- To_CS --
   -----------

   function To_CS
     (Str   : Utf8_String;
      Cs    : Unicode.CCS.Character_Set := Unicode.CCS.Unicode_Character_Set;
      Order : Byte_Order := Default_Byte_Order) return Utf8_String is
   begin
      return Internal_Convert (Str, Cs.To_CS, Order);
   end To_CS;
end Unicode.CES.Utf8;
