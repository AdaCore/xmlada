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
-----------------------------------------------------------------------

with Unicode.CES.Utf32;  use Unicode.CES.Utf32;
with Unicode.CCS;        use Unicode.CCS;

package body Unicode.CES.Utf16 is

   ------------
   -- Encode --
   ------------

   function Encode (Char : Unicode_Char) return Utf16_LE_String is
      C, D : Unicode_Char;
   begin
      if Char < 16#10000# then
         C := Char and 16#00FF#;
         D := (Char and 16#FF00#) / (2 ** 8);
         return Character'Val (C) & Character'Val (D);
      else
         C := 16#D800#
           + ((Char - 16#10000#) and 2#11111111110000000000#) / (2 ** 10);
         D := 16#DC00#
           + ((Char - 16#10000#) and 2#1111111111#);
         return Character'Val (C and 16#00FF#)
           & Character'Val ((C and 16#FF00#) / (2 ** 8))
           & Character'Val (D and 16#00FF#)
           & Character'Val ((D and 16#FF00#) / (2 ** 8));
      end if;
   end Encode;

   ---------------
   -- Encode_BE --
   ---------------

   function Encode_BE (Char : Unicode_Char) return Utf16_BE_String is
      C, D : Unicode_Char;
   begin
      if Char < 16#10000# then
         C := Char and 16#00FF#;
         D := (Char and 16#FF00#) / (2 ** 8);
         return Character'Val (D) & Character'Val (C);
      else
         C := 16#D800#
           + ((Char - 16#10000#) and 2#11111111110000000000#) / (2 ** 10);
         D := 16#DC00#
           + ((Char - 16#10000#) and 2#1111111111#);
         return Character'Val ((C and 16#FF00#) / (2 ** 8))
           & Character'Val (C and 16#00FF#)
           & Character'Val ((D and 16#FF00#) / (2 ** 8))
           & Character'Val (D and 16#00FF#);
      end if;
   end Encode_BE;

   ----------
   -- Read --
   ----------

   function Read (Str : Utf16_LE_String; Index : Positive) return Unicode_Char
   is
      C, D : Unicode_Char;
   begin
      C := Character'Pos (Str (Index + 1)) * 256 + Character'Pos (Str (Index));

      --  High surrogate value
      if C in 16#D800# .. 16#DBFF# then
         D := Character'Pos (Str (Index + 3)) * 256
           + Character'Pos (Str (Index + 2));

         --  Not a low surrogate ?
         if not (D in 16#DC00# .. 16#DFFF#) then
            raise Invalid_Encoding;
         end if;

         C := C and 2#1111111111#;
         D := D and 2#1111111111#;
         C := C * 2#10000000000# + D + 16#10000#;
      end if;

      return C;
   end Read;

   -------------
   -- Read_BE --
   -------------

   function Read_BE
     (Str : Utf16_BE_String; Index : Positive) return Unicode_Char
   is
      C, D : Unicode_Char;
   begin
      C := Character'Pos (Str (Index)) * 256 + Character'Pos (Str (Index + 1));

      --  High surrogate value
      if C in 16#D800# .. 16#DBFF# then
         D := Character'Pos (Str (Index + 2)) * 256
           + Character'Pos (Str (Index + 3));

         --  Not a low surrogate ?
         if not (D in 16#DC00# .. 16#DFFF#) then
            raise Invalid_Encoding;
         end if;

         C := C and 2#1111111111#;
         D := D and 2#1111111111#;
         C := C * 2#10000000000# + D + 16#10000#;
      end if;

      return C;
   end Read_BE;

   -----------
   -- Width --
   -----------

   function Width (Char : Unicode_Char) return Natural is
   begin
      if Char >= 16#10000# then
         return 4;
      else
         return 2;
      end if;
   end Width;

   ------------
   -- Length --
   ------------

   function Length (Str : Utf16_String) return Natural is
      Pos : Natural := Str'First;
      Len : Natural := 0;
   begin
      while Pos <= Str'Last loop
         Pos := Pos + Width (Read (Str, Pos));
         Len := Len + 1;
      end loop;
      return Len;
   end Length;

   ----------------
   -- From_Utf32 --
   ----------------

   function From_Utf32
     (Str : Unicode.CES.Utf32.Utf32_LE_String)
      return Utf16_LE_String
   is
      Result : Utf16_LE_String (1 .. (Str'Length / Utf32_Char_Width) * 4);
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

   function To_Utf32
     (Str : Utf16_LE_String)
      return Unicode.CES.Utf32.Utf32_LE_String
   is
      Result : Utf32_LE_String (1 .. (Str'Length / 2) * Utf32_Char_Width);
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

   -------------------
   -- To_Unicode_LE --
   -------------------
   --  ??? Note: this assumes that the original character and its
   --  conversion are encoded on the same length, which is always
   --  right so far with Unicode.

   function To_Unicode_LE
     (Str   : Utf16_String;
      Cs    : Unicode.CCS.Character_Set := Unicode.CCS.Unicode_Character_Set;
      Order : Byte_Order := Default_Byte_Order) return Utf16_LE_String
   is
      BOM    : Bom_Type;
      Offset : Natural := 0;
      O      : Byte_Order := Order;
      J      : Positive := Str'First;
      S      : Utf16_LE_String (1 .. Str'Length);
      C      : Unicode_Char;

   begin
      Read_Bom (Str, Offset, BOM);

      case BOM is
         when Utf16_LE => O := Low_Byte_First;
         when Utf16_BE => O := High_Byte_First;
         when Unknown  => null;
         when others   => raise Invalid_Encoding;
      end case;

      if O = Low_Byte_First then
         if Cs.To_Unicode = Identity'Access then
            return Str (Str'First + Offset .. Str'Last);
         else
            J := J + Offset;
            while J <= Str'Last loop
               C := Cs.To_Unicode (Read (Str, J));
               S (J .. J + Width (C) - 1) := Encode (C);
               J := J + Width (C);
            end loop;
            return S (S'First + Offset .. S'Last);
         end if;
      else
         J := J + Offset;
         if Cs.To_Unicode = Identity'Access then
            while J <= Str'Last loop
               S (J + 1) := Str (J);
               S (J)     := Str (J + 1);
               J := J + 2;
            end loop;
         else
            while J <= Str'Last loop
               C := Cs.To_Unicode (Read_BE (Str, J));
               S (J .. J + Width (C) - 1) := Encode (C);
               J := J + Width (C);
            end loop;
            return S (S'First + Offset .. S'Last);
         end if;
         return S (S'First + Offset .. S'Last);
      end if;
   end To_Unicode_LE;

   -----------
   -- To_CS --
   -----------

   function To_CS
     (Str   : Utf16_LE_String;
      Cs    : Unicode.CCS.Character_Set := Unicode.CCS.Unicode_Character_Set;
      Order : Byte_Order := Default_Byte_Order) return Utf16_String
   is
      Offset : Natural := 0;
      J      : Positive := Str'First;
      S      : Utf16_LE_String (1 .. Str'Length);
      C      : Unicode_Char;
   begin
      if Order = Low_Byte_First then
         if Cs.To_CS = Identity'Access then
            return Str (Str'First + Offset .. Str'Last);
         else
            J := J + Offset;
            while J <= Str'Last loop
               C := Cs.To_CS (Read (Str, J));
               S (J .. J + Width (C) - 1) := Encode (C);
               J := J + Width (C);
            end loop;
            return S (S'First + Offset .. S'Last);
         end if;
      else
         J := J + Offset;
         if Cs.To_CS = Identity'Access then
            while J <= Str'Last loop
               S (J + 1) := Str (J);
               S (J)     := Str (J + 1);
               J := J + 2;
            end loop;
         else
            while J <= Str'Last loop
               C := Cs.To_CS (Read (Str, J));
               S (J .. J + Width (C) - 1) := Encode_BE (C);
               J := J + Width (C);
            end loop;
            return S (S'First + Offset .. S'Last);
         end if;
         return S (S'First + Offset .. S'Last);
      end if;
   end To_CS;

end Unicode.CES.Utf16;
