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

with Unicode.Names.Basic_Latin;  use Unicode.Names.Basic_Latin;
with Unicode.CCS;                use Unicode.CCS;

package body Unicode.CES.Utf32 is

   ------------
   -- Encode --
   ------------

   procedure Encode
     (Char   : Unicode_Char;
      Output : in out Byte_Sequence;
      Index  : in out Natural) is
   begin
      Output (Index + 1) := Character'Val (Char and 16#000000FF#);
      Output (Index + 2) := Character'Val ((Char and 16#0000FF00#) / (2 ** 8));
      Output (Index + 3) :=
        Character'Val ((Char and 16#00FF0000#) / (2 ** 16));
      Output (Index + 4) :=
        Character'Val ((Char and 16#FF000000#) / (2 ** 24));
      Index := Index + 4;
   end Encode;

   ---------------
   -- Encode_BE --
   ---------------

   procedure Encode_BE
     (Char   : Unicode_Char;
      Output : in out Byte_Sequence;
      Index  : in out Natural) is
   begin
      Output (Index + 1) :=
        Character'Val ((Char and 16#FF000000#) / (2 ** 24));
      Output (Index + 2) :=
        Character'Val ((Char and 16#00FF0000#) / (2 ** 16));
      Output (Index + 3) := Character'Val ((Char and 16#0000FF00#) / (2 ** 8));
      Output (Index + 4) := Character'Val (Char and 16#000000FF#);
      Index := Index + 4;
   end Encode_BE;

   ----------
   -- Read --
   ----------

   procedure Read
     (Str   : Utf32_LE_String;
      Index : in out Positive;
      Char  : out Unicode_Char) is
   begin
      if Index > Str'Last - 3 then
         Char := Nul;
      else
         Char := Character'Pos (Str (Index))
           + Character'Pos (Str (Index + 1)) * (2 ** 8)
           + Character'Pos (Str (Index + 2)) * (2 ** 16)
           + Character'Pos (Str (Index + 3)) * (2 ** 24);
         Index := Index + 4;
      end if;
   end Read;

   -------------
   -- Read_BE --
   -------------

   procedure Read_BE
     (Str   : Utf32_BE_String;
      Index : in out Positive;
      Char  : out Unicode_Char) is
   begin
      if Index > Str'Last - 3 then
         Char := Nul;
      else
         Char := Character'Pos (Str (Index + 3))
           + Character'Pos (Str (Index + 2)) * (2 ** 8)
           + Character'Pos (Str (Index + 1)) * (2 ** 16)
           + Character'Pos (Str (Index)) * (2 ** 24);
         Index := Index + 4;
      end if;
   end Read_BE;

   -----------
   -- Width --
   -----------

   function Width (Char : Unicode_Char) return Natural is
      pragma Warnings (Off, Char);
   begin
      return 4;
   end Width;

   ------------
   -- Length --
   ------------

   function Length (Str : Utf32_String) return Natural is
   begin
      return Str'Length / 4;
   end Length;

   -----------------
   -- To_Utf32_LE --
   -----------------

   function To_Unicode_LE
     (Str   : Utf32_String;
      Cs    : Unicode.CCS.Character_Set := Unicode.CCS.Unicode_Character_Set;
      Order : Byte_Order := Default_Byte_Order) return Utf32_LE_String
   is
      Offset : Natural;
      O : Byte_Order := Order;
      J : Natural := Str'First;
      J_Out : Natural;
      S : Utf32_LE_String (1 .. Str'Length);
      C : Unicode_Char;
      BOM : Bom_Type;

   begin
      Read_Bom (Str, Offset, BOM);

      case BOM is
         when Utf32_LE => O := Low_Byte_First;
         when Utf32_BE => O := High_Byte_First;
         when Unknown  => null;
         when others   => raise Invalid_Encoding;
      end case;

      if O = Low_Byte_First then
         if Cs.To_Unicode = Identity'Access then
            return Str (Str'First + Offset .. Str'Last);
         else
            J := J + Offset;
            J_Out := S'First - 1;
            while J <= Str'Last loop
               Read (Str, J, C);
               Encode (Cs.To_Unicode (C), S, J_Out);
            end loop;
            return S (S'First + Offset .. S'Last);
         end if;
      else
         J := J + Offset;
         if Cs.To_Unicode = Identity'Access then
            while J <= Str'Last loop
               S (J + 3) := Str (J);
               S (J + 2) := Str (J + 1);
               S (J + 1) := Str (J + 2);
               S (J)     := Str (J + 3);
               J := J + 4;
            end loop;
         else
            J_Out := S'First - 1;
            while J <= Str'Last loop
               Read_BE (Str, J, C);
               Encode (Cs.To_Unicode (C), S, J_Out);
            end loop;
         end if;
         return S (S'First + Offset .. S'Last);
      end if;
   end To_Unicode_LE;

   -----------
   -- To_CS --
   -----------

   function To_CS
     (Str   : Utf32_LE_String;
      Cs    : Unicode.CCS.Character_Set := Unicode.CCS.Unicode_Character_Set;
      Order : Byte_Order := Default_Byte_Order) return Utf32_String
   is
      J : Natural := Str'First;
      S : Utf32_LE_String (1 .. Str'Length);
      C : Unicode_Char;

   begin
      if Order = Low_Byte_First then
         if Cs.To_CS = Identity'Access then
            return Str;
         else
            J := J - 1;
            while J <= Str'Last loop
               Read (Str, J, C);
               Encode (Cs.To_CS (C), S, J);
            end loop;
            return S;
         end if;
      else
         if Cs.To_CS = Identity'Access then
            while J <= Str'Last loop
               S (J + 3) := Str (J);
               S (J + 2) := Str (J + 1);
               S (J + 1) := Str (J + 2);
               S (J)     := Str (J + 3);
               J := J + 4;
            end loop;
         else
            J := J - 1;
            while J <= Str'Last loop
               Read_BE (Str, J, C);
               Encode (Cs.To_CS (C), S, J);
            end loop;
         end if;
         return S;
      end if;
   end To_CS;

end Unicode.CES.Utf32;
