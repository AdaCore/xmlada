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

with Interfaces;                 use Interfaces;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with Unicode.CES;                use Unicode.CES;
with System.Address_Image;

package body Sax.Symbols is

   -----------------
   -- Debug_Print --
   -----------------

   function Debug_Print (S : Symbol) return String is
   begin
      if S = No_Symbol then
         return "<Symbol: null>";
      else
         return "<Symbol: " & System.Address_Image (S.all'Address)
           & " {" & S.all & "}>";
      end if;
   end Debug_Print;

   ----------
   -- Hash --
   ----------

   function Hash (Str : Cst_Byte_Sequence_Access) return Unsigned_32 is
   begin
      --  For the cases of 1-12 characters, all characters participate in the
      --  hash. The positioning is randomized, with the bias that characters
      --  later on participate fully (i.e. are added towards the right side).

      case Str'Length is

         when 0 =>
            return 0;

         when 1 =>
            return Character'Pos (Str (Str'First));

         when 2 =>
            return ((
              Character'Pos (Str (Str'First))) * 64 +
              Character'Pos (Str (Str'First + 1))) mod Hash_Num;

         when 3 =>
            return (((
              Character'Pos (Str (Str'First))) * 16 +
              Character'Pos (Str (Str'First + 2))) * 16 +
              Character'Pos (Str (Str'First + 1))) mod Hash_Num;

         when 4 =>
            return ((((
              Character'Pos (Str (Str'First))) * 8 +
              Character'Pos (Str (Str'First + 1))) * 8 +
              Character'Pos (Str (Str'First + 2))) * 8 +
              Character'Pos (Str (Str'First + 3))) mod Hash_Num;

         when 5 =>
            return (((((
              Character'Pos (Str (Str'First + 3))) * 8 +
              Character'Pos (Str (Str'First))) * 4 +
              Character'Pos (Str (Str'First + 2))) * 4 +
              Character'Pos (Str (Str'First + 4))) * 8 +
              Character'Pos (Str (Str'First + 1))) mod Hash_Num;

         when 6 =>
            return ((((((
              Character'Pos (Str (Str'First + 4))) * 4 +
              Character'Pos (Str (Str'First))) * 4 +
              Character'Pos (Str (Str'First + 3))) * 4 +
              Character'Pos (Str (Str'First + 1))) * 4 +
              Character'Pos (Str (Str'First + 5))) * 4 +
              Character'Pos (Str (Str'First + 2))) mod Hash_Num;

         when 7 =>
            return (((((((
              Character'Pos (Str (Str'First + 3))) * 4 +
              Character'Pos (Str (Str'First + 2))) * 4 +
              Character'Pos (Str (Str'First))) * 4 +
              Character'Pos (Str (Str'First + 1))) * 2 +
              Character'Pos (Str (Str'First + 4))) * 2 +
              Character'Pos (Str (Str'First + 6))) * 2 +
              Character'Pos (Str (Str'First + 5))) mod Hash_Num;

         when 8 =>
            return ((((((((
              Character'Pos (Str (Str'First + 1))) * 4 +
              Character'Pos (Str (Str'First))) * 4 +
              Character'Pos (Str (Str'First + 2))) * 2 +
              Character'Pos (Str (Str'First + 4))) * 2 +
              Character'Pos (Str (Str'First + 6))) * 2 +
              Character'Pos (Str (Str'First + 5))) * 2 +
              Character'Pos (Str (Str'First + 3))) * 2 +
              Character'Pos (Str (Str'First + 7))) mod Hash_Num;

         when 9 =>
            return (((((((((
              Character'Pos (Str (Str'First + 1))) * 4 +
              Character'Pos (Str (Str'First))) * 4 +
              Character'Pos (Str (Str'First + 2))) * 4 +
              Character'Pos (Str (Str'First + 3))) * 2 +
              Character'Pos (Str (Str'First + 7))) * 2 +
              Character'Pos (Str (Str'First + 6))) * 2 +
              Character'Pos (Str (Str'First + 4))) * 2 +
              Character'Pos (Str (Str'First + 5))) * 2 +
              Character'Pos (Str (Str'First + 8))) mod Hash_Num;

         when 10 =>
            return ((((((((((
              Character'Pos (Str (Str'First))) * 2 +
              Character'Pos (Str (Str'First + 1))) * 2 +
              Character'Pos (Str (Str'First + 7))) * 2 +
              Character'Pos (Str (Str'First + 2))) * 2 +
              Character'Pos (Str (Str'First + 3))) * 2 +
              Character'Pos (Str (Str'First + 8))) * 2 +
              Character'Pos (Str (Str'First + 5))) * 2 +
              Character'Pos (Str (Str'First + 4))) * 2 +
              Character'Pos (Str (Str'First + 6))) * 2 +
              Character'Pos (Str (Str'First + 9))) mod Hash_Num;

         when 11 =>
            return (((((((((((
              Character'Pos (Str (Str'First + 4))) * 2 +
              Character'Pos (Str (Str'First))) * 2 +
              Character'Pos (Str (Str'First + 5))) * 2 +
              Character'Pos (Str (Str'First + 8))) * 2 +
              Character'Pos (Str (Str'First + 6))) * 2 +
              Character'Pos (Str (Str'First + 2))) * 2 +
              Character'Pos (Str (Str'First + 7))) * 2 +
              Character'Pos (Str (Str'First + 1))) * 2 +
              Character'Pos (Str (Str'First + 9))) * 2 +
              Character'Pos (Str (Str'First + 3))) * 2 +
              Character'Pos (Str (Str'First + 10))) mod Hash_Num;

         when 12 =>
            return ((((((((((((
              Character'Pos (Str (Str'First + 2))) * 2 +
              Character'Pos (Str (Str'First + 1))) * 2 +
              Character'Pos (Str (Str'First + 4))) * 2 +
              Character'Pos (Str (Str'First))) * 2 +
              Character'Pos (Str (Str'First + 5))) * 2 +
              Character'Pos (Str (Str'First + 3))) * 2 +
              Character'Pos (Str (Str'First + 7))) * 2 +
              Character'Pos (Str (Str'First + 10))) * 2 +
              Character'Pos (Str (Str'First + 6))) * 2 +
              Character'Pos (Str (Str'First + 8))) * 2 +
              Character'Pos (Str (Str'First + 9))) * 2 +
              Character'Pos (Str (Str'First + 11))) mod Hash_Num;

         --  Names longer than 12 characters are handled by taking the first
         --  6 odd numbered characters and the last 6 even numbered characters.

         when others => declare
               Even_Name_Len : constant Integer :=
                 Str'First - 1 + (Str'Length) / 2 * 2;
         begin
            return ((((((((((((
              Character'Pos (Str (Str'First))) * 2 +
              Character'Pos (Str (Even_Name_Len - 10))) * 2 +
              Character'Pos (Str (Str'First + 2))) * 2 +
              Character'Pos (Str (Even_Name_Len - 08))) * 2 +
              Character'Pos (Str (Str'First + 4))) * 2 +
              Character'Pos (Str (Even_Name_Len - 06))) * 2 +
              Character'Pos (Str (Str'First + 6))) * 2 +
              Character'Pos (Str (Even_Name_Len - 04))) * 2 +
              Character'Pos (Str (Str'First + 8))) * 2 +
              Character'Pos (Str (Even_Name_Len - 02))) * 2 +
              Character'Pos (Str (Str'First + 10))) * 2 +
              Character'Pos (Str (Even_Name_Len))) mod Hash_Num;
         end;
      end case;
   end Hash;

   -------------
   -- Get_Key --
   -------------

   function Get_Key (Str : Symbol) return Cst_Byte_Sequence_Access is
   begin
      return Cst_Byte_Sequence_Access (Str);
   end Get_Key;

   ----------
   -- Free --
   ----------

   procedure Free (Str : in out Symbol) is
      function Convert is new Ada.Unchecked_Conversion
        (Symbol, Byte_Sequence_Access);
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Byte_Sequence, Byte_Sequence_Access);
      S : Byte_Sequence_Access := Convert (Str);
   begin
      Unchecked_Free (S);
      Str := No_Symbol;
   end Free;

   ---------------
   -- Key_Equal --
   ---------------

   function Key_Equal (Key1, Key2 : Cst_Byte_Sequence_Access) return Boolean is
   begin
      return Key1.all = Key2.all;
   end Key_Equal;

   ----------
   -- Find --
   ----------

   function Find
     (Table : access Symbol_Table_Record;
      Str   : Unicode.CES.Byte_Sequence) return Symbol
   is
      Result : Symbol;
   begin
      if Str'Length = 0 then
         return Empty_String;
      else
         Result := String_Htable.Get (Table.Hash, Str'Unrestricted_Access);

         if Result = No_Symbol then
            Result := new Byte_Sequence'(Str);
            String_Htable.Set (Table.Hash, Result);
         end if;
         return Result;
      end if;
   end Find;

   ---------
   -- Get --
   ---------

   function Get (Sym : Symbol) return Cst_Byte_Sequence_Access is
   begin
      return Cst_Byte_Sequence_Access (Sym);
   end Get;

   ----------
   -- Free --
   ----------

   procedure Free (Table : in out Symbol_Table_Record) is
   begin
      String_Htable.Reset (Table.Hash);
   end Free;

   ----------
   -- Hash --
   ----------

   function Hash (S : Sax.Symbols.Symbol) return Interfaces.Unsigned_32 is
   begin
      return Hash (Cst_Byte_Sequence_Access (S));
   end Hash;

end Sax.Symbols;
