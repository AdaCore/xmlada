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

with Ada.Unchecked_Deallocation;
with Unicode.CES;                use Unicode.CES;

package body Sax.Symbols is

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Name_Entries_Array, Name_Entries_Access);

   Initial_Entries_Size : constant := 50;
   Entries_Increment    : constant := 50;

   Cst_Empty_String : aliased String := "";

   subtype One_Char_Sequence is Byte_Sequence (1 .. 1);
   One_Char_Strings :
     array (Symbol range 2 .. 255) of aliased One_Char_Sequence;

   function Hash (Str : Byte_Sequence) return Hash_Index_Type;
   pragma Inline (Hash);
   --  Compute hash code for Str. We consider the bytes, not the characters,
   --  for efficiency

   procedure Initialize;
   --  Initialize this package

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      for C in One_Char_Strings'Range loop
         One_Char_Strings (C) := (1 => Character'Val (Integer (C)));
      end loop;
   end Initialize;

   ----------
   -- Hash --
   ----------

   function Hash (Str : Byte_Sequence) return Hash_Index_Type is
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

   ----------
   -- Find --
   ----------

   function Find
     (Table : access Symbol_Table_Record;
      Str   : Unicode.CES.Byte_Sequence) return Symbol
   is
      New_Id     : Symbol;
      Hash_Index : Hash_Index_Type; --  Computed hash index
      Tmp        : Name_Entries_Access;
   begin
      if Str'Length = 0 then
         return Empty_String;

      elsif Str'Length = 1 then
         return Character'Pos (Str (Str'First));

      else
         Hash_Index := Hash (Str);
         New_Id := Table.Hash (Hash_Index);

         if New_Id = No_Symbol then
            Table.Hash (Hash_Index) := Table.Entries_Last + 1;

         else
            loop
               if Str = Table.Entries (New_Id).Value.all then
                  return New_Id;
               end if;

               if Table.Entries (New_Id).Hash_Link /= No_Symbol then
                  New_Id := Table.Entries (New_Id).Hash_Link;
               else
                  Table.Entries (New_Id).Hash_Link := Table.Entries_Last + 1;
                  exit;
               end if;
            end loop;
         end if;

         --  We fall through here only if a matching entry was not found in the
         --  hash table. We now create a new entry. The hash
         --  link pointing to the new entry (Entries_Last+1) has been set.

         if Table.Entries = null then
            Table.Entries := new Name_Entries_Array
              (256 .. 256 + Initial_Entries_Size);
            Table.Entries_Last := 255;

         elsif Table.Entries_Last = Table.Entries'Last then
            Tmp := Table.Entries;
            Table.Entries := new Name_Entries_Array
              (Table.Entries'First .. Table.Entries'Last + Entries_Increment);
            Table.Entries (Tmp'Range) := Tmp.all;
            Unchecked_Free (Tmp);
         end if;

         Table.Entries_Last := Table.Entries_Last + 1;
         Table.Entries (Table.Entries_Last) := Name_Entry'
           (Value     => new Unicode.CES.Byte_Sequence'(Str),
            Hash_Link => No_Symbol);

         return Table.Entries_Last;
      end if;
   end Find;

   ---------
   -- Get --
   ---------

   function Get
     (Table : Symbol_Table_Record;
      Sym   : Symbol) return Unicode.CES.Byte_Sequence_Access is
   begin
      if Sym = No_Symbol then
         return null;
      elsif Sym = Empty_String then
         return Cst_Empty_String'Access;
      elsif Sym < 256 then
         return Byte_Sequence (One_Char_Strings (Sym))'Unrestricted_Access;
      else
         return Table.Entries (Sym).Value;
      end if;
   end Get;

   ----------
   -- Free --
   ----------

   procedure Free (Table : in out Symbol_Table_Record) is
   begin
      if Table.Entries /= null then
         for E in Table.Entries'Range loop
            if Table.Entries (E).Value /= null then
               Free (Table.Entries (E).Value);
            end if;
         end loop;
         Unchecked_Free (Table.Entries);
      end if;
   end Free;

   ----------
   -- Hash --
   ----------

   function Hash (S : Sax.Symbols.Symbol) return Interfaces.Unsigned_32 is
   begin
      return Interfaces.Unsigned_32 (S);
   end Hash;

begin
   Initialize;
end Sax.Symbols;
