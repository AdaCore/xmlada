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
      --  This hash function looks at every character, in order to make it
      --  likely that similar strings get different hash values. The rotate by
      --  7 bits has been determined empirically to be good, and it doesn't
      --  lose bits like a shift would. The final conversion can't overflow,
      --  because the table is 2**16 in size. This function probably needs to
      --  be changed if the hash table size is changed.

      --  Note that we could get some speed improvement by aligning the string
      --  to 32 or 64 bits, and doing word-wise xor's. We could also implement
      --  a growable table. It doesn't seem worth the trouble to do those
      --  things, for now.

      Result : Unsigned_32 := 0;
   begin
      for J in Str'Range loop
         Result := Rotate_Left (Result, 7) xor Character'Pos (Str (J));
      end loop;

      return Result;
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
      use String_Htable;
      Result : String_Htable.Element_Ptr;
      Hashed : Interfaces.Unsigned_32;
      Str_A  : Symbol;
   begin
      if Str'Length = 0 then
         return Empty_String;
      else
         Hashed := Hash (Cst_Byte_Sequence_Access'(Str'Unrestricted_Access));
         Result := String_Htable.Get_Ptr_With_Hash
           (Table.Hash, Str'Unrestricted_Access, Hashed);

         if Result = null then
            Str_A := new Byte_Sequence'(Str);
            String_Htable.Set_With_Hash (Table.Hash, Str_A, Hashed);
            return Str_A;
         end if;
         return Result.all;
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
