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

--  A symbol table.
--  This provides integers to represent strings internally. The implementation
--  is copied from namet.adb, in the GNAT sources

with Interfaces;
with Sax.Pointers;
with Unicode.CES;

package Sax.Symbols is

   type Symbol_Table_Record is new Sax.Pointers.Root_Encapsulated with private;
   type Symbol_Table_Access is access all Symbol_Table_Record'Class;
   --  A symbol table associating integers with strings.
   --  By default, this is not task safe, so you will need to extend this if
   --  the symbol is to be shared between multiple tasks.

   type Symbol is private;
   No_Symbol    : constant Symbol;
   Empty_String : constant Symbol;

   Symbol_Percent   : constant Symbol;
   Symbol_Ampersand : constant Symbol;

   function Find
     (Table : access Symbol_Table_Record;
      Str   : Unicode.CES.Byte_Sequence) return Symbol;
   --  Return the internal version of Str.
   --  Comparing Symbol is the same as comparing the string itself, but much
   --  faster.

   function Get
     (Table : Symbol_Table_Record;
      Sym   : Symbol) return Unicode.CES.Byte_Sequence_Access;
   --  The string associated with the symbol.
   --  The returned string must not be deallocated, it points to internal data

   procedure Free (Table : in out Symbol_Table_Record);
   --  Free the table

   function Hash (S : Sax.Symbols.Symbol) return Interfaces.Unsigned_32;
   --  Return the symbol

private
   Hash_Num : constant := 2**12;
   --  Number of headers in the hash table. Current hash algorithm is closely
   --  tailored to this choice, so it can only be changed if a corresponding
   --  change is made to the hash algorithm.

   Hash_Max : constant := Hash_Num - 1;
   --  Indexes in the hash header table run from 0 to Hash_Num - 1

   subtype Hash_Index_Type is Integer range 0 .. Hash_Max;
   --  Range of hash index values

   type Symbol is new Natural;
   No_Symbol        : constant Symbol := 0;
   Empty_String     : constant Symbol := 1;
   Symbol_Percent   : constant Symbol := Character'Pos ('%');
   Symbol_Ampersand : constant Symbol := Character'Pos ('&');

   --  Below 255 is for one-character string. These are not actually entered in
   --  the tables.

   type Hash_Table_Array is array (Hash_Index_Type) of Symbol;
   --  The hash table is used to locate existing entries in the names table.
   --  The entries point to the first names table entry whose hash value
   --  matches the hash code. Then subsequent names table entries with the
   --  same hash code value are linked through the Hash_Link fields.

   type Name_Entry is record
      Value     : Unicode.CES.Byte_Sequence_Access;
      Hash_Link : Symbol := No_Symbol;
      --  Link to next entry in names table for same hash code
   end record;

   type Name_Entries_Array is array (Symbol range <>) of Name_Entry;
   type Name_Entries_Access is access all Name_Entries_Array;
   --  The entries start at 256. Below that is for one
   --  character latin-1 strings

   type Symbol_Table_Record is new Sax.Pointers.Root_Encapsulated with record
      Hash    : Hash_Table_Array := (others => No_Symbol);
      Entries : Name_Entries_Access;
      Entries_Last : Symbol := 255;
   end record;

end Sax.Symbols;
