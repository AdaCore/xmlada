-----------------------------------------------------------------------
--                XML/Ada - An XML suite for Ada95                   --
--                                                                   --
--                       Copyright (C) 2004-2010, AdaCore            --
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
with Interfaces; use Interfaces;

package body Sax.HTable is

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Htable_Item, Item_Ptr);

   -----------
   -- Reset --
   -----------

   procedure Reset (Hash_Table : in out HTable) is
      Item, Tmp : Item_Ptr;
   begin
      for Index in Hash_Table.Table'Range loop
         if Hash_Table.Table (Index).Set then
            Free (Hash_Table.Table (Index).Elem);
            Item := Hash_Table.Table (Index).Next;

            while Item /= null loop
               Free (Item.Elem);
               Tmp := Item;
               Item := Item.Next;
               Unchecked_Free (Tmp);
            end loop;

            Hash_Table.Table (Index).Set := False;
         end if;
      end loop;
   end Reset;

   ---------
   -- Set --
   ---------

   procedure Set (Hash_Table : in out HTable; E : Element) is
      Index : constant Unsigned_32 :=
        Hash (Get_Key (E)) mod Hash_Table.Size + 1;
   begin
      if Hash_Table.Table (Index).Set then
         Hash_Table.Table (Index).Next := new Htable_Item'
           (Elem => E,
            Next => Hash_Table.Table (Index).Next);
      else
         Hash_Table.Table (Index) :=
           (Elem => E,
            Next => null,
            Set  => True);
      end if;
   end Set;

   ---------
   -- Get --
   ---------

   function Get (Hash_Table : HTable; K : Key) return Element is
      Tmp : constant Element_Ptr := Get_Ptr (Hash_Table, K);
   begin
      if Tmp = null then
         return Empty_Element;
      else
         return Tmp.all;
      end if;
   end Get;

   -------------
   -- Get_Ptr --
   -------------

   function Get_Ptr (Hash_Table : HTable; K : Key) return Element_Ptr is
      H : constant Unsigned_32 := Hash (K) mod Hash_Table.Size + 1;
      Elmt : Item_Ptr;
   begin
      if Hash_Table.Table (H).Set then
         if Equal (Get_Key (Hash_Table.Table (H).Elem), K) then
            return Hash_Table.Table (H).Elem'Unrestricted_Access;
         else
            Elmt := Hash_Table.Table (H).Next;

            while Elmt /= null loop
               if Equal (Get_Key (Elmt.Elem), K) then
                  return Elmt.Elem'Access;
               end if;

               Elmt := Elmt.Next;
            end loop;
         end if;
      end if;
      return null;
   end Get_Ptr;

   ------------
   -- Remove --
   ------------

   procedure Remove (Hash_Table : in out HTable; K : Key) is
      Index     : constant Unsigned_32 :=
        Hash (K) mod Hash_Table.Size + 1;
      Elmt      : Item_Ptr;
      Next_Elmt : Item_Ptr;
   begin
      if not Hash_Table.Table (Index).Set then
         return;

      elsif Equal (Get_Key (Hash_Table.Table (Index).Elem), K) then
         Free (Hash_Table.Table (Index).Elem);
         Elmt := Hash_Table.Table (Index).Next;  --  second element in list
         if Elmt = null then
            Hash_Table.Table (Index).Set := False;
         else
            Hash_Table.Table (Index).Elem := Elmt.Elem;
            Hash_Table.Table (Index).Next := Elmt.Next;  --  to third element
            Unchecked_Free (Elmt); --  no longer needed, was copied to first
         end if;

      else
         Next_Elmt := Hash_Table.Table (Index).Next;
         loop
            if Next_Elmt = null then
               return;

            elsif Equal (Get_Key (Next_Elmt.Elem), K) then
               if Elmt = null then
                  Hash_Table.Table (Index).Next := Next_Elmt.Next;
               else
                  Elmt.Next := Next_Elmt.Next;
               end if;

               Free (Next_Elmt.Elem);
               Unchecked_Free (Next_Elmt);
               return;
            end if;

            Elmt := Next_Elmt;
            Next_Elmt :=  Elmt.Next;
         end loop;
      end if;
   end Remove;

   -----------
   -- First --
   -----------

   function First (Hash_Table : HTable) return Iterator is
   begin
      for Index in Hash_Table.Table'Range loop
         if Hash_Table.Table (Index).Set then
            return (Index => Index,
                    Elem  => Hash_Table.Table (Index).Elem'Unrestricted_Access,
                    Item => null);
         end if;
      end loop;

      return No_Iterator;
   end First;

   ----------
   -- Next --
   ----------

   procedure Next (Hash_Table : in out HTable; Iter : in out Iterator) is
   begin
      pragma Assert (Iter /= No_Iterator);

      if Iter.Item = null then
         Iter.Item := Hash_Table.Table (Iter.Index).Next;
      else
         Iter.Item := Iter.Item.Next;
      end if;

      if Iter.Item /= null then
         Iter.Elem := Iter.Item.Elem'Unrestricted_Access;
         return;
      end if;

      loop
         Iter.Index := Unsigned_32'Succ (Iter.Index);
         exit when Iter.Index > Hash_Table.Table'Last
           or else Hash_Table.Table (Iter.Index).Set;
      end loop;

      if Iter.Index > Hash_Table.Table'Last then
         Iter := No_Iterator;
      else
         Iter.Item := null;
         Iter.Elem := Hash_Table.Table (Iter.Index).Elem'Unrestricted_Access;
      end if;
   end Next;

   function Current (Iter : Iterator) return Element is
   begin
      return Iter.Elem.all;
   end Current;

end Sax.HTable;
