with Ada.Unchecked_Deallocation;
with Interfaces; use Interfaces;

package body Sax.HTable is

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Element, Element_Ptr);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Htable_Item, Item_Ptr);

   -----------
   -- Reset --
   -----------

   procedure Reset (Hash_Table : in out HTable) is
      Item, Tmp : Item_Ptr;
   begin
      for Index in Hash_Table.Table'Range loop
         Item := Hash_Table.Table (Index);
         while Item /= null loop
            Free (Item.Elem.all);
            Unchecked_Free (Item.Elem);
            Tmp := Item;
            Item := Item.Next;
            Unchecked_Free (Tmp);
         end loop;
      end loop;
   end Reset;

   ---------
   -- Set --
   ---------

   procedure Set (Hash_Table : in out HTable; E : Element) is
      Index : constant Unsigned_32 :=
        Hash (Get_Key (E)) mod Hash_Table.Size + 1;
   begin
      Hash_Table.Table (Index) := new Htable_Item'
        (Elem => new Element'(E),
         Next => Hash_Table.Table (Index));
   end Set;

   ---------
   -- Get --
   ---------

   function Get (Hash_Table : HTable; K : Key) return Element is
      Elmt : Item_Ptr := Hash_Table.Table
        (Hash (K) mod Hash_Table.Size + 1);
   begin
      while Elmt /= null loop
         if Equal (Get_Key (Elmt.Elem.all), K) then
            return Elmt.Elem.all;
         end if;

         Elmt := Elmt.Next;
      end loop;
      return Empty_Element;
   end Get;

   ------------
   -- Remove --
   ------------

   procedure Remove (Hash_Table : in out HTable; K : Key) is
      Index     : constant Unsigned_32 :=
        Hash (K) mod Hash_Table.Size + 1;
      Elmt      : Item_Ptr := Hash_Table.Table (Index);
      Next_Elmt : Item_Ptr;
   begin
      if Elmt = null then
         return;

      elsif Equal (Get_Key (Elmt.Elem.all), K) then
         Hash_Table.Table (Index) := Elmt.Next;

      else
         loop
            Next_Elmt :=  Elmt.Next;

            if Next_Elmt = null then
               return;

            elsif Equal (Get_Key (Next_Elmt.Elem.all), K) then
               Elmt.Next := Next_Elmt.Next;

               Free (Next_Elmt.Elem.all);
               Unchecked_Free (Next_Elmt.Elem);
               Unchecked_Free (Next_Elmt);
               return;
            end if;

            Elmt := Next_Elmt;
         end loop;
      end if;
   end Remove;

   -----------
   -- First --
   -----------

   function First (Hash_Table : HTable) return Iterator is
   begin
      for Index in Hash_Table.Table'Range loop
         if Hash_Table.Table (Index) /= null then
            return (Index => Index,
                    Item  => Hash_Table.Table (Index));
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

      Iter.Item := Iter.Item.Next;

      if Iter.Item /= null then
         return;
      end if;

      loop
         Iter.Index := Unsigned_32'Succ (Iter.Index);
         exit when Iter.Index > Hash_Table.Table'Last
           or else Hash_Table.Table (Iter.Index) /= null;
      end loop;

      if Iter.Index > Hash_Table.Table'Last then
         Iter := No_Iterator;
      else
         Iter.Item := Hash_Table.Table (Iter.Index);
      end if;
   end Next;

   function Current (Iter : Iterator) return Element is
   begin
      return Iter.Item.Elem.all;
   end Current;

end Sax.HTable;
