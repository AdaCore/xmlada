with Interfaces;

generic
   type Element (<>) is private;
   --  The type of element to be stored

   Empty_Element : Element;

   with procedure Free (Elmt : in out Element);
   --  Free the memory used by Elmt

   type Key (<>) is limited private;
   with function Get_Key (E : Element)  return Key;
   with function Hash    (F : Key)      return Interfaces.Unsigned_32;
   with function Equal   (F1, F2 : Key) return Boolean;

package Sax.HTable is

   type HTable (Size : Interfaces.Unsigned_32) is private;

   procedure Reset (Hash_Table : in out HTable);
   --  Resets the hash table by freeing all the elements.

   procedure Set (Hash_Table : in out HTable; E : Element);
   --  Insert the element pointer in the HTable

   function Get (Hash_Table : HTable; K : Key) return Element;
   --  Returns the latest inserted element pointer with the given Key
   --  or Empty_Element if none.

   procedure Remove (Hash_Table : in out HTable; K : Key);
   --  Removes the latest inserted element pointer associated with the
   --  given key if any, does nothing if none.

   type Iterator is private;
   No_Iterator : constant Iterator;

   function First (Hash_Table : HTable) return Iterator;
   --  Return the first element in the table
   --  There is no guarantee that 2 calls to this function will return the same
   --  element.

   procedure Next
     (Hash_Table : in out HTable;
      Iter       : in out Iterator);
   --  Move to the next element in the htash table, that hasn't been returned
   --  yet. All the elements in the table will eventually be visited if there
   --  is no call to Set since the call to First.
   --  Iter is set to No_Iterator if there is no more element in the table.

   function Current (Iter : Iterator) return Element;
   --  Return the element pointed to by Iter.

private

   type Element_Ptr is access Element;

   type Htable_Item;
   type Item_Ptr is access Htable_Item;
   type Htable_Item is record
      Elem : Element_Ptr;
      Next : Item_Ptr;
   end record;

   type Item_Array is array (Interfaces.Unsigned_32 range <>) of Item_Ptr;

   type HTable (Size : Interfaces.Unsigned_32) is record
      Table : Item_Array (1 .. Size);
   end record;

   type Iterator is record
      Index : Interfaces.Unsigned_32;
      Item  : Item_Ptr;
   end record;

   No_Iterator : constant Iterator := (Interfaces.Unsigned_32'Last, null);
end Sax.HTable;
