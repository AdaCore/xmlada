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

with Unicode.CES;  use Unicode.CES;
with Unchecked_Deallocation;

package body Sax.Attributes is

   procedure Free (Attr : in out Attribute);
   --  Free the memory allocated for a single attribute.
   --  This doesn't free the memory allocated for Attr itself, nor any other
   --  node in the list.

   procedure Free_Node is new Unchecked_Deallocation
     (Attribute, Attribute_Access);

   function Get
     (Attr : Attributes_Impl; Index : Natural) return Attribute_Access;
   --  Return the Index-th attribute in the list, or raise Out_Of_Bounds if
   --  Index is too big

   procedure Get (Attr : Attributes_Impl;
                  Qname : Byte_Sequence;
                  Index : out Integer;
                  Att   : out Attribute_Access);
   --  Return the first attribute whose Qname matches

   procedure Get (Attr       : Attributes_Impl;
                  URI        : Byte_Sequence;
                  Local_Name : Byte_Sequence;
                  Index      : out Integer;
                  Att        : out Attribute_Access);
   --  Return the first attribute whose name matches

   ----------
   -- Free --
   ----------

   procedure Free (Attr : in out Attribute) is
   begin
      Free (Attr.URI);
      Free (Attr.Local_Name);
      Free (Attr.Att_Type);
      Free (Attr.Value);
      Free (Attr.Qname);
   end Free;

   ---------
   -- Get --
   ---------

   function Get (Attr : Attributes_Impl; Index : in Natural)
      return Attribute_Access
   is
      Tmp : Attribute_Access := Attr.First;
   begin
      if Index >= Attr.Length then
         raise Out_Of_Bounds;
      end if;

      for J in 0 .. Index - 1 loop
         Tmp := Tmp.Next;
      end loop;
      pragma Assert (Tmp /= null, "Get returned a null attribute");
      return Tmp;
   end Get;

   ---------
   -- Get --
   ---------

   procedure Get (Attr : Attributes_Impl;
                  Qname : Byte_Sequence;
                  Index : out Integer;
                  Att   : out Attribute_Access) is
   begin
      Index := 0;
      Att := Attr.First;
      while Att /= null loop
         if Att.Qname.all = Qname then
            return;
         end if;
         Index := Index + 1;
         Att := Att.Next;
      end loop;
      Index := -1;
   end Get;

   ---------
   -- Get --
   ---------

   procedure Get (Attr       : Attributes_Impl;
                  URI        : Byte_Sequence;
                  Local_Name : Byte_Sequence;
                  Index      : out Integer;
                  Att        : out Attribute_Access) is
   begin
      Index := 0;
      Att := Attr.First;
      while Att /= null loop
         if Att.URI.all = URI
           and then Att.Local_Name.all = Local_Name
         then
            return;
         end if;
         Att := Att.Next;
         Index := Index + 1;
      end loop;
      Index := -1;
   end Get;

   -------------------
   -- Add_Attribute --
   -------------------

   procedure Add_Attribute
     (Attr       : in out Attributes_Impl;
      URI        : Unicode.CES.Byte_Sequence;
      Local_Name : Unicode.CES.Byte_Sequence;
      Qname      : Unicode.CES.Byte_Sequence;
      Att_Type   : Unicode.CES.Byte_Sequence;
      Value      : Unicode.CES.Byte_Sequence)
   is
   begin
      if Attr.Last = null then
         Attr.First := new Attribute;
         Attr.Last := Attr.First;
      else
         Attr.Last.Next := new Attribute;
         Attr.Last := Attr.Last.Next;
      end if;

      Attr.Last.URI := new Byte_Sequence' (URI);
      Attr.Last.Local_Name := new Byte_Sequence' (Local_Name);
      Attr.Last.Att_Type := new Byte_Sequence' (Att_Type);
      Attr.Last.Value := new Byte_Sequence' (Value);
      Attr.Last.Qname := new Byte_Sequence' (Qname);
      Attr.Length := Attr.Length + 1;
   end Add_Attribute;

   -----------
   -- Clear --
   -----------

   procedure Clear (Attr : in out Attributes_Impl) is
      Tmp : Attribute_Access;
   begin
      while Attr.First /= null loop
         Tmp := Attr.First.Next;
         Free (Attr.First.all);
         Free_Node (Attr.First);
         Attr.First := Tmp;
      end loop;
      Attr.Last := null;
      Attr.Length := 0;
   end Clear;

   ----------------------
   -- Remove_Attribute --
   ----------------------

   procedure Remove_Attribute
     (Attr : in out Attributes_Impl;
      Index : Natural)
   is
      Tmp : Attribute_Access;
      Tmp2 : Attribute_Access;
   begin
      if Index = 0 then
         Tmp := Attr.First;
         if Attr.Last = Attr.First then
            Attr.Last := null;
         end if;
         Attr.First := Attr.First.Next;
         Free (Tmp.all);
         Free_Node (Tmp);
      else
         Tmp := Get (Attr, Index - 1);
         if Attr.Last = Tmp then
            Attr.Last := Attr.First;
            while Attr.Last.Next /= null loop
               Attr.Last := Attr.Last.Next;
            end loop;
         end if;
         Tmp2 := Tmp.Next;
         Tmp.Next := Tmp2.Next;
         Free (Tmp2.all);
         Free_Node (Tmp2);
      end if;
      Attr.Length := Attr.Length - 1;
   end Remove_Attribute;

   -------------------
   -- Set_Attribute --
   -------------------

   procedure Set_Attribute
     (Attr       : in out Attributes_Impl;
      Index      : Natural;
      URI        : Unicode.CES.Byte_Sequence;
      Local_Name : Unicode.CES.Byte_Sequence;
      Qname      : Unicode.CES.Byte_Sequence;
      Att_Type   : Unicode.CES.Byte_Sequence;
      Value      : Unicode.CES.Byte_Sequence)
   is
      Att : Attribute_Access := Get (Attr, Index);
   begin
      Free (Att.all);
      Att.URI := new Byte_Sequence' (URI);
      Att.Local_Name := new Byte_Sequence' (Local_Name);
      Att.Att_Type := new Byte_Sequence' (Att_Type);
      Att.Value := new Byte_Sequence' (Value);
      Att.Qname := new Byte_Sequence' (Qname);
   end Set_Attribute;

   --------------------
   -- Set_Attributes --
   --------------------

   procedure Set_Attributes
     (Attr : in out Attributes_Impl;
      From : Attributes'Class)
   is
      Length : Natural := Get_Length (From);
   begin
      for J in 0 .. Length - 1 loop
         Add_Attribute (Attr,
                        URI        => Get_URI (From, J),
                        Local_Name => Get_Local_Name (From, J),
                        Qname      => Get_Qname (From, J),
                        Att_Type   => Get_Type (From, J),
                        Value      => Get_Value (From, J));
      end loop;
   end Set_Attributes;

   --------------------
   -- Set_Local_Name --
   --------------------

   procedure Set_Local_Name
     (Attr       : in out Attributes_Impl;
      Index      : Natural;
      Local_Name : Unicode.CES.Byte_Sequence)
   is
      Tmp : Attribute_Access := Get (Attr, Index);
   begin
      Free (Tmp.Local_Name);
      Tmp.Local_Name := new Byte_Sequence' (Local_Name);
   end Set_Local_Name;

   ---------------
   -- Set_Qname --
   ---------------

   procedure Set_Qname
     (Attr  : in out Attributes_Impl;
      Index : Natural;
      Qname : Unicode.CES.Byte_Sequence)
   is
      Tmp : Attribute_Access := Get (Attr, Index);
   begin
      Free (Tmp.Qname);
      Tmp.Qname := new Byte_Sequence' (Qname);
   end Set_Qname;

   --------------
   -- Set_Type --
   --------------

   procedure Set_Type
     (Attr     : in out Attributes_Impl;
      Index    : Natural;
      Att_Type : Unicode.CES.Byte_Sequence)
   is
      Tmp : Attribute_Access := Get (Attr, Index);
   begin
      Free (Tmp.Att_Type);
      Tmp.Att_Type := new Byte_Sequence' (Att_Type);
   end Set_Type;

   -------------
   -- Set_URI --
   -------------

   procedure Set_URI
     (Attr  : in out Attributes_Impl;
      Index : Natural;
      URI   : Unicode.CES.Byte_Sequence)
   is
      Tmp : Attribute_Access := Get (Attr, Index);
   begin
      Free (Tmp.URI);
      Tmp.URI := new Byte_Sequence' (URI);
   end Set_URI;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value
     (Attr  : in out Attributes_Impl;
      Index : Natural;
      Value : Unicode.CES.Byte_Sequence)
   is
      Tmp : Attribute_Access := Get (Attr, Index);
   begin
      pragma Assert (Tmp /= null, "Unexpected null attribute");
      Free (Tmp.Value);
      Tmp.Value := new Byte_Sequence' (Value);
   end Set_Value;

   ---------------
   -- Get_Index --
   ---------------

   function Get_Index
     (Attr  : Attributes_Impl;
      Qname : Unicode.CES.Byte_Sequence) return Integer
   is
      J : Integer;
      Tmp : Attribute_Access;
   begin
      Get (Attr, Qname, J, Tmp);
      return J;
   end Get_Index;

   ---------------
   -- Get_Index --
   ---------------

   function Get_Index
     (Attr       : Attributes_Impl;
      URI        : Unicode.CES.Byte_Sequence;
      Local_Name : Unicode.CES.Byte_Sequence)
      return Integer
   is
      J : Integer;
      Tmp : Attribute_Access;
   begin
      Get (Attr, URI, Local_Name, J, Tmp);
      return J;
   end Get_Index;

   ----------------
   -- Get_Length --
   ----------------

   function Get_Length (Attr : Attributes_Impl) return Natural is
   begin
      return Attr.Length;
   end Get_Length;

   --------------------
   -- Get_Local_Name --
   --------------------

   function Get_Local_Name (Attr : Attributes_Impl; Index : Natural)
      return Unicode.CES.Byte_Sequence is
   begin
      return Get (Attr, Index).Local_Name.all;
   end Get_Local_Name;

   ---------------
   -- Get_Qname --
   ---------------

   function Get_Qname (Attr : Attributes_Impl; Index : Natural)
      return Unicode.CES.Byte_Sequence is
   begin
      return Get (Attr, Index).Qname.all;
   end Get_Qname;

   --------------
   -- Get_Type --
   --------------

   function Get_Type (Attr : Attributes_Impl; Index : Natural)
      return Unicode.CES.Byte_Sequence is
   begin
      return Get (Attr, Index).Att_Type.all;
   end Get_Type;

   --------------
   -- Get_Type --
   --------------

   function Get_Type
     (Attr : Attributes_Impl;
      Qname : Unicode.CES.Byte_Sequence)
      return Unicode.CES.Byte_Sequence
   is
      J : Integer;
      Tmp : Attribute_Access;
   begin
      Get (Attr, Qname, J, Tmp);
      return Tmp.Att_Type.all;
   end Get_Type;

   --------------
   -- Get_Type --
   --------------

   function Get_Type
     (Attr       : Attributes_Impl;
      URI        : Unicode.CES.Byte_Sequence;
      Local_Name : Unicode.CES.Byte_Sequence)
      return Unicode.CES.Byte_Sequence
   is
      J : Integer;
      Tmp : Attribute_Access;
   begin
      Get (Attr, URI, Local_Name, J, Tmp);
      return Tmp.Att_Type.all;
   end Get_Type;

   -------------
   -- Get_URI --
   -------------

   function Get_URI (Attr : Attributes_Impl; Index : Natural)
      return Unicode.CES.Byte_Sequence is
   begin
      return Get (Attr, Index).URI.all;
   end Get_URI;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value (Attr : Attributes_Impl; Index : Natural)
      return Unicode.CES.Byte_Sequence is
   begin
      return Get (Attr, Index).Value.all;
   end Get_Value;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value
     (Attr : Attributes_Impl;
      Qname : Unicode.CES.Byte_Sequence)
      return Unicode.CES.Byte_Sequence
   is
      J : Integer;
      Tmp : Attribute_Access;
   begin
      Get (Attr, Qname, J, Tmp);
      return Tmp.Value.all;
   end Get_Value;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value
     (Attr       : Attributes_Impl;
      URI        : Unicode.CES.Byte_Sequence;
      Local_Name : Unicode.CES.Byte_Sequence)
      return Unicode.CES.Byte_Sequence
   is
      J : Integer;
      Tmp : Attribute_Access;
   begin
      Get (Attr, URI, Local_Name, J, Tmp);
      return Tmp.Value.all;
   end Get_Value;
end Sax.Attributes;
