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

with Unicode.CES;  use Unicode.CES;
with Unchecked_Deallocation;
with Sax.Models;   use Sax.Models;

package body Sax.Attributes is

   procedure Free (Attr : in out Attribute);
   --  Free the memory allocated for a single attribute.
   --  This doesn't free the memory allocated for Attr itself, nor any other
   --  node in the list.

   procedure Free_Node is new Unchecked_Deallocation
     (Attribute, Attribute_Access);

   function Get
     (Attr : Attributes'Class; Index : Natural) return Attribute_Access;
   --  Return the Index-th attribute in the list, or raise Out_Of_Bounds if
   --  Index is too big

   procedure Get (Attr : Attributes'Class;
                  Qname : Byte_Sequence;
                  Index : out Integer;
                  Att   : out Attribute_Access);
   --  Return the first attribute whose Qname matches

   procedure Get (Attr       : Attributes'Class;
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
      Free (Attr.Value);
      Free (Attr.Qname);

      --  Do not free Attr.Content, since this is a pointer to an external
      --  structure, shared by all attributes with the same model
   end Free;

   ---------
   -- Get --
   ---------

   function Get (Attr : Attributes'Class; Index : in Natural)
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

   procedure Get (Attr : Attributes'Class;
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

   procedure Get (Attr       : Attributes'Class;
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
     (Attr       : in out Attributes;
      URI        : Unicode.CES.Byte_Sequence;
      Local_Name : Unicode.CES.Byte_Sequence;
      Qname      : Unicode.CES.Byte_Sequence;
      Att_Type   : Attribute_Type;
      Content    : Sax.Models.Element_Model_Ptr;
      Value      : Unicode.CES.Byte_Sequence;
      Default_Decl : Default_Declaration := Default) is
   begin
      if Attr.Last = null then
         Attr.First := new Attribute;
         Attr.Last := Attr.First;
      else
         Attr.Last.Next := new Attribute;
         Attr.Last := Attr.Last.Next;
      end if;

      Attr.Last.URI := new Byte_Sequence'(URI);
      Attr.Last.Local_Name := new Byte_Sequence'(Local_Name);
      Attr.Last.Att_Type := Att_Type;
      Attr.Last.Value := new Byte_Sequence'(Value);
      Attr.Last.Qname := new Byte_Sequence'(Qname);
      Attr.Last.Default_Decl := Default_Decl;
      Attr.Last.Content := Content;
      Attr.Length := Attr.Length + 1;
   end Add_Attribute;

   -----------
   -- Clear --
   -----------

   procedure Clear (Attr : in out Attributes) is
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
     (Attr : in out Attributes;
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
     (Attr       : in out Attributes;
      Index      : Natural;
      URI        : Unicode.CES.Byte_Sequence;
      Local_Name : Unicode.CES.Byte_Sequence;
      Qname      : Unicode.CES.Byte_Sequence;
      Att_Type   : Attribute_Type;
      Content    : Sax.Models.Element_Model_Ptr;
      Value      : Unicode.CES.Byte_Sequence;
      Default_Decl : Default_Declaration := Default)
   is
      Att : Attribute_Access := Get (Attr, Index);
   begin
      Free (Att.all);
      Att.URI := new Byte_Sequence'(URI);
      Att.Local_Name := new Byte_Sequence'(Local_Name);
      Att.Att_Type := Att_Type;
      Att.Value := new Byte_Sequence'(Value);
      Att.Qname := new Byte_Sequence'(Qname);
      Att.Default_Decl := Default_Decl;
      Att.Content := Content;
   end Set_Attribute;

   --------------------
   -- Set_Attributes --
   --------------------

   procedure Set_Attributes
     (Attr : in out Attributes;
      From : Attributes'Class)
   is
      Length : constant Natural := Get_Length (From);
      Att : Attribute_Access;
   begin
      for J in 0 .. Length - 1 loop
         Att := Get (From, J);
         Add_Attribute (Attr,
                        URI        => Att.URI.all,
                        Local_Name => Att.Local_Name.all,
                        Qname      => Att.Qname.all,
                        Att_Type   => Att.Att_Type,
                        Content    => Att.Content,
                        Value      => Att.Value.all);
      end loop;
   end Set_Attributes;

   --------------------
   -- Set_Local_Name --
   --------------------

   procedure Set_Local_Name
     (Attr       : in out Attributes;
      Index      : Natural;
      Local_Name : Unicode.CES.Byte_Sequence)
   is
      Tmp : Attribute_Access := Get (Attr, Index);
   begin
      Free (Tmp.Local_Name);
      Tmp.Local_Name := new Byte_Sequence'(Local_Name);
   end Set_Local_Name;

   ---------------
   -- Set_Qname --
   ---------------

   procedure Set_Qname
     (Attr  : in out Attributes;
      Index : Natural;
      Qname : Unicode.CES.Byte_Sequence)
   is
      Tmp : Attribute_Access := Get (Attr, Index);
   begin
      Free (Tmp.Qname);
      Tmp.Qname := new Byte_Sequence'(Qname);
   end Set_Qname;

   --------------
   -- Set_Type --
   --------------

   procedure Set_Type
     (Attr     : in out Attributes;
      Index    : Natural;
      Att_Type : Attribute_Type) is
   begin
      Get (Attr, Index).Att_Type := Att_Type;
   end Set_Type;

   -------------
   -- Set_URI --
   -------------

   procedure Set_URI
     (Attr  : in out Attributes;
      Index : Natural;
      URI   : Unicode.CES.Byte_Sequence)
   is
      Tmp : Attribute_Access := Get (Attr, Index);
   begin
      Free (Tmp.URI);
      Tmp.URI := new Byte_Sequence'(URI);
   end Set_URI;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value
     (Attr  : in out Attributes;
      Index : Natural;
      Value : Unicode.CES.Byte_Sequence)
   is
      Tmp : Attribute_Access := Get (Attr, Index);
   begin
      pragma Assert (Tmp /= null, "Unexpected null attribute");
      Free (Tmp.Value);
      Tmp.Value := new Byte_Sequence'(Value);
   end Set_Value;

   ---------------
   -- Get_Index --
   ---------------

   function Get_Index
     (Attr  : Attributes;
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
     (Attr       : Attributes;
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

   function Get_Length (Attr : Attributes) return Natural is
   begin
      return Attr.Length;
   end Get_Length;

   --------------------
   -- Get_Local_Name --
   --------------------

   function Get_Local_Name (Attr : Attributes; Index : Natural)
      return Unicode.CES.Byte_Sequence is
   begin
      return Get (Attr, Index).Local_Name.all;
   end Get_Local_Name;

   ---------------
   -- Get_Qname --
   ---------------

   function Get_Qname (Attr : Attributes; Index : Natural)
      return Unicode.CES.Byte_Sequence is
   begin
      return Get (Attr, Index).Qname.all;
   end Get_Qname;

   --------------
   -- Get_Type --
   --------------

   function Get_Type (Attr : Attributes; Index : Natural)
      return Attribute_Type is
   begin
      return Get (Attr, Index).Att_Type;
   end Get_Type;

   --------------
   -- Get_Type --
   --------------

   function Get_Type
     (Attr : Attributes;
      Qname : Unicode.CES.Byte_Sequence)
      return Attribute_Type
   is
      J : Integer;
      Tmp : Attribute_Access;
   begin
      Get (Attr, Qname, J, Tmp);
      return Tmp.Att_Type;
   end Get_Type;

   --------------
   -- Get_Type --
   --------------

   function Get_Type
     (Attr       : Attributes;
      URI        : Unicode.CES.Byte_Sequence;
      Local_Name : Unicode.CES.Byte_Sequence)
      return Attribute_Type
   is
      J : Integer;
      Tmp : Attribute_Access;
   begin
      Get (Attr, URI, Local_Name, J, Tmp);
      return Tmp.Att_Type;
   end Get_Type;

   -------------
   -- Get_URI --
   -------------

   function Get_URI (Attr : Attributes; Index : Natural)
      return Unicode.CES.Byte_Sequence is
   begin
      return Get (Attr, Index).URI.all;
   end Get_URI;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value (Attr : Attributes; Index : Natural)
      return Unicode.CES.Byte_Sequence is
   begin
      return Get (Attr, Index).Value.all;
   end Get_Value;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value
     (Attr : Attributes;
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
     (Attr       : Attributes;
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

   -----------------------------
   -- Get_Default_Declaration --
   -----------------------------

   function Get_Default_Declaration
     (Attr : Attributes; Index : Natural) return Default_Declaration is
   begin
      return Get (Attr, Index).Default_Decl;
   end Get_Default_Declaration;

   -----------------
   -- Set_Content --
   -----------------

   procedure Set_Content
     (Attr    : Attributes;
      Index   : Natural;
      Content : Sax.Models.Element_Model_Ptr) is
   begin
      Get (Attr, Index).Content := Content;
   end Set_Content;

   -----------------
   -- Get_Content --
   -----------------

   function Get_Content (Attr : Attributes; Index : Natural)
      return Sax.Models.Element_Model_Ptr is
   begin
      return Get (Attr, Index).Content;
   end Get_Content;

end Sax.Attributes;
