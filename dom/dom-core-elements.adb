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

with DOM.Core.Nodes;     use DOM.Core.Nodes;
with DOM.Core.Attrs;     use DOM.Core.Attrs;
with DOM.Core.Documents; use DOM.Core.Documents;

package body DOM.Core.Elements is

   -------------------
   -- Get_Attribute --
   -------------------

   function Get_Attribute (Elem : Element; Name : DOM_String)
      return DOM_String
   is
      Att : constant Attr := Get_Named_Item (Elem.Attributes, Name);
   begin
      if Att /= null then
         return Node_Value (Att);
      else
         return "";
      end if;
   end Get_Attribute;

   ----------------------
   -- Get_Attribute_NS --
   ----------------------

   function Get_Attribute_NS
     (Elem : Element; Namespace_URI : DOM_String; Local_Name : DOM_String)
      return DOM_String
   is
      Att : constant Attr := Get_Named_Item_NS
        (Elem.Attributes, Namespace_URI, Local_Name);
   begin
      if Att /= null then
         return Node_Value (Att);
      else
         return "";
      end if;
   end Get_Attribute_NS;

   -------------------
   -- Set_Attribute --
   -------------------

   procedure Set_Attribute
     (Elem : Element; Name : DOM_String; Value : DOM_String)
   is
      Att : constant Attr := Create_Attribute (Owner_Document (Elem), Name);
   begin
      Set_Value (Att, Value);
      Set_Named_Item (Elem.Attributes, Att);
   end Set_Attribute;

   ----------------------
   -- Set_Attribute_NS --
   ----------------------

   procedure Set_Attribute_NS
     (Elem : Element;
      Namespace_URI : DOM_String;
      Qualified_Name : DOM_String;
      Value : DOM_String)
   is
      Att : constant Attr := Create_Attribute_NS
        (Owner_Document (Elem), Namespace_URI, Qualified_Name);
   begin
      Set_Value (Att, Value);
      Set_Named_Item_NS (Elem.Attributes, Att);
   end Set_Attribute_NS;

   ----------------------
   -- Remove_Attribute --
   ----------------------

   procedure Remove_Attribute (Elem : Element; Name : DOM_String) is
   begin
      Remove_Named_Item (Elem.Attributes, Name);
   end Remove_Attribute;

   -------------------------
   -- Remove_Attribute_NS --
   -------------------------

   procedure Remove_Attribute_NS
     (Elem : Element; Namespace_URI : DOM_String; Local_Name : DOM_String) is
   begin
      Remove_Named_Item_NS (Elem.Attributes, Namespace_URI, Local_Name);
   end Remove_Attribute_NS;

   ------------------------
   -- Get_Attribute_Node --
   ------------------------

   function Get_Attribute_Node (Elem : Element; Name : DOM_String)
      return Attr is
   begin
      return Get_Named_Item (Elem.Attributes, Name);
   end Get_Attribute_Node;

   ---------------------------
   -- Get_Attribute_Node_Ns --
   ---------------------------

   function Get_Attribute_Node_NS
     (Elem : Element; Namespace_URI : DOM_String; Local_Name : DOM_String)
      return Attr is
   begin
      return Get_Named_Item_NS (Elem.Attributes, Namespace_URI, Local_Name);
   end Get_Attribute_Node_NS;

   ------------------------
   -- Set_Attribute_Node --
   ------------------------

   function Set_Attribute_Node (Elem : Element; New_Attr : Attr) return Attr is
   begin
      if Owner_Element (New_Attr) /= null then
         raise Inuse_Attribute_Err;
      end if;
      Set_Named_Item (Elem.Attributes, New_Attr);
      return New_Attr;
   end Set_Attribute_Node;

   ---------------------------
   -- Set_Attribute_Node_NS --
   ---------------------------

   function Set_Attribute_Node_NS (Elem : Element; New_Attr : Attr)
      return Attr is
   begin
      if Owner_Element (New_Attr) /= null then
         raise Inuse_Attribute_Err;
      end if;
      Set_Named_Item_NS (Elem.Attributes, New_Attr);
      return New_Attr;
   end Set_Attribute_Node_NS;

   ---------------------------
   -- Remove_Attribute_Node --
   ---------------------------

   function Remove_Attribute_Node (Elem : Element; Old_Attr : Attr)
      return Attr is
   begin
      pragma Assert (Owner_Element (Old_Attr) = Elem);
      Remove_Named_Item (Elem.Attributes, Old_Attr);
      return Old_Attr;
   end Remove_Attribute_Node;

   ------------------------------
   -- Remove_Attribute_Node_NS --
   ------------------------------

   function Remove_Attribute_Node_NS (Elem : Element; Old_Attr : Attr)
      return Attr renames Remove_Attribute_Node;

   ------------------------------
   -- Get_Elements_By_Tag_Name --
   ------------------------------

   function Get_Elements_By_Tag_Name (Elem : Element; Name : DOM_String := "*")
      return Node_List
   is
      procedure Get_Elements_From_Node (N : Node; List : in out Node_List);
      --  Depth search in N or its children/sibling for matching children.

      ----------------------------
      -- Get_Elements_From_Node --
      ----------------------------

      procedure Get_Elements_From_Node (N : Node; List : in out Node_List) is
         L : constant Node_List := Child_Nodes (N);
      begin
         if N.Node_Type = Element_Node
           and then (Name = "*" or else Node_Name (N) = Name)
         then
            Append (List, N);
         end if;

         for J in 0 .. L.Last loop
            Get_Elements_From_Node (L.Items (J), List);
         end loop;
      end Get_Elements_From_Node;

      L : Node_List;
   begin
      Get_Elements_From_Node (Elem, L);
      return L;
   end Get_Elements_By_Tag_Name;

   ---------------------------------
   -- Get_Elements_By_Tag_Name_NS --
   ---------------------------------

   function Get_Elements_By_Tag_Name_NS
     (Elem : Element;
      Namespace_URI : DOM_String := "*";
      Local_Name : DOM_String := "*")
      return Node_List
   is
      procedure Get_Elements_From_Node (N : Node; List : in out Node_List);
      --  Depth search in N or its children/sibling for matching children.

      ----------------------------
      -- Get_Elements_From_Node --
      ----------------------------

      procedure Get_Elements_From_Node (N : Node; List : in out Node_List) is
         L : constant Node_List := Child_Nodes (N);
      begin
         if N.Node_Type = Element_Node
           and then (Namespace_URI = "*"
                     or else DOM.Core.Nodes.Namespace_URI (N) = Namespace_URI)
           and then (Local_Name = "*"
                     or else DOM.Core.Nodes.Local_Name (N) = Local_Name)
         then
            Append (List, N);
         end if;

         for J in 0 .. L.Last loop
            Get_Elements_From_Node (L.Items (J), List);
         end loop;
      end Get_Elements_From_Node;

      L : Node_List;
   begin
      Get_Elements_From_Node (Elem, L);
      return L;
   end Get_Elements_By_Tag_Name_NS;

end DOM.Core.Elements;
