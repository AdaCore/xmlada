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

with DOM.Core.Nodes;   use DOM.Core.Nodes;

package DOM.Core.Elements is

   function Get_Tag_Name (Elem : Element) return DOM_String
      renames DOM.Core.Nodes.Node_Value;
   --  Return the tag of the element

   --------------------------
   -- Attributes by string --
   --------------------------

   function Get_Attribute (Elem : Element; Name : DOM_String)
      return DOM_String;
   --  Return the value of a specific attribute, or the empty string if the
   --  attribute is unknown and doesn't have a default value.
   --  Use Get_Attribute_NS for a namespace-compatible version.

   function Get_Attribute_NS
     (Elem : Element; Namespace_URI : DOM_String; Local_Name : DOM_String)
      return DOM_String;
   --  Like Get_Attribute but provides namespace support

   procedure Set_Attribute
     (Elem : Element; Name : DOM_String; Value : DOM_String);
   --  Set the value of a specific attribute. The attribute is created if it
   --  doesn't exist yet.
   --  Note that Value is not parsed, thus any entity markup will not be
   --  expanded. Use Set_Attribute_Node if you need to create an attribute
   --  that contains an entity reference.
   --  Use Set_Attribute_NS for a namespace-compatible version.
   --  Invalid_Character_Err raised if Name contains an illegal character.

   procedure Set_Attribute_NS
     (Elem : Element;
      Namespace_URI : DOM_String;
      Qualified_Name : DOM_String;
      Value : DOM_String);
   --  Like Set_Attribute, but provides namespace support

   procedure Remove_Attribute (Elem : Element; Name : DOM_String);
   --  Remove an attribute by name. If there is a defaul value for that
   --  attribute, it is reset.
   --  Use Remove_Attribute_NS for a namespace-compatible version.

   procedure Remove_Attribute_NS
     (Elem : Element; Namespace_URI : DOM_String; Local_Name : DOM_String);
   --  Like Remove_Attribute, but provides namespace support

   ------------------------
   -- Attributes by node --
   ------------------------

   function Get_Attribute_Node (Elem : Element; Name : DOM_String) return Attr;
   --  Return the attribute node for a specific name

   function Get_Attribute_Node_NS
     (Elem : Element; Namespace_URI : DOM_String; Local_Name : DOM_String)
      return Attr;
   --  Like Get_Attribute_Node but provides namespace support

   function Set_Attribute_Node (Elem : Element; New_Attr : Attr) return Attr;
   --  Set or override a given attribute.
   --  Inuse_Attribute_Err raised if New_Attr belongs to another element

   function Set_Attribute_Node_NS (Elem : Element; New_Attr : Attr)
      return Attr;
   --  Like Set_Attribute, but provides namespace support

   function Remove_Attribute_Node (Elem : Element; Old_Attr : Attr)
      return Attr;
   --  Remove an attribute, and return it.
   --  If Old_Attr doesn't belong to Elem, null is returned.

   function Remove_Attribute_Node_NS (Elem : Element; Old_Attr : Attr)
      return Attr;
   --  Like Remove_Attribute, but provides namespace support

   --------------
   -- Elements --
   --------------

   function Get_Elements_By_Tag_Name (Elem : Element; Name : DOM_String := "*")
      return Node_List;
   --  Returns a NodeList of all descendant elements with a given tag name,
   --  in the order in which they would be encountered in a preorder traversal
   --  of the Element tree.
   --  The special value "*" matches all tags

   function Get_Elements_By_Tag_Name_NS
     (Elem : Element;
      Namespace_URI : DOM_String := "*";
      Local_Name : DOM_String := "*")
      return Node_List;
   --  Same as Get_Elements_By_Tag_Name, but provides namespacesupport.
   --  "*" matches all namespaces or all local_names.

end DOM.Core.Elements;
