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

package DOM.Core.Documents is

   function Doc_Type (Doc : Document) return Document_Type;
   --  Return the DTD associated with Doc.
   --  This might return null if there is no such DTD.

   function Implementation (Doc : Document) return DOM_Implementation;
   --  Return the DOM_Implementation to which Doc belongs.

   function Get_Element (Doc : Document) return Element;
   --  Return the top-element of DOC

   function Create_Element (Doc : Document; Tag_Name : DOM_String)
      return Element;
   --  Create a new element, and its default attributes.
   --  See also Create_Element_NS
   --  Invalid_Character_Err is raised if Tag_Name contains invalid
   --  characters.

   function Create_Element_NS
     (Doc : Document;
      Namespace_URI : DOM_String;
      Qualified_Name : DOM_String) return Element;
   --  Create a new element, and its default attributes.
   --  Invalid_Character_Err is raised if Tag_Name contains invalid
   --  characters.
   --  Namespace_Err raised if Qualified_Name is incorrect.

   function Create_Document_Fragment (Doc : Document) return Document_Fragment;
   --  Create an empty document fragment;

   function Create_Text_Node (Doc : Document; Data : DOM_String)
      return Text;
   --  Create a text node given a specific string

   function Create_Comment (Doc : Document; Data : DOM_String)
      return Comment;
   --  Create a comment node given a specific string

   function Create_Cdata_Section (Doc : Document; Data : DOM_String)
      return Cdata_Section;
   --  Create a Cdata section for a specific string
   --  Not_Supported_Err is raised for HTML documents

   function Create_Processing_Instruction
     (Doc : Document; Target : DOM_String; Data : DOM_String)
      return Processing_Instruction;
   --  Create a processing instruction.
   --  Invalid_Character_Err raised if Target is invalid.
   --  Not_Supported_Err raised for HTML documents

   function Create_Attribute (Doc : Document; Name : DOM_String)
      return Attr;
   --  Create a new attribute.
   --  Use Set_Attribute to associate it with an element.
   --  See Create_Attribute_NS to create an attribute with a namespace.
   --  Invalid_Character_Err raised if Name is invalid

   function Create_Attribute_NS
     (Doc : Document;
      Namespace_URI : DOM_String;
      Qualified_Name : DOM_String) return Attr;
   --  Create a new attribute.
   --  Use Set_Attribute to associate it with an element.
   --  Invalid_Character_Err raised if Name is invalid
   --  Namespace_Err raised if Qualified_Name is incorrect.

   function Create_Entity_Reference (Doc : Document; Name : DOM_String)
      return Entity_Reference;
   --  Create a new entity reference.
   --  If the referenced entity is known, the child list of the entity
   --  reference is made the same as that of the Entity.
   --  Invalid_Character_Err raised if Target is invalid.
   --  Not_Supported_Err raised for HTML documents

   function Get_Elements_By_Tag_Name
     (Doc : Document; Tag_Name : DOM_String := "*") return Node_List;
   --  Returns a NodeList of all the Elements with a given tag name in the
   --  order in which they would be encountered in a preorder traversal
   --  of the Document tree.
   --  The special case "*" matches all tags

   function Get_Elements_By_Tag_Name_NS
     (Doc : Document;
      Namespace_URI : DOM_String := "*";
      Local_Name : DOM_String := "*") return Node_List;
   --  Returns a NodeList of all the matching Elements.
   --  "*" matches all namespaces or all local names

   function Get_Element_By_Id
     (Doc : Document; Element_Id : DOM_String) return Node;
   --  Return the element whose id is Element_Id. The first matching element
   --  is returned.
   --  The DOM implementation must know which attributes are of type Id, or
   --  null is returned.

   function Import_Node (Doc : Document; Import_Node : Node; Deep : Boolean)
      return Node;
   --  Imports a copy of Import_Node into Doc.
   --  It returns the imported node.
   --  This behaves mostly as if there had been a merge of the two XML
   --  files that contained the document and the imported node, but also takes
   --  into account the possibly different DTDs.

end DOM.Core.Documents;
