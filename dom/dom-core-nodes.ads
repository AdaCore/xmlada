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

package DOM.Core.Nodes is

   ----------
   -- Node --
   ----------

   function Node_Name (N : Node) return DOM_String;
   --  Return the name of the tag.
   --  Its meaning depends on the type of the node, see the DOM specifications.

   function Node_Value (N : Node) return DOM_String;
   --  Return the value of the node.
   --  Its meaning depends on the type of the node, see the DOM specifications.

   procedure Set_Node_Value (N : Node; Value : DOM_String);
   --  Change the value of the node.
   --  No_Modification_Allowed_Err is raised when Node is read-only.

   function Child_Nodes (N : Node) return Node_List;
   --  Return the list of children for this node. This could be empty depending
   --  on the type of the node.

   function First_Child (N : Node) return Node;
   --  Return the first child of N.

   function Last_Child (N : Node) return Node;
   --  Return the last child of N.

   function Parent_Node (N : Node) return Node;
   --  Return the parent for this node. Note that for attribute nodes, this
   --  is always null

   function Previous_Sibling (N : Node) return Node;
   --  Return the node preceding N in their common parent.
   --  null is returned if there is no such node.
   --  Note that it is much more efficient to get the list of all children
   --  from the parent and use Item on the list to get each one of them.

   function Next_Sibling (N : Node) return Node;
   --  Return the node following N in their common parent.
   --  null is returned if there is no such node.

   function Attributes (N : Node) return Named_Node_Map;
   --  Return the list of attributes for N.
   --  null is returned, except for Element nodes.

   function Owner_Document (N : Node) return Node;
   --  Return the document to which N belongs.

   function Namespace_URI (N : Node) return DOM_String;
   --  Return the URI associated with N.
   --  This is the URI used when the node was created, and is independent from
   --  the prefix (see below).

   function Prefix (N : Node) return DOM_String;
   --  Return the prefix associated with N (first part of the qualified name)

   procedure Set_Prefix (N : Node; Prefix : DOM_String);
   --  Changing this prefix will affect the qualified name.

   function Local_Name (N : Node) return DOM_String;
   --  Return the local name of N (second part of the qualified name). This is
   --  null if the node was created with a DOM level 1 method (no namespace at
   --  creation time).

   function Insert_Before
     (N         : Node;
      New_Child : Node;
      Ref_Child : Node := null) return Node;
   --  Insert New_Child just before Ref_Child in the list of children for N.
   --  If Ref_Child is null, New_Child is inserted at the end.
   --  If New_Child is a document_fragment, all of its children are inserted.
   --  If New_Child is already in the tree, it is first removed.
   --  raises:
   --    * Hierarchy_Request_Err: N doesn't allow a child of this type, or
   --      New_Child is already an ancestor of N.
   --    * Wrong_Document_Err: if New_Child was created from another document
   --    * No_Modification_Allowed_Err: N or New_Child is read-only.
   --    * Not_Found_Err: Ref_Child is not a child of N.

   function Replace_Child
     (N         : Node;
      New_Child : Node;
      Old_Child : Node) return Node;
   --  Replace Old_Child with New_Child in the list of children of N.
   --  If New_Child is a document fragment, all its children are inserted in
   --  place of Old_Child. Returns the replaced node.
   --  raises:
   --    * Hierarchy_Request_Err: N doesn't allow a child of this type, or
   --      New_Child is already an ancestor of N.
   --    * Wrong_Document_Err: if New_Child was created from another document
   --    * No_Modification_Allowed_Err: N or New_Child is read-only.
   --    * Not_Found_Err: Old_Child is not a child of N.

   function Remove_Child
     (N         : Node;
      Old_Child : Node) return Node;
   --  Remove Old_Child from the list of children of N, and return it.
   --  raises:
   --    * No_Modification_Allowed_Err: N is read-only
   --    * Not_Found_Err: Old_Child is not a child of N

   function Append_Child
     (N         : Node;
      New_Child : Node) return Node;
   --  Append New_Child at the end of the list of children of N, and return
   --  the added node.
   --  raises:
   --    * Hierarchy_Request_Err: N doesn't allow a child of this type, or
   --      New_Child is already an ancestor of N.
   --    * Wrong_Document_Err: if New_Child was created from another document
   --    * No_Modification_Allowed_Err: N or New_Child is read-only.

   function Has_Child_Nodes (N : Node) return Boolean;
   --  True if N has any children, False otherwise

   function Clone_Node (N : Node; Deep : Boolean) return Node;
   --  Returns a duplicate of N.
   --  The duplicate node has no parent.

   procedure Normalize (N : Node);
   --  Make sure there are no adjacent text nodes in the children of N.
   --  This processes the full-depth of the sub-tree underneath N.

   function Supports
     (N : Node;
      Feature : DOM_String;
      Version : DOM_String) return Boolean;
   --  Test whether the DOM implementation implements a specific feature, and
   --  that feature is supported by N.

   ---------------
   -- Node_List --
   ---------------

   function Item (List : Node_List; Index : Natural) return Node;
   --  Return index-nth element in the list (starting from 0)
   --  If Index is greated than or equal to the number of items in the list,
   --  null is returned.

   function Length (List : Node_List) return Natural;
   --  Return the number of elements in the list.

   --------------------
   -- Named_Node_Map --
   --------------------

   function Get_Named_Item
     (Map : Named_Node_Map; Name : DOM_String) return Node;
   --  Retrieve a node specified by name.
   --  null is returned if no such node exists
   --  Consider using Get_Named_Item_NS instead for DOM level 2

   procedure Set_Named_Item
     (Map : in out Named_Node_Map; Arg : Node; Replaces : out Node);
   procedure Set_Named_Item (Map : in out Named_Node_Map; Arg : Node);
   --  Add a node using its Node_Name attribute. Note that you can not have
   --  multiple instances of nodes with special names (#Document, ...).
   --  It returns the node that Arg replaces in Map, or null if it didn't
   --  replace anything.
   --  Consider using Set_Named_Item_NS instead for DOM level 2

   procedure Remove_Named_Item
     (Map : in out Named_Node_Map; Name : DOM_String; Removed : out Node);
   procedure Remove_Named_Item
     (Map : in out Named_Node_Map; Name : DOM_String);
   --  Remove a node from Map, and returns it.
   --  Consider using Remove_Named_Item_NS instead for DOM level 2

   procedure Remove_Named_Item (Map : in out Named_Node_Map; N : Node);
   --  Remove a specific node from the map

   function Item
     (Map : Named_Node_Map; Index : Natural) return Node;
   --  Return the Index-nth node in the list (starting from 0)

   function Length (Map : Named_Node_Map) return Natural;
   --  Return the number of elements in the map.

   function Get_Named_Item_NS
     (Map           : Named_Node_Map;
      Namespace_URI : DOM_String;
      Local_Name    : DOM_String) return Node;
   --  Retrieve a node specified by its (namespace, local_name)

   procedure Set_Named_Item_NS
     (Map : in out Named_Node_Map; Arg : Node; Replaces : out Node);
   procedure Set_Named_Item_NS
     (Map : in out Named_Node_Map; Arg : Node);
   --  Add a node using its namespace and local_name.
   --  It returns the node that Arg replaces (or null if none)

   procedure Remove_Named_Item_NS
     (Map           : in out Named_Node_Map;
      Namespace_URI : DOM_String;
      Local_Name    : DOM_String;
      Removed       : out Node);
   procedure Remove_Named_Item_NS
     (Map           : in out Named_Node_Map;
      Namespace_URI : DOM_String;
      Local_Name    : DOM_String);
   --  Remove a node specified by its namespace and local_name.

   -----------------------
   -- Extra subprograms --
   -----------------------
   --  The following subprograms are not part of the standard DOM interface.
   --  However, they are needed for a full usage of this DOM implementation

   procedure Print
     (N              : Node;
      Print_Comments : Boolean := False;
      Print_XML_PI   : Boolean := False;
      With_URI       : Boolean := False);
   --  Print the contents of Node and its children in XML format.
   --  If Print_Comments is True, then nodes associated with comments are
   --  also displayed.
   --  The <?xml?> processing instruction is displayed only if Print_XML_PI
   --  By default, names are of the form  ns_prefix:local_name. However, if
   --  with_URI is True, names will be  ns_URI:local_name instead

   procedure Print
     (List           : Node_List;
      Print_Comments : Boolean := False;
      Print_XML_PI   : Boolean := False;
      With_URI       : Boolean := False);
   --  Same as Print, byt for all the nodes in the list.

   procedure Free (N : in out Node; Deep : Boolean := True);
   --  This procedure is not part of the DOM standard, but is required to
   --  free the memory used by a node.
   --  Beware that a node is not removed from its parent.
   --  If Deep is True, then the children are also removed

end DOM.Core.Nodes;
