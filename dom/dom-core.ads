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

--  This package is the root hierarchy for the Core part of the DOM
--  interface.
--  It is in fact made of several subpackages, since DOM provides
--  two views of the tree: Object-oriented throught the Element, Document,...
--  types; and direct access through the Node interface.

with Unicode.CES;
with Unchecked_Deallocation;

package DOM.Core is

   subtype DOM_String is Unicode.CES.Byte_Sequence;
   --  A simple redefinition of the strings, to be compatible with the
   --  standard DOM interface
   --  See the package Encodings for the exact encoding used for DOM_Strings

   subtype DOM_String_Access is Unicode.CES.Byte_Sequence_Access;

   -----------
   -- Nodes --
   -----------
   --  This is the base type for all DOM.Core types. It is declared in this
   --  package for visibility reasons, so that all DOM.Core.* packages have
   --  access to the components.

   type Node_Types is
     (Element_Node,
      Attribute_Node,
      Cdata_Section_Node,
      Entity_Reference_Node,
      Entity_Node,
      Processing_Instruction_Node,
      Text_Node,
      Comment_Node,
      Document_Node,
      Document_Type_Node,
      Document_Fragment_Node,
      Notation_Node);

   subtype Character_Data_Types is Node_Types range Text_Node .. Comment_Node;

   type Node_Record (Node_Type : Node_Types) is private;
   type Node is access Node_Record;

   subtype Character_Data is Node;
   subtype Element is Node (Element_Node);
   subtype Attr is Node (Attribute_Node);
   subtype Cdata_Section is Character_Data (Cdata_Section_Node);
   subtype Entity_Reference is Node (Entity_Reference_Node);
   subtype Entity is Node (Entity_Node);
   subtype Processing_Instruction is Node (Processing_Instruction_Node);
   subtype Text is Character_Data (Text_Node);
   subtype Comment is Character_Data (Comment_Node);
   subtype Document is Node (Document_Node);
   subtype Document_Type is Node (Document_Type_Node);
   subtype Document_Fragment is Node (Document_Fragment_Node);
   subtype Notation is Node (Notation_Node);

   type Node_List is private;
   --  A simple ordered list of nodes

   type Named_Node_Map is private;
   --  A collection of nodes accessible by their names.
   --  This is unordered.

   procedure Free (List : in out Node_List);
   --  Free the memory occupied by the list. The items contained in the list
   --  are not freed, since they still exist in the XML tree.

   ------------------------
   -- Dom implementation --
   ------------------------
   --  This provides a number of methods for performing operations that are
   --  independent of any particular instance of the document object model.

   type DOM_Implementation is private;
   --  There are multiple implementations of DOM.
   --  They can be specialized for some special cases (HTML, Stylesheets,...)

   function Has_Feature
     (Implementation : DOM_Implementation;
      Feature        : DOM_String;
      Version        : String := "2.0") return Boolean;
   --  Return TRUE if this implementation of DOM has the Feature.

   function Create_Document
     (Implementation : DOM_Implementation;
      NameSpace_URI  : DOM_String := "";
      Qualified_Name : DOM_String := "";
      Doc_Type       : Node := null) return Node;
   --  Create an new document with its element.
   --  Note that NameSpace_URI can be the empty string if you do not want
   --  to use namespaces.
   --  The Document Type Definition can be null if there is none associated
   --  with the document.
   --  Wrong_Document_Err is raised if Doc_Type has already been used for
   --  another document.

   --------------------
   -- Dom exceptions --
   --------------------
   --  The following exceptions are declared in the DOM interface. If we
   --  were to follow exactly the interface, we should a single exception to
   --  which we associate an integer code. It seems easier to provide one
   --  exception for each case. However, we kept the standard names.

   Index_Size_Err : exception;
   --  If Index or size is invalid (negative or greated than max value).

   Domstring_Size_Err : exception;
   --  If the specified range of text does not fit into a DomString.

   Hierarchy_Request_Err : exception;
   --  If a node is inserted somewhere it doesn't belong.

   Wrong_Document_Err : exception;
   --  If a node is used with a document other than its own.

   Invalid_Character_Err : exception;
   --  If an invalid character is used, for instance in a name.

   No_Data_Allowed_Err : exception;
   --  If data is specified for a node that doesn't support data.

   No_Modification_Allowed_Err : exception;
   --  If an attempt is made to modify a read-only object.

   Not_Found_Err : exception;
   --  If an attempt is made to reference a node in a concept where it doesn't
   --  exist.

   Not_Supported_Err : exception;
   --  If the implementation does not support the type of object requested.

   Inuse_Attribute_Err : exception;
   --  If an attempt is made to add an attribute that is already used.

   Invalid_State_Err : exception;
   --  If an attempt is made to use an object that is not or no longer
   --  available.

   Syntax_Err : exception;
   --  If an invalid string is specified.

   Invalid_Modification_Err : exception;
   --  If an attempt is made to modify the type of the underlying object.

   Namespace_Err : exception;
   --  If an attempt is made to create or modify an object in a way
   --  incompatible with the namespace.

   Invalid_Access_Err : exception;
   --  If a parameter or an operation is not supported by the underlying
   --  object.

private
   type DOM_Implementation is null record;

   type Node_Array is array (Natural range <>) of Node;
   type Node_Array_Access is access Node_Array;

   procedure Free is new Unchecked_Deallocation
     (Node_Array, Node_Array_Access);

   type Node_List is record
      Items  : Node_Array_Access := null;
      Last   : Integer := -1;
   end record;

   Null_List : constant Node_List := (null, -1);

   --  Not the most efficient way to implement a hash-table, but these are
   --  generally short lists anyway (attributes,...)
   type Named_Node_Map is new Node_List;

   Null_Node_Map : constant Named_Node_Map := (null, -1);

   type Node_Record (Node_Type : Node_Types) is record
      Parent   : Node;
      case Node_Type is
         when Element_Node =>
            Prefix     : DOM_String_Access;
            Local_Name : DOM_String_Access;
            Namespace  : DOM_String_Access;
            Children   : Node_List;
            Attributes : Named_Node_Map;

         when Attribute_Node =>
            Attr_Prefix     : DOM_String_Access;
            Attr_Local_Name : DOM_String_Access;
            Attr_Value      : DOM_String_Access;
            Attr_Namespace  : DOM_String_Access;
            Specified       : Boolean := False;
            --   ??? In fact, attributes can have children (text or
            --   entity_reference).

         when Text_Node =>
            Text : DOM_String_Access;

         when Cdata_Section_Node =>
            Cdata : DOM_String_Access;

         when Entity_Reference_Node =>
            Entity_Reference_Name : DOM_String_Access;

         when Entity_Node =>
            Entity_Name : DOM_String_Access;
            --  ??? Allows children for the substitution of the entity

         when Processing_Instruction_Node =>
            Target  : DOM_String_Access;
            Pi_Data : DOM_String_Access;

         when Comment_Node =>
            Comment : DOM_String_Access;

         when Document_Node =>
            Doc_Children   : Node_List;
            Doc_Type       : Node;
            Implementation : DOM_Implementation;

         when Document_Type_Node =>
            Document_Type_Name : DOM_String_Access;
            Doc_Type_Children  : Node_List;

         when Document_Fragment_Node =>
            Doc_Frag_Children : Node_List;

         when Notation_Node =>
            Public_ID : DOM_String_Access;
            System_ID : DOM_String_Access;
      end case;
   end record;

   procedure Append (List : in out Node_List; N : Node);
   --  Insert N as the last element in List

   procedure Remove (List : in out Node_List; N : Node);
   --  Remove N from the list
   --  N must be an element of List, this is not checked.

end DOM.Core;
