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
with Ada.Unchecked_Deallocation;
with Interfaces;
with Sax.HTable;

package DOM.Core is

   Shared_Strings : constant Boolean := True;
   --  Set this to true if the name of namespaces and elements should be shared
   --  among nodes.
   --  This can result in dramatic memory use reduction (more than 30% is
   --  possible if you have lots of nodes).
   --  Speed difference is very minor: even though we have to do a hash-table
   --  lookup every time, we also save on the number of system calls to
   --  malloc().

   Shared_Node_Names : constant Boolean := True;
   --  Whether the local_name+prefix+namespace are shared between nodes. This
   --  also results in memory usage reduction.

   Node_List_Growth_Rate : constant Natural := 1;
   --  How many empty items are added to a Node_List every time a node is
   --  added to a full list. The higher this rate, the less memory allocations
   --  will be required (and thus the faster your program will run).
   --  Setting this to 1 will require more allocations, but will save memory,
   --  since no empty node will remain in the final tree.

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
   --  A simple ordered list of nodes (see DOM.Core.Nodes for subprograms)

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

   procedure Free is new Ada.Unchecked_Deallocation
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

   ---------------------
   --  Shared_Strings --
   ---------------------
   --  If Shared_Strings is True, then the namespaces and node names are not
   --  duplicated for each node in the tree, This provides significant memory
   --  usage reduction.

   type Shared_String is record
      Str : DOM_String_Access;
   end record;
   --  Use a constrained type, so that we have thin pointers later on to access
   --  the name, this saves 4 bytes per node in the tree.

   type Shared_String_Access is access Shared_String;
   No_String : constant Shared_String_Access := null;

   procedure Force_Free (Str : in out Shared_String_Access);
   --  Free the memory pointed to by Str. Warning: Do not use this if you are
   --  using Shared_Strings, use Free_Unless_Shared instead.

   procedure Free_Unless_Shared (Str : in out Shared_String_Access);
   --  Reset Str to null, and free the memory if needed

   function Get_Key
     (Str : Shared_String_Access) return DOM_String_Access;
   function Hash (Key : DOM_String_Access) return Interfaces.Unsigned_32;
   function Key_Equal (Key1, Key2 : DOM_String_Access) return Boolean;

   package String_Htable is new Sax.HTable
     (Element       => Shared_String_Access,
      Empty_Element => No_String,
      Free          => Force_Free,
      Key           => DOM_String_Access,
      Get_Key       => Get_Key,
      Hash          => Hash,
      Equal         => Key_Equal);
   type String_Htable_Access is access String_Htable.HTable;

   function Internalize_String
     (Doc  : Document; Name : DOM_String) return Shared_String_Access;
   --  Return an internal version of Name, meant to save space in the
   --  DOM tree

   procedure Clone_Shared
     (Dest   : out Shared_String_Access;
      Source : Shared_String_Access);
   pragma Inline (Clone_Shared);
   --  Clone the value of Source into Dest, so that whatever happens to Source,
   --  Dest remains readable.

   ------------------
   -- Nodes htable --
   ------------------

   type Node_String is record
      N   : Node;
      Key : DOM_String_Access;
   end record;
   No_Node_String : constant Node_String := (null, null);

   procedure Free (N : in out Node_String);
   function Get_Key (N : Node_String) return DOM_String_Access;
   pragma Inline (Free, Get_Key);

   package Nodes_Htable is new Sax.HTable
     (Element       => Node_String,
      Empty_Element => No_Node_String,
      Free          => Free,
      Key           => DOM_String_Access,
      Get_Key       => Get_Key,
      Hash          => Hash,
      Equal         => Key_Equal);
   type Nodes_Htable_Access is access Nodes_Htable.HTable;

   -------------------
   -- Node_Name_Def --
   -------------------
   --  Attributes and Elements share the same kind description. These are
   --  grouped in the same type for ease of use

   type Node_Name_Def is record
      Prefix     : Shared_String_Access;
      Local_Name : Shared_String_Access;
      Namespace  : Shared_String_Access;
   end record;
   No_Node_Name : constant Node_Name_Def :=
     (Prefix => null, Local_Name => null, Namespace => null);

   function Qualified_Name (N : Node_Name_Def) return DOM_String;
   pragma Inline (Qualified_Name);
   --  Return the qualified name of N

   function Get_Prefix (N : Node_Name_Def) return DOM_String;
   procedure Set_Prefix
     (Doc : Document; N : in out Node_Name_Def; Prefix : DOM_String);
   pragma Inline (Set_Prefix);
   --  Return or set the prefix of N

   function Get_Local_Name (N : Node_Name_Def) return DOM_String;
   --  Return the local name of N

   function Get_Namespace_URI (N : Node_Name_Def) return DOM_String;
   --  Return the namespace of N

   procedure Clone (Dest : out Node_Name_Def; Source : Node_Name_Def);
   pragma Inline (Clone);
   --  Clone Source

   procedure Force_Free (N : in out Node_Name_Def);
   pragma Inline (Force_Free);
   --  Free the contents of N

   -----------------------
   --  Shared_Node_Name --
   -----------------------
   --  The Node_Name_Def above can be shared to save even more memory.

   type Shared_Node_Name_Def is access Node_Name_Def;

   procedure Force_Free (N : in out Shared_Node_Name_Def);
   --  Free the contents of N. Warning: Do not call this yourself, if the
   --  strings are shared. Call Free_Unless_Shared instead

   procedure Free_Unless_Shared (N : in out Shared_Node_Name_Def);
   pragma Inline (Free_Unless_Shared);
   --  Free N unless we use Shared_Node_Names

   function Self (N : Shared_Node_Name_Def) return Node_Name_Def;
   --  Return N itself

   function Equal (N1, N2 : Node_Name_Def) return Boolean;
   --  Whether N1 and N2 are the same

   function Hash (N : Node_Name_Def) return Interfaces.Unsigned_32;
   --  Return a hash code

   package Node_Name_Htable is new Sax.HTable
     (Element       => Shared_Node_Name_Def,
      Empty_Element => null,
      Free          => Force_Free,
      Key           => Node_Name_Def,
      Get_Key       => Self,
      Hash          => Hash,
      Equal         => Equal);
   type Node_Name_Htable_Access is access Node_Name_Htable.HTable;

   function From_Qualified_Name
     (Doc       : Document;
      Name      : DOM_String;
      Namespace : Shared_String_Access := null) return Shared_Node_Name_Def;
   --  Build a node name from its qualified name. This is shared if
   --  Shared_Node_Names is True

   procedure Clone_Node_Name
     (Dest   : out Shared_Node_Name_Def;
      Source : Shared_Node_Name_Def);
   pragma Inline (Clone_Node_Name);

   -----------------
   -- Node_Record --
   -----------------

   type Node_Record (Node_Type : Node_Types) is record
      Parent_Is_Owner : Boolean;
      --  If False, the Parent node points to the owner document, not to the
      --  real parent in the tree (which is null).
      --  This boolean doesn't increase the size of this record, since because
      --  of alignment issues Node_Type already occupies more space than it
      --  really needs.

      Parent   : Node;
      case Node_Type is
         when Element_Node =>
            Name       : Shared_Node_Name_Def;
            Children   : Node_List;
            Attributes : Named_Node_Map;

         when Attribute_Node =>
            Attr_Name       : Shared_Node_Name_Def;
            Attr_Value      : DOM_String_Access;

            Owner_Element   : Node;
            --  Generally an Element, but it can be a Document if the attribute
            --  hasn't been associated yet.

            Is_Id           : Boolean := False;
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
            Shared_Strings : String_Htable_Access;
            Node_Names     : Node_Name_Htable_Access;
            Doc_Children   : Node_List;
            Doc_Type       : Node;
            Implementation : DOM_Implementation;
            Ids            : Nodes_Htable_Access;

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

   procedure Document_Add_Id
     (Doc  : Document;
      Id   : DOM_String;
      Elem : Element);
   --  Store in the document as fast access to Elem by its ID

   procedure Document_Remove_Id
     (Doc  : Document;
      Id   : DOM_String);
   --  Remove an ID associated with Elem in the fast htable access

end DOM.Core;
