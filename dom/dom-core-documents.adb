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

with DOM.Core.Nodes;            use DOM.Core.Nodes;
with DOM.Core.Elements;         use DOM.Core.Elements;
with Unicode;                   use Unicode;
with Sax.Encodings;             use Sax.Encodings;
with Unicode.Names.Basic_Latin; use Unicode.Names.Basic_Latin;

package body DOM.Core.Documents is

   --------------
   -- Doc_Type --
   --------------

   function Doc_Type (Doc : Document) return Document_Type is
   begin
      return Doc.Doc_Type;
   end Doc_Type;

   --------------------
   -- Implementation --
   --------------------

   function Implementation (Doc : Document) return DOM_Implementation is
   begin
      return Doc.Implementation;
   end Implementation;

   -----------------
   -- Get_Element --
   -----------------

   function Get_Element (Doc : Document) return Element is
      Child : Node := First_Child (Doc);
   begin
      while Child /= null loop
         if Child.Node_Type = Element_Node then
            return Child;
         end if;
         Child := Next_Sibling (Child);
      end loop;
      return null;
   end Get_Element;

   --------------------
   -- Create_Element --
   --------------------

   function Create_Element (Doc : Document; Tag_Name : DOM_String)
      return Element
   is
      pragma Warnings (Off, Doc);
   begin
      --  ??? Test for Invalid_Character_Err
      --  ??? Must convert Tag_Name to uppercase for HTML documents
      return new Node_Record'
        (Node_Type  => Element_Node,
         Parent     => null,
         Prefix     => null,
         Local_Name => new DOM_String'(Tag_Name),
         Namespace  => null,
         Children   => Null_List,
         Attributes => Null_Node_Map);
   end Create_Element;

   -----------------------
   -- Create_Element_NS --
   -----------------------

   function Create_Element_NS
     (Doc : Document;
      Namespace_URI : DOM_String;
      Qualified_Name : DOM_String) return Element
   is
      pragma Warnings (Off, Doc);
      Colon_Pos : Integer;
      C : Unicode_Char;
      Prefix : DOM_String_Access;
      Local : DOM_String_Access;
      Index : Positive := Qualified_Name'First;
   begin
      --  ??? Test for Invalid_Character_Err
      --  ??? Must convert Tag_Name to uppercase for HTML documents
      --  ??? Test for Namespace_Err

      while Index <= Qualified_Name'Last loop
         Colon_Pos := Index;
         Encoding.Read (Qualified_Name, Index, C);
         exit when C = Colon;
      end loop;

      if C = Colon then
         Prefix := new DOM_String'(Qualified_Name
            (Qualified_Name'First .. Colon_Pos - 1));
         Local := new DOM_String'(Qualified_Name
           (Index .. Qualified_Name'Last));
      else
         Local := new DOM_String'(Qualified_Name);
      end if;

      return new Node_Record'
        (Node_Type  => Element_Node,
         Parent     => null,
         Prefix     => Prefix,
         Local_Name => Local,
         Namespace  => new DOM_String'(Namespace_URI),
         Children   => Null_List,
         Attributes => Null_Node_Map);
   end Create_Element_NS;

   ------------------------------
   -- Create_Document_Fragment --
   ------------------------------

   function Create_Document_Fragment (Doc : Document) return Document_Fragment
   is
      pragma Warnings (Off, Doc);
   begin
      return new Node_Record'
        (Node_Type         => Document_Fragment_Node,
         Parent            => null,
         Doc_Frag_Children => Null_List);
   end Create_Document_Fragment;

   ----------------------
   -- Create_Text_Node --
   ----------------------

   function Create_Text_Node (Doc : Document; Data : DOM_String)
      return Text
   is
      pragma Warnings (Off, Doc);
   begin
      return new Node_Record'
        (Node_Type => Text_Node,
         Parent    => null,
         Text      => new DOM_String'(Data));
   end Create_Text_Node;

   --------------------
   -- Create_Comment --
   --------------------

   function Create_Comment (Doc : Document; Data : DOM_String)
      return Comment
   is
      pragma Warnings (Off, Doc);
   begin
      return new Node_Record'
        (Node_Type => Comment_Node,
         Parent    => null,
         Comment   => new DOM_String'(Data));
   end Create_Comment;

   --------------------------
   -- Create_Cdata_Section --
   --------------------------

   function Create_Cdata_Section (Doc : Document; Data : DOM_String)
      return Cdata_Section
   is
      pragma Warnings (Off, Doc);
   begin
      --  ??? Must raise Not_Supported_Err for HTML documents
      return new Node_Record'
        (Node_Type => Cdata_Section_Node,
         Parent    => null,
         Cdata     => new DOM_String'(Data));
   end Create_Cdata_Section;

   -----------------------------------
   -- Create_Processing_Instruction --
   -----------------------------------

   function Create_Processing_Instruction
     (Doc : Document; Target : DOM_String; Data : DOM_String)
      return Processing_Instruction
   is
      pragma Warnings (Off, Doc);
   begin
      --  ??? Test for Invalid_Character_Err
      --  ??? Must raise Not_Supported_Err for HTML documents
      return new Node_Record'
        (Node_Type => Processing_Instruction_Node,
         Parent    => null,
         Target    => new DOM_String'(Target),
         Pi_Data   => new DOM_String'(Data));
   end Create_Processing_Instruction;

   ----------------------
   -- Create_Attribute --
   ----------------------

   function Create_Attribute (Doc : Document; Name : DOM_String)
      return Attr
   is
      pragma Warnings (Off, Doc);
   begin
      --  ??? Test for Invalid_Character_Err
      return new Node_Record'
        (Node_Type       => Attribute_Node,
         Parent          => null,
         Specified       => False,
         Attr_Prefix     => null,
         Attr_Local_Name => new DOM_String'(Name),
         Attr_Value      => null,
         Attr_Namespace  => null);
   end Create_Attribute;

   -------------------------
   -- Create_Attribute_NS --
   -------------------------

   function Create_Attribute_NS
     (Doc : Document;
      Namespace_URI : DOM_String;
      Qualified_Name : DOM_String) return Attr
   is
      pragma Warnings (Off, Doc);
      Index : Natural := Qualified_Name'First;
      Colon_Pos : Natural;
      C : Unicode_Char;
      Prefix : DOM_String_Access;
      Local : DOM_String_Access;
   begin
      --  ??? Test for Invalid_Character_Err
      --  ??? Must convert Tag_Name to uppercase for HTML documents
      --  ??? Test for Namespace_Err

      while Index <= Qualified_Name'Last loop
         Colon_Pos := Index;
         Encoding.Read (Qualified_Name, Index, C);
         exit when C = Colon;
      end loop;

      if C = Colon then
         Prefix := new DOM_String'(Qualified_Name
            (Qualified_Name'First .. Colon_Pos - 1));
         Local := new DOM_String'(Qualified_Name
           (Index .. Qualified_Name'Last));
      else
         Local := new DOM_String'(Qualified_Name);
      end if;

      return new Node_Record'
        (Node_Type       => Attribute_Node,
         Parent          => null,
         Specified       => False,
         Attr_Prefix     => Prefix,
         Attr_Local_Name => Local,
         Attr_Value      => null,
         Attr_Namespace  => new DOM_String'(Namespace_URI));
   end Create_Attribute_NS;

   -----------------------------
   -- Create_Entity_Reference --
   -----------------------------

   function Create_Entity_Reference (Doc : Document; Name : DOM_String)
      return Entity_Reference
   is
      pragma Warnings (Off, Doc);
   begin
      --  ??? Test for Invalid_Character_Err
      --  ??? Must raise Not_Supported_Err for HTML documents
      --  ??? Must test if entity is already known
      return new Node_Record'
        (Node_Type => Entity_Reference_Node,
         Parent    => null,
         Entity_Reference_Name => new DOM_String'(Name));
   end Create_Entity_Reference;

   ------------------------------
   -- Get_Elements_By_Tag_Name --
   ------------------------------

   function Get_Elements_By_Tag_Name
     (Doc : Document; Tag_Name : DOM_String := "*") return Node_List is
   begin
      return DOM.Core.Elements.Get_Elements_By_Tag_Name
        (Get_Element (Doc), Tag_Name);
   end Get_Elements_By_Tag_Name;

   ---------------------------------
   -- Get_Elements_By_Tag_Name_NS --
   ---------------------------------

   function Get_Elements_By_Tag_Name_NS
     (Doc : Document;
      Namespace_URI : DOM_String := "*";
      Local_Name : DOM_String := "*") return Node_List is
   begin
      return DOM.Core.Elements.Get_Elements_By_Tag_Name_NS
        (Get_Element (Doc), Namespace_URI, Local_Name);
   end Get_Elements_By_Tag_Name_NS;

   -----------------
   -- Import_Node --
   -----------------

   function Import_Node (Doc : Document; Import_Node : Node; Deep : Boolean)
      return Node
   is
      pragma Warnings (Off, Doc);
      N : constant Node := Clone_Node (Import_Node, Deep);
   begin
      pragma Assert (False); --  ??? Unimplemented
      case N.Node_Type is
         when Element_Node =>
            --  ??? Shouldn't import defaulted attribute nodes
            --  ??? Should assign default attributes from Doc
            null;
         when Attribute_Node => null;
         when Text_Node | Cdata_Section_Node | Comment_Node => null;
         when Entity_Reference_Node => null;
         when Entity_Node => null;
         when Processing_Instruction_Node => null;
         when Document_Node => null;
         when Document_Type_Node => null;
         when Document_Fragment_Node => null;
         when Notation_Node => null;
      end case;
      return N;
   end Import_Node;

   -----------------------
   -- Get_Element_By_Id --
   -----------------------

   function Get_Element_By_Id
     (Doc : Document; Element_Id : DOM_String) return Node
   is
      pragma Warnings (Off, Doc);
      pragma Warnings (Off, Element_Id);
   begin
      --  ??? Unimplemented
      return null;
   end Get_Element_By_Id;

end DOM.Core.Documents;
