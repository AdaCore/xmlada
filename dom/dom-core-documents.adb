------------------------------------------------------------------------------
--                     XML/Ada - An XML suite for Ada95                     --
--                                                                          --
--                     Copyright (C) 2001-2017, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

with DOM.Core.Nodes;            use DOM.Core.Nodes;
with DOM.Core.Elements;         use DOM.Core.Elements;
with Sax.Symbols;               use Sax.Symbols;
with Sax.Utils;                 use Sax.Utils;

package body DOM.Core.Documents is
   use Nodes_Htable, Symbol_Table_Pointers;

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
      return Element is
   begin
      --  ??? Test for Invalid_Character_Err
      --  ??? Must convert Tag_Name to uppercase for HTML documents
      return new Node_Record'
        (Node_Type  => Element_Node,
         Parent          => Doc,
         Parent_Is_Owner => True,
         Name       => From_Qualified_Name
           (Doc, Doc.Symbols, Find (Doc.Symbols, Tag_Name)),
         Children   => Null_List,
         Attributes => Null_Node_Map);
   end Create_Element;

   -----------------------
   -- Create_Element_NS --
   -----------------------

   function Create_Element_NS
     (Doc : Document;
      Namespace_URI : DOM_String;
      Qualified_Name : DOM_String) return Element is
   begin
      return new Node_Record'
        (Node_Type  => Element_Node,
         Parent          => Doc,
         Parent_Is_Owner => True,
         Name       => From_Qualified_Name
           (Doc,
            Doc.Symbols,
            Find (Doc.Symbols, Qualified_Name),
            Find (Doc.Symbols, Namespace_URI)),
         Children   => Null_List,
         Attributes => Null_Node_Map);
   end Create_Element_NS;

   -----------------------
   -- Create_Element_NS --
   -----------------------

   function Create_Element_NS
     (Doc            : Document;
      Symbols        : Sax.Utils.Symbol_Table;
      Namespace_URI  : Sax.Symbols.Symbol;
      Prefix         : Sax.Symbols.Symbol;
      Local_Name     : Sax.Symbols.Symbol) return Element
   is
      Name : Node_Name_Def;
   begin
      if Symbols = Doc.Symbols then
         Name := (Local_Name => Local_Name,
                  Prefix     => Prefix,
                  Namespace  => Namespace_URI);
      else
         Name := (Local_Name => Convert (Doc.Symbols, Local_Name),
                  Prefix     => Convert (Doc.Symbols, Prefix),
                  Namespace  => Convert (Doc.Symbols, Namespace_URI));
      end if;

      return new Node_Record'
        (Node_Type       => Element_Node,
         Parent          => Doc,
         Parent_Is_Owner => True,
         Name            => Name,
         Children        => Null_List,
         Attributes      => Null_Node_Map);
   end Create_Element_NS;

   ------------------------------
   -- Create_Document_Fragment --
   ------------------------------

   function Create_Document_Fragment (Doc : Document) return Document_Fragment
   is
   begin
      return new Node_Record'
        (Node_Type         => Document_Fragment_Node,
         Parent          => Doc,
         Parent_Is_Owner => True,
         Doc_Frag_Children => Null_List);
   end Create_Document_Fragment;

   ----------------------
   -- Create_Text_Node --
   ----------------------

   function Create_Text_Node (Doc : Document; Data : DOM_String)
      return Text is
   begin
      return new Node_Record'
        (Node_Type       => Text_Node,
         Parent          => Doc,
         Parent_Is_Owner => True,
         Text            => new DOM_String'(Data));
   end Create_Text_Node;

   function Create_Text_Node (Doc : Document; Data : DOM_String_Access)
      return Text is
   begin
      return new Node_Record'
        (Node_Type       => Text_Node,
         Parent          => Doc,
         Parent_Is_Owner => True,
         Text            => Data);
   end Create_Text_Node;

   --------------------
   -- Create_Comment --
   --------------------

   function Create_Comment (Doc : Document; Data : DOM_String)
      return Comment is
   begin
      return new Node_Record'
        (Node_Type => Comment_Node,
         Parent          => Doc,
         Parent_Is_Owner => True,
         Comment   => new DOM_String'(Data));
   end Create_Comment;

   --------------------------
   -- Create_Cdata_Section --
   --------------------------

   function Create_Cdata_Section (Doc : Document; Data : DOM_String)
      return Cdata_Section is
   begin
      --  ??? Must raise Not_Supported_Err for HTML documents
      return new Node_Record'
        (Node_Type => Cdata_Section_Node,
         Parent          => Doc,
         Parent_Is_Owner => True,
         Cdata     => new DOM_String'(Data));
   end Create_Cdata_Section;

   -----------------------------------
   -- Create_Processing_Instruction --
   -----------------------------------

   function Create_Processing_Instruction
     (Doc : Document; Target : DOM_String; Data : DOM_String)
      return Processing_Instruction is
   begin
      --  ??? Test for Invalid_Character_Err
      --  ??? Must raise Not_Supported_Err for HTML documents
      return new Node_Record'
        (Node_Type       => Processing_Instruction_Node,
         Parent          => Doc,
         Parent_Is_Owner => True,
         Target          => Find
           (Symbol_Table_Pointers.Get (Doc.Symbols), Target),
         Pi_Data         => Find
           (Symbol_Table_Pointers.Get (Doc.Symbols), Data));
   end Create_Processing_Instruction;

   ----------------------
   -- Create_Attribute --
   ----------------------

   function Create_Attribute (Doc : Document; Name : DOM_String)
      return Attr is
   begin
      --  ??? Test for Invalid_Character_Err
      return new Node_Record'
        (Node_Type       => Attribute_Node,
         Parent          => Doc,
         Parent_Is_Owner => True,
         Specified       => False,
         Owner_Element   => Doc,
         Is_Id           => False,
         Attr_Name       => From_Qualified_Name
           (Doc, Doc.Symbols, Find (Doc.Symbols, Name)),
         Attr_Value      => No_Symbol);
   end Create_Attribute;

   -------------------------
   -- Create_Attribute_NS --
   -------------------------

   function Create_Attribute_NS
     (Doc : Document;
      Namespace_URI : DOM_String;
      Qualified_Name : DOM_String) return Attr is
   begin
      return new Node_Record'
        (Node_Type       => Attribute_Node,
         Parent          => Doc,
         Parent_Is_Owner => True,
         Specified       => False,
         Owner_Element   => Doc,
         Is_Id           => False,
         Attr_Name       => From_Qualified_Name
           (Doc, Doc.Symbols,
            Find (Doc.Symbols, Qualified_Name),
            Find (Doc.Symbols, Namespace_URI)),
         Attr_Value      => No_Symbol);
   end Create_Attribute_NS;

   -------------------------
   -- Create_Attribute_NS --
   -------------------------

   function Create_Attribute_NS
     (Doc           : Document;
      Symbols       : Symbol_Table;
      Namespace_URI : Sax.Symbols.Symbol;
      Prefix        : Sax.Symbols.Symbol;
      Local_Name    : Sax.Symbols.Symbol) return Attr
   is
      Name : Node_Name_Def;
   begin
      if Symbols = Doc.Symbols then
         Name := (Local_Name => Local_Name,
                  Namespace  => Namespace_URI,
                  Prefix     => Prefix);
      else
         Name := (Local_Name => Convert (Doc.Symbols, Local_Name),
                  Namespace  => Convert (Doc.Symbols, Namespace_URI),
                  Prefix     => Convert (Doc.Symbols, Prefix));
      end if;

      return new Node_Record'
        (Node_Type       => Attribute_Node,
         Parent          => Doc,
         Parent_Is_Owner => True,
         Specified       => False,
         Owner_Element   => Doc,
         Is_Id           => False,
         Attr_Name       => Name,
         Attr_Value      => No_Symbol);
   end Create_Attribute_NS;

   -----------------------------
   -- Create_Entity_Reference --
   -----------------------------

   function Create_Entity_Reference (Doc : Document; Name : DOM_String)
      return Entity_Reference is
   begin
      --  ??? Test for Invalid_Character_Err
      --  ??? Must raise Not_Supported_Err for HTML documents
      --  ??? Must test if entity is already known
      return new Node_Record'
        (Node_Type => Entity_Reference_Node,
         Parent          => Doc,
         Parent_Is_Owner => True,
         Entity_Reference_Name => Find (Doc.Symbols, Name));
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

   ----------------
   -- Adopt_Node --
   ----------------

   function Adopt_Node (Doc : Document; Source : Node) return Node is
      Old_Doc : constant Document := Owner_Document (Source);

      procedure Copy (S : in out Symbol);
      procedure Copy (S : in out Node_Name_Def);
      --  Duplicate the symbol into the new document

      procedure Recurse (Parent : Node; N : Node);
      --  Adopt node recursively.
      --  Parent is the new parent for the node

      procedure Copy (S : in out Symbol) is
      begin
         if S /= Sax.Symbols.No_Symbol then
            S := Find (Doc.Symbols, Get (S).all);
         end if;
      end Copy;

      procedure Copy (S : in out Node_Name_Def) is
      begin
         Copy (S.Prefix);
         Copy (S.Local_Name);
         Copy (S.Namespace);
      end Copy;

      procedure Recurse (Parent : Node; N : Node) is
         Dest  : Integer;
         Dummy : Attr;
         pragma Unreferenced (Dummy);
      begin
         case N.Node_Type is
            when Document_Node | Document_Type_Node =>
               raise Not_Supported_Err with
                  "Cannot adopt a document or document type node";

            when Attribute_Node =>
               if N.Is_Id then
                  Document_Remove_Id (Old_Doc, N.Attr_Value);
                  Copy (N.Attr_Value);

                  if Parent /= null then
                     Document_Add_Id (Doc, N.Attr_Value, Parent);
                  end if;
               else
                  Copy (N.Attr_Value);
               end if;

               if Parent = null then
                  Dummy := Remove_Attribute_Node
                     (Element (N.Owner_Element), Attr (N));
               end if;

               Copy (N.Attr_Name);

            when Document_Fragment_Node =>
               if N.Doc_Frag_Children.Items /= null then
                  for J in
                    N.Doc_Frag_Children.Items'First .. N.Doc_Frag_Children.Last
                  loop
                     Recurse (N, N.Doc_Frag_Children.Items (J));
                  end loop;
               end if;

            when Element_Node =>
               Copy (N.Name);

               --  Default attributes must be discarded

               if N.Attributes.Items /= null then
                  Dest := N.Attributes.Items'First - 1;
                  for A in N.Attributes.Items'First .. N.Attributes.Last loop
                     if N.Attributes.Items (A).Specified then
                        Dest := Dest + 1;
                        Recurse (N, N.Attributes.Items (A));
                        if A /= Dest then
                           N.Attributes.Items (Dest) := N.Attributes.Items (A);
                           N.Attributes.Items (A) := null;
                        end if;
                     end if;
                  end loop;
                  N.Attributes.Last := Dest;
               end if;

               if N.Children.Items /= null then
                  for A in N.Children.Items'First .. N.Children.Last loop
                     Recurse (N, N.Children.Items (A));
                  end loop;
               end if;

            when Entity_Node =>
               raise Not_Supported_Err with "Cannot adopt an entity node";

            when Notation_Node =>
               raise Not_Supported_Err with "Cannot adopt a notation node";

            when Entity_Reference_Node =>
               Copy (N.Entity_Reference_Name);

            when Processing_Instruction_Node =>
               Copy (N.Target);
               Copy (N.Pi_Data);

            when Text_Node =>
               null;  --  nothing to do

            when Cdata_Section_Node =>
               null;  --  nothing to do

            when Comment_Node =>
               null;  --  nothing to do
         end case;
      end Recurse;

   begin
      --  ??? Should raise No_Modification_Allowed_Err if source is readonly

      Recurse (null, Source);
      Source.Parent_Is_Owner := True;
      Source.Parent := Node (Doc);
      return Source;
   end Adopt_Node;

   -----------------
   -- Import_Node --
   -----------------

   function Import_Node
      (Doc : Document; Imported_Node : Node; Deep : Boolean := True)
      return Node is
   begin
      return Adopt_Node (Doc, Clone_Node (Imported_Node, Deep));
   end Import_Node;

   -----------------------
   -- Get_Element_By_Id --
   -----------------------

   function Get_Element_By_Id
     (Doc : Document; Element_Id : DOM_String) return Node
   is
      N : Symbol;
   begin
      if Doc.Ids = null then
         return null;
      else
         N := Find (Doc.Symbols, Element_Id);
         return Get (Doc.Ids.all, N).N;
      end if;
   end Get_Element_By_Id;

end DOM.Core.Documents;
