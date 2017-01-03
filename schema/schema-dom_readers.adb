------------------------------------------------------------------------------
--                     XML/Ada - An XML suite for Ada95                     --
--                                                                          --
--                     Copyright (C) 2006-2017, AdaCore                     --
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

pragma Ada_05;
with Sax.Attributes;       use Sax.Attributes;
with Sax.Utils;            use Sax.Utils;
with Unicode;              use Unicode;
with Unicode.CES;          use Unicode.CES;
with DOM.Core.Attrs;       use DOM.Core.Attrs;
with DOM.Core.Nodes;       use DOM.Core.Nodes;
with DOM.Core.Documents;   use DOM.Core.Documents;
with DOM.Core.Elements;    use DOM.Core.Elements;
with DOM.Core.Character_Datas; use DOM.Core.Character_Datas;
with Sax.Readers;          use Sax.Readers;

package body Schema.Dom_Readers is

   --------------------
   -- Start_Document --
   --------------------

   procedure Start_Document (Handler : in out Tree_Reader) is
      Implementation : DOM_Implementation;
   begin
      Handler.Tree := Create_Document
        (Implementation, Symbols => Get_Symbol_Table (Handler));
      Handler.Current_Node := Handler.Tree;
   end Start_Document;

   -------------------
   -- Start_Element --
   -------------------

   overriding procedure Start_Element
     (Handler       : in out Tree_Reader;
      NS            : Sax.Utils.XML_NS;
      Local_Name    : Sax.Symbols.Symbol;
      Atts          : Sax.Readers.Sax_Attribute_List)
   is
      Att, Att2 : Attr;
      pragma Warnings (Off, Att2);
   begin
      Handler.Current_Node := Append_Child
        (Handler.Current_Node,
         Create_Element_NS
           (Handler.Tree,
            Symbols       => Get_Symbol_Table (Handler),
            Namespace_URI => Get_URI (NS),
            Prefix        => Get_Prefix (NS),
            Local_Name    => Local_Name));

      --  Insert the attributes in the right order.
      for J in 1 .. Get_Length (Atts) loop
         Att := Create_Attribute_NS
           (Handler.Tree,
            Symbols        => Get_Symbol_Table (Handler),
            Namespace_URI  => Get_Name (Atts, J).NS,
            Prefix         => Get_Prefix (Atts, J),
            Local_Name     => Get_Name (Atts, J).Local);
         Set_Value (Att, Get_Value (Atts, J));
         Att2 := Set_Attribute_Node (Handler.Current_Node, Att);
         if Get_Type (Atts, J) = Id then
            Set_Id_Attribute_Node (Handler.Current_Node, Att, Is_Id => True);
         end if;
      end loop;
   end Start_Element;

   -----------------
   -- End_Element --
   -----------------

   overriding procedure End_Element
     (Handler       : in out Tree_Reader;
      NS            : Sax.Utils.XML_NS;
      Local_Name    : Sax.Symbols.Symbol)
   is
      pragma Warnings (Off, NS);
      pragma Warnings (Off, Local_Name);
   begin
      Handler.Current_Node := Parent_Node (Handler.Current_Node);
   end End_Element;

   ----------------
   -- Characters --
   ----------------

   procedure Characters
     (Handler : in out Tree_Reader; Ch : Unicode.CES.Byte_Sequence)
   is
      pragma Unmodified (Handler);
      Tmp : Node;
      pragma Unreferenced (Tmp);
   begin
      --  If previous child is already a text node, we should just concatenate
      --  the two, as required in DOM specifications (in Text node description)
      if Last_Child (Handler.Current_Node) /= null
        and then Node_Type (Last_Child (Handler.Current_Node)) = Text_Node
      then
         Append_Data (Last_Child (Handler.Current_Node), Ch);
      else
         Tmp := Append_Child
           (Handler.Current_Node, Create_Text_Node (Handler.Tree, Ch));
      end if;
   end Characters;

   -------------
   -- Comment --
   -------------

   procedure Comment
     (Handler : in out Tree_Reader;
      Comment : Unicode.CES.Byte_Sequence)
   is
      pragma Unmodified (Handler);
      Tmp : Node;
      pragma Unreferenced (Tmp);
   begin
      Tmp := Append_Child
        (Handler.Current_Node, Create_Comment (Handler.Tree, Comment));
   end Comment;

   --------------------------
   -- Ignorable_Whitespace --
   --------------------------

   procedure Ignorable_Whitespace
     (Handler : in out Tree_Reader; Ch : Unicode.CES.Byte_Sequence)
   is
      pragma Unmodified (Handler);
      Tmp : Node;
      pragma Unreferenced (Tmp);
   begin
      --  Ignore these white spaces at the toplevel
      if Handler.Current_Node /= Handler.Tree then
         Tmp := Append_Child
           (Handler.Current_Node, Create_Text_Node (Handler.Tree, Ch));
      end if;
   end Ignorable_Whitespace;

   ----------------------------
   -- Processing_Instruction --
   ----------------------------

   procedure Processing_Instruction
     (Handler : in out Tree_Reader;
      Target  : Unicode.CES.Byte_Sequence;
      Data    : Unicode.CES.Byte_Sequence)
   is
      pragma Unmodified (Handler);
      Tmp : Node;
      pragma Unreferenced (Tmp);
   begin
      if not Handler.In_DTD then
         Tmp := Append_Child
           (Handler.Current_Node,
            Create_Processing_Instruction (Handler.Tree, Target, Data));
      end if;
   end Processing_Instruction;

   --------------
   -- Get_Tree --
   --------------

   function Get_Tree (Read : Tree_Reader) return Document is
   begin
      return Read.Tree;
   end Get_Tree;

   ---------------
   -- Start_DTD --
   ---------------

   procedure Start_DTD
     (Handler   : in out Tree_Reader;
      Name      : Unicode.CES.Byte_Sequence;
      Public_Id : Unicode.CES.Byte_Sequence := "";
      System_Id : Unicode.CES.Byte_Sequence := "")
   is
      pragma Warnings (Off, Name);
      pragma Warnings (Off, Public_Id);
      pragma Warnings (Off, System_Id);
   begin
      Handler.In_DTD := True;
   end Start_DTD;

   -------------
   -- End_DTD --
   -------------

   procedure End_DTD (Handler : in out Tree_Reader) is
   begin
      Handler.In_DTD := False;
   end End_DTD;

   -----------
   -- Error --
   -----------

   procedure Error
     (Handler : in out Tree_Reader;
      Except  : Sax.Exceptions.Sax_Parse_Exception'Class) is
   begin
      Fatal_Error (Handler, Except);
   end Error;

   -------------
   -- Warning --
   -------------

   procedure Warning
     (Handler : in out Tree_Reader;
      Except : Sax.Exceptions.Sax_Parse_Exception'Class) is
   begin
      if Handler.Warnings_As_Error then
         Fatal_Error (Handler, Except);
      end if;
   end Warning;

   ----------
   -- Free --
   ----------

   procedure Free (Read : in out Tree_Reader) is
   begin
      --  Do not free the tree, but resets the internal pointer to it.
      --      Free (Read.Tree);
      Read.Tree := null;
      Schema.Readers.Free (Schema.Readers.Validating_Reader (Read));
   end Free;

   ----------------------------
   -- Set_Warnings_As_Errors --
   ----------------------------

   procedure Set_Warnings_As_Errors
     (Read : in out Tree_Reader; Warnings_As_Error : Boolean) is
   begin
      Read.Warnings_As_Error := Warnings_As_Error;
   end Set_Warnings_As_Errors;

   ------------------
   -- Current_Node --
   ------------------

   function Current_Node (Read : Tree_Reader) return Node is
   begin
      return Read.Current_Node;
   end Current_Node;

end Schema.Dom_Readers;
