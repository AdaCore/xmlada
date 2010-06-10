-----------------------------------------------------------------------
--                XML/Ada - An XML suite for Ada95                   --
--                                                                   --
--                Copyright (C) 2006-2010, AdaCore                   --
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
            Namespace_URI  => Get_URI (Atts, J),
            Prefix         => Get_Prefix (Atts, J),
            Local_Name     => Get_Local_Name (Atts, J));
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
      Free (Read.Tree);
      Read.Tree := null;
   end Free;

   ----------------------------
   -- Set_Warnings_As_Errors --
   ----------------------------

   procedure Set_Warnings_As_Errors
     (Read : in out Tree_Reader; Warnings_As_Error : Boolean) is
   begin
      Read.Warnings_As_Error := Warnings_As_Error;
   end Set_Warnings_As_Errors;

end Schema.Dom_Readers;
