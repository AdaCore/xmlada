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

with Sax.Attributes;       use Sax.Attributes;
with Unicode;              use Unicode;
with Unicode.CES;          use Unicode.CES;
with DOM.Core.Nodes;       use DOM.Core.Nodes;
with DOM.Core.Documents;   use DOM.Core.Documents;
with DOM.Core.Elements;    use DOM.Core.Elements;

package body DOM.Readers is
   --------------------
   -- Start_Document --
   --------------------

   procedure Start_Document (Handler : in out Tree_Reader) is
      Implementation : DOM_Implementation;
   begin
      Handler.Tree := Create_Document (Implementation);
      Handler.Current_Node := Handler.Tree;
   end Start_Document;

   -------------------
   -- Start_Element --
   -------------------

   procedure Start_Element
     (Handler       : in out Tree_Reader;
      Namespace_URI : Unicode.CES.Byte_Sequence := "";
      Local_Name    : Unicode.CES.Byte_Sequence := "";
      Qname         : Unicode.CES.Byte_Sequence := "";
      Atts          : Sax.Attributes.Attributes'Class)
   is
      pragma Warnings (Off, Local_Name);
   begin
      Handler.Current_Node := Append_Child
        (Handler.Current_Node,
         Create_Element_NS (Handler.Tree,
                            Namespace_URI => Namespace_URI,
                            Qualified_Name => Qname));

      --  Insert the attributes in the right order.
      for J in 0 .. Get_Length (Atts) - 1 loop
         Set_Attribute_NS
           (Handler.Current_Node,
            Get_URI (Atts, J),
            Get_Qname (Atts, J),
            Get_Value (Atts, J));
      end loop;
   end Start_Element;

   -----------------
   -- End_Element --
   -----------------

   procedure End_Element
     (Handler : in out Tree_Reader;
      Namespace_URI : Unicode.CES.Byte_Sequence := "";
      Local_Name    : Unicode.CES.Byte_Sequence := "";
      Qname         : Unicode.CES.Byte_Sequence := "")
   is
      pragma Warnings (Off, Namespace_URI);
      pragma Warnings (Off, Local_Name);
      pragma Warnings (Off, Qname);
   begin
      Handler.Current_Node := Parent_Node (Handler.Current_Node);
   end End_Element;

   ----------------
   -- Characters --
   ----------------

   procedure Characters
     (Handler : in out Tree_Reader; Ch : Unicode.CES.Byte_Sequence)
   is
      Tmp : Node;
   begin
      Tmp := Append_Child
        (Handler.Current_Node, Create_Text_Node (Handler.Tree, Ch));
   end Characters;

   --------------------------
   -- Ignorable_Whitespace --
   --------------------------

   procedure Ignorable_Whitespace
     (Handler : in out Tree_Reader; Ch : Unicode.CES.Byte_Sequence)
   is
      Tmp : Node;
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
      Tmp : Node;
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

end DOM.Readers;
