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

with Sax.Encodings;      use Sax.Encodings;
with Unicode.CES;        use Unicode.CES;
with DOM.Core.Documents; use DOM.Core.Documents;
with DOM.Core.Nodes;     use DOM.Core.Nodes;

package body DOM.Core.Texts is

   ----------------
   -- Split_Text --
   ----------------

   function Split_Text (Te : Text; Offset : Natural) return Text is
      Index : constant Integer := Index_From_Offset
        (Te.Text.all, Offset, Encoding);
      N : Node;
   begin
      if Index < 0 then
         raise Index_Size_Err;
      end if;

      N := Insert_Before
        (Parent_Node (Te),
         Create_Text_Node
           (Owner_Document (Te), Te.Text.all (Index .. Te.Text'Last)),
         Next_Sibling (Te));
      Set_Node_Value (Te, Te.Text (Te.Text'First .. Index - 1));
      return N;
   end Split_Text;

end DOM.Core.Texts;
