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

package body DOM.Core is

   ---------------------
   -- Create_Document --
   ---------------------

   function Create_Document
     (Implementation : DOM_Implementation;
      NameSpace_URI  : DOM_String := "";
      Qualified_Name : DOM_String := "";
      Doc_Type       : Node := null) return Node
   is
      pragma Warnings (Off, NameSpace_URI);
      pragma Warnings (Off, Qualified_Name);
   begin
      pragma Assert
        (Doc_Type = null or else Doc_Type.Node_Type = Document_Type_Node);
      return new Node_Record'
        (Node_Type      => Document_Node,
         Parent         => null,
         Doc_Children   => Null_List,
         Doc_Type       => Doc_Type,
         Implementation => Implementation);
   end Create_Document;

   -----------------
   -- Has_Feature --
   -----------------

   function Has_Feature
     (Implementation : DOM_Implementation;
      Feature        : DOM_String;
      Version        : String := "2.0") return Boolean
   is
      pragma Warnings (Off, Implementation);
   begin
      return Feature = "XML" and then Version = "2.0";
   end Has_Feature;

   ------------
   -- Append --
   ------------

   procedure Append (List : in out Node_List; N : Node) is
      Old : Node_Array_Access := List.Items;
   begin
      if Old = null or else Old'Last = List.Last then
         List.Items := new Node_Array (0 .. List.Last + 5);
         if Old /= null then
            List.Items (0 .. List.Last) := Old.all;
            Free (Old);
         end if;
      end if;
      List.Last := List.Last + 1;
      List.Items (List.Last) := N;
   end Append;

   ------------
   -- Remove --
   ------------

   procedure Remove (List : in out Node_List; N : Node) is
   begin
      if List.Items = null or else List.Last = 0 then
         Free (List.Items);
         List.Last := -1;
      else
         for J in 0 .. List.Last loop
            if List.Items (J) = N then
               List.Items (J .. List.Last - 1) :=
                 List.Items (J + 1 .. List.Last);
               List.Last := List.Last - 1;
               return;
            end if;
         end loop;
      end if;
   end Remove;

   ----------
   -- Free --
   ----------

   procedure Free (List : in out Node_List) is
   begin
      Free (List.Items);
   end Free;

end DOM.Core;
