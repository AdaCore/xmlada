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

with DOM.Core.Nodes; use DOM.Core.Nodes;

package DOM.Core.Attrs is
   function Name (Att : Attr) return DOM_String
      renames DOM.Core.Nodes.Node_Name;
   --  Return the name of the node

   function Specified (Att : Attr) return Boolean;
   --  Return True if the attribute was set explicitly in the XML file,
   --  or False if this is the default value for the node

   function Value (Att : Attr) return DOM_String
      renames DOM.Core.Nodes.Node_Value;
   --  Return the value of the attribute

   procedure Set_Value (Att : Attr; Value : DOM_String)
      renames DOM.Core.Nodes.Set_Node_Value;
   --  Set the value of the attribute

   function Owner_Element (Att : Attr) return Element;
   --  return the node to which Att belongs

end DOM.Core.Attrs;
