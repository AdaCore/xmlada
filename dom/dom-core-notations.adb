-----------------------------------------------------------------------
--                XML/Ada - An XML suite for Ada95                   --
--                                                                   --
--                       Copyright (C) 2001                          --
--                            ACT-Europe                             --
--                       Author: Emmanuel Briot                      --
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
-----------------------------------------------------------------------

with Unicode.CES; use Unicode.CES;

package body DOM.Core.Notations is

   ---------------
   -- Public_Id --
   ---------------

   function Public_Id (N : Notation) return DOM_String is
   begin
      if N.Public_ID = null then
         return "";
      else
         return N.Public_ID.all;
      end if;
   end Public_Id;

   ---------------
   -- System_Id --
   ---------------

   function System_Id (N : Notation) return DOM_String is
   begin
      if N.System_ID = null then
         return "";
      else
         return N.System_ID.all;
      end if;
   end System_Id;
end DOM.Core.Notations;
