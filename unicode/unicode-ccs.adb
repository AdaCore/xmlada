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

with Unicode.CCS.Iso_8859_1;
with Unicode.CCS.Iso_8859_2;
with Unicode.CCS.Iso_8859_3;
with Unicode.CCS.Iso_8859_4;

package body Unicode.CCS is

   -----------------------
   -- Get_Character_Set --
   -----------------------

   function Get_Character_Set (Name : String) return Character_Set is
   begin
      if Name = Iso_8859_1.Name1 or else Name = Iso_8859_1.Name2 then
         return Iso_8859_1.Iso_8859_1_Character_Set;
      elsif Name = Iso_8859_2.Name1 or else Name = Iso_8859_2.Name2 then
         return Iso_8859_2.Iso_8859_2_Character_Set;
      elsif Name = Iso_8859_3.Name1 then
         return Iso_8859_3.Iso_8859_3_Character_Set;
      elsif Name = Iso_8859_4.Name1 then
         return Iso_8859_4.Iso_8859_4_Character_Set;
      else
         raise Unknown_Character_Set;
      end if;
   end Get_Character_Set;

   --------------
   -- Identity --
   --------------

   function Identity (Char : Unicode_Char) return Unicode_Char is
   begin
      return Char;
   end Identity;

end Unicode.CCS;
