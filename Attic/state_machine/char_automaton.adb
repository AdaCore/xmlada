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

with Unicode;                     use Unicode;
with Unicode.Names.Basic_Latin;   use Unicode.Names.Basic_Latin;

package body Char_Automaton is

   ------------------
   -- Is_Any_Event --
   ------------------

   function Is_Any_Event (C : Unicode_Char) return Boolean is
   begin
      return C = Any_Char;
   end Is_Any_Event;

   -----------
   -- Equal --
   -----------

   function Equal (Event, Transition : Unicode_Char) return Boolean is
   begin
      if Transition = Space and then Is_White_Space (Event) then
         return True;
      else
         return Event = Transition;
      end if;
   end Equal;
end Char_Automaton;
