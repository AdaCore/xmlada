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

--  A specific instantiation of a state machine, that transitions on
--  Unicode characters, with special handling for white spaces.

with Generic_State_Machine;
with Unicode;
with Unicode.Names.Basic_Latin;

package Char_Automaton is

   Any_Char : constant Unicode.Unicode_Char := Unicode.Names.Basic_Latin.Nul;
   --  The type of characters that can be read from the input stream.
   --  When defining transitions:
   --    If Event is Any_Char, the transition will be done for any
   --    character seen on the input.
   --    If Event is ' ', the transition will be done also on tabs,
   --    line feeds and carriage returns if the automaton has been configured
   --    with Set_Expanded_Spaces;

   function Is_Any_Event (C : Unicode.Unicode_Char) return Boolean;
   --  Return True if a transition based on C is activated for any type of
   --  character, ie if C is equal to Any_Char.

   function Equal (Event, Transition : Unicode.Unicode_Char) return Boolean;
   --  Return True if two characters are the same.
   --  This handles the spaces, ie if the transition is based on
   --  Unicode.Names.Basic_Latin.Space, and Event matches Is_White_Space.

   package Character_Automaton is new
     Generic_State_Machine (Unicode.Unicode_Char, Is_Any_Event, Equal);
   use Character_Automaton;

   type Character_State_Machine is new Character_Automaton.State_Machine
     with private;
   --  A state machine whose transitions are based on characters.

private
   type Character_State_Machine is new Character_Automaton.State_Machine with
      null record;
end Char_Automaton;
