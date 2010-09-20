-----------------------------------------------------------------------
--                XML/Ada - An XML suite for Ada95                   --
--                                                                   --
--                       Copyright (C) 2010, AdaCore                 --
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

--  This package implements state machines (non-deterministic, aka NFA, and
--  deterministic, aka DFA).

with GNAT.Dynamic_Tables;

generic
   type Symbol is private;
   --  The symbols accepted by the state machine as input.

   type Transition_Symbol is private;
   --  One such symbol might be associated with each transition (although this
   --  is optional) to speed up the processing of the state machine

   with function Match
     (Trans : Transition_Symbol; Input : Symbol) return Boolean;
   --  Whether the two symbols match. In particular this means that the
   --  corresponding transition is valid.
   --  Using the "=" operator might be enough in a lot of cases, but will not
   --  handle the case where the transitions are more general (for instance,
   --  allowing a transition on integers where the symbol is between 1 and 10).

   with function Image (Sym : Transition_Symbol) return String;
   --  Display Sym.

package Sax.State_Machines is

   type State is new Positive;
   --  A state of a state machine

   -----------------------------------------------
   -- Non-deterministic automatons construction --
   -----------------------------------------------

   type NFA is tagged private;
   type NFA_Access is access all NFA'Class;
   --  A non-deterministic automaton

   procedure Initialize (Self : in out NFA);
   --  Initializes a new automaton

   procedure Free (Self : in out NFA);
   procedure Free (Automaton : in out NFA_Access);
   --  Free the memory allocated to [Self]

   function Add_State (Self : access NFA) return State;
   --  Add a new state into the table.

   Start_State : constant State;
   Final_State : constant State;
   --  The start and final states of an automation
   --  There is a single one of each per automaton, but you can of course
   --  connect them, through empty transitions, to any number of states within
   --  [Self], thus making them start states in effect.
   --  These two states always exist.

   procedure Add_Transition
     (Self       : access NFA;
      From       : State;
      To         : State;
      On_Symbol  : Transition_Symbol);
   --  Add a new transition between the two states.
   --  If the symbol given as input to [Self] matches On_Symbol, a transition
   --  will occur from [From_State] to [To_State].
   --  Both states might be equal.
   --  You cannot add transitions from the final state

   procedure Add_Empty_Transition
     (Self : access NFA;
      From : State;
      To   : State);
   --  Indicates that any time [Self] is in [From_State], it should also be
   --  considered as in [To_State]. Both states are basically equivalent.
   --  You cannot add transitions from the final state

   procedure Repeat
     (Self       : access NFA;
      From, To   : State;
      Min_Occurs : Natural := 1;
      Max_Occurs : Positive := 1);
   --  Modify the automaton to repat the subautomaton From_State .. To_State a
   --  specific number of times.
   --  Note that this requires expansion (for instance "e{3,4}" is expanded to
   --  "eeee?", so requires more states), so Max_Occurs should not be too big.
   --
   --  Here is an example of use (equivalent to 'b{2,3}' in traditional regexp)
   --     A := N.Add_State;
   --     B := N.Add_State;
   --     N.Add_Transition (A, B, 'b');
   --     N.Repeat (A, B, 2, 3);
   --  On exit, A and B are still the two ends of the sub-automaton, so you
   --  would still connect to B if you have further states to add. You should
   --  not, however, directly connect from or to any state within A..B (since
   --  they might have been duplicated).
   --
   --  No error is reported if Min_Occurs > Max_Occurs. But nothing is done
   --  either.

   function Dump
     (Self : access NFA'Class; Compact : Boolean := False) return String;
   --  Dump the NFA into a string.
   --  This is mostly for debug reasons, and the output might change from one
   --  version to the next.
   --  If [Compact] is True then the output does not include newlines.

   -------------------------------------------
   -- Non-deterministic automatons matching --
   -------------------------------------------

   type NFA_Matcher is private;
   --  When processing an input, the state machine is left untouched. Instead,
   --  the required information is stored in a separate object, so that
   --  multiple objects can test the same machine in parallel.

   procedure Free (Self : in out NFA_Matcher);
   --  Free the memory allocated for [Self]

   function Start_Match (Self : access NFA) return NFA_Matcher;
   --  Return a matcher which is in [Self]'s initial states.
   --  The matcher holds a reference to [Self], so is only valid while [Self]
   --  is in the scope.

   function In_Final (Self : NFA_Matcher) return Boolean;
   --  Whether [Self] is in the final step: if True, it means that all input
   --  processed so far matches the state machine. It is possible to keep
   --  submitting input

   procedure Process
     (Self    : in out NFA_Matcher;
      Input   : Symbol;
      Success : out Boolean);
   --  Processes one input symbol, and compute the transitions.
   --  [Success] is set to False if the input was invalid, and no transition
   --  could be found for it. In such a case, [Self] is left unmodified.
   --  If [Success] is set to True, a new set of active states was computed,
   --  and at least one state is active.

   function Expected (Self : NFA_Matcher) return String;
   --  Return a textual description of the valid input symbols from the current
   --  state. This should be used for error messages for instance.

   procedure Debug_Print (Self : NFA_Matcher);
   --  Print on stdout some debug information for [Self]

private
   type Transition_Id is new Natural;
   type Transition (Is_Empty : Boolean := True) is record
      To_State       : State;
      Next_For_State : Transition_Id;

      case Is_Empty  is
         when True  => null;
         when False => Sym : Transition_Symbol;
      end case;
   end record;
   No_Transition : constant Transition_Id := 0;

   type State_Data is record
      First_Transition : Transition_Id;
   end record;

   package Transition_Tables is new GNAT.Dynamic_Tables
     (Table_Component_Type => Transition,
      Table_Index_Type     => Transition_Id,
      Table_Low_Bound      => No_Transition + 1,
      Table_Initial        => 100,
      Table_Increment      => 50);
   subtype Transition_Table is Transition_Tables.Instance;

   Start_State : constant State := 1;           --  Exists in NFA.States
   Final_State : constant State := State'Last;  --  Not shown in NFA.States
   No_State    : constant State := Final_State - 1;

   package State_Tables is new GNAT.Dynamic_Tables
     (Table_Component_Type => State_Data,
      Table_Index_Type     => State,
      Table_Low_Bound      => Start_State,
      Table_Initial        => 100,
      Table_Increment      => 50);
   subtype State_Table is State_Tables.Instance;

   type NFA is tagged record
      States      : State_Table;
      Transitions : Transition_Table;
   end record;

   type Boolean_State_Array is array (State range <>) of Boolean;
   pragma Pack (Boolean_State_Array);

   type NFA_Matcher_Record (Last : State) is record
      NFA      : NFA_Access;
      In_Final : Boolean;
      Current  : Boolean_State_Array (State_Tables.First .. Last);
      --  The states we are currently in
   end record;

   type NFA_Matcher is access all NFA_Matcher_Record;

end Sax.State_Machines;
