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

--  This package implements a finite-state automaton, with the capacity
--  to send events every time a transition from a state to another.
--  Any kind of event can be used to force transitions.
--
--  States are references by integers,
--  The transition between states is indicated by an array of type
--  Automaton_Def, that defines all the possible transition between each
--  states.
--  It is possible to define one special transition that means: "transition
--  on any event" (see the generic parameter Is_Any_Event).

with Ada.Finalization;

generic
   type Event_Type is private;
   --  The type of events that trigger state changes.
   --  This could for instance be a simple character, when read from a file.

   with function Is_Any_Event (A : Event_Type) return Boolean;
   --  Should return True if a transition associated with A is triggered
   --  for any event. This is the equivalent of the "*" pattern in regular
   --  expressions.

   with function Equal (Event, Transition : Event_Type) return Boolean;
   --  Should return True if two events are the same.
   --  This is used when deciding whether to trigger a state change.

package Generic_State_Machine is

   type State_Machine is tagged limited private;
   --  The finite-state machine itself.

   type Automaton_State is new Integer;
   Exit_Sub_Automaton : constant Automaton_State := -1;
   --  This represents one of the states of the machine.
   --  It is also used to describe the initial and final state of a transition,
   --  and for the latter Exit_Sub_Automaton can be used to force the exit
   --  from one of the sub-automatons.

   Max_Automaton_Depth : constant := 64;
   --  Maximal number of sub-automatons.
   --  Before entering a sub-automaton, the state machine saves the current
   --  state. When a new state of Exit_Sub_Automaton is found, this means to
   --  go back to that saved state.

   type Transition_Function is
     access procedure (Automaton      : in out State_Machine'Class;
                       Event          : Event_Type);
   --  A callback function that can be called after every transition.
   --  The state of the automaton has already been changed when this
   --  function is called.

   type Transition_Def is record
      Initial_State : Automaton_State;
      Event         : Event_Type;
      Final_State   : Automaton_State;
      Func          : Transition_Function;
   end record;
   --  Definition of a transition.  If Final_State is negative, a
   --  sub-automaton is created: the current state is saved, and the new
   --  state becomes -Final_State. Whenever Exit_Sub_Automaton is found, the
   --  automaton exits that sub-automaton and goes back to the state it was
   --  previously in.

   type Automaton_Def is array (Natural range <>) of Transition_Def;
   --  Definition of an automaton.
   --  This should list all the possible transitions that should be defined
   --  for the automaton.

   procedure Compile (Automaton     : in out State_Machine;
                      Highest_State : in Automaton_State;
                      Initial_State : in Automaton_State;
                      Def           : in Automaton_Def);
   --  Compile an automaton with Highest_States possible states.
   --  Note that it is possible to create states without any input or output
   --  transition, that can be defined later.
   --  Transition_Exists is raised if a transition from Initial_State based
   --  on Event is duplicated.
   --  You must always call this function at least once to initialize
   --  internal data. However, you can provide an empty Def array.

   procedure Reset (Automaton : in out State_Machine);
   --  Reclaim all the memory used by the machine.

   procedure Send_Event (Automaton : in out State_Machine;
                         Event     : Event_Type);
   --  Send an event to the machine, and trigger a state transition.
   --  Invalid_Syntax is raised if there is no possible transition from the
   --  current state based on Event.
   --  Subautomaton_Error is raised when attempting to create too many
   --  nested sub-automatons or when trying to exit from no sub-automaton.

   procedure Add_Transition (Automaton     : in out State_Machine;
                             Initial_State : Automaton_State;
                             Event         : Event_Type;
                             Final_State   : Automaton_State;
                             Func          : Transition_Function);
   --  Add a new transition to the machine.
   --  Note that Initial_State and Final_State must be in the range
   --  0 .. Highest_State (as set when first Compiling the machine), or
   --  Invalid_State is raised.
   --  This function can be used when you can not easily put the events in
   --  the Automaton_Def array (for instance with string pointers).

   procedure Set_Current_State (Automaton : in out State_Machine;
                                State_Num : Automaton_State);
   --  Change the current state of the automaton.
   --  This function handles sub-automatons: if State_Num is negative, a new
   --  sub-automaton is created, that is exited only when State_Num is
   --  Exit_Sub_Automaton.

   function Get_Current_State (Automaton : State_Machine)
      return Automaton_State;
   --  Return the current state of the automaton

   Transition_Exists  : exception;
   Subautomaton_Error : exception;
   Invalid_Syntax     : exception;
   Invalid_State      : exception;

private
   type Transition_Record;
   type Transition is access Transition_Record;
   type Transition_Record is
      record
         Event       : Event_Type;
         Final_State : Automaton_State;
         Func        : Transition_Function := null;
         Next        : Transition := null;
      end record;

   type Transitions_Array is array (Automaton_State range <>) of Transition;
   type Transitions_Array_Ptr is access Transitions_Array;
   type Subautomaton_Array is array (Natural range <>) of Automaton_State;

   type State_Machine is new Ada.Finalization.Limited_Controlled with
      record
         Current_State     : Automaton_State;
         States            : Transitions_Array_Ptr := null;

         --  Sub-automatons handling
         Sub_Automatons    : Subautomaton_Array (1 .. Max_Automaton_Depth);
         Current_Automaton : Natural := 1;
      end record;

   pragma Finalize_Storage_Only (State_Machine);
   procedure Finalize (Automaton : in out State_Machine);
end Generic_State_Machine;
