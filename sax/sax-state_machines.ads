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

--  ??? Would be nice to have:
--  - Quick save-to-string and restore features
--    This could limit the number of memory allocations and speed up the
--    creation of a state machine, but is difficult to do because of callbacks
--  - Merge: merging two state machines
--    For instance, in XML we can have multiple grammars each with its own
--    schema

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

   type State_User_Data is private;
   Default_Data : State_User_Data;
   --  User data associated with each state

package Sax.State_Machines is

   type State is new Natural;
   --  A state of a state machine

   type State_Data_Access is access all State_User_Data;

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

   function Add_State
     (Self : access NFA; Data : State_User_Data := Default_Data) return State;
   --  Add a new state into the table.

   No_State    : constant State;
   Start_State : constant State;
   Final_State : constant State;
   --  The start and final states of an automation
   --  There is a single one of each per automaton, but you can of course
   --  connect them, through empty transitions, to any number of states within
   --  [Self], thus making them start states in effect.
   --  These two states always exist.

   function Get_Data (Self : access NFA; S : State) return State_Data_Access;
   --  Returns an access to the state's user data. This can be modified in
   --  place, but the access type should not be stored since it still belongs
   --  to the NFA.
   --  This API is slightly faster and more convenient than having a
   --  [Get_User_Data] and [Set_User_Data] set of subprograms.

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

   ----------------------------------------
   -- Hierarchical finite state machines --
   ----------------------------------------
   --  It is possible to build hierarchical state machines: in such machines,
   --  some of the states will contain nested state machines.
   --  For instance:
   --                +----2-----+
   --                |          |---'b'--> 7
   --      1 ------->|-4->5-->6----------> 3
   --                |          |
   --                +----------+
   --
   --  In the case above, the machine could be in both state 2 (the superstate)
   --  and in state 5 (the inner state).
   --  When an input is processed, all active states (super and inner) will
   --  proceed the event. If 5 matches, we might go to 6. Next time, if 6
   --  matches, we would exit 2 and go to 3.
   --
   --  But when 2 and 5 are active, it is also possible that 2 itself matches,
   --  and then we go to 3 whatever inner state we were in at the same time.
   --  This is the usual behavior (as defined for instance in UML). However,
   --  this package has a mode ([Nested_Must_Be_Final]) whereby the transitions
   --  from the superstate are only considered if the nested NFA is in a
   --  final state.
   --
   --  The above would be created as follows. Note that this example also does
   --  not assume that the nested NFA has been created before we create the
   --  toplevel NFA.
   --
   --     S1 := N.Add_State; S2 := N.Add_State; S3 := N.Add_State;
   --     N.Add_Transition (S1, S2, ...);  --  will enter "2" and nested NFA
   --                                      --  so activate S4
   --     N.Add_Transition (S2, S7, 'b');  --  will exit nested NFA whatever
   --                                      --  state we are in.
   --
   --  Later on we create the nested automaton:
   --     S4 := N.Add_State;
   --     S5 := N.Add_State;  N.Add_Transition (S4, S5, ...);
   --     S6 := N.Add_State;  N.Add_Transition (S5, S6, ...);
   --
   --     E := N.Create_Nested (S4);
   --     N.Set_Nested (E);  --  Wraps E (we could have several states wrapping
   --                        --  the same nested)
   --     N.On_Nested_Exit (S2, S3);  --  on exit of nested NFA, moves to 3
   --
   --  It is possible to build state machines that cannot be executed later on:
   --  if the state machine within 2 is the same as the outer state machine
   --  (therefore we have a recursive state machine, somewhat), and we have an
   --  empty transition from 1 to 4, then the initial state would require
   --  infinite storage for the NFA_Matcher: we start in state 1, which through
   --  the empty transition is the same as state 4 (and therefore state 2 is
   --  also active). But in this recursive NFA, state 4 is another instance
   --  equivalent to state 1. In turn, we have another nested state 1, then
   --  another,... ad infinitum. So you should always have a non-empty
   --  transition into a nested state machine (in thhe schema above, transition
   --  from 1->4 should not be empty.
   --
   --  It is invalid to add a transition from one of the nested states from one
   --  of the outer states. The nested automaton must be fully independent,
   --  since it might be reused in several places.

   type Nested_NFA is private;
   No_Nested : constant Nested_NFA;

   function Create_Nested
     (Self : access NFA'Class; From : State) return Nested_NFA;
   --  Marks the part of the machine starting at [From] and ending at [To] as
   --  a nested automaton. It is possible that some states have been created
   --  between the two that do not belong to the nested automaton, this isn't
   --  an issue.
   --  The state [From] is the default state for the nested automaton. For
   --  instance, a camera has two superstates: "on" and "off". The "on" state
   --  has a nested NFA for "record" and "playback" modes. By default, if you
   --  enter the "on" state, the "record" mode is also selected. However, using
   --  the appropriate camera button, it is possible to enter the "playback"
   --  button directly.
   --
   --  Within the nested NFA, transitions to [Final_State] play a special role:
   --  upon reaching it, the nested automaton will be terminated, and control
   --  returned to the super state (that state is one of the states for which
   --  we have called Set_Nested). Any empty transition for the superstate will
   --  be navigated to find out the new list of active states.
   --
   --  No further internal transition must be added to the nested automaton
   --  after this call, since its states have been marked specially. It is
   --  still valid to add transitions to the ouside.

   procedure Set_Nested (Self : access NFA; S : State; Nested : Nested_NFA);
   function Get_Nested (Self : access NFA; S : State) return Nested_NFA;
   --  Setup state [S] so that it includes a nested NFA defined by [Nested]

   function Get_Start_State (Self : Nested_NFA) return State;
   --  Return the start state that was defined for the nested NFA

   procedure Set_Nested_Must_Be_Final
     (Self : access NFA; Must_Be_Final : Boolean);
   --  Activates the [Nested_Must_Be_Final] mode.
   --  In this mode, the transitions from a superstate are only considered if
   --  the nested NFA is in a final state.

   procedure On_Nested_Exit
     (Self      : access NFA;
      From      : State;
      To        : State;
      On_Symbol : Transition_Symbol);
   procedure On_Empty_Nested_Exit
     (Self      : access NFA;
      From      : State;
      To        : State);
   --  When the nested NFA in [From] is terminated (because it has reached
   --  [Final_State] after processing [On_Symbol]), a transition from [From] to
   --  [To] is performed. [Set_Nested] must have been called for [From] first.

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

   generic
      with procedure Callback
        (Self : access NFA'Class;
         S    : State) is <>;
   procedure For_Each_Active_State (Self : NFA_Matcher);
   --  Iterates over all currently active states.
   --  If [Nested_Must_Be_Final] mode is on, the states with a nested NFA are
   --  not returned unless their nested NFA is in a final state (that's because
   --  we would be ignoring events on them otherwise).

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

   -------------------------
   -- Dumping information --
   -------------------------
   --  The following subprograms are used mostly for debugging, and can be used
   --  to visualize the contents of a state machine, either textually or
   --  graphically

   function Default_Image (S : State; Data : State_User_Data) return String;
   --  The default display for states (only displays the state number)

   type Dump_Mode is
     (Dump_Multiline, Dump_Compact, Dump_Dot, Dump_Dot_Compact);
   --  The type of dump we can do for the graph:
   --  [Dump_Multiline]: Each state is displayed on one line
   --  [Dump_Compact]:   Output is on a single line
   --  [Dump_Dot):       Output that can be cut-and-pasted to use by the
   --                    graphviz suite to display a graphical representation

   generic
      with function State_Image
        (S : State; Data : State_User_Data) return String is Default_Image;
      --  This function is never called for the final state, which has no
      --  user data associated with it. Nor it is called for the start state.

   package Pretty_Printers is
      function Dump
        (Self                : access NFA'Class;
         Mode                : Dump_Mode := Dump_Compact;
         Show_Details        : Boolean := True;
         Show_Isolated_Nodes : Boolean := True) return String;
      --  Dump the NFA into a string.
      --  This is mostly for debug reasons, and the output might change from
      --  one version to the next.
      --  If [Compact] is True then the output does not include newlines.
      --  If [Show_Details] is False, then only the count of nodes will be
      --  displayed, not the actual list of nodes and transitions.
      --  If [Show_Isolated_Nodes] is false, then nodes that are not linked
      --  to any other and have no nested node will not be displayed.

      function Dump
        (Self   : access NFA'Class;
         Nested : Nested_NFA;
         Mode   : Dump_Mode := Dump_Compact) return String;
      --  Dump the NFA into a string.

   end Pretty_Printers;

   procedure Debug_Print
     (Self : NFA_Matcher; Mode : Dump_Mode := Dump_Multiline);
   --  Print on stdout some debug information for [Self]

private
   type Transition_Id is new Natural range 0 .. 2 ** 16 - 1;
   type Transition (Is_Empty : Boolean := True) is record
      To_State       : State;
      Next_For_State : Transition_Id;

      case Is_Empty  is
         when True  => null;
         when False => Sym : Transition_Symbol;
      end case;
   end record;
   No_Transition : constant Transition_Id := 0;

   Start_State : constant State := 1;           --  Exists in NFA.States
   Final_State : constant State := State'Last;  --  Not shown in NFA.States
   No_State    : constant State := Final_State - 1;

   type State_Data is record
      First_Transition : Transition_Id;
      On_Nested_Exit   : Transition_Id;
      Nested           : State := No_State;
      Data             : aliased State_User_Data;
   end record;
   --  [Nested], if defined, indicates that this state contains a nested
   --  state machine, for which the default is Nested. Any transition to this
   --  state will also activate [Nested].

   package Transition_Tables is new GNAT.Dynamic_Tables
     (Table_Component_Type => Transition,
      Table_Index_Type     => Transition_Id,
      Table_Low_Bound      => No_Transition + 1,
      Table_Initial        => 100,
      Table_Increment      => 50);
   subtype Transition_Table is Transition_Tables.Instance;

   package State_Tables is new GNAT.Dynamic_Tables
     (Table_Component_Type => State_Data,
      Table_Index_Type     => State,
      Table_Low_Bound      => Start_State,
      Table_Initial        => 100,
      Table_Increment      => 50);
   subtype State_Table is State_Tables.Instance;

   type NFA is tagged record
      States       : State_Table;
      Transitions  : Transition_Table;
      Nested_Must_Be_Final : Boolean := False;
   end record;

   type Nested_NFA is record
      Default_Start : State;
   end record;
   No_Nested : constant Nested_NFA := (Default_Start => No_State);

   type Active is mod 4;
   for Active'Size use 2;
   State_Inactive      : constant Active := 2#00#;
   State_Active        : constant Active := 2#01#;
   State_Future_Active : constant Active := 2#10#;

   type Boolean_State_Array is array (State range <>) of Active;
   type Boolean_State_Array_Access is access all Boolean_State_Array;
   pragma Pack (Boolean_State_Array);

   type NFA_Matcher_Record;
   type NFA_Matcher_List is access all NFA_Matcher_Record;

   type NFA_Matcher is record
      NFA        : NFA_Access;
      In_Final   : Boolean;
      Current    : Boolean_State_Array_Access;
      Nested     : NFA_Matcher_List;
   end record;

   type NFA_Matcher_Record is record
      Super_State : State;
      Matcher     : NFA_Matcher;
      Next        : NFA_Matcher_List;
   end record;

end Sax.State_Machines;
