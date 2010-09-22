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

with Ada.Exceptions;         use Ada.Exceptions;
with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;
with GNAT.IO;                use GNAT.IO;
with Ada.Unchecked_Deallocation;

package body Sax.State_Machines is
   use Transition_Tables, State_Tables;

   procedure Mark_Active
     (Self     : in out NFA_Matcher;
      From     : State;
      Flag     : Active);
   --  Mark [From] as active next time, as well as all states reachable
   --  through an empty transition.

   procedure Remove_From_List
     (Self : in out NFA_Matcher;
      Node : NFA_Matcher_List);
   --  Remove [Node] from the list of nested matchers.
   --  This frees [Node]

   function Start_Match
     (Self : access NFA'Class; S : State; Flag : Active) return NFA_Matcher;
   --  Returns a new matcher, initially in state [S] (and all empty transitions
   --  form it).

   function Image (S : State) return String;
   --  Display [S], for [Dump]

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : in out NFA) is
   begin
      Init (Self.States);
      Init (Self.Transitions);

      --  Create start state
      Append
        (Self.States,
         State_Data'
           (Nested           => No_State,
            On_Nested_Exit   => No_Transition,
            First_Transition => No_Transition,
            Data             => Default_Data));
   end Initialize;

   ----------
   -- Free --
   ----------

   procedure Free (Self : in out NFA) is
   begin
      Free (Self.States);
      Free (Self.Transitions);
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Automaton : in out NFA_Access) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (NFA'Class, NFA_Access);
   begin
      if Automaton /= null then
         Free (Automaton.all);
         Unchecked_Free (Automaton);
      end if;
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Self : in out NFA_Matcher) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Boolean_State_Array, Boolean_State_Array_Access);
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (NFA_Matcher_Record, NFA_Matcher_List);
      L : NFA_Matcher_List;
   begin
      Unchecked_Free (Self.Current);
      while Self.Nested /= null loop
         L := Self.Nested.Next;
         Free (Self.Nested.Matcher);
         Unchecked_Free (Self.Nested);
         Self.Nested := L;
      end loop;
   end Free;

   ---------------
   -- Add_State --
   ---------------

   function Add_State
     (Self : access NFA; Data : State_User_Data := Default_Data) return State
   is
   begin
      Append
        (Self.States,
         State_Data'
           (Nested           => No_State,
            Data             => Data,
            On_Nested_Exit   => No_Transition,
            First_Transition => No_Transition));
      return Last (Self.States);
   end Add_State;

   --------------------
   -- Add_Transition --
   --------------------

   procedure Add_Transition
     (Self      : access NFA;
      From      : State;
      To        : State;
      On_Symbol : Transition_Symbol) is
   begin
      if From = Final_State then
         Raise_Exception
           (Program_Error'Identity,
            "Can't add transitions from final_state");
      end if;

      Append
        (Self.Transitions,
         Transition'
           (Is_Empty       => False,
            To_State       => To,
            Next_For_State => Self.States.Table (From).First_Transition,
            Sym            => On_Symbol));
      Self.States.Table (From).First_Transition := Last (Self.Transitions);
   end Add_Transition;

   --------------------------
   -- Add_Empty_Transition --
   --------------------------

   procedure Add_Empty_Transition
     (Self : access NFA;
      From : State;
      To   : State)
   is
   begin
      if From = Final_State then
         Raise_Exception
           (Program_Error'Identity,
            "Can't add transitions from final_state");
      end if;

      Append
        (Self.Transitions,
         Transition'
           (Is_Empty       => True,
            To_State       => To,
            Next_For_State => Self.States.Table (From).First_Transition));
      Self.States.Table (From).First_Transition := Last (Self.Transitions);
   end Add_Empty_Transition;

   ------------
   -- Repeat --
   ------------

   procedure Repeat
     (Self       : access NFA;
      From, To   : State;
      Min_Occurs : Natural := 1;
      Max_Occurs : Positive := 1)
   is
      function Clone_And_Append (From : State) return State;
      --  Duplicate the automaton From..To, and create a new automaton starting
      --  at Append_To.
      --
      --     -|From|--E--|To|--         ---|Append_To|
      --
      --  becomes
      --
      --     ---|Append_To|--E--|New_End|--
      --
      --  The contents of Append_To is overwritten.

      procedure Rename_State (Old_State, New_State : State);
      --  Replace all references to [Old_State] with references to [New_State]
      --  This doesn't change transitions.

      ------------------
      -- Rename_State --
      ------------------

      procedure Rename_State (Old_State, New_State : State) is
      begin
         for T in Transition_Tables.First .. Last (Self.Transitions) loop
            if Self.Transitions.Table (T).To_State = Old_State then
               Self.Transitions.Table (T).To_State := New_State;
            end if;
         end loop;
      end Rename_State;

      ----------------------
      -- Clone_And_Append --
      ----------------------

      function Clone_And_Append (From : State) return State is
         New_To : constant State := Add_State (Self);

         Cloned : array (State_Tables.First .. Last (Self.States)) of State :=
           (others => No_State);
         --  Id of the clones corresponding to the states in From..To

         procedure Clone_Internal_Nodes (S : State);
         --  Clone all nodes internal to the subautomation.
         --  The algorithm is as follows: starting from [From], we follow all
         --  transitions until we reach [To]. We do not follow any transition
         --  from [To]. In the end, the internal nodes are the ones with an
         --  an entry in [Cloned].

         procedure Clone_Transitions;
         --  Clone all transitions for all cloned nodes. Only the transitions
         --  leading to internal nodes are cloned

         --------------------------
         -- Clone_Internal_Nodes --
         --------------------------

         procedure Clone_Internal_Nodes (S : State) is
            T   : Transition_Id;
         begin
            if S = From then
               Cloned (From) := New_To;
            elsif S = New_To then
               return;  --  Do not follow transitions from [To]
            else
               Cloned (S) := Add_State (Self);
            end if;

            T := Self.States.Table (S).First_Transition;
            while T /= No_Transition loop
               declare
                  Tr : Transition renames Self.Transitions.Table (T);
               begin
                  if Tr.To_State = Final_State then
                     null;
                  elsif Cloned (Tr.To_State) = No_State then
                     Clone_Internal_Nodes (Tr.To_State);
                  end if;
                  T := Tr.Next_For_State;
               end;
            end loop;
         end Clone_Internal_Nodes;

         -----------------------
         -- Clone_Transitions --
         -----------------------

         procedure Clone_Transitions is
            Prev : Transition_Id;
            T   : Transition_Id;
            Tmp : State;
         begin
            Self.States.Table (New_To) := Self.States.Table (To);
            Self.States.Table (To).First_Transition := No_Transition;

            for S in reverse Cloned'Range loop
               if Cloned (S) /= No_State then
                  T := Self.States.Table (S).First_Transition;
                  while T /= No_Transition loop
                     declare
                        Tr : Transition renames Self.Transitions.Table (T);
                     begin
                        if Tr.To_State = Final_State then
                           Tmp := Final_State;
                           if Cloned (S) = To then
                              Tmp := No_State; --  No copy, will be done later
                           end if;
                        elsif Tr.To_State > Cloned'Last then
                           Tmp := No_State;  --  Link to the outside
                        else
                           Tmp := Cloned (Tr.To_State);
                        end if;

                        if Tmp /= No_State then
                           case Tr.Is_Empty is
                              when True =>
                                 Add_Empty_Transition (Self, Cloned (S), Tmp);
                              when False =>
                                 Add_Transition
                                   (Self, Cloned (S), Tmp, Tr.Sym);
                           end case;
                        end if;

                        T := Tr.Next_For_State;
                     end;
                  end loop;
               end if;
            end loop;

            --  Last pass to move external transition from [New_To] to [To],
            --  ie from the end of the sub-automaton

            Prev := No_Transition;
            T := Self.States.Table (New_To).First_Transition;

            while T /= No_Transition loop
               declare
                  Tr : Transition renames Self.Transitions.Table (T);
                  Next : constant Transition_Id := Tr.Next_For_State;
               begin
                  if Tr.To_State = Final_State
                    or else
                      (Tr.To_State /= To
                       and then Tr.To_State <= Cloned'Last
                       and then Cloned (Tr.To_State) = No_State)
                  then
                     if Prev = No_Transition then
                        Self.States.Table (New_To).First_Transition :=
                          Tr.Next_For_State;
                     else
                        Self.Transitions.Table (Prev).Next_For_State :=
                          Tr.Next_For_State;
                     end if;

                     Tr.Next_For_State :=
                       Self.States.Table (To).First_Transition;
                     Self.States.Table (To).First_Transition := T;

                  else
                     Prev := T;
                  end if;

                  T := Next;
               end;
            end loop;
         end Clone_Transitions;

      begin
         --  Replace [To] with a new node, so that [To] is still
         --  the end state

         Rename_State (To, New_To);

         --  Need to duplicate the sub-automaton

         Cloned (New_To) := To;
         Clone_Internal_Nodes (From);
         Clone_Transitions;

         return New_To;
      end Clone_And_Append;

      N : State := From;

   begin
      if Min_Occurs = 1 and then Max_Occurs = 1 then
         return;  --  Nothing to do
      end if;

      if Min_Occurs > Max_Occurs then
         return;  --  As documented, nothing is done
      end if;

      if Min_Occurs = 0 then
         Add_Empty_Transition (Self, From, To);

      elsif Min_Occurs > 1 then
         for Occur in 2 .. Min_Occurs loop
            N := Clone_And_Append (From => N);
         end loop;
      end if;

      if Max_Occurs = Natural'Last then
         Add_Empty_Transition (Self, From => To, To => From);

      else
         declare
            Local_Ends : array (Min_Occurs + 1 .. Max_Occurs) of State;
         begin
            for Occur in Min_Occurs + 1 .. Max_Occurs loop
               N := Clone_And_Append (N);
               Local_Ends (Occur) := N;
            end loop;

            --  Make those occurrences optional. For efficiency, the start of
            --  each points to the end of the regexp, rather than to the start
            --  of the next optional (both result in the same behavior, but
            --  the latter require more iteration when processing the NFA to
            --  traverse empty transitions).

            for L in Local_Ends'Range loop
               Add_Empty_Transition (Self, Local_Ends (L), To);
            end loop;
         end;
      end if;
   end Repeat;

   -----------------
   -- Mark_Active --
   -----------------

   procedure Mark_Active
     (Self     : in out NFA_Matcher;
      From     : State;
      Flag     : Active)
   is
      T : Transition_Id := Self.NFA.States.Table (From).First_Transition;
   begin
      Self.Current (From) := Self.Current (From) or Flag;

      --  Mark (recursively) all states reachable from an empty transition
      --  as active too.

      while T /= No_Transition loop
         declare
            Tr : Transition renames Self.NFA.Transitions.Table (T);
         begin
            if Tr.Is_Empty then
               if Tr.To_State = Final_State then
                  Self.In_Final := True;

               --  Nothing to do if already marked active
               elsif (Self.Current (Tr.To_State) and Flag) = 0 then
                  Mark_Active (Self, Tr.To_State, Flag);
               end if;
            end if;

            T := Tr.Next_For_State;
         end;
      end loop;
   end Mark_Active;

   -----------------
   -- Start_Match --
   -----------------

   function Start_Match (Self : access NFA) return NFA_Matcher is
   begin
      return Start_Match (Self, S => Start_State, Flag => State_Active);
   end Start_Match;

   -----------------
   -- Start_Match --
   -----------------

   function Start_Match
     (Self : access NFA'Class; S : State; Flag : Active) return NFA_Matcher
   is
      R : NFA_Matcher;
   begin
      R.NFA      := NFA_Access (Self);
      R.Current  := new Boolean_State_Array'
        (1 .. Last (Self.States) => State_Inactive);
      R.In_Final := False;
      Mark_Active (R, S, Flag => Flag);
      return R;
   end Start_Match;

   ----------------------
   -- Remove_From_List --
   ----------------------

   procedure Remove_From_List
     (Self : in out NFA_Matcher;
      Node : NFA_Matcher_List)
   is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (NFA_Matcher_Record, NFA_Matcher_List);
      Prev : NFA_Matcher_List;
      N    : NFA_Matcher_List := Self.Nested;
   begin
      while N /= Node loop
         Prev := N;
         N := N.Next;
      end loop;

      if N /= null then
         if Prev = null then
            Self.Nested := N.Next;
         else
            Prev.Next := N.Next;
         end if;

         Free (N.Matcher);
         Unchecked_Free (N);
      end if;
   end Remove_From_List;

   ---------------------------
   -- For_Each_Active_State --
   ---------------------------

   procedure For_Each_Active_State (Self : NFA_Matcher) is
   begin
      for S in Self.Current'Range loop
         if (Self.Current (S) and State_Active) /= 0 then
            Callback (S, Self.NFA.States.Table (S).Data);
         end if;
      end loop;
   end For_Each_Active_State;

   -------------
   -- Process --
   -------------

   procedure Process
     (Self    : in out NFA_Matcher;
      Input   : Symbol;
      Success : out Boolean)
   is
      All_Inactive : constant Boolean_State_Array := Boolean_State_Array'
        (Self.Current'Range => State_Inactive);

      procedure Process_Transitions
        (Self     : in out NFA_Matcher;
         List     : Transition_Id;
         Has_Next : in out Boolean);
      pragma Inline (Process_Transitions);
      --  Process all the transitions in [List]
      --  [Has_Next] is set to True if at least one active state was marked for
      --  the future.

      procedure Process_Matcher
        (Self : in out NFA_Matcher; Success : out Boolean);
      --  Process a (potentially nested) matcher

      procedure Change_States
        (Self       : in out NFA_Matcher;
         Has_Active : out Boolean;
         Preserve   : Boolean);
      --  Mark the current active status of states based on the future status.
      --  If [Preserve] is True, the current status is simply kept.
      --  Returns True if Self has no active state left

      ---------------------
      -- Process_Matcher --
      ---------------------

      procedure Process_Matcher
        (Self : in out NFA_Matcher; Success : out Boolean)
      is
         Has_Next  : Boolean := False;
         N         : NFA_Matcher_List;
         Event_Processed_In_Nested : Boolean;

      begin
         --  For each currently live state:
         --   - if there are nested NFA, we process these first. If the event
         --     is processed by them, it will not be passed on to the
         --     corresponding super state (event bubbling stopped).
         --   - if there are no nested NFA, or they did not process the event,
         --     the event is then processed directly by the super state.
         --  This corresponds to standard semantics of event bubbling in
         --  hierarchical NFA.

         for S in Self.Current'Range loop
            if (Self.Current (S) and State_Active) /= 0 then  --  if active
               Event_Processed_In_Nested := False;

               --  Find the nested NFA, if any
               N := Self.Nested;
               while N /= null and then N.Super_State /= S loop
                  N := N.Next;
               end loop;

               if N /= null then
                  Process_Matcher (N.Matcher, Success);

                  if Success then
                     Event_Processed_In_Nested := True;

                     if N.Matcher.In_Final then
                        --  Exits the nested NFA, and thus transitions from its
                        --  super state (which is no longer active).
                        --  Do not keep [N] for next turn (on [Next_Nested])

                        Process_Transitions
                          (Self,
                           Self.NFA.States.Table
                             (N.Super_State).On_Nested_Exit,
                           Has_Next);

                     else
                        --  We are still within the nested NFA, the super state
                        --  remains active
                        Has_Next := True;
                        Mark_Active (Self, N.Super_State, State_Future_Active);
                     end if;

                  else
                     --  Error: nothing matches anymore in the nested NFA. We
                     --  terminate it, but keep processing this event in its
                     --  superstate (for instance, a camera in state "on" has a
                     --  nested NFA "record"<->"play"). If the nested receives
                     --  the event "turn off", it won't match the nested, but
                     --  that's not an error because the event is handled by
                     --  the super state "on".

                     null;  --  Do not put back [N] on [Next_Nested]
                  end if;
               end if;

               if not Event_Processed_In_Nested then
                  Process_Transitions
                    (Self,
                     Self.NFA.States.Table (S).First_Transition,
                     Has_Next);
               end if;
            end if;
         end loop;

         if not Has_Next then
            --  Do not change Self
            --  ??? In fact, Self.Next_Nested has already been altered...
            Success := False;
         else
            Success          := True;
         end if;
      end Process_Matcher;

      -------------------------
      -- Process_Transitions --
      -------------------------

      procedure Process_Transitions
        (Self     : in out NFA_Matcher;
         List     : Transition_Id;
         Has_Next : in out Boolean)
      is
         T : Transition_Id := List;
      begin
         while T /= No_Transition loop
            declare
               Tr : Transition renames Self.NFA.Transitions.Table (T);
            begin
               --  Empty transitions were already considered in the previous
               --  call to Process, so are ignored now.
               --  We also ignore this transition if [To_State] is already
               --  active next time, no need to retest it.

               if not Tr.Is_Empty then
                  if Tr.To_State = Final_State then
                     if not Self.In_Final and then Match (Tr.Sym, Input) then
                        Self.In_Final := True;
                        Has_Next := True;
                     end if;

                  elsif
                    (Self.Current (Tr.To_State) and State_Future_Active) = 0
                    and then Match (Tr.Sym, Input)
                  then
                     Has_Next := True;
                     Mark_Active (Self, Tr.To_State, State_Future_Active);

                     --  ??? If we are entering any state with a nested NFA,
                     --  we should activate that NFA next turn.
                     --  ??? Should check states with empty transitions

                     if Self.NFA.States.Table (Tr.To_State).Nested /=
                       No_State
                     then
                        Self.Nested := new NFA_Matcher_Record'
                          (Next        => Self.Nested,
                           Matcher     => Start_Match
                             (Self.NFA,
                              Self.NFA.States.Table (Tr.To_State).Nested,
                              State_Future_Active),
                           Super_State => Tr.To_State);
                     end if;
                  end if;
               end if;

               T := Tr.Next_For_State;
            end;
         end loop;
      end Process_Transitions;

      -------------------
      -- Change_States --
      -------------------

      procedure Change_States
        (Self       : in out NFA_Matcher;
         Has_Active : out Boolean;
         Preserve   : Boolean)
      is
         N : NFA_Matcher_List := Self.Nested;
         N_Next : NFA_Matcher_List;
      begin
         if Preserve then
            for C in Self.Current'Range loop
               Self.Current (C) :=
                 Self.Current (C) and not State_Future_Active;
            end loop;
         else
            for C in Self.Current'Range loop
               if (Self.Current (C) and State_Future_Active) /= 0 then
                  Self.Current (C) := State_Active;
               else
                  Self.Current (C) := State_Inactive;
               end if;
            end loop;

            if Self.Current.all = All_Inactive then
               Has_Active := False;
               return;
            end if;
         end if;

         --  Investigate nested NFA
         while N /= null loop
            N_Next := N.Next;
            Change_States (N.Matcher, Has_Active, Preserve);

            if not Has_Active then
               --  Remove the state from the list, it is no longer used
               Remove_From_List (Self, N);
            end if;

            N := N_Next;
         end loop;

         Has_Active := True;
         return;
      end Change_States;

      Has_Active : Boolean;
   begin
      Self.In_Final := False;
      Process_Matcher (Self, Success);
      Change_States (Self, Has_Active => Has_Active, Preserve => not Success);
   end Process;

   --------------
   -- Expected --
   --------------

   function Expected (Self : NFA_Matcher) return String is
      Msg : Unbounded_String;
      T    : Transition_Id;
      N    : NFA_Matcher_List := Self.Nested;

   begin
      while N /= null loop
         declare
            E : constant String := Expected (N.Matcher);
         begin
            if E /= "" then
               if Msg /= Null_Unbounded_String then
                  Append (Msg, "|");
               end if;
               Append (Msg, E);
            end if;
         end;

         N := N.Next;
      end loop;

      for S in Self.Current'Range loop
         if (Self.Current (S) and State_Active) /= 0 then
            T := Self.NFA.States.Table (S).First_Transition;
            while T /= No_Transition loop
               declare
                  Tr : Transition renames Self.NFA.Transitions.Table (T);
               begin
                  if not Tr.Is_Empty then
                     if Msg /= Null_Unbounded_String then
                        Append (Msg, "|");
                     end if;

                     Append (Msg, Image (Tr.Sym));
                  end if;

                  T := Tr.Next_For_State;
               end;
            end loop;
         end if;
      end loop;

      return To_String (Msg);
   end Expected;

   --------------
   -- In_Final --
   --------------

   function In_Final (Self : NFA_Matcher) return Boolean is
   begin
      return Self.In_Final;
   end In_Final;

   -------------------
   -- Create_Nested --
   -------------------

   function Create_Nested
     (Self : access NFA'Class; From : State) return Nested_NFA
   is
      pragma Unreferenced (Self);
   begin
      return (Default_Start => From);
   end Create_Nested;

   --------------------
   -- On_Nested_Exit --
   --------------------

   procedure On_Nested_Exit
     (Self      : access NFA;
      From      : State;
      To        : State;
      On_Symbol : Transition_Symbol) is
   begin
      if Self.States.Table (From).Nested = No_State then
         Raise_Exception
           (Program_Error'Identity,
            "On_Nested_Exit must be called on a state with a nested NFA");
      end if;

      Append
        (Self.Transitions,
         Transition'
           (Is_Empty       => False,
            To_State       => To,
            Next_For_State => Self.States.Table (From).On_Nested_Exit,
            Sym            => On_Symbol));
      Self.States.Table (From).On_Nested_Exit := Last (Self.Transitions);
   end On_Nested_Exit;

   --------------------------
   -- On_Empty_Nested_Exit --
   --------------------------

   procedure On_Empty_Nested_Exit
     (Self      : access NFA;
      From      : State;
      To        : State) is
   begin
      if Self.States.Table (From).Nested = No_State then
         Raise_Exception
           (Program_Error'Identity,
            "On_Nested_Exit must be called on a state with a nested NFA");
      end if;

      Append
        (Self.Transitions,
         Transition'
           (Is_Empty       => True,
            To_State       => To,
            Next_For_State => Self.States.Table (From).On_Nested_Exit));
      Self.States.Table (From).On_Nested_Exit := Last (Self.Transitions);
   end On_Empty_Nested_Exit;

   ----------------
   -- Set_Nested --
   ----------------

   procedure Set_Nested (Self : access NFA; S : State; Nested : Nested_NFA) is
   begin
      Self.States.Table (S).Nested := Nested.Default_Start;
   end Set_Nested;

   -----------
   -- Image --
   -----------

   function Image (S : State) return String is
      Str : constant String := State'Image (S);
   begin
      return "S" & Str (Str'First + 1 .. Str'Last);
   end Image;

   ----------
   -- Dump --
   ----------

   function Dump
     (Self   : access NFA;
      Nested : Nested_NFA;
      Mode   : Dump_Mode := Dump_Compact) return String
   is
      Dumped : array (State_Tables.First .. Last (Self.States)) of Boolean :=
        (others => False);
      Result : Unbounded_String;

      procedure Internal (S : State);

      procedure Internal (S : State) is
         T : Transition_Id;
      begin
         if Dumped (S) then
            return;
         end if;

         Dumped (S) := True;

         if S = Start_State then
            Append (Result, " <start>");
         else
            Append (Result, " " & Image (S));
         end if;

         T := Self.States.Table (S).First_Transition;
         while T /= No_Transition loop
            declare
               Tr : Transition renames Self.Transitions.Table (T);
            begin
               if Tr.Is_Empty then
                  Append (Result, "(");
               else
                  Append (Result, "(" & Image (Tr.Sym));
               end if;

               if Tr.To_State = Final_State then
                  Append (Result, ",<final>)");
               else
                  Append (Result, "," & Image (Tr.To_State) & ")");
               end if;

               T := Tr.Next_For_State;
            end;
         end loop;

         if Self.States.Table (S).Nested /= No_State then
            Append
              (Result,
               "{nested:" & Image (Self.States.Table (S).Nested) & "}");
            Append
              (Result,
               Dump (Self,
                 Nested => (Default_Start => Self.States.Table (S).Nested),
                 Mode   => Mode));
         end if;

         T := Self.States.Table (S).First_Transition;
         while T /= No_Transition loop
            declare
               Tr : Transition renames Self.Transitions.Table (T);
            begin
               if Tr.To_State /= Final_State then
                  Internal (Tr.To_State);
               end if;
               T := Tr.Next_For_State;
            end;
         end loop;

         if Mode = Dump_Multiline then
            Append (Result, ASCII.LF);
         end if;
      end Internal;

   begin
      Internal (Nested.Default_Start);
      return To_String (Result);
   end Dump;

   ----------
   -- Dump --
   ----------

   function Dump
     (Self    : access NFA'Class;
      Mode    : Dump_Mode := Dump_Compact) return String
   is
      Dumped : array (State_Tables.First .. Last (Self.States)) of Boolean :=
        (others => False);

      Result : Unbounded_String;

      procedure Newline;
      --  Append a newline to [Result] if needed

      function Image_Dot
        (S : State; Nested_In : State := No_State) return String;
      procedure Dump_Dot (Start_At, Nested_In : State; Prefix : String);
      procedure Dump_Dot_Transitions
        (S : State; First : Transition_Id; Prefix : String;
         Label_Prefix : String; Nested_In : State := No_State);

      procedure Dump_Nested (S : State);
      --  Dump a cluster that represents a nested NFA.
      --  Such nested NFAs are represented only once, even though they can in
      --  fact be nested within several nodes. That would make huge graphs
      --  otherwise.

      -------------
      -- Newline --
      -------------

      procedure Newline is
      begin
         case Mode is
            when Dump_Compact | Dump_Dot_Compact => null;
            when others => Append (Result, ASCII.LF);
         end case;
      end Newline;

      ---------------
      -- Image_Dot --
      ---------------

      function Image_Dot
        (S : State; Nested_In : State := No_State) return String is
      begin
         if S = Final_State then
            if Nested_In /= No_State then
               return "Sf" & Image (Nested_In);
            else
               return "Sf";
            end if;

         else
            return Image (S);
         end if;
      end Image_Dot;

      --------------------------
      -- Dump_Dot_Transitions --
      --------------------------

      procedure Dump_Dot_Transitions
        (S : State; First : Transition_Id; Prefix : String;
         Label_Prefix : String; Nested_In : State := No_State)
      is
         T : Transition_Id := First;
      begin
         while T /= No_Transition loop
            declare
               Tr : Transition renames Self.Transitions.Table (T);
            begin
               Append (Result,
                       Prefix & Image_Dot (S, Nested_In)
                       & "->" & Image_Dot (Tr.To_State, Nested_In)
                       & " [label=""");

               if not Tr.Is_Empty then
                  Append (Result, Label_Prefix & Image (Tr.Sym) & """];");
               else
                  Append (Result, Label_Prefix & """");
                  Append (Result, " style=dashed];");
               end if;

               Newline;

               if Tr.To_State /= Final_State then
                  Dump_Dot (Tr.To_State, Nested_In, Prefix);
               end if;

               T := Tr.Next_For_State;
            end;
         end loop;
      end Dump_Dot_Transitions;

      -----------------
      -- Dump_Nested --
      -----------------

      procedure Dump_Nested (S : State) is
      begin
         Append (Result, "subgraph cluster" & Image (S) & "{");
         Newline;
         Append (Result, " label=""" & Image (S) & """;");
         Newline;
         Append (Result, " " & Image (S) & " [shape=doublecircle];");
         Newline;
         Append
           (Result,
            " " & Image_Dot (Final_State, S) & " [shape=doublecircle];");
         Newline;

         Dump_Dot (S, Prefix => " ", Nested_In => S);

         Append (Result, "};");
         Newline;
      end Dump_Nested;

      --------------
      -- Dump_Dot --
      --------------

      procedure Dump_Dot (Start_At, Nested_In : State; Prefix : String) is
      begin
         if Start_At = Final_State or else Dumped (Start_At) then
            return;
         end if;

         Dumped (Start_At) := True;
         Dump_Dot_Transitions
           (Start_At,
            Self.States.Table (Start_At).First_Transition, Prefix, "",
            Nested_In);
         Dump_Dot_Transitions
           (Start_At,
            Self.States.Table (Start_At).On_Nested_Exit, Prefix, "on_exit:",
            Nested_In);
      end Dump_Dot;

   begin
      case Mode is
         when Dump_Multiline | Dump_Compact =>
            return Dump (Self   => Self,
                         Nested => (Default_Start => Start_State),
                         Mode   => Mode);

         when Dump_Dot | Dump_Dot_Compact =>
            Append (Result, "/* Use   dot -O -Tpdf file.dot */" & ASCII.LF);
            Append (Result, "digraph finite_state_machine{");
            Newline;
            Append (Result, "compound=true;");
            Newline;
            Append (Result, "rankdir=LR;");
            Newline;
            Append (Result, Image (Start_State) & " [shape=doublecircle];");
            Newline;
            Append
              (Result, Image_Dot (Final_State) & " [shape=doublecircle];");
            Newline;

            --  First, create all the clusters for the nested NFA. That helps
            --  remove their states from the global lists, so that we can then
            --  only dump the toplevel states

            for S in State_Tables.First .. Last (Self.States) loop
               if Self.States.Table (S).Nested /= No_State then
                  Dump_Nested (Self.States.Table (S).Nested);
                  Append (Result, Image (S)
                          & " [label=""" & Image (S)
                          & " (" & Image (Self.States.Table (S).Nested)
                          & ")""];");
                  Newline;
               end if;
            end loop;

            --  Now dump the toplevel states (that is the ones that haven't
            --  been dumped yet)

            for S in State_Tables.First .. Last (Self.States) loop
               Dump_Dot (S, No_State, "");
            end loop;

            Append (Result, "}" & ASCII.LF);
      end case;

      return To_String (Result);
   end Dump;

   -----------------
   -- Debug_Print --
   -----------------

   procedure Debug_Print (Self : NFA_Matcher) is
      procedure Internal (M : NFA_Matcher; Prefix : String);
      procedure Internal (M : NFA_Matcher; Prefix : String) is
         N : NFA_Matcher_List := M.Nested;
      begin
         Put (Prefix & "Active:");
         for C in M.Current'Range loop
            if (M.Current (C) and State_Active) /= 0 then
               Put (C'Img);
            end if;
         end loop;

         while N /= null loop
            Internal (N.Matcher, Prefix => Prefix & "  +Nested("
                      & N.Super_State'Img & "): ");
            N := N.Next;
         end loop;
      end Internal;
   begin
      Internal (Self, Prefix => "");
      New_Line;
   end Debug_Print;

end Sax.State_Machines;
