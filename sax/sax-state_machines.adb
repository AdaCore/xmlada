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
   use Transition_Tables, State_Tables, Matcher_State_Arrays;

   Debug : constant Boolean := False;
   --  Whether to print on stdout the actions performed on the machine.
   --  Copy-pasting those actions would allow recreating the exact same
   --  machine.

   procedure Mark_Active
     (Self         : in out NFA_Matcher;
      List_Start   : in out Matcher_State_Index;
      From         : State;
      First_Nested : Matcher_State_Index := No_Matcher_State);
   --  Mark [From] as active next time, as well as all states reachable
   --  through an empty transition. THe nested state machine for the new state
   --  is set to [First_Nested].

   function Start_Match
     (Self : access NFA'Class; S : State) return NFA_Matcher;
   --  Returns a new matcher, initially in state [S] (and all empty transitions
   --  form it).

   function Nested_In_Final
     (Self : NFA_Matcher;
      S    : Matcher_State_Index) return Boolean;
   --  Return true if the nested NFA for [S] is in a final state, or if [S]
   --  has no nested automaton.
   --  [List_Start] is the first state in the level that contains [S]

   function Is_Active
     (Self       : NFA_Matcher;
      List_Start : Matcher_State_Index;
      S          : State) return Boolean;
   pragma Inline (Is_Active);
   --  Whether [S] is marked as active in the given list

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self                 : in out NFA;
      States_Are_Statefull : Boolean := False)
   is
   begin
      Self.States_Are_Statefull := States_Are_Statefull;

      Init (Self.States);
      Init (Self.Transitions);

      --  Create start state
      Append
        (Self.States,
         State_Data'
           (Nested           => No_State,
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
   begin
      Free (Self.Active);
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
            First_Transition => No_Transition));

      if Debug then
         Put_Line (Last (Self.States)'Img & " := NFA.Add_State");
      end if;

      return Last (Self.States);
   end Add_State;

   --------------
   -- Set_Data --
   --------------

   procedure Set_Data (Self : access NFA; S : State; Data : State_User_Data) is
   begin
      Self.States.Table (S).Data := Data;
   end Set_Data;

   --------------
   -- Get_Data --
   --------------

   function Get_Data (Self : access NFA; S : State) return State_Data_Access is
   begin
      return Self.States.Table (S).Data'Access;
   end Get_Data;

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

      pragma Assert (From /= No_State);
      pragma Assert (From <= Last (Self.States));
      pragma Assert (To /= No_State);
      pragma Assert (To = Final_State or else To <= Last (Self.States));

      Append
        (Self.Transitions,
         Transition'
           (Kind           => Transition_On_Symbol,
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
           (Kind           => Transition_On_Empty,
            To_State       => To,
            Next_For_State => Self.States.Table (From).First_Transition));
      Self.States.Table (From).First_Transition := Last (Self.Transitions);
   end Add_Empty_Transition;

   ------------
   -- Repeat --
   ------------

   function Repeat
     (Self       : access NFA;
      From, To   : State;
      Min_Occurs : Natural := 1;
      Max_Occurs : Natural := 1) return State
   is
      type State_Array is
        array (State_Tables.First .. Last (Self.States)) of State;

      procedure Clone_And_Count_Nodes
        (Cloned       : in out State_Array;
         Cloned_Count : out Natural);
      --  Clone all nodes internal to the subautomaton.
      --  The algorithm is as follows: starting from [From], we follow all
      --  transitions until we reach [To]. We do not follow any transition
      --  from [To]. In the end, the internal nodes are the ones with an
      --  an entry in [Cloned].

      function Complete_All_Clones
        (Cloned : State_Array; Cloned_Count : Natural; Max : Natural)
         return State;
      --  [Clone_And_Count_Nodes] was used to do one clone of the internal
      --  nodes (and count them). This procedure does the remaining number of
      --  clones for [Max_Occurs] repeats.
      --  On exit, [From} has been cloned to [Cloned (From)],
      --  [Cloned (From)+Cloned_Count], [Cloned (From)+Cloned_Count*2],...
      --  Returns the final node in the cloned automaton.

      procedure Clone_Transitions
        (Cloned : State_Array;
         Cloned_Count : Natural;
         New_To : State;
         Max    : Natural);
      --  Clone all transitions for all cloned nodes. Only the transitions
      --  leading to internal nodes are cloned
      --     |From|--|To| -- .. |Cloned (From)+Offset|...
      --  becomes
      --     |From|--|To| -- .. |Cloned (From)+Offset|...|<returned state>|
      --  [New_To] is the final node in the cloned automaton

      function Add_Stateless (New_To : State) return State;
      --  Add a new stateless (ie with no user data) state at the end of the
      --  subautomaton.
      --     ...--|New_To|--
      --  becomes
      --     ...--|N|--|New_To|
      --  where N is returned, has the user data of New_To, and New_To does not
      --  have any user data.

      ---------------------------
      -- Clone_And_Count_Nodes --
      ---------------------------

      procedure Clone_And_Count_Nodes
        (Cloned       : in out State_Array;
         Cloned_Count : out Natural)
      is
         procedure Internal (S : State);
         procedure Do_Transitions (First : Transition_Id);

         procedure Do_Transitions (First : Transition_Id) is
            T : Transition_Id := First;
         begin
            while T /= No_Transition loop
               declare
                  Tr : Transition renames Self.Transitions.Table (T);
               begin
                  if Tr.To_State = Final_State then
                     null;
                  else
                     Internal (Tr.To_State);
                  end if;
                  T := Tr.Next_For_State;
               end;
            end loop;
         end Do_Transitions;

         procedure Internal (S : State) is
         begin
            if S = Final_State then
               return;
            elsif Cloned (S) /= No_State then
               return;
            elsif S = From then
               Cloned (From) := To; --  Do not duplicate data or nested

               if Debug then
                  Put_Line ("From:Clone(" & From'Img & ") = "
                            & Cloned (From)'Img & " don't copy data");
               end if;
            else
               Cloned_Count := Cloned_Count + 1;
               Cloned (S) := Add_State (Self, Self.States.Table (S).Data);
               if Debug then
                  Put_Line ("Clone(" & S'Img & ") = " & Cloned (S)'Img);
               end if;
               Self.States.Table (Cloned (S)).Nested :=
                 Self.States.Table (S).Nested;

               if S = To then
                  return;  --  No need to examine transitions from [To]
               end if;
            end if;

            Do_Transitions (Self.States.Table (S).First_Transition);
         end Internal;
      begin
         Cloned_Count := 0;
         Internal (From);
      end Clone_And_Count_Nodes;

      -------------------------
      -- Complete_All_Clones --
      -------------------------

      function Complete_All_Clones
        (Cloned : State_Array; Cloned_Count : Natural; Max : Natural)
         return State
      is
         Tmp    : State;
      begin
         if Max <= 2 then
            if Min_Occurs = Max_Occurs
              or else Max_Occurs = Natural'Last
            then
               return Cloned (To);
            else
               return Add_Stateless (Cloned (To));
            end if;
         end if;

         --  Reserve immediately the space for all the other repetitions, to
         --  limit calls to malloc()
         Set_Last
           (Self.States,
            Last (Self.States) + State (Cloned_Count * (Max - 2)));

         for C in Cloned'Range loop
            if Cloned (C) /= No_State then
               for R in 1 .. Max - 2 loop
                  Tmp := Cloned (C) + State (R * Cloned_Count);

                  if C /= From then
                     Self.States.Table (Tmp) :=
                       State_Data'
                         (Nested           => Self.States.Table (C).Nested,
                          Data             => Self.States.Table (C).Data,
                          First_Transition => No_Transition);
                     if Debug then
                        Put_Line
                          ("Extra clone(" & C'Img & ") at " & Tmp'Img);
                     end if;

                  else
                     if Debug then
                        Tmp := Cloned (To) + State ((R - 1) * Cloned_Count);
                        Put_Line
                          ("Extra clone(" & C'Img & ") at " & Tmp'Img
                           & " (don't copy data)");
                     end if;
                  end if;
               end loop;
            end if;
         end loop;

         --  If needed, add an extra node at the end

         if Min_Occurs = Max_Occurs
           or else Max_Occurs = Natural'Last
         then
            return Cloned (To) + State ((Max - 2) * Cloned_Count);
         else
            return Add_Stateless
              (Cloned (To) + State ((Max - 2) * Cloned_Count));
         end if;
      end Complete_All_Clones;

      -----------------------
      -- Clone_Transitions --
      -----------------------

      procedure Clone_Transitions
        (Cloned : State_Array;
         Cloned_Count : Natural;
         New_To : State;
         Max    : Natural)
      is
         procedure Do_Transitions (S : State; First : Transition_Id);

         procedure Do_Transitions (S : State; First : Transition_Id) is
            T   : Transition_Id;
            Tmp : State;
            Offs1, Offs2 : State;
         begin
            T := First;
            while T /= No_Transition loop
               declare
                  --  Not a "renames", because Self.Transitions might be
                  --  resized within this loop
                  Tr : constant Transition := Self.Transitions.Table (T);
               begin
                  if Tr.To_State = Final_State then
                     Tmp := Final_State;
                     if Cloned (S) = Cloned (To) then
                        Tmp := No_State; --  No copy, will be done later
                     end if;
                  elsif Tr.To_State > Cloned'Last then
                     Tmp := No_State;  --  Link to the outside
                  else
                     Tmp := Cloned (Tr.To_State);
                  end if;

                  if Tmp /= No_State then
                     for R in 0 .. Max - 2 loop
                        if S = From then
                           --  Since the first clone of [From] is already in
                           --  the NFA, the computation of the following clones
                           --  is more complex: the user might have inserted
                           --  more states in the NFA after inserting [To], so
                           --  the clones are not in the same order

                           if R = 0 then
                              Offs1 := Cloned (From);
                           else
                              Offs1 :=
                                Cloned (To) + State ((R - 1) * Cloned_Count);
                           end if;

                        else
                           Offs1 := Cloned (S) + State (R * Cloned_Count);
                        end if;

                        Offs2 := Tmp + State (R * Cloned_Count);

                        case Tr.Kind is
                           when Transition_On_Exit_Empty =>
                              On_Empty_Nested_Exit (Self, Offs1, Offs2);
                           when Transition_On_Empty =>
                              if Debug then
                                 Put_Line
                                   ("Empty: from" & Offs1'Img
                                    & " to" & Offs2'Img & " R=" & R'Img);
                              end if;
                              Add_Empty_Transition (Self, Offs1, Offs2);
                           when Transition_On_Exit_Symbol =>
                              On_Nested_Exit (Self, Offs1, Offs2, Tr.Sym);
                           when Transition_On_Symbol =>
                              if Debug then
                                 Put_Line ("Trans: from" & Offs1'Img
                                           & " to" & Offs2'Img & " on "
                                           & Image (Tr.Sym));
                              end if;
                              Add_Transition (Self, Offs1, Offs2, Tr.Sym);
                        end case;
                     end loop;
                  end if;

                  T := Tr.Next_For_State;
               end;
            end loop;
         end Do_Transitions;

         Prev   : Transition_Id;
         T      : Transition_Id;
      begin
         for S in reverse Cloned'Range loop
            if Cloned (S) /= No_State then
               Do_Transitions (S, Self.States.Table (S).First_Transition);
            end if;
         end loop;

         --  Last pass to move external transition from [To] to [New_To],
         --  ie from the end of the sub-automaton

         Prev   := No_Transition;
         T      := Self.States.Table (To).First_Transition;

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
                     Self.States.Table (To).First_Transition :=
                       Tr.Next_For_State;
                  else
                     Self.Transitions.Table (Prev).Next_For_State :=
                       Tr.Next_For_State;
                  end if;

                  Tr.Next_For_State :=
                    Self.States.Table (New_To).First_Transition;
                  Self.States.Table (New_To).First_Transition := T;
               else
                  Prev := T;
               end if;

               T := Next;
            end;
         end loop;
      end Clone_Transitions;

      -------------------
      -- Add_Stateless --
      -------------------

      function Add_Stateless (New_To : State) return State is
         N : State := New_To;
      begin
         if Self.States_Are_Statefull then
            --  Add extra stateless node
            N := Add_State (Self);
            Add_Empty_Transition (Self, New_To, N);
         end if;
         return N;
      end Add_Stateless;

      N : State;

   begin
      if Debug then
         Put_Line ("Repeat" & From'Img & " to" & To'Img
                   & " Min,Max=" & Min_Occurs'Img & Max_Occurs'Img);
      end if;

      --  First the simple and usual cases (that cover the usual "*", "+" and
      --  "?" operators in regular expressions. It is faster to first handle
      --  those, since we don't need any additional new state for those.

      if Min_Occurs = 1 and then Max_Occurs = 1 then
         return To;  --  Nothing to do
      elsif Min_Occurs > Max_Occurs then
         return To;  --  As documented, nothing is done
      elsif Max_Occurs = 0 then
         Self.States.Table (From).First_Transition := No_Transition;
         Add_Empty_Transition (Self, From, To);
         return To;
      elsif Min_Occurs = 0 and then Max_Occurs = 1 then
         N := Add_Stateless (To);
         Add_Empty_Transition (Self, From, N);
         return N;
      elsif Min_Occurs = 1 and then Max_Occurs = Natural'Last then
         Add_Empty_Transition (Self, From => To, To => From);
         return To;
      elsif Min_Occurs = 0 and then Max_Occurs = Natural'Last then
         N := Add_Stateless (To);
         Add_Empty_Transition (Self, From, N);
         Add_Empty_Transition (Self, N, From);
         return N;
      end if;

      --  We now deal with the more complex cases (always Max_Occurs > 1)

      declare
         Cloned : State_Array := (others => No_State);
         Cloned_Count : Natural := 0;
         --  Number of nodes in the subautomaton to clone.

         New_To : State;

      begin
         Clone_And_Count_Nodes (Cloned, Cloned_Count);

         if Max_Occurs = Natural'Last then
            New_To := Complete_All_Clones (Cloned, Cloned_Count, Min_Occurs);
            Clone_Transitions (Cloned, Cloned_Count, New_To, Min_Occurs);

            if Min_Occurs > 2 then
               N := Cloned (To) + State ((Min_Occurs - 2) * Cloned_Count);
            elsif Min_Occurs = 2 then
               N := Cloned (From);
            else
               raise Program_Error;  --  cases 0..* and 1..* already handled
            end if;

            Add_Empty_Transition (Self, New_To, N);
            if Debug then
               Put_Line ("Empty trans from" & New_To'Img & " to" & N'Img);
            end if;

            return New_To;

         else
            New_To := Complete_All_Clones (Cloned, Cloned_Count, Max_Occurs);

            if Min_Occurs = 0 then
               Add_Empty_Transition (Self, From, New_To);
            end if;

            for R in Integer'Max (0, Min_Occurs - 1) .. Max_Occurs - 2 loop
               if R = 0 then
                  N := Cloned (From);
               else
                  N := Cloned (To) + State ((R - 1) * Cloned_Count);
               end if;

               if Debug then
                  Put_Line ("Empty trans from" & N'Img & " to" & New_To'Img);
               end if;
               Add_Empty_Transition (Self, N, New_To);
            end loop;

            Clone_Transitions (Cloned, Cloned_Count, New_To, Max_Occurs);
            return New_To;
         end if;
      end;
   end Repeat;

   ---------------
   -- Is_Active --
   ---------------

   function Is_Active
     (Self       : NFA_Matcher;
      List_Start : Matcher_State_Index;
      S          : State) return Boolean
   is
      T : Matcher_State_Index := List_Start;
   begin
      while T /= No_Matcher_State loop
         if Self.Active.Table (T).S = S then
            return True;
         end if;
         T := Self.Active.Table (T).Next;
      end loop;
      return False;
   end Is_Active;

   -----------------
   -- Mark_Active --
   -----------------

   procedure Mark_Active
     (Self         : in out NFA_Matcher;
      List_Start   : in out Matcher_State_Index;
      From         : State;
      First_Nested : Matcher_State_Index := No_Matcher_State)
   is
      T          : Transition_Id;
      From_Index : Matcher_State_Index;  --  Where we added [From]
      Tmp        : Matcher_State_Index;
   begin
      if Debug then
         Put_Line ("Mark_Active " & From'Img);
      end if;

      --  ??? Not very efficient, but the lists are expected to be short. We
      --  could try to use a state->boolean array, but then we need one such
      --  array for all nested NFA, which requires a lot of storage.

      if Is_Active (Self, List_Start, From) then
         return;
      end if;
      --  Always leave the Final_State first in the list

      if List_Start /= No_Matcher_State
        and then Self.Active.Table (List_Start).S = Final_State
      then
         Self.Active.Table (List_Start).S      := From;
         Self.Active.Table (List_Start).Nested := First_Nested;
         From_Index := List_Start;
         Append
           (Self.Active,
            Matcher_State'
              (S      => Final_State,
               Next   => List_Start,
               Nested => No_Matcher_State));
      else
         Append
           (Self.Active,
            Matcher_State'
              (S      => From,
               Next   => List_Start,
               Nested => First_Nested));
         From_Index := Last (Self.Active);
      end if;

      List_Start := Last (Self.Active);

      --  Mark (recursively) all states reachable from an empty transition
      --  as active too.

      if From /= Final_State then
         T := Self.NFA.States.Table (From).First_Transition;
         while T /= No_Transition loop
            declare
               Tr : Transition renames Self.NFA.Transitions.Table (T);
            begin
               if Tr.Kind = Transition_On_Empty then
                  Mark_Active (Self, List_Start, Tr.To_State);
               end if;
               T := Tr.Next_For_State;
            end;
         end loop;

         --  If we are entering any state with a nested NFA, we should activate
         --  that NFA next turn (unless the nested NFA is already active)

         if Self.NFA.States.Table (From).Nested /= No_State
           and then Self.Active.Table (From_Index).Nested = No_Matcher_State
         then
            --  We can't pass directly Self.Active.Table (From_Index) as a
            --  parameter to Mark_Active: if the table Self.Active is
            --  reallocated during that call, the address we passed becomes
            --  invalid, and as a result the table is not updated and we might
            --  event get a storage_error.

            Tmp := Self.Active.Table (From_Index).Nested;
            Mark_Active
              (Self,
               List_Start => Tmp,
               From       => Self.NFA.States.Table (From).Nested);
            Self.Active.Table (From_Index).Nested := Tmp;
         end if;
      end if;
   end Mark_Active;

   -----------------
   -- Start_Match --
   -----------------

   function Start_Match
     (Self : access NFA; Start_At : State := Start_State) return NFA_Matcher is
   begin
      return Start_Match (Self, S => Start_At);
   end Start_Match;

   -----------------
   -- Start_Match --
   -----------------

   function Start_Match
     (Self : access NFA'Class; S : State) return NFA_Matcher
   is
      R : NFA_Matcher;
   begin
      R.NFA          := NFA_Access (Self);
      R.First_Active := No_Matcher_State;
      Init (R.Active);
      Mark_Active (R, R.First_Active, S);
      return R;
   end Start_Match;

   ---------------------------
   -- For_Each_Active_State --
   ---------------------------

   procedure For_Each_Active_State
     (Self             : NFA_Matcher;
      Ignore_If_Nested : Boolean := False;
      Ignore_If_Default : Boolean := False)
   is
      S2 : State;
   begin
      for S in 1 .. Last (Self.Active) loop
         S2 := Self.Active.Table (S).S;

         if S2 /= Final_State then
            --  Either we have no nested automaton
            --  Or we always want to return the states anyway
            --  Or the nested state has completed

            if not Ignore_If_Nested
              or else Self.Active.Table (S).Nested = No_Matcher_State
              or else Self.Active.Table (Self.Active.Table (S).Nested).S =
                Final_State
            then
               if not Ignore_If_Default
                 or else Self.NFA.States.Table (S2).Data /= Default_Data
               then
                  Callback (Self.NFA, S2);
               end if;
            end if;
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
      NFA   : constant NFA_Access := Self.NFA;
      Saved : constant Matcher_State_Arrays.Table_Type :=
        Self.Active.Table (1 .. Last (Self.Active));
      Saved_First_Active : constant Matcher_State_Index := Self.First_Active;

      procedure Process_Level
        (First     : Matcher_State_Index;
         New_First : in out Matcher_State_Index;
         Success   : out Boolean);
      --  Process all the nodes with a common parent (either all toplevel
      --  states, or all nested states within a specific state).

      type Transition_Filter is array (Transition_Kind) of Boolean;

      procedure Process_Transitions
        (From_State   : State;
         New_First    : in out Matcher_State_Index;
         Filter       : Transition_Filter);
      --  Check all transitions from [First]

      -------------------------
      -- Process_Transitions --
      -------------------------

      procedure Process_Transitions
        (From_State   : State;
         New_First    : in out Matcher_State_Index;
         Filter       : Transition_Filter)
      is
         T : Transition_Id := NFA.States.Table (From_State).First_Transition;
      begin
         while T /= No_Transition loop
            declare
               Tr : Transition renames NFA.Transitions.Table (T);
            begin
               if Filter (Tr.Kind) then
                  case Tr.Kind is
                     when Transition_On_Empty | Transition_On_Exit_Empty =>
                        Mark_Active (Self, New_First, Tr.To_State);
                     when others =>
                        if not Is_Active (Self, New_First, Tr.To_State)
                          and then Match (Self.NFA, From_State, Tr.Sym, Input)
                        then
                           Mark_Active (Self, New_First, Tr.To_State);
                        end if;
                  end case;
               end if;

               T := Tr.Next_For_State;
            end;
         end loop;
      end Process_Transitions;

      -------------------
      -- Process_Level --
      -------------------

      procedure Process_Level
        (First     : Matcher_State_Index;
         New_First : in out Matcher_State_Index;
         Success   : out Boolean)
      is
         N                : Matcher_State_Index := First;
         Event_Processed_In_Nested : Boolean;
         Nested_Final     : Boolean;
         S                : Matcher_State;
         Nested_First     : Matcher_State_Index;
         At_Current_Level : Matcher_State_Index;

      begin
         --  For each currently live state:
         --   - if there are nested NFA, we process these first. If the event
         --     is processed by them, it will not be passed on to the
         --     corresponding super state (event bubbling stopped).
         --   - if there are no nested NFA, or they did not process the event,
         --     the event is then processed directly by the super state.
         --  This corresponds to standard semantics of event bubbling in
         --  hierarchical NFA.

         while N /= No_Matcher_State loop
            S := Saved (N);
            Event_Processed_In_Nested := False;
            Nested_Final := True;

            if S.Nested /= No_Matcher_State then
               declare
                  Tmp : Matcher_State_Index := New_First;
               begin
                  At_Current_Level := No_Matcher_State;
                  while Tmp /= No_Matcher_State loop
                     if Self.Active.Table (Tmp).S = S.S then
                        At_Current_Level := Tmp;
                        exit;
                     end if;

                     Tmp := Self.Active.Table (Tmp).Next;
                  end loop;
               end;

               if At_Current_Level /= No_Matcher_State then
                  Process_Level
                    (First     => S.Nested,
                     New_First => Self.Active.Table (At_Current_Level).Nested,
                     Success   => Success);
                  Nested_First := Self.Active.Table (At_Current_Level).Nested;

               else
                  Nested_First := No_Matcher_State;
                  Process_Level (First     => S.Nested,
                                 New_First => Nested_First,
                                 Success   => Success);
               end if;

               if Success then
                  --  Exits the nested NFA, and thus transitions from its super
                  --  state. The super state itself is terminated.
                  --  ??? Should the superstate remain active, in case it has
                  --  standard transitions ?

                  Nested_Final := Nested_In_Final (Self, Nested_First);
                  Event_Processed_In_Nested := True;

                  if Nested_Final then
                     Process_Transitions
                       (S.S, New_First,
                        Filter => (Transition_On_Exit_Empty => True,
                                   Transition_On_Exit_Symbol => True,
                                   others => False));
                  else
                     Mark_Active (Self, New_First, S.S, Nested_First);
                  end if;

               else
                  Nested_Final := False;
                  --  Error: nothing matches anymore in the nested NFA. We
                  --  terminate it, but keep processing this event in its
                  --  superstate (for instance, a camera in state "on" has a
                  --  nested NFA "record"<->"play"). If the nested receives
                  --  the event "turn off", it won't match the nested, but
                  --  that's not an error because the event is handled by
                  --  the super state "on".
               end if;
            end if;

            if S.S /= Final_State
              and then not Event_Processed_In_Nested
            then
               Process_Transitions
                 (S.S, New_First,
                  Filter => (Transition_On_Empty => False,
                             Transition_On_Symbol => True,
                             others => False));
            end if;

            N := S.Next;
         end loop;

         Success := New_First /= No_Matcher_State;
      end Process_Level;

   begin
      --  Reset the matcher

      Set_Last (Self.Active, No_Matcher_State);
      Self.First_Active := No_Matcher_State;
      Process_Level (Saved_First_Active, Self.First_Active, Success);

      if not Success then
         Set_Last (Self.Active, Saved'Last);
         Self.Active.Table (1 .. Saved'Last) := Saved;
         Self.First_Active := Saved_First_Active;
      end if;
   end Process;

   ---------------------
   -- Nested_In_Final --
   ---------------------

   function Nested_In_Final
     (Self : NFA_Matcher;
      S    : Matcher_State_Index) return Boolean is
   begin
      return S = No_Matcher_State
        or else Self.Active.Table (S).S = Final_State;
   end Nested_In_Final;

   --------------
   -- Expected --
   --------------

   function Expected (Self : NFA_Matcher) return String is
      Msg : Unbounded_String;

      procedure Callback (The_NFA : access NFA'Class; S : State);
      procedure Callback (The_NFA : access NFA'Class; S : State) is
         T : Transition_Id := The_NFA.States.Table (S).First_Transition;
      begin
         while T /= No_Transition loop
            declare
               Tr : Transition renames The_NFA.Transitions.Table (T);
            begin
               case Tr.Kind is
                  when Transition_On_Empty
                     | Transition_On_Exit_Empty
                     | Transition_On_Exit_Symbol =>
                     null;

                  when Transition_On_Symbol =>
                     if Msg /= Null_Unbounded_String then
                        Append (Msg, "|");
                     end if;

                     Append (Msg, Image (Tr.Sym));
               end case;

               T := Tr.Next_For_State;
            end;
         end loop;
      end Callback;

      procedure For_All_Active is new For_Each_Active_State (Callback);

   begin
      For_All_Active (Self);
      return To_String (Msg);
   end Expected;

   --------------
   -- In_Final --
   --------------

   function In_Final (Self : NFA_Matcher) return Boolean is
   begin
      return Nested_In_Final (Self, Self.First_Active);
   end In_Final;

   -------------------
   -- Create_Nested --
   -------------------

   function Create_Nested
     (Self : access NFA'Class; From : State) return Nested_NFA
   is
      pragma Unreferenced (Self);
   begin
      if Debug then
         Put_Line ("E := Create_Nested (" & From'Img & ")");
      end if;
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
      Append
        (Self.Transitions,
         Transition'
           (Kind           => Transition_On_Exit_Symbol,
            To_State       => To,
            Next_For_State => Self.States.Table (From).First_Transition,
            Sym            => On_Symbol));
      Self.States.Table (From).First_Transition := Last (Self.Transitions);
   end On_Nested_Exit;

   --------------------------
   -- On_Empty_Nested_Exit --
   --------------------------

   procedure On_Empty_Nested_Exit
     (Self      : access NFA;
      From      : State;
      To        : State) is
   begin
      Append
        (Self.Transitions,
         Transition'
           (Kind           => Transition_On_Exit_Empty,
            To_State       => To,
            Next_For_State => Self.States.Table (From).First_Transition));
      Self.States.Table (From).First_Transition := Last (Self.Transitions);
   end On_Empty_Nested_Exit;

   ----------------
   -- Set_Nested --
   ----------------

   procedure Set_Nested (Self : access NFA; S : State; Nested : Nested_NFA) is
   begin
      if Debug then
         Put_Line
           ("Set_Nested (" & S'Img & "," & Nested.Default_Start'Img & ")");
      end if;
      Self.States.Table (S).Nested := Nested.Default_Start;
   end Set_Nested;

   ----------------
   -- Get_Nested --
   ----------------

   function Get_Nested (Self : access NFA; S : State) return Nested_NFA is
   begin
      return Nested_NFA'(Default_Start => Self.States.Table (S).Nested);
   end Get_Nested;

   -------------------
   -- Default_Image --
   -------------------

   function Default_Image
     (Self : access NFA'Class; S : State; Data : State_User_Data) return String
   is
      pragma Unreferenced (Self, Data);
      Str : constant String := State'Image (S);
   begin
      return "S" & Str (Str'First + 1 .. Str'Last);
   end Default_Image;

   ---------------------
   -- Pretty_Printers --
   ---------------------

   package body Pretty_Printers is

      function Node_Label
        (Self : access NFA'Class;
         S    : State) return String;
      function Node_Name
        (Self : access NFA'Class; S : State; Nested_In : State := No_State)
         return String;
      procedure Append_Node
        (Self : access NFA'Class;
         S : State; R : in out Unbounded_String;
         Nested_In : State := No_State);

      ---------------
      -- Node_Name --
      ---------------

      function Node_Name
        (Self : access NFA'Class; S : State; Nested_In : State := No_State)
         return String is
      begin
         if S = Start_State then
            return "Start";
         elsif S = Final_State then
            if Nested_In /= No_State then
               return "Sf" & Node_Name (Self, Nested_In);
            else
               return "Sf";
            end if;
         else
            return Default_Image (Self, S, Default_Data);
         end if;
      end Node_Name;

      ----------------
      -- Node_Label --
      ----------------

      function Node_Label
        (Self : access NFA'Class;
         S    : State) return String is
      begin
         if S = Start_State then
            return "Start";
         elsif S = Final_State then
            return "Final";
         else
            declare
               Img : constant String :=
                 State_Image (Self, S, Self.States.Table (S).Data);
            begin
               if Img = "" then
                  if Self.States.Table (S).Nested /= No_State then
                     return Node_Name (Self, S)
                       & ":" & Node_Label (Self, Self.States.Table (S).Nested);
                  else
                     return Node_Name (Self, S);
                  end if;
               else
                  if Self.States.Table (S).Nested /= No_State then
                     return Node_Name (Self, S) & "_" & Img
                       & ":" & Node_Label (Self, Self.States.Table (S).Nested);
                  else
                     return Node_Name (Self, S) & "_" & Img;
                  end if;
               end if;
            end;
         end if;
      end Node_Label;

      -----------------
      -- Append_Node --
      -----------------

      procedure Append_Node
        (Self : access NFA'Class;
         S : State; R : in out Unbounded_String;
         Nested_In : State := No_State)
      is
         Name  : constant String := Node_Name (Self, S, Nested_In);
         Label : constant String := Node_Label (Self, S);
      begin
         Append (R, Name);
         if Label /= Name then
            if S = Start_State
              or else S = Final_State
              or else S = Nested_In
            then
               if Label /= "" then
                  Append (R, "[label=""" & Label & """ shape=doublecircle];");
               else
                  Append (R, "[shape=doublecircle];");
               end if;
            elsif Label /= "" then
               Append (R, "[label=""" & Label & """];");
            else
               Append (R, ";");
            end if;
         else
            if S = Start_State or else S = Nested_In then
               Append (R, "[shape=doublecircle];");
            else
               Append (R, ";");
            end if;
         end if;
      end Append_Node;

      ----------
      -- Dump --
      ----------

      function Dump
        (Self   : access NFA'Class;
         Nested : Nested_NFA;
         Mode   : Dump_Mode := Dump_Compact) return String
      is
         Dumped : array (State_Tables.First .. Last (Self.States)) of Boolean
           := (others => False);
         Result : Unbounded_String;

         procedure Internal (S : State);

         procedure Internal (S : State) is
            T : Transition_Id;
         begin
            if Dumped (S) then
               return;
            end if;

            Dumped (S) := True;
            Append (Result, " " & Node_Label (Self, S));

            T := Self.States.Table (S).First_Transition;
            while T /= No_Transition loop
               declare
                  Tr : Transition renames Self.Transitions.Table (T);
               begin
                  case Tr.Kind is
                     when Transition_On_Empty =>
                        Append (Result, "(");
                     when Transition_On_Exit_Empty =>
                        Append (Result, "(Exit");
                     when Transition_On_Symbol =>
                        Append (Result, "(" & Image (Tr.Sym));
                     when Transition_On_Exit_Symbol =>
                        Append (Result, "(Exit_" & Image (Tr.Sym));
                  end case;

                  Append (Result, "," & Node_Name (Self, Tr.To_State) & ")");

                  T := Tr.Next_For_State;
               end;
            end loop;

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

            if Self.States.Table (S).Nested /= No_State
              and then not Dumped (Self.States.Table (S).Nested)
            then
               Internal (Self.States.Table (S).Nested);
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
        (Self                : access NFA'Class;
         Mode                : Dump_Mode := Dump_Compact;
         Show_Details        : Boolean := True;
         Show_Isolated_Nodes : Boolean := True;
         Since               : NFA_Snapshot := No_NFA_Snapshot) return String
      is
         Dumped : array (State_Tables.First .. Last (Self.States)) of Boolean
           := (others => False);

         Result : Unbounded_String;

         procedure Newline;
         --  Append a newline to [Result] if needed

         procedure Dump_Dot (Start_At, Nested_In : State; Prefix : String);
         procedure Dump_Dot_Transitions
           (S : State; First : Transition_Id; Prefix : String;
            Nested_In : State := No_State);

         procedure Dump_Nested (S : State);
         --  Dump a cluster that represents a nested NFA.
         --  Such nested NFAs are represented only once, even though they can
         --  in fact be nested within several nodes. That would make huge
         --  graphs otherwise.

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

         --------------------------
         -- Dump_Dot_Transitions --
         --------------------------

         procedure Dump_Dot_Transitions
           (S : State; First : Transition_Id; Prefix : String;
            Nested_In : State := No_State)
         is
            T : Transition_Id := First;
         begin
            while T /= No_Transition loop
               declare
                  Tr : Transition renames Self.Transitions.Table (T);
               begin
                  if Tr.To_State > Since.States then
                     Append (Result,
                             Prefix & Node_Name (Self, S, Nested_In)
                             & "->" & Node_Name (Self, Tr.To_State, Nested_In)
                             & "[");

                     case Tr.Kind is
                     when Transition_On_Symbol =>
                        Append (Result, "label=""" & Image (Tr.Sym) & """");
                     when Transition_On_Exit_Symbol =>
                        Append
                          (Result, "label=""on_exit:" & Image (Tr.Sym)
                           & """ style=dotted");
                     when Transition_On_Empty =>
                        Append (Result, "style=dashed");
                        null;
                     when Transition_On_Exit_Empty =>
                        Append (Result, "label=on_exit style=dotted");
                     end case;

                     Append (Result, "];");
                     Newline;

                     if Tr.To_State /= Final_State then
                        Dump_Dot (Tr.To_State, Nested_In, Prefix);
                     end if;
                  end if;

                  T := Tr.Next_For_State;
               end;
            end loop;
         end Dump_Dot_Transitions;

         -----------------
         -- Dump_Nested --
         -----------------

         procedure Dump_Nested (S : State) is
            Name  : constant String := Node_Name (Self, S);
            Label : constant String := Node_Label (Self, S);
         begin
            if S > Since.States then
               Append (Result, "subgraph cluster" & Name & "{");
               Newline;
               Append (Result, " label=""" & Label & """;");
               Newline;
               Append_Node (Self, S, Result, S);
               Append_Node (Self, Final_State, Result, S);

               Dump_Dot (S, Prefix => " ", Nested_In => S);

               Append (Result, "};");
               Newline;
            end if;
         end Dump_Nested;

         --------------
         -- Dump_Dot --
         --------------

         procedure Dump_Dot (Start_At, Nested_In : State; Prefix : String) is
         begin
            if Start_At = Final_State
              or else Dumped (Start_At)
            then
               return;
            end if;

            Dumped (Start_At) := True;
            Dump_Dot_Transitions
              (Start_At,
               Self.States.Table (Start_At).First_Transition, Prefix,
               Nested_In);
         end Dump_Dot;

      begin
         Append (Result, "Total states:" & Last (Self.States)'Img
                 & ASCII.LF);
         Append (Result, "Total transitions:" & Last (Self.Transitions)'Img
                 & ASCII.LF);

         if Since /= No_NFA_Snapshot then
            Append (Result, "Dump since " & Since.States'Img & ASCII.LF);
         end if;

         if not Show_Details then
            return To_String (Result);
         end if;

         case Mode is
         when Dump_Multiline | Dump_Compact =>
            return Dump (Self   => Self,
                         Nested => (Default_Start => Start_State),
                         Mode   => Mode);

         when Dump_Dot | Dump_Dot_Compact =>
            Append (Result, "Use   dot -O -Tpdf file.dot" & ASCII.LF);
            Append (Result, "digraph finite_state_machine{");
            Newline;
            Append (Result, "compound=true;");
            Newline;
            Append (Result, "rankdir=LR;");
            Newline;
            Append_Node (Self, Start_State, Result);
            Append_Node (Self, Final_State, Result);

            --  First, create all the clusters for the nested NFA. That helps
            --  remove their states from the global lists, so that we can then
            --  only dump the toplevel states

            for S in Since.States + 1 .. Last (Self.States) loop
               if Self.States.Table (S).Nested /= No_State then
                  Dump_Nested (Self.States.Table (S).Nested);
               end if;
            end loop;

            --  Now dump the labels for all nodes. These do not need to go
            --  into the clusters, as long as the nodes where first encountered
            --  there

            for S in Since.States + 1 .. Last (Self.States) loop
               if Show_Isolated_Nodes
                  or else Self.States.Table (S).Nested /= No_State
                  or else Self.States.Table (S).First_Transition /=
                    No_Transition
               then
                  Append_Node (Self, S, Result);
               end if;
            end loop;

            --  Now dump the toplevel states (that is the ones that haven't
            --  been dumped yet)

            Dump_Dot (Start_State, No_State, "");  --  Always

            for S in Since.States + 1 .. Last (Self.States) loop
               if S /= Start_State and then
                 (Show_Isolated_Nodes
                  or else Self.States.Table (S).Nested /= No_State
                  or else Self.States.Table (S).First_Transition /=
                    No_Transition)
               then
                  Dump_Dot (S, No_State, "");
               end if;
            end loop;

            Append (Result, "}" & ASCII.LF);
         end case;

         return To_String (Result);
      end Dump;

      -----------------
      -- Debug_Print --
      -----------------

      procedure Debug_Print
        (Self   : NFA_Matcher;
         Mode   : Dump_Mode := Dump_Multiline;
         Prefix : String := "")
      is
         NFA : constant NFA_Access := Self.NFA;

         procedure Internal (From : Matcher_State_Index; Prefix : String);

         procedure Internal (From : Matcher_State_Index; Prefix : String) is
            F : Matcher_State_Index := From;
         begin
            while F /= No_Matcher_State loop
               Put (Node_Label (NFA, Self.Active.Table (F).S));

               if Self.Active.Table (F).Nested /= No_Matcher_State then
                  if Mode = Dump_Multiline then
                     New_Line;
                  end if;
                  Put (Prefix & " [");

                  if Mode = Dump_Multiline then
                     Internal (Self.Active.Table (F).Nested, Prefix & "  ");
                  else
                     Internal (Self.Active.Table (F).Nested, Prefix);
                  end if;

                  Put ("]");
               end if;

               F := Self.Active.Table (F).Next;

               if F /= No_Matcher_State then
                  Put (" ");
               end if;
            end loop;
         end Internal;

      begin
         if Self.First_Active = No_Matcher_State then
            Put_Line (Prefix & "[no active state]");
         else
            Put (Prefix);
            Internal (Self.First_Active, "");
            New_Line;
         end if;
      end Debug_Print;
   end Pretty_Printers;

   ---------------------
   -- Get_Start_State --
   ---------------------

   function Get_Start_State (Self : Nested_NFA) return State is
   begin
      return Self.Default_Start;
   end Get_Start_State;

   ------------------
   -- Get_Snapshot --
   ------------------

   function Get_Snapshot (Self : access NFA) return NFA_Snapshot is
   begin
      return (States      => Last (Self.States),
              Transitions => Last (Self.Transitions),
              Start_State_Transition =>
                Self.States.Table (Start_State).First_Transition);
   end Get_Snapshot;

   -----------------------
   -- Reset_To_Snapshot --
   -----------------------

   procedure Reset_To_Snapshot (Self : access NFA; Snapshot : NFA_Snapshot) is
   begin
      if Snapshot /= No_NFA_Snapshot then
         Set_Last (Self.States, Snapshot.States);
         Set_Last (Self.Transitions, Snapshot.Transitions);
         Self.States.Table (Start_State).First_Transition :=
           Snapshot.Start_State_Transition;
      end if;
   end Reset_To_Snapshot;

   ------------
   -- Exists --
   ------------

   function Exists (Snapshot : NFA_Snapshot; S : State) return Boolean is
   begin
      return S <= Snapshot.States;
   end Exists;

end Sax.State_Machines;
