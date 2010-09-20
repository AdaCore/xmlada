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
     (Self     : access NFA'Class;
      Active   : in out Boolean_State_Array;
      In_Final : in out Boolean;
      From     : State);
   --  Mark [From] as active next time, as well as all states reachable
   --  through an empty transition.

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : in out NFA) is
   begin
      Init (Self.States);
      Init (Self.Transitions);

      --  Create start state
      Append (Self.States, State_Data'(First_Transition => No_Transition));
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
        (NFA_Matcher_Record, NFA_Matcher);
   begin
      if Self /= null then

         Unchecked_Free (Self);
      end if;
   end Free;

   ---------------
   -- Add_State --
   ---------------

   function Add_State (Self : access NFA) return State is
   begin
      Append (Self.States, State_Data'(First_Transition => No_Transition));
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
            Self.States.Table (To) := (First_Transition => No_Transition);

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
     (Self     : access NFA'Class;
      Active   : in out Boolean_State_Array;
      In_Final : in out Boolean;
      From     : State)
   is
      T : Transition_Id := Self.States.Table (From).First_Transition;
   begin
      Active (From) := True;

      --  Mark (recursively) all states reachable from an empty transition
      --  as active too.

      while T /= No_Transition loop
         declare
            Tr : Transition renames Self.Transitions.Table (T);
         begin
            if Tr.Is_Empty then
               if Tr.To_State = Final_State then
                  In_Final := True;

               elsif not Active (Tr.To_State) then
                  Mark_Active (Self, Active, In_Final, Tr.To_State);
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
      R : NFA_Matcher := new NFA_Matcher_Record (Last => Last (Self.States));
   begin
      R.NFA      := NFA_Access (Self);
      R.Current  := (others => False);
      R.In_Final := False;
      Mark_Active (Self, R.Current, R.In_Final, Start_State);
      return R;
   end Start_Match;

   -------------
   -- Process --
   -------------

   procedure Process
     (Self    : in out NFA_Matcher;
      Input   : Symbol;
      Success : out Boolean)
   is
      T    : Transition_Id;
      Next : Boolean_State_Array (Self.Current'Range) := (others => False);
      Has_Next  : Boolean := False;
      In_Final  : Boolean := False;

   begin
      for S in Self.Current'Range loop
         if Self.Current (S) then  --  If state [S] is active
            T := Self.NFA.States.Table (S).First_Transition;
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
                        if not In_Final and then Match (Tr.Sym, Input) then
                           In_Final := True;
                           Has_Next := True;
                        end if;

                     elsif not Next (Tr.To_State)
                       and then Match (Tr.Sym, Input)
                     then
                        Has_Next := True;
                        Mark_Active (Self.NFA, Next, In_Final, Tr.To_State);
                     end if;
                  end if;

                  T := Tr.Next_For_State;
               end;
            end loop;
         end if;
      end loop;

      if not Has_Next then
         --  Do not change Self
         Success := False;
      else
         Success       := True;
         Self.In_Final := In_Final;
         Self.Current  := Next;
      end if;
   end Process;

   --------------
   -- Expected --
   --------------

   function Expected (Self : NFA_Matcher) return String is
      Msg : Unbounded_String;
      T    : Transition_Id;

   begin
      for S in Self.Current'Range loop
         if Self.Current (S) then  --  If state [S] is active
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

   ----------
   -- Dump --
   ----------

   function Dump
     (Self : access NFA'Class; Compact : Boolean := False) return String
   is
      T : Transition_Id;
      Result : Unbounded_String;
   begin
      Append (Result, "Transitions:" & Last (Self.Transitions)'Img & " ");

      if not Compact then
         Append (Result, ASCII.LF);
      end if;

      for S in State_Tables.First .. Last (Self.States) loop
         if S = Start_State then
            Append (Result, "<start> ");
         else
            Append (Result, S'Img & " ");
         end if;

         T := Self.States.Table (S).First_Transition;
         while T /= No_Transition loop
            declare
               Tr : Transition renames Self.Transitions.Table (T);
            begin
               if Tr.Is_Empty then
                  Append (Result, "(<>");
               else
                  Append (Result, "(" & Image (Tr.Sym));
               end if;

               if Tr.To_State = Final_State then
                  Append (Result, ",<final>)");
               else
                  Append (Result, "," & Tr.To_State'Img & ")");
               end if;

               T := Tr.Next_For_State;
            end;
         end loop;

         if not Compact then
            Append (Result, ASCII.LF);
         end if;
      end loop;

      return To_String (Result);
   end Dump;

   -----------------
   -- Debug_Print --
   -----------------

   procedure Debug_Print (Self : NFA_Matcher) is
   begin
      Put ("Active states:");
      for C in Self.Current'Range loop
         if Self.Current (C) then
            Put (C'Img);
         end if;
      end loop;
      New_Line;
   end Debug_Print;

end Sax.State_Machines;
