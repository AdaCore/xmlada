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

with Unchecked_Deallocation;
with Ada.Exceptions; use Ada.Exceptions;

package body Generic_State_Machine is

   procedure Free is new Unchecked_Deallocation
     (Transition_Record, Transition);
   procedure Free is new Unchecked_Deallocation
     (Transitions_Array, Transitions_Array_Ptr);

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Automaton : in out State_Machine) is
   begin
      Reset (Automaton);
   end Finalize;

   -----------------------
   -- Set_Current_State --
   -----------------------

   procedure Set_Current_State (Automaton : in out State_Machine;
                                State_Num : Automaton_State)
   is
   begin
      --  Exit the current sub-automaton
      if State_Num = Exit_Sub_Automaton then
         if Automaton.Current_Automaton = 1 then
            raise Subautomaton_Error;
         end if;
         Automaton.Current_Automaton := Automaton.Current_Automaton - 1;
         Automaton.Current_State
           := Automaton.Sub_Automatons (Automaton.Current_Automaton);

      --  Start a sub-automaton
      elsif State_Num < 0 then
         if Automaton.Current_Automaton = Max_Automaton_Depth then
            raise Subautomaton_Error;
         end if;
         Automaton.Sub_Automatons (Automaton.Current_Automaton)
           := Automaton.Current_State;
         Automaton.Current_Automaton := Automaton.Current_Automaton + 1;
         Automaton.Current_State := -State_Num;

      --  Simple transition
      else
         Automaton.Current_State := State_Num;
      end if;
   end Set_Current_State;

   -----------
   -- Reset --
   -----------

   procedure Reset (Automaton : in out State_Machine) is
      Transitions : Transition;
      Tmp_Trans   : Transition;
   begin
      if Automaton.States /= null then
         for State in Automaton.States.all'Range loop
            Transitions := Automaton.States (State);
            while Transitions /= null loop
               Tmp_Trans := Transitions.Next;
               Free (Transitions);
                  Transitions := Tmp_Trans;
            end loop;
         end loop;
      end if;

      Free (Automaton.States);
      Automaton.Current_Automaton := 1;
      Automaton.Current_State     := 0;
   end Reset;

   -------------
   -- Compile --
   -------------

   procedure Compile (Automaton     : in out State_Machine;
                      Highest_State : in Automaton_State;
                      Initial_State : in Automaton_State;
                      Def           : in Automaton_Def) is
   begin
      Reset (Automaton);
      Automaton.States := new Transitions_Array (0 .. Highest_State);
      Automaton.Current_State := Initial_State;

      for Index in Def'Range loop
         Add_Transition (Automaton,
                         Def (Index).Initial_State,
                         Def (Index).Event,
                         Def (Index).Final_State,
                         Def (Index).Func);
      end loop;
   end Compile;

   ----------------
   -- Send_Event --
   ----------------

   procedure Send_Event (Automaton : in out State_Machine;
                         Event     : Event_Type)
   is
      Trans : Transition := Automaton.States (Automaton.Current_State);
   begin
      while Trans /= null loop

         if Equal (Event, Trans.Event)
           or else Is_Any_Event (Trans.Event)
         then
            Set_Current_State (Automaton, Trans.Final_State);
            if Trans.Func /= null then
               Trans.Func (Automaton, Event);
            end if;
            return;
         end if;

         Trans := Trans.Next;
      end loop;

      Raise_Exception
        (E => Invalid_Syntax'Identity,
         Message => "Transition from state" & Automaton.Current_State'Img
           & " invalid");
   end Send_Event;

   --------------------
   -- Add_Transition --
   --------------------

   procedure Add_Transition (Automaton     : in out State_Machine;
                             Initial_State : Automaton_State;
                             Event         : Event_Type;
                             Final_State   : Automaton_State;
                             Func          : Transition_Function)
   is
      Trans,
      Prev_Trans : Transition;
      Final      : Automaton_State := Final_State;
   begin
      if Final_State < 0 then
         Final := -Final_State;
      end if;
      if not (Initial_State in Automaton.States'Range)
        or else not (Final in Automaton.States'Range)
      then
         raise Invalid_State;
      end if;

      --  Check if the transition already exists
      Trans := Automaton.States (Initial_State);
      while Trans /= null loop
         if Trans.Event = Event then
            raise Transition_Exists;
         end if;
         Prev_Trans := Trans;
         Trans := Trans.Next;
      end loop;

      --  Add the new transition at the end of the list
      if Is_Any_Event (Event)
        and then Prev_Trans /= null
      then
         Prev_Trans.Next
           := new Transition_Record'
           (Event       => Event,
            Final_State => Final_State,
            Func        => Func,
            Next        => null);

         --  Or at the beginning of the list
      else
         Automaton.States (Initial_State)
           := new Transition_Record'
           (Event       => Event,
            Final_State => Final_State,
            Func        => Func,
            Next        => Automaton.States (Initial_State));
      end if;
   end Add_Transition;

   -----------------------
   -- Get_Current_State --
   -----------------------

   function Get_Current_State (Automaton : State_Machine)
      return Automaton_State is
   begin
      return Automaton.Current_State;
   end Get_Current_State;

end Generic_State_Machine;
