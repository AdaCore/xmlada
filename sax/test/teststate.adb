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

pragma Ada_05;

with Ada.Text_IO;         use Ada.Text_IO;
with Sax.State_Machines;

procedure TestState is

   type Transition_Kind is
     (Any_Char,
      Char);

   type Transition_Descr (Kind : Transition_Kind := Char) is record
      case Kind is
         when Char   => C : Character;
         when others => null;
      end case;
   end record;

   function Match (Trans : Transition_Descr; Input : Character) return Boolean;
   function Image (Trans : Transition_Descr) return String;
   --  To instantiation Sax.State_Machines

   function Match
     (Trans : Transition_Descr; Input : Character) return Boolean is
   begin
      case Trans.Kind is
         when Any_Char => return True;
         when Char     => return Trans.C = Input;
      end case;
   end Match;

   function Image (Trans : Transition_Descr) return String is
   begin
      case Trans.Kind is
         when Any_Char => return "<.>";
         when Char     => return "" & Trans.C;
      end case;
   end Image;

   package Character_Machines is new Sax.State_Machines
     (Symbol            => Character,
      Transition_Symbol => Transition_Descr,
      Image             => Image,
      Match             => Match);
   use Character_Machines;

   procedure Display_Result (Msg, Str : String; S : Positive);
   --  Display the result of a test

   procedure Assert (Str1, Str2 : String; Msg : String);
   --  Compare Str1 and Str2 and display error if they are not equal

   procedure Assert (Msg : String;
                     N : NFA_Access; Str : String; Final : Boolean := True);
   --  Check that when processing [Str], [N] ends up in either a temporary
   --  valid state, or in a final state (if [In_Final] is True).

   procedure Assert_Error
     (Msg : String;
      N : NFA_Access; Str : String; At_Char : Natural; Error : String);
   --  Check that we end up with an error on processing the [At_Char]-th
   --  character of [Str]

   procedure Test1;
   procedure Test2;
   procedure Test3;
   procedure Test4;
   --  Various tests

   ------------
   -- Assert --
   ------------

   procedure Assert (Str1, Str2 : String; Msg : String) is
   begin
      if Str1 /= Str2 then
         Put_Line ("ERROR: " & Msg);
         Put_Line ("- " & Str1);
         Put_Line ("+ " & Str2);
      end if;
   end Assert;

   --------------------
   -- Display_Result --
   --------------------

   procedure Display_Result (Msg, Str : String; S : Positive) is
   begin
      Put_Line
        (Msg & " on character " & Str (Str'First .. S - 1) & '|'
         & Str (S .. Str'Last));
   end Display_Result;

   ------------
   -- Assert --
   ------------

   procedure Assert (Msg : String;
                     N : NFA_Access; Str : String; Final : Boolean := True)
   is
      Success : Boolean;
      M : NFA_Matcher := N.Start_Match;
   begin
      for S in Str'Range loop
         Process (M, Str (S), Success);

         if not Success then
            Display_Result ("Unexpected error", Str, S);
            Free (M);
            return;
         end if;
      end loop;

      if In_Final (M) /= Final then
         Put_Line (Msg & "(" & Str & ")"
                   & " => Unexpected final result: " & In_Final (M)'Img);
      end if;

      Free (M);
   end Assert;

   ------------------
   -- Assert_Error --
   ------------------

   procedure Assert_Error
     (Msg : String;
      N : NFA_Access; Str : String; At_Char : Natural; Error : String)
   is
      Success : Boolean;
      M : NFA_Matcher := N.Start_Match;
   begin
      for S in Str'Range loop
         Process (M, Str (S), Success);

         if not Success then
            if S /= At_Char then
               Display_Result
                 ("Error on unexpected char " & Msg
                  & " (expected" & At_Char'Img & ")",
                  Str, S);

            else
               Assert (Error, Expected (M),
                       "Unexpected message, " & Msg & " (" & Str & ")");
            end if;

            Free (M);
            return;
         end if;
      end loop;

      Put_Line ("Expected an error on " & Msg & " (" & Str & ")");
      Free (M);
   end Assert_Error;

   -----------
   -- Test1 --
   -----------

   procedure Test1 is
      Regexp : constant String := "a((c|d)+){2}b";
      N : NFA_Access := new NFA;
      S2, S3, S4, S5 : State;
   begin
      N.Initialize;

      S2 := N.Add_State;  --  Start of choice
      N.Add_Transition (Start_State, S2, (Char, 'a'));

      S3 := N.Add_State;  --  End of choice

      S4 := N.Add_State;
      N.Add_Transition (S2, S4, (Char, 'c'));
      N.Add_Empty_Transition (S4, S3);

      S5 := N.Add_State;
      N.Add_Transition (S2, S5, (Char, 'd'));
      N.Add_Empty_Transition (S5, S3);

      N.Repeat (S2, S3, 1, Natural'Last);  --  Make the "+" for the choice

      N.Add_Transition (S3, Final_State, (Char, 'b'));

      N.Repeat (S2, S3, 2, 2);  --  Make the "{2}" for the choice

      Assert
        ("Transitions: 12 <start> (a, 2) 2 (d, 5)(c, 4)"
         & " 3 (b,<final>)(<>, 6)"
          & " 4 (<>, 6) 5 (<>, 6) 6 (c, 8)(d, 7)(<>, 2)"
         & " 7 (<>, 3) 8 (<>, 3)",
         Dump (N, Compact => True),
         Regexp);

      Free (N);
   end Test1;

   procedure Test2 is
      --  Test where start state directly has empty transitions
      --    <empty>cd

      Regexp : constant String := "Test2 <>cd";

      S2, S3 : State;
      N : NFA_Access := new NFA;
   begin
      N.Initialize;

      S2 := N.Add_State;
      N.Add_Empty_Transition (Start_State, S2);

      S3 := N.Add_State;
      N.Add_Transition (S2, S3, (Char, 'c'));

      N.Add_Transition (S3, Final_State, (Char, 'd'));

      Assert (Regexp, N, "cd");
      Assert_Error (Regexp, N, "d", 1, "c");

      Free (N);
   end Test2;

   -----------
   -- Test3 --
   -----------

   procedure Test3 is
      Regexp : constant String := "Test3 a{3,6}b";
      S2     : State;
      N      : NFA_Access := new NFA;
   begin
      N.Initialize;

      S2 := N.Add_State;
      N.Add_Transition (Start_State, S2, (Char, 'a'));
      N.Repeat (Start_State, S2, 3, 6);

      N.Add_Transition (S2, Final_State, (Char, 'b'));

      Assert ("Transitions: 10 <start> (a, 3) 2 (b,<final>) 3 (a, 4)"
              & " 4 (a, 5) 5 (<>, 2)(a, 6) 6 (<>, 2)(a, 7)"
              & " 7 (<>, 2)(a, 2)",
              Dump (N, Compact => True),
              Regexp);

      Assert_Error (Regexp, N, "ab", 2, "a");
      Assert_Error (Regexp, N, "aab", 3, "a");
      Assert (Regexp, N, "aaab");
      Assert (Regexp, N, "aaaab");
      Assert (Regexp, N, "aaaaab");
      Assert (Regexp, N, "aaaaab");
      Assert (Regexp, N, "aaaaaab");
      Assert_Error (Regexp, N, "aaaaaaab", 7, "b");
      Free (N);
   end Test3;

   -----------
   -- Test4 --
   -----------

   procedure Test4 is
      Regexp : constant String := "Test4 ab{1,2}(c|d).e+";
      N : NFA_Access := new NFA;
      A, B, Choice1, Choice2, C, D, E, Dot : State;
   begin
      N.Initialize;

      A := N.Add_State;
      N.Add_Transition (Start_State, A, (Char, 'a'));

      B := N.Add_State;
      N.Add_Transition (A, B, (Char, 'b'));
      N.Repeat (A, B, Min_Occurs => 1, Max_Occurs => 2);

      Choice1 := N.Add_State;
      Choice2 := N.Add_State;
      N.Add_Empty_Transition (B, Choice1);

      C := N.Add_State;
      N.Add_Transition (Choice1, C, (Char, 'c'));
      N.Add_Empty_Transition (C, Choice2);

      D := N.Add_State;
      N.Add_Transition (Choice1, D, (Char, 'd'));
      N.Add_Empty_Transition (D, Choice2);

      Dot := N.Add_State;
      N.Add_Transition (Choice2, Dot, (Kind => Any_Char));

      E := N.Add_State;
      N.Add_Transition (Dot, E, (Char, 'e'));
      N.Repeat (Dot, E, 1, Natural'Last);

      N.Add_Empty_Transition (E, Final_State);

      Assert
        ("Transitions: 13 <start> (a, 2) 2 (b, 4) 3 (<>, 5)"
         & " 4 (<>, 3)(b, 3)"
         & " 5 (d, 8)(c, 7) 6 (<.>, 9) 7 (<>, 6) 8 (<>, 6) 9 (e, 10)"
         & " 10 (<>,<final>)(<>, 9)",
         Dump (N, Compact => True),
         Regexp);

      Assert (Regexp, N, "ab", Final => False);
      Assert (Regexp, N, "abcfe");
      Assert (Regexp, N, "abdfe");
      Assert (Regexp, N, "abdee");
      Assert_Error (Regexp, N, "abe", 3, "b|d|c");

      Assert (Regexp, N, "abbcfe");
      Assert_Error (Regexp, N, "abbbcfe", 4, "d|c");
      Assert (Regexp, N, "abbceeee");
      Assert_Error (Regexp, N, "abbceef", 7, "e");

      Free (N);
   end Test4;

begin
   Test1;
   Test2;
   Test3;
   Test4;
end TestState;
