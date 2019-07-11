------------------------------------------------------------------------------
--                     XML/Ada - An XML suite for Ada95                     --
--                                                                          --
--                     Copyright (C) 2016, Nicolas Boulenguez               --
--                     Copyright (C) 2016-2017, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

pragma Ada_2012;

with Ada.Strings.Maps.Constants;
with Ada.Text_IO;

package body Translators.Alias is

   function Default_Translation
      (Translator : An_Alias_Translator; Original   : String)
      return String
   is
      pragma Unreferenced (Translator);
      Result      : String (1 .. Original'Length);
      Result_Last : Integer := 0;
      Word_Start  : Boolean := True;
   begin
      pragma Assert (Original'Length /= 0);
      if Original (Original'First) not in 'A' .. 'Z'
        or Original (Original'Last) not in 'A' .. 'Z' | '0' .. '9'
      then
         Ada.Text_IO.Put_Line ("Unused alias: """ & Original & '"');
         return "";
      end if;
      for I in Original'Range loop
         case Original (I) is
            when 'A' .. 'Z' =>
               Result_Last := Result_Last + 1;
               if Word_Start then
                  Result (Result_Last) := Original (I);
                  Word_Start := False;
               else
                  Result (Result_Last) := Ada.Strings.Maps.Value
                    (Ada.Strings.Maps.Constants.Lower_Case_Map, Original (I));
               end if;
            when '0' .. '9' =>
               Result_Last := Result_Last + 1;
               Result (Result_Last) := Original (I);
               Word_Start := False;
            when ' ' | '-' =>
               --  Ignore dash after or before a letter.
               if not        ((Original (I - 1) in 'A' .. 'Z'
                           and Original (I .. I + 1) = "- ")
                       or     (Original (I + 1) in 'A' .. 'Z'
                           and Original (I - 1 .. I) = " -"))
               then
                  pragma Assert (not Word_Start);
                  Result_Last := Result_Last + 1;
                  Result (Result_Last) := '_';
                  Word_Start := True;
               end if;
            when others =>
               Ada.Text_IO.Put_Line ("Unused alias: """ & Original & '"');
               return "";
         end case;
      end loop;
      return Result (1 .. Result_Last);
   exception
      when others =>
         Ada.Text_IO.Put_Line ("Cannot translate point """ & Original & '"');
         raise;
   end Default_Translation;

   procedure Set_Exceptions (Translator : in out An_Alias_Translator) is
      procedure Add (Key, Value : String) with inline;
      procedure Add (Key, Value : String) is
      begin
         Translator.Exceptions.Insert (Key, (Value'Length, Value, 0));
      end Add;
   begin
      --  Would be handled correctly, but trigger a noisy log line.
      Add ("<control>",                     "");

      --  Ada keywords
      Add ("XOR",                           "Unicode_Xor"); --  22BB
      Add ("PACKAGE",                       "Unicode_Package"); -- 1F4E6
      Add ("NULL",                          "Unicode_Null");

      --  Uniquify names only differing only by a dash.
      Add ("TIBETAN LETTER -A",             "Tibetan_Letter_Dash_A");
      Add ("TIBETAN SUBJOINED LETTER -A",   "Tibetan_Subjoined_Letter_Dash_A");
      Add ("MARCHEN LETTER -A",             "Marchen_Letter_Dash_A");
   end Set_Exceptions;

end Translators.Alias;
