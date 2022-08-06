------------------------------------------------------------------------------
--                     XML/Ada - An XML suite for Ada95                     --
--                                                                          --
--                     Copyright (C) 2016, Nicolas Boulenguez               --
--                     Copyright (C) 2016-2022, AdaCore                     --
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

package body Translators is

   use type Exception_Maps.Cursor;

   procedure Iterate_On_Unused_Exceptions
     (Translator : A_Translator;
      Process    : not null access procedure (Replaced    : String;
                                              Replacement : String)) is
      use Exception_Maps;
      Position : Cursor := Translator.Exceptions.First;
   begin
      while Position /= No_Element loop
         if Element (Position).Used = 0 then
            Process.all (Key (Position), Element (Position).Replacement);
         end if;
         Position := Next (Position);
      end loop;
   end Iterate_On_Unused_Exceptions;

   function New_Translation (Translator : in out A_Translator;
                             Original   :        String)
                            return A_Translation is
      Position : constant Exception_Maps.Cursor
        := Translator.Exceptions.Find (Original);
   begin
      if Position /= Exception_Maps.No_Element then
         declare
            procedure Process (Key     :        String;
                               Element : in out An_Exception);
            procedure Process (Key     :        String;
                               Element : in out An_Exception) is
               pragma Unreferenced (Key);
            begin
               Element.Used := Element.Used + 1;
            end Process;
         begin
            Translator.Exceptions.Update_Element (Position, Process'Access);
         end;
         return (Original'Length, Original, Position);
      else
         declare
            Translated : constant String := Default_Translation
              (A_Translator'Class (Translator), Original);
         begin
            return (Translated'Length, Translated, Position);
         end;
      end if;
   end New_Translation;

   function Is_Exception (Translation : A_Translation) return Boolean is
   begin
      return Translation.Position /= Exception_Maps.No_Element;
   end Is_Exception;

   function Original (Translation : A_Translation) return String is
   begin
      return Translation.Name;
   end Original;

   function Translated (Translator  : A_Translator;
                        Translation : A_Translation)
                       return String is
   begin
      if Translation.Position /= Exception_Maps.No_Element then
         return Translator.Exceptions.Element (Translation.Name).Replacement;
      else
         return Translation.Name;
      end if;
   end Translated;

   function Valid_Ada_Identifier (Name : String) return Boolean is
      Word_Start : Boolean := True;
   begin
      for C of Name loop
         case C is
            when 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' =>
               Word_Start := False;
            when '_' =>
               if Word_Start then
                  return False;
               end if;
               Word_Start := True;
            when others =>
               return False;
         end case;
      end loop;
      return (not Word_Start) and then Name (Name'First) not in '0' .. '9';
   end Valid_Ada_Identifier;

end Translators;
