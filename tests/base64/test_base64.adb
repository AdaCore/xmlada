------------------------------------------------------------------------------
--                     XML/Ada - An XML suite for Ada95                     --
--                                                                          --
--                       Copyright (C) 2021, AdaCore                        --
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

with Ada.Text_IO; use Ada.Text_IO;
with Sax.Utils;   use Sax.Utils;

procedure Test_Base64 is

   procedure Check (Value : String);

   procedure Check (Value : String) is
   begin
      if Is_Valid_Base64Binary (Value) then
         Put ("RIGHT: ");
      else
         Put ("WRONG: ");
      end if;

      Put_Line ('"' & Value & '"');
   end Check;

begin
   Check ("MzMzZGQK");
   Check ("MQo=");
   Check ("MTIK");
   Check ("MTIzCg==");
   Check ("");
   Check ("VVVV");
   Check ("VAV="); -- Wrong, was incorrectly OK before S823-015 fix.
   Check ("VV=="); -- Wrong
end Test_Base64;
