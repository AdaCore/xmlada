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

with Unicode;      use Unicode;
with Unicode.CES;  use Unicode.CES;

package body Input_Sources.Strings is

   ----------
   -- Open --
   ----------

   procedure Open
     (Str      : Unicode.CES.Byte_Sequence;
      Encoding : Unicode.CES.Encoding_Scheme;
      Input    : out String_Input) is
   begin
      Input.Encoding := Encoding;
      Input.Buffer := new Byte_Sequence' (Str);
      Input.Index := Input.Buffer'First;
   end Open;

   -----------
   -- Close --
   -----------

   procedure Close (Input : in out String_Input) is
   begin
      Free (Input.Buffer);
   end Close;

   ---------------
   -- Next_Char --
   ---------------

   procedure Next_Char
     (From : in out String_Input;
      C    : out Unicode.Unicode_Char) is
   begin
      C := From.Encoding.Read (From.Buffer.all, From.Index);
      From.Index := From.Index + From.Encoding.Width (C);
   end Next_Char;

   ---------
   -- Eof --
   ---------

   function Eof (From : String_Input) return Boolean is
   begin
      return From.Index > From.Buffer'Last;
   end Eof;
end Input_Sources.Strings;
