-----------------------------------------------------------------------
--                XML/Ada - An XML suite for Ada95                   --
--                                                                   --
--                       Copyright (C) 2001-2002                     --
--                            ACT-Europe                             --
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

--  This package provides support for the ISO/8859-1 (aka Latin-1)
--  encoding.
--  The ISO-8859-1 character set, often simply referred to as Latin 1, can
--  represent most Western European languages including: Albanian, Catalan,
--  Danish, Dutch, English, Faroese, Finnish, French, Galician, German, Irish,
--  Icelandic, Italian, Norwegian, Portuguese, Spanish and Swedish.

package Unicode.CCS.Iso_8859_1 is

   Name1 : constant String := "ISO-8859-1";
   Name2 : constant String := "Latin1";

   function To_Unicode    (Char : Unicode_Char) return Unicode_Char;
   function To_Iso_8859_1 (Char : Unicode_Char) return Unicode_Char;

   Iso_8859_1_Character_Set : constant Character_Set :=
     (To_Unicode => To_Unicode'Access,
      To_Cs      => To_Iso_8859_1'Access);
end Unicode.CCS.Iso_8859_1;
