------------------------------------------------------------------------------
--                     XML/Ada - An XML suite for Ada95                     --
--                                                                          --
--                     Copyright (C) 2004-2017, AdaCore                     --
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

--  This package provides support for the ISO/8859-15 (aka Latin-15)
--  encoding.
--  This is a superset of ISO/8859-1, but also includes the Euro symbol

package Unicode.CCS.Iso_8859_15 is

   Name1 : aliased constant String := "ISO-8859-15";

   function To_Unicode     (Char : Unicode_Char) return Unicode_Char;
   function To_Iso_8859_15 (Char : Unicode_Char) return Unicode_Char;

   Iso_8859_15_Character_Set : constant Character_Set :=
     (To_Unicode => To_Unicode'Access,
      To_CS      => To_Iso_8859_15'Access);

end Unicode.CCS.Iso_8859_15;
