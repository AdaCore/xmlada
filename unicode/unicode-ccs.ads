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

--  This is the root of the hierarchy that provides coded character sets.
--  These CCS can be used to convert from any encoding (like Iso/Latin-1)
--  to Unicode.
--  See http://www.isoc.org:8080/codage/iso8859/jeuxiso.en.htm for a list
--  of the ISO 8859-*  character sets

package Unicode.CCS is

   --  Each of the child package shall have two public functions with
   --  the following profile:
   --    function Convert (Char : Unicode_Char) return Unicode_Char
   --  that converts from a code point representing an abstract character in
   --  the specific encoding to the code point for the same character in
   --  Unicode, or reverse

   type Conversion_Function is access
     function (Char : Unicode_Char) return Unicode_Char;

   type Character_Set is record
      To_Unicode : Conversion_Function;
      To_CS      : Conversion_Function;
   end record;

   --------------------
   -- Character sets --
   --------------------

   function Get_Character_Set (Name : String) return Character_Set;
   --  Get the character set associated with a given name
   --  The valid names are given in the packages Unicode.CCS.*.
   --  Unknown_Character_Set is raised if Name is not found.

   function Identity (Char : Unicode_Char) return Unicode_Char;
   --  return its parameter directly.

   Unicode_Character_Set : constant Character_Set :=
     (To_Unicode => Identity'Access,
      To_Cs      => Identity'Access);

   Invalid_Code : exception;
   --  Exception raised when trying to convert to a code point that
   --  is not available in the specific character set. This can never be
   --  raised when converting to Unicode, since it is universal by definition.

   Unknown_Character_Set : exception;
   --  Raised by Get_Conversion or Get_Revert_Conversion when the name is
   --  unknown.

end Unicode.CCS;
