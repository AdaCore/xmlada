-----------------------------------------------------------------------
--                XML/Ada - An XML suite for Ada95                   --
--                                                                   --
--                       Copyright (C) 2004-2007, AdaCore            --
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

with Unicode.CCS;
with Unicode.CES;

--  This package groups a character set and an encoding scheme under names
--  assigned by the Internet Assigned Numbers Authority.
--  See http://www.iana.org/assignments/character-sets
--  These names are used in the <?xml encoding="..." ?> part of XML
--  documents.

package Unicode.Encodings is

   type Cst_String_Access is access constant String;

   type Unicode_Encoding is record
      Name            : Cst_String_Access;
      Character_Set   : Unicode.CCS.Character_Set;
      Encoding_Scheme : Unicode.CES.Encoding_Scheme;
   end record;

   function Get_By_Name (Name : String) return Unicode_Encoding;
   --  Return the unicode encoding from its name.
   --  Name is case insensitive

   function Convert
     (Str  : Unicode.CES.Byte_Sequence;
      From : Unicode_Encoding := Get_By_Name ("iso-8859-15");
      To   : Unicode_Encoding := Get_By_Name ("utf-8"))
      return Unicode.CES.Byte_Sequence;
   --  Convert a string between two encodings.

end Unicode.Encodings;
