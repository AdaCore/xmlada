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

with GNAT.Regpat;  use GNAT.Regpat;
with Ada.Unchecked_Deallocation;

package Schema.Validators.Facets is

   -------------------------
   --  Byte_Sequence_List --
   -------------------------

   type Byte_Sequence_List is array (Natural range <>)
      of Unicode.CES.Byte_Sequence_Access;
   type Byte_Sequence_List_Access is access Byte_Sequence_List;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Byte_Sequence_List, Byte_Sequence_List_Access);

   procedure Free (List : in out Byte_Sequence_List_Access);
   --  Free the contents of List, including contained sequences

   procedure Append
     (List  : in out Byte_Sequence_List_Access;
      Value : Unicode.CES.Byte_Sequence);
   --  Append a new value to List

   ------------
   -- Facets --
   ------------

   type Pattern_Matcher_Access is access GNAT.Regpat.Pattern_Matcher;
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Pattern_Matcher, Pattern_Matcher_Access);

   type Whitespace_Restriction is (Preserve, Replace, Collapse);

   type Value_Validator is access function
     (Str : Unicode.CES.Byte_Sequence) return Boolean;
   --  Return True if Str is a valid value.
   --  Str is encoded with Sax.Encodings.Encoding

   --------------------
   --  Common facets --
   --------------------
   --  These are the facets that are shared by most base types.

   type Facets_Names is (Facet_Whitespace,
                         Facet_Pattern,
                         Facet_Enumeration,
                         Facet_Implicit_Enumeration,
                         Facet_Length,
                         Facet_Min_Length,
                         Facet_Max_Length,
                         Facet_Total_Digits,
                         Facet_Fraction_Digits,
                         Facet_Max_Inclusive,
                         Facet_Min_Inclusive,
                         Facet_Max_Exclusive,
                         Facet_Min_Exclusive);
   type Facets_Mask is array (Facets_Names) of Boolean;
   pragma Pack (Facets_Mask);
   --  The list of all possible facets. Not all facets_description will support
   --  these, however.

   type Common_Facets_Description is new Facets_Description_Record with record
      Settable             : Facets_Mask               := (others => True);
      Mask                 : Facets_Mask               := (others => False);
      --  List of facets than can be set or are currently set

      Whitespace           : Whitespace_Restriction    := Collapse;
      Pattern              : Pattern_Matcher_Access    := null;
      Pattern_String       : Unicode.CES.Byte_Sequence_Access      := null;
      Implicit_Enumeration : Value_Validator           := null;
      Enumeration          : Byte_Sequence_List_Access := null;
      --  ??? Could use a htable here for faster access
   end record;
   --  Facets shared by all basic types

   procedure Free (Facets : in out Common_Facets_Description);
   procedure Check_Facet
     (Facets : in out Common_Facets_Description;
      Value  : Unicode.CES.Byte_Sequence);
   procedure Copy
     (From : Common_Facets_Description;
      To   : in out Facets_Description_Record'Class);
   procedure Add_Facet
     (Facets      : in out Common_Facets_Description;
      Facet_Name  : Unicode.CES.Byte_Sequence;
      Facet_Value : Unicode.CES.Byte_Sequence;
      Applied     : out Boolean);
   --  See inherited documentation

   procedure Set_Implicit_Enumeration
     (Facets : in out Common_Facets_Description; Validator : Value_Validator);
   procedure Set_Whitespace
     (Facets     : in out Common_Facets_Description;
      Whitespace : Whitespace_Restriction);
   --  Set the various facets more efficiently than going through a string

   ------------------------
   --  Internal use only --
   ------------------------
   --  These are put in the specs for the sake of the testsuite

   function Convert_Regexp (Regexp : String) return String;
   --  Return a regular expresssion that converts the XML-specification
   --  regexp Regexp to a GNAT.Regpat compatible one

end Schema.Validators.Facets;
