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

with Unicode.CES;

package Sax.Locators is

   -------------------------------------------------
   -- General Locator interface as defined in SAX --
   -------------------------------------------------

   type Locator is abstract tagged private;
   type Locator_Access is access all Locator'Class;

   function Get_Line_Number (Loc : Locator) return Natural is abstract;
   --  Return the line number where the current document event ends

   function Get_Column_Number (Loc : Locator) return Natural is abstract;
   --  Return the column number where the current document event ends

   function Get_System_Id (Loc : Locator) return Unicode.CES.Byte_Sequence
      is abstract;
   --  Return the system id for the current document (see input_sources.ads)

   function Get_Public_Id (Loc : Locator) return Unicode.CES.Byte_Sequence
      is abstract;
   --  Return the public id for the current document (see input_sources.ads)

   --------------------
   -- Added features --
   --------------------
   --  The subprograms below are not part of the SAX2 standard, but have been
   --  added for convenience

   function To_String (Loc : Locator) return String is abstract;
   --  Print the location found in the location, with a standard format:
   --     Public_Id:Line:Column
   --  Public_Id is not printed if it is null.
   --  Column is not printed if it is zero (unknown)

   ---------------------------------------------------------
   -- Convenience implementation of the locator interface --
   ---------------------------------------------------------

   type Locator_Impl is new Locator with private;
   type Locator_Impl_Access is access all Locator_Impl'Class;

   procedure Free (Loc : in out Locator_Impl);
   procedure Free (Loc : in out Locator_Impl_Access);
   --  Free the memory allocated internally for the strings.
   --  For the second subprogram, we also free the memory allocated for the
   --  access type itself

   procedure Copy (Loc : in out Locator_Impl; Loc_I : in Locator'Class);
   --  Copy the location information from Loc_I to Loc
   --  This calls the Set_* functions below, so that you don't need to
   --  rewrite it for all your classes.

   function Get_Line_Number (Loc : Locator_Impl) return Natural;
   function Get_Column_Number (Loc : Locator_Impl) return Natural;
   function Get_System_Id (Loc : Locator_Impl)
      return Unicode.CES.Byte_Sequence;
   function Get_Public_Id (Loc : Locator_Impl)
      return Unicode.CES.Byte_Sequence;

   procedure Set_Column_Number
     (Loc : in out Locator_Impl; Column : Natural := 0);
   --  Set the column number for the locator.
   --  Set this to zero if the column is unknown.

   procedure Set_Line_Number
     (Loc : in out Locator_Impl; Line : Natural := 0);
   --  Set the line number for the locator

   procedure Set_Public_Id
     (Loc : in out Locator_Impl; Id  : Unicode.CES.Byte_Sequence);
   --  Set the public Id for the allocator

   procedure Set_System_Id
     (Loc : in out Locator_Impl; Id  : Unicode.CES.Byte_Sequence);
   --  Set the system Id for the allocator

   function To_String (Loc : Locator_Impl) return String;

private
   type Locator is abstract tagged null record;

   type Locator_Impl is new Locator with record
      Line   : Natural := 1;
      Column : Natural := 1;
      Public_Id : Unicode.CES.Byte_Sequence_Access;
      System_Id : Unicode.CES.Byte_Sequence_Access;
   end record;

end Sax.Locators;
