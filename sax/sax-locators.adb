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

with Unicode.CES;  use Unicode.CES;
with Unchecked_Deallocation;

package body Sax.Locators is

   ----------
   -- Free --
   ----------

   procedure Free (Loc : in out Locator_Impl) is
   begin
      Free (Loc.Public_Id);
      Free (Loc.System_Id);
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Loc : in out Locator_Impl_Access) is
      procedure Internal is new Unchecked_Deallocation
        (Locator_Impl'Class, Locator_Impl_Access);
   begin
      if Loc /= null then
         Free (Loc.all);
         Internal (Loc);
      end if;
   end Free;

   ----------
   -- Copy --
   ----------

   procedure Copy
     (Loc : in out Locator_Impl; Loc_I : in Locator'Class) is
   begin
      Set_Line_Number (Loc, Get_Line_Number (Loc_I));
      Set_Column_Number (Loc, Get_Column_Number (Loc_I));
      Set_Public_Id (Loc, Get_Public_Id (Loc_I));
      Set_System_Id (Loc, Get_System_Id (Loc_I));
   end Copy;

   ---------------------
   -- Get_Line_Number --
   ---------------------

   function Get_Line_Number (Loc : Locator_Impl) return Natural is
   begin
      return Loc.Line;
   end Get_Line_Number;

   -----------------------
   -- Get_Column_Number --
   -----------------------

   function Get_Column_Number (Loc : Locator_Impl) return Natural is
   begin
      return Loc.Column;
   end Get_Column_Number;

   -------------------
   -- Get_System_Id --
   -------------------

   function Get_System_Id (Loc : Locator_Impl)
      return Unicode.CES.Byte_Sequence is
   begin
      if Loc.System_Id /= null then
         return Loc.System_Id.all;
      else
         return "";
      end if;
   end Get_System_Id;

   -------------------
   -- Get_Public_Id --
   -------------------

   function Get_Public_Id (Loc : Locator_Impl)
      return Unicode.CES.Byte_Sequence is
   begin
      if Loc.Public_Id /= null then
         return Loc.Public_Id.all;
      else
         return "";
      end if;
   end Get_Public_Id;

   -----------------------
   -- Set_Column_Number --
   -----------------------

   procedure Set_Column_Number
     (Loc : in out Locator_Impl; Column : Natural := 0) is
   begin
      Loc.Column := Column;
   end Set_Column_Number;

   ---------------------
   -- Set_Line_Number --
   ---------------------

   procedure Set_Line_Number
     (Loc : in out Locator_Impl; Line : Natural := 0) is
   begin
      Loc.Line := Line;
   end Set_Line_Number;

   -------------------
   -- Set_Public_Id --
   -------------------

   procedure Set_Public_Id
     (Loc : in out Locator_Impl;
      Id  : Unicode.CES.Byte_Sequence) is
   begin
      Free (Loc.Public_Id);
      Loc.Public_Id := new Byte_Sequence'(Id);
   end Set_Public_Id;

   -------------------
   -- Set_System_Id --
   -------------------

   procedure Set_System_Id
     (Loc : in out Locator_Impl;
      Id  : Unicode.CES.Byte_Sequence) is
   begin
      Free (Loc.System_Id);
      Loc.System_Id := new Byte_Sequence'(Id);
   end Set_System_Id;

   ---------------
   -- To_String --
   ---------------

   function To_String (Loc : Locator_Impl) return String is
      Line : constant String := Natural'Image (Get_Line_Number (Loc));
      Col  : constant String := Natural'Image (Get_Column_Number (Loc));
   begin
      if Get_Column_Number (Loc) /= 0 then
         return (Get_Public_Id (Loc) & ':'
                 & Line (Line'First + 1 .. Line'Last)
                 & ':' & Col (Col'First + 1 .. Col'Last));
      else
         return (Get_Public_Id (Loc) & ':'
                 & Line (Line'First + 1 .. Line'Last));
      end if;
   end To_String;

end Sax.Locators;
