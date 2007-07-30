-----------------------------------------------------------------------
--                XML/Ada - An XML suite for Ada95                   --
--                                                                   --
--                       Copyright (C) 2001-2007, AdaCore            --
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

with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Unicode.CES;  use Unicode.CES;

package body Sax.Locators is
   use Locators;

   procedure Allocate_If_Null (Loc : in out Locator);
   --  Allocate new memory for Loc if it is unset

   ----------------------
   -- Allocate_If_Null --
   ----------------------

   procedure Allocate_If_Null (Loc : in out Locator) is
      Tmp : Locators.Encapsulated_Access;
   begin
      if Get (Loc) = null then
         Tmp := new Locator_Record;
         Loc := Allocate (Tmp);
      end if;
   end Allocate_If_Null;

   ----------
   -- Free --
   ----------

   procedure Free (Loc : in out Locator_Record) is
   begin
      Free (Loc.Public_Id);
      Free (Loc.System_Id);
   end Free;

   ---------------------
   -- Get_Line_Number --
   ---------------------

   function Get_Line_Number (Loc : Locator) return Natural is
   begin
      if Get (Loc) = null then
         return 0;
      else
         return Get_Line_Number (Get (Loc));
      end if;
   end Get_Line_Number;

   function Get_Line_Number (Loc : access Locator_Record) return Natural is
   begin
      return Loc.Line;
   end Get_Line_Number;

   -----------------------
   -- Get_Column_Number --
   -----------------------

   function Get_Column_Number (Loc : Locator) return Natural is
   begin
      if Get (Loc) = null then
         return 0;
      else
         return Get_Column_Number (Get (Loc));
      end if;
   end Get_Column_Number;

   function Get_Column_Number (Loc : access Locator_Record) return Natural is
   begin
      return Loc.Column;
   end Get_Column_Number;

   -------------------
   -- Get_System_Id --
   -------------------

   function Get_System_Id (Loc : Locator) return Unicode.CES.Byte_Sequence is
   begin
      if Get (Loc) = null then
         return "";
      else
         return Get_System_Id (Get (Loc));
      end if;
   end Get_System_Id;

   function Get_System_Id (Loc : access Locator_Record)
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

   function Get_Public_Id (Loc : Locator) return Unicode.CES.Byte_Sequence is
   begin
      if Get (Loc) = null then
         return "";
      else
         return Get_Public_Id (Get (Loc));
      end if;
   end Get_Public_Id;

   function Get_Public_Id (Loc : access Locator_Record)
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

   procedure Set_Column_Number (Loc : in out Locator; Column : Natural := 0) is
   begin
      Allocate_If_Null (Loc);
      Set_Column_Number (Get (Loc), Column);
   end Set_Column_Number;

   procedure Set_Column_Number
     (Loc : access Locator_Record; Column : Natural := 0) is
   begin
      Loc.Column := Column;
   end Set_Column_Number;

   ---------------------
   -- Set_Line_Number --
   ---------------------

   procedure Set_Line_Number (Loc : in out Locator; Line : Natural := 0) is
   begin
      Allocate_If_Null (Loc);
      Set_Line_Number (Get (Loc), Line);
   end Set_Line_Number;

   procedure Set_Line_Number
     (Loc : access Locator_Record; Line : Natural := 0) is
   begin
      Loc.Line := Line;
   end Set_Line_Number;

   ----------
   -- Copy --
   ----------

   procedure Copy (Loc : in out Locator; Source : Locator) is
   begin
      Allocate_If_Null (Loc);
      Copy (Get (Loc), Source);
   end Copy;

   procedure Copy
     (Loc : access Locator_Record; Source : Locator) is
   begin
      Set_Line_Number (Loc, Get_Line_Number (Source));
      Set_Column_Number (Loc, Get_Column_Number (Source));
      Set_Public_Id (Loc, Get_Public_Id (Source));
      Set_System_Id (Loc, Get_System_Id (Source));
   end Copy;

   -------------------
   -- Set_Public_Id --
   -------------------

   procedure Set_Public_Id
     (Loc : in out Locator; Id : Unicode.CES.Byte_Sequence) is
   begin
      Allocate_If_Null (Loc);
      Set_Public_Id (Get (Loc), Id);
   end Set_Public_Id;

   procedure Set_Public_Id
     (Loc : access Locator_Record;
      Id  : Unicode.CES.Byte_Sequence) is
   begin
      Free (Loc.Public_Id);
      Loc.Public_Id := new Byte_Sequence'(Id);
   end Set_Public_Id;

   -------------------
   -- Set_System_Id --
   -------------------

   procedure Set_System_Id
     (Loc : in out Locator; Id : Unicode.CES.Byte_Sequence) is
   begin
      Allocate_If_Null (Loc);
      Set_System_Id (Get (Loc), Id);
   end Set_System_Id;

   procedure Set_System_Id
     (Loc : access Locator_Record;
      Id  : Unicode.CES.Byte_Sequence) is
   begin
      Free (Loc.System_Id);
      Loc.System_Id := new Byte_Sequence'(Id);
   end Set_System_Id;

   ---------------
   -- To_String --
   ---------------

   function To_String
     (Loc : Locator; Use_Basename : Boolean := False) return String
   is
      C    : constant Natural := Get_Column_Number (Loc);
      Line : constant String := Natural'Image (Get_Line_Number (Loc));
      Col  : constant String := Natural'Image (C);
   begin
      if C /= 0 then
         if Use_Basename then
            return (Base_Name (Get_Public_Id (Loc)) & ':'
                    & Line (Line'First + 1 .. Line'Last)
                    & ':' & Col (Col'First + 1 .. Col'Last));
         else
            return (Get_Public_Id (Loc) & ':'
                    & Line (Line'First + 1 .. Line'Last)
                    & ':' & Col (Col'First + 1 .. Col'Last));
         end if;
      else
         if Use_Basename then
            return (Base_Name (Get_Public_Id (Loc)) & ':'
                    & Line (Line'First + 1 .. Line'Last));
         else
            return (Get_Public_Id (Loc) & ':'
                    & Line (Line'First + 1 .. Line'Last));
         end if;
      end if;
   end To_String;

end Sax.Locators;
