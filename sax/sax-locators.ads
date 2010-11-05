-----------------------------------------------------------------------
--                XML/Ada - An XML suite for Ada95                   --
--                                                                   --
--                       Copyright (C) 2001-2010, AdaCore            --
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

--  This package defines the Locator type, which is used to identify locations
--  within the XML streams where events have occurred.
--  This type is reference-counted in XML/Ada, which means that memory is
--  managed automatically. However, if you keep a copy of a locator, its
--  attributes will be changed as the XML stream is parsed. You must use Copy
--  to preserve the value of these attributes over time.

with Sax.Pointers;
with Sax.Symbols;

package Sax.Locators is

   type Locator is private;
   No_Locator : constant Locator;

   type Location is record
      Line      : Natural := 1;
      Column    : Natural := 1;
      Public_Id : Sax.Symbols.Symbol := Sax.Symbols.Empty_String;
      System_Id : Sax.Symbols.Symbol := Sax.Symbols.Empty_String;
   end record;
   No_Location : constant Location :=
     (1, 1, Sax.Symbols.Empty_String, Sax.Symbols.Empty_String);
   --  A static location, ie a location that will not be changed automatically
   --  by the parser (as opposed to a [Locator] which is basically a pointer to
   --  such a location, modified dynamically by the parser).
   --  For efficiency, a [Location] is made a public record, where you can
   --  access the fields directly.

   procedure Set_Line_Number (Loc : in out Locator; Line : Natural := 0);
   function Get_Line_Number (Loc : Locator) return Natural;
   pragma Inline (Get_Line_Number, Set_Line_Number);
   --  Return the line number where the current document event ends

   procedure Set_Column_Number (Loc : in out Locator; Column : Natural := 0);
   function Get_Column_Number  (Loc : Locator) return Natural;
   pragma Inline (Get_Column_Number, Set_Column_Number);
   --  Return the column number where the current document event ends

   procedure Increase_Line_Number (Loc : in out Locator; Inc : Natural := 1);
   procedure Increase_Column_Number (Loc : in out Locator; Inc : Natural := 1);
   pragma Inline (Increase_Column_Number, Increase_Line_Number);
   --  Increment the column number. This assume Loc has already been
   --  initialized

   procedure Set_System_Id (Loc : in out Locator; Id : Sax.Symbols.Symbol);
   function Get_System_Id (Loc : Locator) return Sax.Symbols.Symbol;
   --  Return the system id for the current document (see input_sources.ads)

   procedure Set_Public_Id (Loc : in out Locator; Id : Sax.Symbols.Symbol);
   function Get_Public_Id (Loc : Locator) return Sax.Symbols.Symbol;
   --  Return the public id for the current document (see input_sources.ads)

   function To_String
     (Loc : Location; Use_Basename : Boolean := False) return String;
   function To_String
     (Loc : Locator; Use_Basename : Boolean := False) return String;
   --  Print the location found in the location, with a standard format:
   --     Public_Id:Line:Column
   --  Public_Id is not printed if it is null.
   --  Column is not printed if it is zero (unknown)
   --  If Use_Basename is true, then the file name will not include any
   --  directory specification.

   procedure Set_Location (Loc : in out Locator; To : Location);
   function Get_Location (Loc : Locator) return Location;
   --  Get the current location information.

private
   type Locator_Record is new Sax.Pointers.Root_Encapsulated with record
      Loc       : Location;
   end record;

   package Locators is new Sax.Pointers.Smart_Pointers (Locator_Record);
   type Locator is new Locators.Pointer;
   No_Locator : constant Locator := Locator (Locators.Null_Pointer);
end Sax.Locators;
