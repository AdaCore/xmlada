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
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

with Unicode.CES;  use Unicode.CES;

package body Input_Sources is

   -----------------
   -- Prolog_Size --
   -----------------

   function Prolog_Size (From : Input_Source) return Natural is
   begin
      return From.Prolog_Size;
   end Prolog_Size;

   ------------------
   -- Set_Encoding --
   ------------------

   procedure Set_Encoding
     (Input    : in out Input_Source;
      Es       : Unicode.CES.Encoding_Scheme) is
   begin
      Input.Es := Es;
   end Set_Encoding;

   ------------------
   -- Get_Encoding --
   ------------------

   function Get_Encoding (Input : Input_Source)
      return Unicode.CES.Encoding_Scheme is
   begin
      return Input.Es;
   end Get_Encoding;

   -----------------------
   -- Set_Character_Set --
   -----------------------

   procedure Set_Character_Set
     (Input : in out Input_Source;
      Cs    : Unicode.CCS.Character_Set) is
   begin
      Input.Cs := Cs;
   end Set_Character_Set;

   -----------------------
   -- Get_Character_Set --
   -----------------------

   function Get_Character_Set (Input : Input_Source)
      return Unicode.CCS.Character_Set is
   begin
      return Input.Cs;
   end Get_Character_Set;

   -------------------
   -- Set_System_Id --
   -------------------

   procedure Set_System_Id (Input : in out Input_Source; Id : Byte_Sequence) is
   begin
      Free (Input.System_Id);
      Input.System_Id := new Byte_Sequence' (Id);
   end Set_System_Id;

   -------------------
   -- Get_System_Id --
   -------------------

   function Get_System_Id (Input : Input_Source) return Byte_Sequence is
   begin
      if Input.System_Id = null then
         return "";
      else
         return Input.System_Id.all;
      end if;
   end Get_System_Id;

   -------------------
   -- Set_Public_Id --
   -------------------

   procedure Set_Public_Id (Input : in out Input_Source; Id : Byte_Sequence) is
   begin
      Free (Input.Public_Id);
      Input.Public_Id := new Byte_Sequence' (Id);
   end Set_Public_Id;

   -------------------
   -- Get_Public_Id --
   -------------------

   function Get_Public_Id (Input : Input_Source) return Byte_Sequence is
   begin
      if Input.Public_Id = null then
         return "";
      else
         return Input.Public_Id.all;
      end if;
   end Get_Public_Id;

   -----------
   -- Close --
   -----------

   procedure Close (Input : in out Input_Source) is
   begin
      Free (Input.Public_Id);
      Free (Input.System_Id);
   end Close;
end Input_Sources;
