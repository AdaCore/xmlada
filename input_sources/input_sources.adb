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

with Ada.Exceptions;            use Ada.Exceptions;
with Ada.Characters.Handling;   use Ada.Characters.Handling;
with Unicode.CES;               use Unicode.CES;
with Unicode.CES.Basic_8bit;    use Unicode.CES.Basic_8bit;
with Unicode.CES.Utf8;          use Unicode.CES.Utf8;
with Unicode.CCS;               use Unicode.CCS;
with Unicode.CCS.Iso_8859_1;    use Unicode.CCS.Iso_8859_1;
with Unicode.CCS.Iso_8859_2;    use Unicode.CCS.Iso_8859_2;
with Unicode.CCS.Iso_8859_3;    use Unicode.CCS.Iso_8859_3;
with Unicode.CCS.Iso_8859_4;    use Unicode.CCS.Iso_8859_4;

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
      Input.System_Id := new Byte_Sequence'(Id);
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
      Input.Public_Id := new Byte_Sequence'(Id);
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

   -------------------------
   -- Set_Stream_Encoding --
   -------------------------

   procedure Set_Stream_Encoding
     (Input    : in out Input_Sources.Input_Source'Class;
      Encoding : String)
   is
      Upper : constant String := To_Upper (Encoding);
   begin
      if Upper = "UTF-16" then
         --  Do nothing, since UTF-16 is automatically detected by the input
         --  stream, based on the first bytes. This also includes detection of
         --  big-endian/little-endian, which we can't do from this function.
         null;

      elsif Upper = "UTF-8" then
         Set_Encoding      (Input, Utf8_Encoding);
         Set_Character_Set (Input, Unicode_Character_Set);

      elsif Upper = Unicode.CCS.Iso_8859_1.Name1
        or else Upper = To_Upper (Unicode.CCS.Iso_8859_1.Name2)
      then
         Set_Encoding      (Input, Basic_8bit_Encoding);
         Set_Character_Set (Input, Iso_8859_1_Character_Set);

      elsif Upper = Unicode.CCS.Iso_8859_2.Name1
        or else Upper = To_Upper (Unicode.CCS.Iso_8859_2.Name2)
      then
         Set_Encoding      (Input, Basic_8bit_Encoding);
         Set_Character_Set (Input, Iso_8859_2_Character_Set);

      elsif Upper = Unicode.CCS.Iso_8859_3.Name1 then
         Set_Encoding      (Input, Basic_8bit_Encoding);
         Set_Character_Set (Input, Iso_8859_3_Character_Set);

      elsif Upper = Unicode.CCS.Iso_8859_4.Name1 then
         Set_Encoding      (Input, Basic_8bit_Encoding);
         Set_Character_Set (Input, Iso_8859_4_Character_Set);

      else
         Raise_Exception
           (Invalid_Encoding'Identity,  "Invalid encoding: " & Upper);
      end if;
   end Set_Stream_Encoding;

end Input_Sources;
