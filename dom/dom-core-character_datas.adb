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

with Sax.Encodings; use Sax.Encodings;
with Unicode.CES;   use Unicode.CES;

package body DOM.Core.Character_Datas is

   ------------
   -- Length --
   ------------

   function Length (N : Character_Data) return Natural is
   begin
      return Encoding.Length (Data (N));
   end Length;

   --------------------
   -- Substring_Data --
   --------------------

   function Substring_Data
     (N : Character_Data;
      Offset : Natural;
      Count : Natural) return DOM_String
   is
      Str  : constant DOM_String := Data (N);
      Offs : constant Integer := Index_From_Offset (Str, Offset, Encoding);
      Last : Integer;
   begin
      if Offs < 0 then
         raise Index_Size_Err;
      end if;

      Last :=  Index_From_Offset (Str (Offs .. Str'Last), Count, Encoding);
      if Last < 0 then
         raise Index_Size_Err;
      end if;
      return Str (Offs .. Last);
   end Substring_Data;

   -----------------
   -- Append_Data --
   -----------------

   procedure Append_Data (N : Character_Data; Arg : DOM_String) is
   begin
      Set_Data (N, Data (N) & Arg);
   end Append_Data;

   -----------------
   -- Insert_Data --
   -----------------

   procedure Insert_Data
     (N : Character_Data;
      Offset : Natural;
      Arg : DOM_String)
   is
      Str : constant DOM_String := Data (N);
      Pos : constant Integer := Index_From_Offset (Str, Offset, Encoding);
   begin
      if Pos < 0 then
         raise Index_Size_Err;
      end if;
      Set_Data (N, Str (Str'First .. Pos - 1) & Arg & Str (Pos .. Str'Last));
   end Insert_Data;

   -----------------
   -- Delete_Data --
   -----------------

   procedure Delete_Data
     (N : Character_Data; Offset : Natural; Count : Natural)
   is
      Str  : constant DOM_String := Data (N);
      Offs : constant Integer := Index_From_Offset (Str, Offset, Encoding);
      Last : Integer;
   begin
      if Offs < 0 then
         raise Index_Size_Err;
      end if;

      Last := Index_From_Offset (Str (Offs .. Str'Last), Count, Encoding);
      if Last < 0 then
         raise Index_Size_Err;
      end if;
      Set_Data (N, Str (Str'First .. Offs - 1) & Str (Last .. Str'Last));
   end Delete_Data;

   ------------------
   -- Replace_Data --
   ------------------

   procedure Replace_Data
     (N : Character_Data;
      Offset : Natural;
      Count : Natural;
      Arg : DOM_String)
   is
      Str  : constant DOM_String := Data (N);
      Offs : constant Integer := Index_From_Offset (Str, Offset, Encoding);
      Last : Integer;
   begin
      if Offs < 0 then
         raise Index_Size_Err;
      end if;

      Last := Index_From_Offset (Str (Offs .. Str'Last), Count, Encoding);
      if Last < 0 then
         raise Index_Size_Err;
      end if;
      Set_Data (N, Str (Str'First .. Offs - 1) & Arg & Str (Last .. Str'Last));
   end Replace_Data;

end DOM.Core.Character_Datas;
