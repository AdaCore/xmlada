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

with Ada.Direct_IO;
with Unicode.CES;        use Unicode.CES;
with Unicode.CES.Utf32;  use Unicode.CES.Utf32;
with Unicode.CES.Utf16;  use Unicode.CES.Utf16;
with Unicode.CES.Utf8;   use Unicode.CES.Utf8;

package body Input_Sources.File is

   procedure Fast_Read (The_File : in String; Buf : in Byte_Sequence_Access);
   --  Read Buf'length characters in The_File and store it in Buf.
   --  This procedure performs a single call to Read.

   ---------------
   -- Fast_Read --
   ---------------

   procedure Fast_Read (The_File : in String;
                        Buf      : in Byte_Sequence_Access) is
      type Fixed_String is new String (Buf'Range);

      package Dir_Fast is new Ada.Direct_IO (Fixed_String);
      use Dir_Fast;

      F : Dir_Fast.File_Type;

   begin
      Dir_Fast.Open (F, In_File, The_File);
      Dir_Fast.Read (F, Fixed_String (Buf.all));
      Dir_Fast.Close (F);
   end Fast_Read;

   ----------
   -- Open --
   ----------

   procedure Open (Filename : String; Input : out File_Input) is
      package Dir is new Ada.Direct_IO (Character);
      F : Dir.File_Type;
      Length : Natural;
      BOM : Bom_Type;
   begin
      Dir.Open (F, Dir.In_File, Filename);
      Length := Natural (Dir.Size (F));
      Dir.Close (F);

      --  If the file is empty, we just create a reader that will not return
      --  any character. This will fail later on when the XML document is
      --  parsed, anyway.
      if Length = 0 then
         Input.Buffer := new String (1 .. 1);
         Input.Index := 2;
         return;
      end if;

      Input.Buffer := new String (1 .. Length);
      Fast_Read (Filename, Input.Buffer);

      Read_Bom (Input.Buffer.all, Input.Prolog_Size, BOM);
      case BOM is
         when Utf32_LE =>
            Set_Encoding (Input, Utf32_LE_Encoding);
         when Utf32_BE =>
            Set_Encoding (Input, Utf32_BE_Encoding);
         when Utf16_LE =>
            Set_Encoding (Input, Utf16_LE_Encoding);
         when Utf16_BE =>
            Set_Encoding (Input, Utf16_BE_Encoding);
         when Ucs4_BE | Ucs4_LE | Ucs4_2143 | Ucs4_3412 =>
            raise Invalid_Encoding;
         when Utf8_All | Unknown =>
            Set_Encoding (Input, Utf8_Encoding);
      end case;

      Input.Index := Input.Buffer'First + Input.Prolog_Size;
   end Open;

   -----------
   -- Close --
   -----------

   procedure Close (Input : in out File_Input) is
   begin
      Input_Sources.Close (Input_Source (Input));
      Free (Input.Buffer);
      Input.Index := Natural'Last;
   end Close;

   ---------------
   -- Next_Char --
   ---------------

   procedure Next_Char
     (From : in out File_Input;
      C    : out Unicode.Unicode_Char) is
   begin
      From.Es.Read (From.Buffer.all, From.Index, C);
      C := From.Cs.To_Unicode (C);
   end Next_Char;

   ---------
   -- Eof --
   ---------

   function Eof (From : File_Input) return Boolean is
   begin
      return From.Index > From.Buffer'Length;
   end Eof;

end Input_Sources.File;
