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

with Ada.Exceptions;     use Ada.Exceptions;
with Ada.IO_Exceptions;  use Ada.IO_Exceptions;
with Unicode.CES;        use Unicode.CES;
with Unicode.CES.Utf32;  use Unicode.CES.Utf32;
with Unicode.CES.Utf16;  use Unicode.CES.Utf16;
with Unicode.CES.Utf8;   use Unicode.CES.Utf8;
with GNAT.OS_Lib;        use GNAT.OS_Lib;

package body Input_Sources.File is

   ----------
   -- Open --
   ----------

   procedure Open (Filename : String; Input : out File_Input'Class) is
      FD : File_Descriptor;
      Length : Natural;
      Cursor : Positive;
      Actual_Length : Integer;
      BOM    : Bom_Type;
   begin
      --  Open the source FD, note that we open in binary mode, because there
      --  is no point in wasting time on text translation when it is not
      --  required.

      FD := Open_Read (Filename, Binary);

      --  Raise Name_Error if file cannot be found

      if FD = Invalid_FD then
         Raise_Exception
            (Name_Error'Identity, "Could not open " & Filename);
      end if;

      Length := Integer (File_Length (FD));

      --  If the file is empty, we just create a reader that will not return
      --  any character. This will fail later on when the XML document is
      --  parsed, anyway.

      if Length = 0 then
         Input.Buffer := new String (1 .. 1);
         Input.Index := 2;
         Close (FD);
         return;
      end if;

      --  Allocate buffer

      Input.Buffer := new String (1 .. Length);

      --  On most systems, the loop below will be executed only once and the
      --  file will be read in one chunk. However, some systems (e.g. VMS) have
      --  file types that require one read per line, so read until we get the
      --  Length bytes or until there are no more bytes to read.

      Cursor := 1;
      loop
         Actual_Length := Read (FD, Input.Buffer (Cursor)'Address, Length);
         Cursor := Cursor + Actual_Length;
         exit when Actual_Length = Length or Actual_Length <= 0;
      end loop;

      Close (FD);

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

      --  Base file name should be used as the public Id
      Set_Public_Id (Input, Filename);
      Set_System_Id (Input, Filename);
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
      return From.Buffer = null or else From.Index > From.Buffer'Length;
   end Eof;

   -------------------
   -- Set_System_Id --
   -------------------

   procedure Set_System_Id (Input : in out File_Input; Id : Byte_Sequence) is
   begin
      if Is_Absolute_Path (Id) then
         Set_System_Id (Input_Source (Input), Id);
      else
         Set_System_Id
           (Input_Source (Input),
            Normalize_Pathname (Id, Resolve_Links => False));
      end if;
   end Set_System_Id;

end Input_Sources.File;
