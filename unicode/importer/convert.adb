------------------------------------------------------------------------------
--                     XML/Ada - An XML suite for Ada95                     --
--                                                                          --
--                     Copyright (C) 2016, Nicolas Boulenguez               --
--                     Copyright (C) 2016-2022, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Command_Line;
with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Vectors;
with Ada.Strings.Bounded;
with Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;
with Ada.Text_IO;

with Translators.Alias;
with Translators.Block;

procedure Convert is

   use Ada.Text_IO;
   use type Translators.A_Translation;
   package ASB is new Ada.Strings.Bounded.Generic_Bounded_Length (256);

   --  Must be compiled with gnata and called with three paths.
   pragma Assert (Ada.Command_Line.Argument_Count = 3);
   Path_To_Blocks_Txt       : String renames Ada.Command_Line.Argument (1);
   Path_To_Name_Aliases_Txt : String renames Ada.Command_Line.Argument (2);
   Path_To_Unicode_Data_Txt : String renames Ada.Command_Line.Argument (3);

   Path_To_License : constant String := "license.txt";
   Output_Dir : constant String := "generated/";

   type A_Code is range 0 .. 16#10FFFF# + 1;

   function Value (Hexadecimal_Digits : String) return A_Code;
   --  The given string must only contain hexadecimal_digits.

   function Image (Code : A_Code) return String;
   --  16#4_hexadecimal_digits# if Code <= 16#FFFF#,
   --  else add the required digits count.

   package Code_IO is new Integer_IO (A_Code);
   --  Default base is set to 16.

   package Translation_Vectors is new Ada.Containers.Indefinite_Vectors
     (Positive, Translators.A_Translation);

   type A_Point is record
      Code  : A_Code;
      Names : Translation_Vectors.Vector;
   end record;

   package Point_Vectors is new Ada.Containers.Vectors (Positive, A_Point);

   procedure Parse_Block_Line (Line : String);
   procedure Process_Block (Start_Code : A_Code;
                            End_Code   : A_Code;
                            Block_Name : String)
     with Pre => Start_Code <= End_Code;
   procedure Output_Ada_Package (Block_Name : String;
                                 Points     : Point_Vectors.Vector);
   procedure Put_Maybe_Split (File             : File_Type;
                              Before_Semicolon : String;
                              After_Semicolon  : String);
   procedure Put_Unused_Exception (Replaced    : String;
                                   Replacement : String);

   type A_Name_File is record
      File : File_Type;
      Code : A_Code;
      Name : ASB.Bounded_String;
   end record;

   procedure Next (Name_File : in out A_Name_File);

   ----------------------------------------------------------------------

   function Image (Code : A_Code) return String is
      Tmp : String (1 .. 3 + 32 / 4 + 1);
      I   : Integer := Tmp'Last - 1;
   begin
      Code_IO.Put (Tmp, Code, Base => 16);
      while Tmp (I) /= '#' loop
         I := I - 1;
      end loop;
      return Tmp (I - 2 .. I) & (Tmp'Last .. I + 4 => '0')
        & Tmp (I + 1 .. Tmp'Last);
   end Image;

   procedure Next (Name_File : in out A_Name_File) is
   begin
      while not End_Of_File (Name_File.File) loop
         declare
            Line : constant String := Get_Line (Name_File.File);
         begin
            if Line'Length /= 0 and then Line (Line'First) /= '#' then
               declare
                  I : constant Natural := Ada.Strings.Fixed.Index (Line, ";");
                  J : constant Natural := Ada.Strings.Fixed.Index (Line, ";",
                                                                   I + 1);
                  pragma Assert (I in 2 .. J - 2);
               begin
                  Name_File.Code := Value (Line (Line'First .. I - 1));
                  ASB.Set_Bounded_String (Name_File.Name,
                                          Line (I + 1 .. J - 1));
                  exit;
               end;
            end if;
         end;
      end loop;
   end Next;

   procedure Parse_Block_Line (Line : String) is
      First_Dot : Integer;
      Semicolon : Integer;
      I         : Integer := Line'First;
   begin
      if I <= Line'Last and then Line (I) /= '#' then
         while Line (I) in '0' .. '9' | 'A' .. 'F' loop
            I := I + 1;
         end loop;
         First_Dot := I;
         pragma Assert (Line'First + 4 <= First_Dot
                          and Line (First_Dot .. First_Dot + 1) = "..");
         I := I + 2;
         while Line (I) in  '0' .. '9' | 'A' .. 'F' loop
            I := I + 1;
         end loop;
         Semicolon := I;
         pragma Assert (First_Dot + 5 < Semicolon
                          and Line (Semicolon .. Semicolon + 1) = "; ");
         Process_Block
           (Start_Code => Value (Line (Line'First .. First_Dot - 1)),
            End_Code   => Value (Line (First_Dot + 2 .. Semicolon - 1)),
            Block_Name => Line (Semicolon + 2 .. Line'Last));
      end if;
   end Parse_Block_Line;

   procedure Put_Maybe_Split (File             : File_Type;
                              Before_Semicolon : String;
                              After_Semicolon  : String) is
      S : constant String := "   " & Before_Semicolon
        & (Before_Semicolon'Length + 1 .. 39 => ' ') & " :";
   begin
      if S'Length + 1 + After_Semicolon'Length <= 79 then
         Put_Line (File, S & ' ' & After_Semicolon);
      else
         Put_Line (File, S);
         Put_Line (File, "      " & After_Semicolon);
      end if;
   end Put_Maybe_Split;

   procedure Put_Unused_Exception (Replaced    : String;
                                   Replacement : String) is
   begin
      Put_Line
        ("Unused exception: " & Replaced & " -> " & Replacement);
   end Put_Unused_Exception;

   function Value (Hexadecimal_Digits : String) return A_Code is
   begin
      return A_Code'Value ("16#" & Hexadecimal_Digits & '#');
   end Value;

   ----------------------------------------------------------------------

   --  Now it is convenient to share some variable among the last procedures.

   Name_Aliases     : A_Name_File;
   Unicode_Data     : A_Name_File;
   Alias_Translator : Translators.Alias.An_Alias_Translator;
   Block_Translator : Translators.Block.A_Block_Translator;
   Unicode_Version  : ASB.Bounded_String;

   procedure Output_Ada_Package (Block_Name : String;
                                 Points     : Point_Vectors.Vector) is
      Pkg  : constant String := Block_Translator.Translated
        (Block_Translator.New_Translation (Block_Name));
      File :  File_Type;
      License : File_Type;
   begin
      --  On VMS, Filename lengths are limited to 39.39 characters.
      pragma Assert (14 + Pkg'Length <= 39, "file name too long: " & Pkg);
      Create (File, Out_File,
              Output_Dir & "unicode-names-" & Ada.Strings.Fixed.Translate
                (Pkg, Ada.Strings.Maps.Constants.Lower_Case_Map) & ".ads");
      Put_Line (File,
                "--  This file is built automatically from data found on the");
      Put_Line (File, "--  unicode web site (http://www.unicode.org)");
      Put (File, "--  in version ");
      Put (File, ASB.To_String (Unicode_Version));
      Put (File, " and thus is a subject to unicode license:");
      New_Line (File);

      Open (License, In_File, Path_To_License);
      while not End_Of_File (License) loop
         Put_Line (File, Get_Line (License));
      end loop;
      Close (License);

      Put (File, "package Unicode.Names.");
      Put (File, Pkg);
      Put (File, " is");
      New_Line (File);
      Put_Line (File, "   pragma Preelaborate;");
      Put_Line (File, "   pragma Style_Checks (Off);");
      New_Line (File);
      for Point of Points loop
         if not Point.Names.Is_Empty then
            if Translators.Is_Exception (Point.Names.Element (1)) then
               Put (File, "   --  Real Unicode name is ");
               Put (File, Translators.Original (Point.Names.Element (1)));
               New_Line (File);
            end if;
            Put_Maybe_Split
              (File, Alias_Translator.Translated (Point.Names.Element (1)),
               "constant Unicode_Char := " & Image (Point.Code) & ";");
            for A in 2 .. Integer (Point.Names.Length) loop
               if Translators.Is_Exception (Point.Names.Element (A)) then
                  Put (File, "   --  Real Unicode name is ");
                  Put (File, Translators.Original (Point.Names.Element (A)));
                  New_Line (File);
               end if;
               Put_Maybe_Split
                 (File,
                  Alias_Translator.Translated (Point.Names.Element (A)),
                  "Unicode_Char renames "
                    & Alias_Translator.Translated (Point.Names.Element (1))
                    & ';');
            end loop;
         end if;
      end loop;
      Put (File, "end Unicode.Names.");
      Put (File, Pkg);
      Put (File, ";");
      New_Line (File);
      Close (File);
   end Output_Ada_Package;

   procedure Process_Block (Start_Code : A_Code;
                            End_Code   : A_Code;
                            Block_Name : String) is
      Points : Point_Vectors.Vector;
   begin
      while (not End_Of_File (Unicode_Data.File))
        and then Unicode_Data.Code < Start_Code loop
         Put ("Code without block: ");
         Code_IO.Put (Unicode_Data.Code);
         New_Line;
         Next (Unicode_Data);
      end loop;
      while (not End_Of_File (Unicode_Data.File))
        and then Unicode_Data.Code <= End_Code loop
         declare
            Point : A_Point;
            Name  : constant Translators.A_Translation
              := Alias_Translator.New_Translation
              (ASB.To_String (Unicode_Data.Name));
         begin
            Point.Code := Unicode_Data.Code;
            if Alias_Translator.Translated (Name) /= "" then
               Point.Names.Append (Name);
            end if;
            while (not End_Of_File (Name_Aliases.File))
              and then Name_Aliases.Code = Point.Code loop
               Point.Names.Append (Alias_Translator.New_Translation
                                     (ASB.To_String (Name_Aliases.Name)));
               Next (Name_Aliases);
            end loop;
            if Point.Names.Is_Empty then
               Put ("Unnamed code: ");
               Code_IO.Put (Point.Code);
               New_Line;
            else
               Points.Append (Point);
            end if;
         end;
         Next (Unicode_Data);
      end loop;
      if Points.Is_Empty then
         Put ("Empty block: ");
         Put (Block_Name);
         New_Line;
      else
         Output_Ada_Package (Block_Name, Points);
      end if;
   end Process_Block;

   Blocks : File_Type;
begin
   Code_IO.Default_Base := 16;

   Alias_Translator.Set_Exceptions;
   Block_Translator.Set_Exceptions;

   Open (Blocks, In_File, Path_To_Blocks_Txt);
   declare
      Line : constant String := Get_Line (Blocks);
      pragma Assert (Line (Line'First .. Line'First + 8) = "# Blocks-"
           and then Line (Line'Last - 3 .. Line'Last) = ".txt",
           "Unable to parse unicode version in " & Name (Blocks));
   begin
      ASB.Set_Bounded_String (Unicode_Version,
                              Line (Line'First + 9 .. Line'Last - 4));
   end;

   Open (Name_Aliases.File, In_File, Path_To_Name_Aliases_Txt);
   Next (Name_Aliases);

   Open (Unicode_Data.File, In_File, Path_To_Unicode_Data_Txt);
   Next (Unicode_Data);

   while not End_Of_File (Blocks) loop
      Parse_Block_Line (Get_Line (Blocks));
   end loop;

   Close (Blocks);

   if not End_Of_File (Unicode_Data.File) then
      Put (Path_To_Unicode_Data_Txt);
      Put (" only parsed until code ");
      Code_IO.Put (Unicode_Data.Code);
      New_Line;
   end if;
   Close (Unicode_Data.File);

   if not End_Of_File (Name_Aliases.File) then
      Put (Path_To_Name_Aliases_Txt);
      Put (" only parsed until code ");
      Code_IO.Put (Name_Aliases.Code);
      New_Line;
   end if;
   Close (Name_Aliases.File);

   Alias_Translator.Iterate_On_Unused_Exceptions (Put_Unused_Exception'Access);

   Block_Translator.Iterate_On_Unused_Exceptions (Put_Unused_Exception'Access);
end Convert;
