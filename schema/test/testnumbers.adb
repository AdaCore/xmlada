-----------------------------------------------------------------------
--                XML/Ada - An XML suite for Ada95                   --
--                                                                   --
--                       Copyright (C) 2007-2010, AdaCore            --
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

with Schema.Decimal;    use Schema.Decimal;
with GNAT.IO;           use GNAT.IO;
with Sax.Locators;      use Sax.Locators;
with Schema.Validators; use Schema.Validators;

procedure TestNumbers is
   procedure Assert_Nan (Num : String);
   --  Check that Num is not a valid number

   procedure Assert (Num1, Num2 : String; Expected : Character);
   --  Compare two numbers

   type Local_Reader is new Abstract_Validation_Reader with null record;
   function Get_Location
     (Reader : Local_Reader) return Sax.Locators.Locator;

   Reader : aliased Local_Reader;
   R      : constant Abstract_Validating_Reader_Access :=
     Reader'Unchecked_Access;

   ------------------
   -- Get_Location --
   ------------------

   function Get_Location
     (Reader : Local_Reader) return Sax.Locators.Locator
   is
      pragma Unreferenced (Reader);
   begin
      return No_Locator;
   end Get_Location;

   ----------------
   -- Assert_Nan --
   ----------------

   procedure Assert_Nan (Num : String) is
      N : Arbitrary_Precision_Number;
      pragma Unreferenced (N);
   begin
      N := Value (R, Num);
      Put_Line (Num & " should not be authorized");
   exception
      when XML_Validation_Error =>
         null;
   end Assert_Nan;

   ------------
   -- Assert --
   ------------

   procedure Assert (Num1, Num2 : String; Expected : Character) is
   begin
      case Expected is
         when '<' =>
            if not (Value (R, Num1) < Value (R, Num2)) then
               Put_Line (Num1 & " < " & Num2);
            end if;
            if not (Value (R, Num2) > Value (R, Num1)) then
               Put_Line (Num2 & " > " & Num1);
            end if;

         when '=' =>
            if not (Value (R, Num1) = Value (R, Num2)) then
               Put_Line (Num1 & " = " & Num2);
            end if;

         when '>' =>
            if not (Value (R, Num1) > Value (R, Num2)) then
               Put_Line (Num1 & " > " & Num2);
            end if;
            if not (Value (R, Num2) < Value (R, Num1)) then
               Put_Line (Num2 & " < " & Num1);
            end if;

         when others =>
            Put_Line ("Unexpected comparision");
      end case;
   end Assert;

   Num_Invalid1 : constant String := "--23";
   Num_Invalid2 : constant String := "-23..";
   Num_Invalid3 : constant String := "2A24";
   Num_Invalid4 : constant String := "@234";
   Num_Invalid5 : constant String := "12E";
   Num_Invalid6 : constant String := "12E@23";

   Num1 : constant String := "1";
   Num2 : constant String := "10";
   Num3 : constant String := "1E-1";
   Num4 : constant String := "9e-1";
   Num5 : constant String := "-100E-2";
   Num6 : constant String := "-124.567E2";
   Num7 : constant String := "-12345.678E1";

   Num8 : constant String := "124.45E5";
   Num9 : constant String := "123.4";

begin
   Assert_Nan (Num_Invalid1);
   Assert_Nan (Num_Invalid2);
   Assert_Nan (Num_Invalid3);
   Assert_Nan (Num_Invalid4);
   Assert_Nan (Num_Invalid5);
   Assert_Nan (Num_Invalid6);

   Assert (Num1, Num2, '<');
   Assert (Num1, Num3, '>');
   Assert (Num1, Num4, '>');
   Assert (Num1, Num5, '>');
   Assert (Num6, Num7, '>');

   Check_Digits (R, Value (R, Num8), Fraction_Digits => 0, Total_Digits => 9);
   Check_Digits (R, Value (R, Num8), Fraction_Digits => 0, Total_Digits => 8);
   begin
      Check_Digits
        (R, Value (R, Num8), Fraction_Digits => 0, Total_Digits => 7);
      Put_Line ("Total_Digits=5 " & Num8);
   exception
      when XML_Validation_Error => null;
   end;

   Check_Digits (R, Value (R, Num9), Total_Digits => 5);
   Check_Digits (R, Value (R, Num9), Total_Digits => 4);
   begin
      Check_Digits (R, Value (R, Num9), Total_Digits => 3);
      Put_Line ("Total_Digits=3 " & Num9);
   exception
      when XML_Validation_Error => null;
   end;

   Check_Digits (R, Value (R, Num9), Fraction_Digits => 2);
   Check_Digits (R, Value (R, Num9), Fraction_Digits => 1);
   begin
      Check_Digits (R, Value (R, Num9), Fraction_Digits => 0);
      Put_Line ("Fraction_Digits=0 " & Num9);
   exception
      when XML_Validation_Error => null;
   end;

   Check_Digits (R, Value (R, Num8), Fraction_Digits => 1);
   Check_Digits (R, Value (R, Num8), Fraction_Digits => 0);
   Check_Digits (R, Value (R, Num6), Fraction_Digits => 1);
   begin
      Check_Digits (R, Value (R, Num6), Fraction_Digits => 0);
      Put_Line ("Fraction_Digits=0 " & Num6);
   exception
      when XML_Validation_Error => null;
   end;

end TestNumbers;
