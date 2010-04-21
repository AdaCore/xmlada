-----------------------------------------------------------------------
--                XML/Ada - An XML suite for Ada95                   --
--                                                                   --
--                    Copyright (C) 2005-2010, AdaCore               --
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

--  This package provides some minimal handling for arbitrary precision
--  numbers as described by the "decimal" and "integer" types.
--  This is *not* an arbitrary-precision library, which is not needed in the
--  context of XML

with Ada.Finalization;
with Schema.Validators;  use Schema.Validators;
with Unicode.CES;

package Schema.Decimal is

   type Arbitrary_Precision_Number is new Ada.Finalization.Controlled
      with private;

   function Image
     (Number : Arbitrary_Precision_Number) return Unicode.CES.Byte_Sequence;
   --  Return a displayable version of Number

   function Value
     (Reader : access Abstract_Validation_Reader'Class;
      Ch     : Unicode.CES.Byte_Sequence) return Arbitrary_Precision_Number;
   --  Convert Ch to a number.
   --  Raises a Validation_Error if this is not a valid number

   function Value_No_Exponent
     (Reader : access Abstract_Validation_Reader'Class;
      Ch : Unicode.CES.Byte_Sequence) return Arbitrary_Precision_Number;
   --  Same as value, but does not allow a "E" part

   function "<"  (Num1, Num2 : Arbitrary_Precision_Number) return Boolean;
   function "<=" (Num1, Num2 : Arbitrary_Precision_Number) return Boolean;
   function "="  (Num1, Num2 : Arbitrary_Precision_Number) return Boolean;
   function ">=" (Num1, Num2 : Arbitrary_Precision_Number) return Boolean;
   function ">"  (Num1, Num2 : Arbitrary_Precision_Number) return Boolean;
   --  Compare two numbers

   procedure Check_Digits
     (Reader : access Abstract_Validation_Reader'Class;
      Num                           : Arbitrary_Precision_Number;
      Fraction_Digits, Total_Digits : Integer := -1);
   --  Check whether the two facets Fraction_Digits and Total_Digits match.
   --  If any of the two values is negative, no check is done for it.

private
   type Arbitrary_Precision_Number is new Ada.Finalization.Controlled with
      record
         Value : Unicode.CES.Byte_Sequence_Access;
      end record;

   procedure Finalize (Object : in out Arbitrary_Precision_Number);
   procedure Adjust   (Object : in out Arbitrary_Precision_Number);
   --  See inherited documentation

end Schema.Decimal;
