
--  This package provides some minimal handling for arbitrary precision
--  numbers as described by the "decimal" and "integer" types.
--  This is *not* an arbitrary-precision library, which is not needed in the
--  context of XML

with Ada.Finalization;
with Unicode.CES;

package Schema.Decimal is

   type Arbitrary_Precision_Number is new Ada.Finalization.Controlled
      with private;

   function Image
     (Number : Arbitrary_Precision_Number) return Unicode.CES.Byte_Sequence;
   --  Return a displayable version of Number

   function Value
     (Ch : Unicode.CES.Byte_Sequence) return Arbitrary_Precision_Number;
   --  Convert Ch to a number.
   --  Raises a Validation_Error if this is not a valid number

   function "<"  (Num1, Num2 : Arbitrary_Precision_Number) return Boolean;
   function "<=" (Num1, Num2 : Arbitrary_Precision_Number) return Boolean;
   function "="  (Num1, Num2 : Arbitrary_Precision_Number) return Boolean;
   function ">=" (Num1, Num2 : Arbitrary_Precision_Number) return Boolean;
   function ">"  (Num1, Num2 : Arbitrary_Precision_Number) return Boolean;
   --  Compare two numbers

   procedure Check_Digits
     (Num                           : Arbitrary_Precision_Number;
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
