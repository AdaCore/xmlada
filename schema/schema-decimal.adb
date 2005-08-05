
with Ada.Finalization;        use Ada.Finalization;
with Unicode.CES;             use Unicode.CES;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Schema.Validators;       use Schema.Validators;

package body Schema.Decimal is

   type Compare_Result is (Less_Than, Equal, Greater_Than);
   function Compare (Num1, Num2 : String) return Compare_Result;
   --  Compare two numbers

   function Get_Exp (Num : String) return Long_Long_Integer;
   --  Return the exponential part of Num (ie the part after 'E'.

   procedure Get_Fore (Num : String; First, Last : out Integer);
   --  Return the position of the first and last digit in the integer part of
   --  Num

   procedure To_Next_Digit (Num : String; Pos : in out Integer);
   --  Move Pos to the next digit in Num

   -----------
   -- Image --
   -----------

   function Image
     (Number : Arbitrary_Precision_Number) return Unicode.CES.Byte_Sequence is
   begin
      if Number.Value /= null then
         return Number.Value.all;
      else
         return "0";
      end if;
   end Image;

   -----------
   -- Value --
   -----------

   function Value
     (Ch : Unicode.CES.Byte_Sequence) return Arbitrary_Precision_Number
   is
      Pos : Integer;
      Saw_Exponent : Boolean := False;
      Saw_Point    : Boolean := False;
   begin
      if Ch'Length = 0 then
         Validation_Error ("Invalid: empty string used as a number");
      end if;

      if Ch (Ch'First) = '+' or Ch (Ch'First) = '-' then
         Pos := Ch'First + 1;
      else
         Pos := Ch'First;
      end if;

      while Pos <= Ch'Last loop
         if Ch (Pos) = '.' then
            if Saw_Point then
               Validation_Error
                 ("Only one decimal separator allowed in " & Ch);
            end if;
            Saw_Point := True;

         elsif Ch (Pos) = 'E' or else Ch (Pos) = 'e' then
            if Saw_Exponent then
               Validation_Error
                 ("Only one exponent allowed in " & Ch);
            end if;
            Saw_Exponent := True;
            Saw_Point := False;

            if Pos = Ch'Last then
               Validation_Error ("No exponent specified in " & Ch);
            elsif Ch (Pos + 1) = '-' or Ch (Pos + 1) = '+' then
               Pos := Pos + 1;
            end if;

         elsif not Is_Decimal_Digit (Ch (Pos)) then
            Validation_Error ("Invalid integer: """ & Ch & """");
         end if;

         Pos := Pos + 1;
      end loop;

      return (Controlled with Value => new Byte_Sequence'(Ch));
   end Value;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Object : in out Arbitrary_Precision_Number) is
   begin
      Free (Object.Value);
   end Finalize;

   ------------
   -- Adjust --
   ------------

   procedure Adjust (Object : in out Arbitrary_Precision_Number) is
   begin
      Object.Value := new Byte_Sequence'(Object.Value.all);
   end Adjust;

   -------------
   -- Get_Exp --
   -------------

   function Get_Exp (Num : String) return Long_Long_Integer is
      Pos : Integer := Num'Last;
   begin
      while Pos >= Num'First
        and then Num (Pos) /= 'E'
        and then Num (Pos) /= 'e'
      loop
         Pos := Pos - 1;
      end loop;

      if Pos >= Num'First then
         return Long_Long_Integer'Value (Num (Pos + 1 .. Num'Last));
      else
         return 0;
      end if;
   end Get_Exp;

   --------------
   -- Get_Fore --
   --------------

   procedure Get_Fore (Num : String; First, Last : out Integer) is
      Pos : Integer;
   begin
      if Num (Num'First) = '-' or else Num (Num'First) = '+' then
         First := Num'First + 1;
      else
         First := Num'First;
      end if;

      Pos := First;
      while Pos <= Num'Last
        and then Num (Pos) /= '.'
        and then Num (Pos) /= 'E'
        and then Num (Pos) /= 'e'
      loop
         Pos := Pos + 1;
      end loop;

      Last := Pos - 1;
   end Get_Fore;

   -------------------
   -- To_Next_Digit --
   -------------------

   procedure To_Next_Digit (Num : String; Pos : in out Integer) is
   begin
      Pos := Pos + 1;
      if Pos <= Num'Last then
         if Num (Pos) = 'E' or Num (Pos) = 'e' then
            Pos := Num'Last + 1;
         elsif Num (Pos) = '.' then
            Pos := Pos + 1;
         end if;
      end if;
   end To_Next_Digit;

   -------------
   -- Compare --
   -------------

   function Compare (Num1, Num2 : String) return Compare_Result is
      Num1_Negative : constant Boolean := Num1 (Num1'First) = '-';
      Num2_Negative : constant Boolean := Num2 (Num2'First) = '-';

      Exp1, Exp2 : Long_Long_Integer;
      Pos1, Pos2 : Integer;
      Fore_First1, Fore_Last1 : Integer;
      Fore_First2, Fore_Last2 : Integer;
   begin
      --  We have to normalize the numbers (take care of exponents
      if Num1_Negative and not Num2_Negative then
         return Less_Than;

      elsif not Num1_Negative and Num2_Negative then
         return Greater_Than;

      else
         --  They have the same sign
         Exp1 := Get_Exp (Num1);
         Exp2 := Get_Exp (Num2);

         Get_Fore (Num1, Fore_First1, Fore_Last1);
         Get_Fore (Num2, Fore_First2, Fore_Last2);

         --  Different lengths ?
         if Long_Long_Integer (Fore_Last1 - Fore_First1) + Exp1 >
           Long_Long_Integer (Fore_Last2 - Fore_First2) + Exp2
         then
            if Num1_Negative then
               return Less_Than;
            else
               return Greater_Than;
            end if;

         elsif Long_Long_Integer (Fore_Last1 - Fore_First1) + Exp1 <
           Long_Long_Integer (Fore_Last2 - Fore_First2) + Exp2
         then
            if Num1_Negative then
               return Greater_Than;
            else
               return Less_Than;
            end if;
         end if;

         --  Same length of fore parts, we need to compare the digits
         Pos1 := Fore_First1;
         Pos2 := Fore_First2;

         loop
            if Num1 (Pos1) > Num2 (Pos2) then
               if Num1_Negative then
                  return Less_Than;
               else
                  return Greater_Than;
               end if;
            elsif Num1 (Pos1) < Num2 (Pos2) then
               if Num1_Negative then
                  return Greater_Than;
               else
                  return Less_Than;
               end if;
            end if;

            To_Next_Digit (Num1, Pos1);
            To_Next_Digit (Num2, Pos2);

            if Pos1 > Num1'Last
              and then Pos2 > Num2'Last
            then
               return Equal;
            elsif Pos1 > Num1'Last then
               if Num1_Negative then
                  return Less_Than;
               else
                  return Greater_Than;
               end if;
            elsif Pos2 > Num2'Last then
               if Num1_Negative then
                  return Greater_Than;
               else
                  return Less_Than;
               end if;
            end if;
         end loop;
      end if;
   end Compare;

   ---------
   -- "<" --
   ---------

   function "<" (Num1, Num2 : Arbitrary_Precision_Number) return Boolean is
   begin
      return Compare (Num1.Value.all, Num2.Value.all) = Less_Than;
   end "<";

   ----------
   -- "<=" --
   ----------

   function "<=" (Num1, Num2 : Arbitrary_Precision_Number) return Boolean is
   begin
      return Compare (Num1.Value.all, Num2.Value.all) /= Greater_Than;
   end "<=";

   ---------
   -- "=" --
   ---------

   function "=" (Num1, Num2 : Arbitrary_Precision_Number) return Boolean is
   begin
      return Compare (Num1.Value.all, Num2.Value.all) = Equal;
   end "=";

   ----------
   -- ">=" --
   ----------

   function ">=" (Num1, Num2 : Arbitrary_Precision_Number) return Boolean is
   begin
      return Compare (Num1.Value.all, Num2.Value.all) /= Less_Than;
   end ">=";

   ---------
   -- ">" --
   ---------

   function ">" (Num1, Num2 : Arbitrary_Precision_Number) return Boolean is
   begin
      return Compare (Num1.Value.all, Num2.Value.all) = Greater_Than;
   end ">";

   ------------------
   -- Check_Digits --
   ------------------

   procedure Check_Digits
     (Num                           : Arbitrary_Precision_Number;
      Fraction_Digits, Total_Digits : Integer := -1)
   is
      Exp : constant Long_Long_Integer := Get_Exp (Num.Value.all);
      Fore_First, Fore_Last : Integer;
      Pos : Integer;
      Digits_Count : Natural := 0;
      Frac_Digits  : Natural := 0;
   begin
      Get_Fore (Num.Value.all, Fore_First, Fore_Last);

      --  Now count the significant digits (including fractional part)
      Pos := Num.Value'First;
      if Num.Value (Pos) = '-' or Num.Value (Pos) = '+' then
         Pos := Pos + 1;
      end if;
      if Num.Value (Pos) = '.' then
         Pos := Pos + 1;
      end if;

      while Pos <= Num.Value'Last loop
         Digits_Count := Digits_Count + 1;
         if Pos > Fore_Last then
            Frac_Digits := Frac_Digits + 1;
         end if;
         To_Next_Digit (Num.Value.all, Pos);
      end loop;

      if Total_Digits > 0 then
         --  Gross estimation
         if Long_Long_Integer (Fore_Last - Fore_First) + Exp >=
           Long_Long_Integer (Total_Digits)
         then
            Validation_Error
              ("Number " & Num.Value.all
               & " has too many digits (totalDigits is"
               & Integer'Image (Total_Digits) & ')');
         end if;

         if Digits_Count > Total_Digits then
            Validation_Error
              ("Number " & Num.Value.all
               & " has too many digits (totalDigits is"
               & Integer'Image (Total_Digits) & ")");
         end if;
      end if;

      if Fraction_Digits >= 0 then
         if Long_Long_Integer (Frac_Digits) - Exp >
           Long_Long_Integer (Fraction_Digits)
         then
            Validation_Error
              ("Number " & Num.Value.all
               & " has too many fractional digits (fractionDigits is"
               & Integer'Image (Fraction_Digits) & ')');
         end if;
      end if;
   end Check_Digits;
end Schema.Decimal;
