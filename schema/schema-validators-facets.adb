with Unicode.CES;   use Unicode.CES;
with Sax.Encodings; use Sax.Encodings;

package body Schema.Validators.Facets is

   ----------
   -- Free --
   ----------

   procedure Free (Facets : in out Facets_Value) is
   begin
      if Facets.Pattern /= null then
         Unchecked_Free (Facets.Pattern);
         Free (Facets.Pattern_String);
      end if;

      if Facets.Enumeration /= null then
         Free (Facets.Enumeration);
      end if;
   end Free;

   -----------------
   -- Check_Facet --
   -----------------

   procedure Check_Facet
     (Facets : in out Facets_Value; Value : Byte_Sequence)
   is
      Found  : Boolean;
      Length : Integer;
      Val    : Long_Long_Integer;
   begin
      if Facets.Mask (Facet_Pattern) then
         if Facets.Pattern = null then
            Facets.Pattern := new Pattern_Matcher '
              (Compile (Facets.Pattern_String.all));
         end if;
         if not Match (Facets.Pattern.all, String (Value)) then
            Validation_Error ("string pattern not matched: "
                              & Facets.Pattern_String.all);
         end if;
      end if;

      if Facets.Mask (Facet_Enumeration) then
         Found := False;
         for E in Facets.Enumeration'Range loop
            if Value = Facets.Enumeration (E).all then
               Found := True;
            end if;
         end loop;

         if not Found then
            Validation_Error ("Element's value not in the enumeration set");
         end if;
      end if;

      if Facets.Mask (Facet_Implicit_Enumeration) then
         if not Facets.Implicit_Enumeration (Value) then
            Validation_Error ("Invalid value: """ & Value & """");
         end if;
      end if;

      if Facets.Mask (Facet_Whitespace) then
         case Facets.Whitespace is
            when Preserve =>
               null; --  Always valid

            when Replace =>
               for C in Value'Range loop
                  if Value (C) = ASCII.HT
                    or else Value (C) = ASCII.LF
                    or else Value (C) = ASCII.CR
                  then
                     Validation_Error ("HT, LF and CR characters not allowed");
                  end if;
               end loop;

            when Collapse =>
               for C in Value'Range loop
                  if Value (C) = ASCII.HT
                    or else Value (C) = ASCII.LF
                    or else Value (C) = ASCII.CR
                  then
                     Validation_Error ("HT, LF and CR characters not allowed");

                  elsif Value (C) = ' '
                    and then C < Value'Last
                    and then Value (C + 1) = ' '
                  then
                     Validation_Error
                       ("Duplicate space characters not allowed");
                  end if;
               end loop;

               --  Leading or trailing white spaces are also forbidden
               if Value'Length /= 0 then
                  if Value (Value'First) = ' ' then
                     Validation_Error ("Leading whitespaces not allowed");
                  elsif Value (Value'Last) = ' ' then
                     Validation_Error ("Trailing whitespaces not allowed");
                  end if;
               end if;
         end case;
      end if;

      if Facets.Mask (Facet_Length)
        or Facets.Mask (Facet_Min_Length)
        or Facets.Mask (Facet_Max_Length)
      then
         Length := Sax.Encodings.Encoding.Length (Value);

         if Facets.Mask (Facet_Length) and then Length /= Facets.Length then
            Validation_Error ("Invalid length");
         end if;

         if Facets.Mask (Facet_Min_Length)
           and then Length < Facets.Min_Length
         then
            Validation_Error ("String is too short, minimum length is"
                              & Integer'Image (Facets.Min_Length)
                              & " characters");
         end if;

         if Facets.Mask (Facet_Max_Length)
           and then Length > Facets.Max_Length
         then
            Validation_Error ("String too long, maximum length is"
                              & Integer'Image (Facets.Max_Length)
                              & " characters");
         end if;
      end if;

      --  If we have a
      if Facets.Mask (Facet_Total_Digits)
        or Facets.Mask (Facet_Fraction_Digits)
        or Facets.Mask (Facet_Max_Exclusive)
        or Facets.Mask (Facet_Max_Inclusive)
        or Facets.Mask (Facet_Min_Exclusive)
        or Facets.Mask (Facet_Min_Inclusive)
      then
         if Facets.Mask (Facet_Total_Digits)
           and then Value'Length /= Facets.Total_Digits
         then
            Validation_Error
              ("The maximum number of digits is"
               & Integer'Image (Facets.Total_Digits));
         end if;

         if Facets.Mask (Facet_Fraction_Digits) then
            for V in Value'Range loop
               if Value (V) = '.' then
                  if Value'Last - V > Facets.Fraction_Digits then
                     Validation_Error
                       ("Too many digits in the fractional part");
                  end if;
               end if;
            end loop;

            if Facets.Fraction_Digits /= 0 then
               declare
                  FVal : Long_Long_Float;
               begin
                  FVal := Long_Long_Float'Value (Value);

                  --  ??? Incorrect, obviously
                  Val := Long_Long_Integer (FVal);
               exception
                  when Constraint_Error =>
                     Validation_Error ("Must have a decimal value");
               end;
            else
               begin
                  Val := Long_Long_Integer'Value (Value);
               exception
                  when Constraint_Error =>
                     Validation_Error ("Value must be an integer");
               end;
            end if;

         else
            begin
               Val := Long_Long_Integer'Value (Value);
            exception
               when Constraint_Error =>
                  Validation_Error ("Value must be an integer");
            end;
         end if;

         if Facets.Mask (Facet_Max_Inclusive)
           and then Facets.Max_Inclusive < Val
         then
            Validation_Error
              ("maxInclusive is set to"
               & Long_Long_Integer'Image (Facets.Max_Inclusive));
         end if;

         if Facets.Mask (Facet_Max_Exclusive)
           and then Facets.Max_Exclusive <= Val
         then
            Validation_Error
              ("maxExclusive is set to"
               & Long_Long_Integer'Image (Facets.Max_Exclusive));
         end if;

         if Facets.Mask (Facet_Min_Inclusive)
           and then Facets.Min_Inclusive > Val
         then
            Validation_Error
              ("minInclusive is set to"
               & Long_Long_Integer'Image (Facets.Min_Inclusive));
         end if;

         if Facets.Mask (Facet_Min_Exclusive)
           and then Facets.Min_Exclusive >= Val
         then
            Validation_Error
              ("minExclusive is set to"
               & Long_Long_Integer'Image (Facets.Min_Exclusive));
         end if;
      end if;
   end Check_Facet;

   ---------------
   -- Add_Facet --
   ---------------

   procedure Add_Facet
     (Facets      : in out Facets_Value;
      Facet_Name  : Byte_Sequence;
      Facet_Value : Byte_Sequence) is
   begin
      if Facet_Name = "enumeration" then
         if not Facets.Settable (Facet_Enumeration) then
            Validation_Error ("Enumeration facet can't be set for this type");
         end if;
         Append (Facets.Enumeration, Facet_Value);
         Facets.Mask (Facet_Enumeration) := True;

      elsif Facet_Name = "whiteSpace" then
         if not Facets.Settable (Facet_Whitespace) then
            Validation_Error ("whiteSpace facet can't be set for this type");
         end if;
         if Facet_Value = "preserve" then
            Facets.Whitespace := Preserve;
         elsif Facet_Value = "replace" then
            Facets.Whitespace := Replace;
         elsif Facet_Value = "collapse" then
            Facets.Whitespace := Collapse;
         else
            Validation_Error
              ("Invalid value for whiteSpace facet: " & Facet_Value);
         end if;
         Facets.Mask (Facet_Whitespace) := True;

      elsif Facet_Name = "pattern" then
         if not Facets.Settable (Facet_Pattern) then
            Validation_Error ("pattern facet can't be set for this type");
         end if;
         Unchecked_Free (Facets.Pattern);
         Free (Facets.Pattern_String);
         Facets.Pattern := null;
         Facets.Pattern_String := new Byte_Sequence'(Facet_Value);
         Facets.Mask (Facet_Pattern) := True;

      elsif Facet_Name = "length" then
         if not Facets.Settable (Facet_Length) then
            Validation_Error ("length facet can't be set for this type");
         end if;
         Facets.Length := Integer'Value (Facet_Value);
         Facets.Mask (Facet_Length) := True;

      elsif Facet_Name = "minLength" then
         if not Facets.Settable (Facet_Min_Length) then
            Validation_Error ("minLength facet can't be set for this type");
         end if;
         Facets.Min_Length := Integer'Value (Facet_Value);
         Facets.Mask (Facet_Min_Length) := True;

      elsif Facet_Name = "maxLength" then
         if not Facets.Settable (Facet_Max_Length) then
            Validation_Error ("maxLength facet can't be set for this type");
         end if;
         Facets.Max_Length := Integer'Value (Facet_Value);
         Facets.Mask (Facet_Max_Length) := True;

      elsif Facet_Name = "totalDigits" then
         if not Facets.Settable (Facet_Total_Digits) then
            Validation_Error ("totalDigits facet can't be set for this type");
         end if;
         Facets.Total_Digits := Integer'Value (Facet_Value);
         Facets.Mask (Facet_Total_Digits) := True;

      elsif Facet_Name = "fractionDigits" then
         if not Facets.Settable (Facet_Fraction_Digits) then
            Validation_Error
              ("fractionDigits facet can't be set for this type");
         end if;
         Facets.Fraction_Digits := Integer'Value (Facet_Value);
         Facets.Mask (Facet_Fraction_Digits) := True;

      elsif Facet_Name = "maxInclusive" then
         if not Facets.Settable (Facet_Max_Inclusive) then
            Validation_Error ("maxInclusive facet can't be set for this type");
         end if;
         Facets.Max_Inclusive := Long_Long_Integer'Value (Facet_Value);
         Facets.Mask (Facet_Max_Inclusive) := True;

      elsif Facet_Name = "maxExclusive" then
         if not Facets.Settable (Facet_Max_Exclusive) then
            Validation_Error ("maxExclusive facet can't be set for this type");
         end if;
         Facets.Max_Exclusive := Long_Long_Integer'Value (Facet_Value);
         Facets.Mask (Facet_Max_Exclusive) := True;

      elsif Facet_Name = "minInclusive" then
         if not Facets.Settable (Facet_Min_Inclusive) then
            Validation_Error ("minInclusive facet can't be set for this type");
         end if;
         Facets.Min_Inclusive := Long_Long_Integer'Value (Facet_Value);
         Facets.Mask (Facet_Min_Inclusive) := True;

      elsif Facet_Name = "minExclusive" then
         if not Facets.Settable (Facet_Min_Exclusive) then
            Validation_Error ("minExclusive facet can't be set for this type");
         end if;
         Facets.Min_Exclusive := Long_Long_Integer'Value (Facet_Value);
         Facets.Mask (Facet_Min_Exclusive) := True;

      else
         Validation_Error ("Invalid facet: " & Facet_Name);
      end if;
   end Add_Facet;

   ------------
   -- Append --
   ------------

   procedure Append
     (List  : in out Byte_Sequence_List_Access;
      Value : Unicode.CES.Byte_Sequence)
   is
      L : Byte_Sequence_List_Access := List;
   begin
      if List /= null then
         L := new Byte_Sequence_List'(List.all & new Byte_Sequence'(Value));
         Unchecked_Free (List);
         List := L;
      else
         List := new Byte_Sequence_List'(1 => new Byte_Sequence'(Value));
      end if;
   end Append;

   ----------
   -- Free --
   ----------

   procedure Free (List : in out Byte_Sequence_List_Access) is
   begin
      if List /= null then
         for L in List'Range loop
            Free (List (L));
         end loop;

         Unchecked_Free (List);
      end if;
   end Free;

end Schema.Validators.Facets;
