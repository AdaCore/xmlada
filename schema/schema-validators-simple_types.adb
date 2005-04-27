with Schema.Validators.Facets; use Schema.Validators.Facets;
with Ada.Calendar;             use Ada.Calendar;
with Sax.Encodings;            use Sax.Encodings;

package body Schema.Validators.Simple_Types is

   function Time_Image
     (Value : Day_Duration) return Unicode.CES.Byte_Sequence;
   --  Convert Value to a string representation

   function Time_Value
     (Ch : Unicode.CES.Byte_Sequence) return Ada.Calendar.Day_Duration;
   --  Convert ch to a Time

   function Time_Component_Image
     (Value : Integer; Num_Digits : Natural := 2) return String;
   --  Return the image of Value, in a string of Num_Digits characters long

   -----------------
   -- Check_Facet --
   -----------------

   procedure Check_Facet
     (Facets : in out Integer_Facets_Description;
      Value  : Unicode.CES.Byte_Sequence)
   is
      Val    : Long_Long_Integer;
      ValF   : Long_Long_Float;
   begin
      Check_Facet (Facets.Facets, Value);

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
                  Validation_Error ("Too many digits in the fractional part");
               end if;
            end if;
         end loop;

         if Facets.Fraction_Digits = 0 then
            begin
               Val := Long_Long_Integer'Value (Value);
            exception
               when Constraint_Error =>
                  Validation_Error ("Value must be an integer");
            end;
         else
            begin
               ValF := Long_Long_Float'Value (Value);
               Val  := Long_Long_Integer (ValF);
            exception
               when Constraint_Error =>
                  Validation_Error ("Must have a decimal value");
            end;
         end if;

      else
         begin
            ValF := Long_Long_Float'Value (Value);
            Val  := Long_Long_Integer (ValF);
         exception
            when Constraint_Error =>
               Validation_Error ("Must have a decimal value");
         end;
      end if;

      if Facets.Mask (Facet_Max_Inclusive)
        and then Facets.Max_Inclusive < Val
      then
         Validation_Error
           (Value & " is too big, maxInclusive is set to"
            & Long_Long_Integer'Image (Facets.Max_Inclusive));
      end if;

      if Facets.Mask (Facet_Max_Exclusive)
        and then Facets.Max_Exclusive <= Val
      then
         Validation_Error
           (Value & " is too big, maxExclusive is set to"
            & Long_Long_Integer'Image (Facets.Max_Exclusive));
      end if;

      if Facets.Mask (Facet_Min_Inclusive)
        and then Facets.Min_Inclusive > Val
      then
         Validation_Error
           (Value & " is too small, minInclusive is set to"
            & Long_Long_Integer'Image (Facets.Min_Inclusive));
      end if;

      if Facets.Mask (Facet_Min_Exclusive)
        and then Facets.Min_Exclusive >= Val
      then
         Validation_Error
           (Value & " is too small, minExclusive is set to"
            & Long_Long_Integer'Image (Facets.Min_Exclusive));
      end if;
   end Check_Facet;

   -------------------------
   -- Validate_Characters --
   -------------------------

   procedure Validate_Characters
     (Validator     : access Integer_Validator_Record;
      Ch            : Unicode.CES.Byte_Sequence;
      Empty_Element : Boolean)
   is
      pragma Unreferenced (Empty_Element);
   begin
      Check_Facet (Validator.Facets, Ch);
   end Validate_Characters;

   ---------------
   -- Add_Facet --
   ---------------

   procedure Add_Facet
     (Facets      : in out Integer_Facets_Description;
      Facet_Name  : Unicode.CES.Byte_Sequence;
      Facet_Value : Unicode.CES.Byte_Sequence;
      Applied     : out Boolean) is
   begin
      Add_Facet (Facets.Facets, Facet_Name, Facet_Value, Applied);
      if Applied then
         null;
      elsif Facet_Name = "totalDigits" then
         Facets.Total_Digits := Integer'Value (Facet_Value);
         Facets.Mask (Facet_Total_Digits) := True;
         Applied := True;
      elsif Facet_Name = "fractionDigits" then
         Facets.Fraction_Digits := Integer'Value (Facet_Value);
         Facets.Mask (Facet_Fraction_Digits) := True;
         Applied := True;
      elsif Facet_Name = "maxInclusive" then
         Facets.Max_Inclusive := Long_Long_Integer'Value (Facet_Value);
         Facets.Mask (Facet_Max_Inclusive) := True;
         Applied := True;
      elsif Facet_Name = "maxExclusive" then
         Facets.Max_Exclusive := Long_Long_Integer'Value (Facet_Value);
         Facets.Mask (Facet_Max_Exclusive) := True;
         Applied := True;
      elsif Facet_Name = "minInclusive" then
         Facets.Min_Inclusive := Long_Long_Integer'Value (Facet_Value);
         Facets.Mask (Facet_Min_Inclusive) := True;
         Applied := True;
      elsif Facet_Name = "minExclusive" then
         Facets.Min_Exclusive := Long_Long_Integer'Value (Facet_Value);
         Facets.Mask (Facet_Min_Exclusive) := True;
         Applied := True;
      else
         Applied := False;
      end if;
   exception
      when Constraint_Error =>
         Applied := False;
   end Add_Facet;

   ---------------
   -- Add_Facet --
   ---------------

   procedure Add_Facet
     (Validator   : access Integer_Validator_Record;
      Facet_Name  : Unicode.CES.Byte_Sequence;
      Facet_Value : Unicode.CES.Byte_Sequence)
   is
      Applies : Boolean;
   begin
      Add_Facet (Validator.Facets, Facet_Name, Facet_Value, Applies);
      if not Applies then
         Validation_Error ("Invalid facet: " & Facet_Name);
      end if;
   end Add_Facet;

   -------------------------
   -- Validate_Characters --
   -------------------------

   procedure Validate_Characters
     (Validator     : access Boolean_Validator_Record;
      Ch            : Unicode.CES.Byte_Sequence;
      Empty_Element : Boolean)
   is
      pragma Unreferenced (Empty_Element);
   begin
      Debug_Output ("Validate_Characters for boolean --" & Ch & "--"
                    & Get_Name (Validator));

      Check_Facet (Validator.Facets, Ch);

      if Ch /= "true"
        and then Ch /= "false"
        and then Ch /= "0"
        and then Ch /= "1"
      then
         Validation_Error ("Invalid value for boolean type: """ & Ch & """");
      end if;
   end Validate_Characters;

   ---------------
   -- Add_Facet --
   ---------------

   procedure Add_Facet
     (Validator   : access Boolean_Validator_Record;
      Facet_Name  : Unicode.CES.Byte_Sequence;
      Facet_Value : Unicode.CES.Byte_Sequence)
   is
      Applies : Boolean;
   begin
      Add_Facet (Validator.Facets, Facet_Name, Facet_Value, Applies);
      if not Applies then
         Validation_Error ("Invalid facet: " & Facet_Name);
      end if;
   end Add_Facet;

   ----------------------------
   -- Get_Facets_Description --
   ----------------------------

   function Get_Facets_Description
     (Validator : access Integer_Validator_Record)
      return Facets_Description
   is
      pragma Unreferenced (Validator);
   begin
      return new Integer_Facets_Description;
   end Get_Facets_Description;

   ----------------------------
   -- Get_Facets_Description --
   ----------------------------

   function Get_Facets_Description
     (Validator : access String_Validator_Record)
      return Facets_Description
   is
      pragma Unreferenced (Validator);
   begin
      return new String_Facets_Description;
   end Get_Facets_Description;

   ----------------------------
   -- Get_Facets_Description --
   ----------------------------

   function Get_Facets_Description
     (Validator : access Time_Validator_Record)
      return Facets_Description
   is
      pragma Unreferenced (Validator);
   begin
      return new Time_Facets_Description;
   end Get_Facets_Description;

   ----------
   -- Free --
   ----------

   procedure Free (Validator : in out Integer_Validator_Record) is
   begin
      Free (Validator.Facets);
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Facets : in out Integer_Facets_Description) is
   begin
      Free (Facets.Facets);
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Validator : in out Boolean_Validator_Record) is
   begin
      Free (Validator.Facets);
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Validator : in out Time_Validator_Record) is
   begin
      Free (Validator.Facets);
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Facets : in out Time_Facets_Description) is
   begin
      Free (Facets.Facets);
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Validator : in out String_Validator_Record) is
   begin
      Free (Validator.Facets);
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Facets : in out String_Facets_Description) is
   begin
      Free (Facets.Facets);
   end Free;

   -----------------
   -- Check_Facet --
   -----------------

   procedure Check_Facet
     (Facets : in out String_Facets_Description;
      Value  : Unicode.CES.Byte_Sequence)
   is
      Length : Integer;
   begin
      Check_Facet (Facets.Facets, Value);

      if Facets.Mask (Facet_Length)
        or else Facets.Mask (Facet_Min_Length)
        or else Facets.Mask (Facet_Max_Length)
      then
         Length := Sax.Encodings.Encoding.Length (Value);

         if Facets.Mask (Facet_Length)
           and then Facets.Length /= Length
         then
            Validation_Error
              ("Invalid length, must be" & Integer'Image (Facets.Length)
               & " characters");
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
   end Check_Facet;

   -------------------------
   -- Validate_Characters --
   -------------------------

   procedure Validate_Characters
     (Validator     : access String_Validator_Record;
      Ch            : Unicode.CES.Byte_Sequence;
      Empty_Element : Boolean)
   is
      pragma Unreferenced (Empty_Element);
   begin
      Check_Facet (Validator.Facets, Ch);
   end Validate_Characters;

   ---------------
   -- Add_Facet --
   ---------------

   procedure Add_Facet
     (Facets      : in out String_Facets_Description;
      Facet_Name  : Unicode.CES.Byte_Sequence;
      Facet_Value : Unicode.CES.Byte_Sequence;
      Applied     : out Boolean) is
   begin
      Add_Facet (Facets.Facets, Facet_Name, Facet_Value, Applied);
      if Applied then
         null;
      elsif Facet_Name = "length" then
         Facets.Length := Integer'Value (Facet_Value);
         Facets.Mask (Facet_Length) := True;
         Applied := True;
      elsif Facet_Name = "minLength" then
         Facets.Min_Length := Integer'Value (Facet_Value);
         Facets.Mask (Facet_Min_Length) := True;
         Applied := True;
      elsif Facet_Name = "maxLength" then
         Facets.Max_Length := Integer'Value (Facet_Value);
         Facets.Mask (Facet_Max_Length) := True;
         Applied := True;
      else
         Applied := False;
      end if;
   end Add_Facet;

   ---------------
   -- Add_Facet --
   ---------------

   procedure Add_Facet
     (Validator   : access String_Validator_Record;
      Facet_Name  : Unicode.CES.Byte_Sequence;
      Facet_Value : Unicode.CES.Byte_Sequence)
   is
      Applies : Boolean;
   begin
      Add_Facet (Validator.Facets, Facet_Name, Facet_Value, Applies);
      if not Applies then
         Validation_Error ("Invalid facet: " & Facet_Name);
      end if;
   end Add_Facet;

   ----------------
   -- Time_Value --
   ----------------

   function Time_Value
     (Ch : Unicode.CES.Byte_Sequence) return Ada.Calendar.Day_Duration
   is
      --  Format is "hh:mm:ss.sss[+-]hh:mm"
      Dur : Day_Duration := 0.0;
      Tmp : Integer;
   begin
      Tmp := Integer'Value (Ch (Ch'First .. Ch'First + 1));
      Dur := Dur + Day_Duration (Tmp) * 3600.0;

      if Ch (Ch'First + 2) /= ':' then
         Validation_Error ("Invalid Time format: """ & Ch & """");
      end if;

      Tmp := Integer'Value (Ch (Ch'First + 3 .. Ch'First + 4));
      Dur := Dur + Day_Duration (Tmp) * 60.0;

      if Ch (Ch'First + 5) /= ':' then
         Validation_Error ("Invalid Time format: """ & Ch & """");
      end if;

      Tmp := Integer'Value (Ch (Ch'First + 6 .. Ch'First + 7));
      Dur := Dur + Day_Duration (Tmp);

      if Ch'Length > 8 then
         if Ch (Ch'First + 8) = '.' then
            Tmp := Integer'Value (Ch (Ch'First + 9 .. Ch'First + 11));
            Dur := Dur + Day_Duration (Tmp) / 1000.0;
         end if;
         --  ??? Timezones ignored
      end if;

      return Dur;

   exception
      when Constraint_Error =>
         Validation_Error ("Invalid Time format: """ & Ch & """");
         return 0.0;
   end Time_Value;

   --------------------------
   -- Time_Component_Image --
   --------------------------

   function Time_Component_Image
     (Value : Integer; Num_Digits : Natural := 2) return String
   is
      Str : constant String := Integer'Image (Value);
      Padding : constant String (1 .. Num_Digits) := (others => '0');
   begin
      return Padding (1 .. Num_Digits - Str'Last + Str'First)
        & Str (Str'First + 1 .. Str'Last);
   end Time_Component_Image;

   ----------------
   -- Time_Image --
   ----------------

   function Time_Image
     (Value : Day_Duration) return Unicode.CES.Byte_Sequence
   is
      Hour : constant Integer := Integer
        (Float (Value) / 3600.0 - 0.5);
      Min  : constant Integer :=
        Integer ((Float (Value) - Float (Hour) * 3600.0) / 60.0 - 0.5);
      Sec  : constant Integer := Integer
        (Float (Value) - Float (Hour) * 3600.0 - Float (Min) * 60.0);
      Sub_Sec : constant Integer := Integer
        ((Float (Value) - Float (Hour) * 3600.0 - Float (Min) * 60.0
         - Float (Sec)) * 1000.0);
   begin
      return Time_Component_Image (Hour) & ':'
        & Time_Component_Image (Min) & ':'
        & Time_Component_Image (Sec) & '.'
        & Time_Component_Image (Sub_Sec, 3);
   end Time_Image;

   -----------------
   -- Check_Facet --
   -----------------

   procedure Check_Facet
     (Facets : in out Time_Facets_Description;
      Value  : Unicode.CES.Byte_Sequence)
   is
      Val : Ada.Calendar.Day_Duration;
   begin
      Check_Facet (Facets.Facets, Value);

      Val := Time_Value (Value);

      if Facets.Mask (Facet_Max_Exclusive)
        and then Facets.Max_Exclusive <= Val
      then
         Validation_Error
           (Value & " is too late, maxExclusive is set to "
            & Time_Image (Facets.Max_Exclusive));
      end if;

      if Facets.Mask (Facet_Max_Inclusive)
        and then Facets.Max_Inclusive < Val
      then
         Validation_Error
           (Value & " is too late, maxInclusive is set to "
            & Time_Image (Facets.Max_Inclusive));
      end if;

      if Facets.Mask (Facet_Min_Inclusive)
        and then Facets.Min_Inclusive > Val
      then
         Validation_Error
           (Value & " is too early, minInclusive is set to "
            & Time_Image (Facets.Min_Inclusive));
      end if;

      if Facets.Mask (Facet_Min_Exclusive)
        and then Facets.Min_Exclusive > Val
      then
         Validation_Error
           (Value & " is too early, minExclusive is set to "
            & Time_Image (Facets.Min_Exclusive));
      end if;
   end Check_Facet;

   -------------------------
   -- Validate_Characters --
   -------------------------

   procedure Validate_Characters
     (Validator     : access Time_Validator_Record;
      Ch            : Unicode.CES.Byte_Sequence;
      Empty_Element : Boolean)
   is
      pragma Unreferenced (Empty_Element);
   begin
      Check_Facet (Validator.Facets, Ch);
   end Validate_Characters;

   ---------------
   -- Add_Facet --
   ---------------

   procedure Add_Facet
     (Facets      : in out Time_Facets_Description;
      Facet_Name  : Unicode.CES.Byte_Sequence;
      Facet_Value : Unicode.CES.Byte_Sequence;
      Applied     : out Boolean) is
   begin
      Add_Facet (Facets.Facets, Facet_Name, Facet_Value, Applied);
      if Applied then
         null;
      elsif Facet_Name = "maxInclusive" then
         Facets.Max_Inclusive := Time_Value (Facet_Value);
         Facets.Mask (Facet_Max_Inclusive) := True;
         Applied := True;
      elsif Facet_Name = "maxExclusive" then
         Facets.Max_Exclusive := Time_Value (Facet_Value);
         Facets.Mask (Facet_Max_Exclusive) := True;
         Applied := True;
      elsif Facet_Name = "minInclusive" then
         Facets.Min_Inclusive := Time_Value (Facet_Value);
         Facets.Mask (Facet_Min_Inclusive) := True;
         Applied := True;
      elsif Facet_Name = "minExclusive" then
         Facets.Min_Exclusive := Time_Value (Facet_Value);
         Facets.Mask (Facet_Min_Exclusive) := True;
         Applied := True;
      else
         Applied := False;
      end if;
   end Add_Facet;

   ---------------
   -- Add_Facet --
   ---------------

   procedure Add_Facet
     (Validator   : access Time_Validator_Record;
      Facet_Name  : Unicode.CES.Byte_Sequence;
      Facet_Value : Unicode.CES.Byte_Sequence)
   is
      Applies : Boolean;
   begin
      Add_Facet (Validator.Facets, Facet_Name, Facet_Value, Applies);
      if not Applies then
         Validation_Error ("Invalid facet: " & Facet_Name);
      end if;
   end Add_Facet;

   ---------------
   -- Add_Facet --
   ---------------

   procedure Add_Facet
     (Facets      : in out Float_Facets_Description;
      Facet_Name  : Unicode.CES.Byte_Sequence;
      Facet_Value : Unicode.CES.Byte_Sequence;
      Applied     : out Boolean) is
   begin
      Add_Facet (Facets.Common, Facet_Name, Facet_Value, Applied);
      if Applied then
         null;
      elsif Facet_Name = "minInclusive" then
         Facets.Min_Inclusive := Long_Long_Float'Value (Facet_Value);
         Facets.Mask (Facet_Min_Inclusive) := True;
         Applied := True;
      elsif Facet_Name = "maxInclusive" then
         Facets.Max_Inclusive := Long_Long_Float'Value (Facet_Value);
         Facets.Mask (Facet_Max_Inclusive) := True;
         Applied := True;
      elsif Facet_Name = "minExclusive" then
         Facets.Min_Exclusive := Long_Long_Float'Value (Facet_Value);
         Facets.Mask (Facet_Min_Exclusive) := True;
         Applied := True;
      elsif Facet_Name = "maxExclusive" then
         Facets.Max_Exclusive := Long_Long_Float'Value (Facet_Value);
         Facets.Mask (Facet_Max_Exclusive) := True;
         Applied := True;
      else
         Applied := False;
      end if;
   exception
      when Constraint_Error =>
         Applied := False;
   end Add_Facet;

   -----------------
   -- Check_Facet --
   -----------------

   procedure Check_Facet
     (Facets : in out Float_Facets_Description;
      Value  : Unicode.CES.Byte_Sequence)
   is
      Val : Long_Long_Float;
   begin
      Check_Facet (Facets.Common, Value);

      if Value = "NaN" then
         if Facets.Mask (Facet_Max_Inclusive)
           or Facets.Mask (Facet_Max_Exclusive)
         then
            Validation_Error
              ("NaN is greater than all numbers, and too big in this context");
         end if;

      elsif Value = "INF" then
         if Facets.Mask (Facet_Max_Inclusive)
           or Facets.Mask (Facet_Max_Exclusive)
         then
            Validation_Error
              ("INF is greater than maxInclusive and maxExclusive");
         end if;

      elsif Value = "-INF" then
         if Facets.Mask (Facet_Min_Inclusive)
           or Facets.Mask (Facet_Min_Exclusive)
         then
            Validation_Error
              ("-INF is smaller than minInclusive and minExclusive");
         end if;

      else
         begin
            Val := Long_Long_Float'Value (Value);
         exception
            when Constraint_Error =>
               Validation_Error ("Expecting a floating number");
         end;

         if Facets.Mask (Facet_Min_Inclusive)
           and then Facets.Min_Inclusive > Val
         then
            Validation_Error (Value & " is smaller than minInclusive");
         end if;

         if Facets.Mask (Facet_Min_Exclusive)
           and then Facets.Min_Exclusive >= Val
         then
            Validation_Error (Value & " is smaller than minExclusive");
         end if;

         if Facets.Mask (Facet_Max_Inclusive)
           and then Facets.Max_Inclusive < Val
         then
            Validation_Error (Value & " is greater than maxInclusive");
         end if;

         if Facets.Mask (Facet_Max_Exclusive)
           and then Facets.Max_Exclusive <= Val
         then
            Validation_Error (Value & " is greater than maxExclusive");
         end if;
      end if;
   end Check_Facet;

   ----------
   -- Free --
   ----------

   procedure Free (Facets : in out Float_Facets_Description) is
   begin
      Free (Facets.Common);
   end Free;

   -------------------------
   -- Validate_Characters --
   -------------------------

   procedure Validate_Characters
     (Validator     : access Float_Validator_Record;
      Ch            : Unicode.CES.Byte_Sequence;
      Empty_Element : Boolean)
   is
      pragma Unreferenced (Empty_Element);
   begin
      Check_Facet (Validator.Facets, Ch);
   end Validate_Characters;

   ---------------
   -- Add_Facet --
   ---------------

   procedure Add_Facet
     (Validator   : access Float_Validator_Record;
      Facet_Name  : Unicode.CES.Byte_Sequence;
      Facet_Value : Unicode.CES.Byte_Sequence)
   is
      Applies : Boolean;
   begin
      Add_Facet (Validator.Facets, Facet_Name, Facet_Value, Applies);
      if not Applies then
         Validation_Error ("Invalid facet: " & Facet_Name);
      end if;
   end Add_Facet;

   ----------
   -- Free --
   ----------

   procedure Free (Validator : in out Float_Validator_Record) is
   begin
      Free (Validator.Facets);
   end Free;

   ----------------------------
   -- Get_Facets_Description --
   ----------------------------

   function Get_Facets_Description
     (Validator : access Float_Validator_Record)
      return Facets_Description
   is
      pragma Unreferenced (Validator);
   begin
      return new Float_Facets_Description;
   end Get_Facets_Description;

end Schema.Validators.Simple_Types;
