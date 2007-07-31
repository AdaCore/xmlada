-----------------------------------------------------------------------
--                XML/Ada - An XML suite for Ada95                   --
--                                                                   --
--                       Copyright (C) 2004-2007, AdaCore            --
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

with Ada.Strings.Fixed;         use Ada.Strings.Fixed;
with Schema.Validators.Facets;  use Schema.Validators.Facets;
with Sax.Encodings;             use Sax.Encodings;
with Sax.Utils;                 use Sax.Utils;
with Schema.Date_Time;          use Schema.Date_Time;
with Schema.Decimal;            use Schema.Decimal;
with Unicode.CES;               use Unicode, Unicode.CES;
with Unicode.Names.Basic_Latin; use Unicode.Names.Basic_Latin;

package body Schema.Validators.Simple_Types is

   B64 : constant array (Unicode_Char range 32 .. 128) of Boolean :=
      (Character'Pos ('A') .. Character'Pos ('Z') => True,
       Character'Pos ('a') .. Character'Pos ('z') => True,
       Character'Pos ('0') .. Character'Pos ('9') => True,
       Character'Pos ('+') => True,
       Character'Pos ('/') => True,
       others => False);
   B04 : constant array (Unicode_Char range 32 .. 128) of Boolean :=
      (Character'Pos ('A') => True,
       Character'Pos ('Q') => True,
       Character'Pos ('g') => True,
       Character'Pos ('w') => True,
       others => False);
   B16 : constant array (Unicode_Char range 32 .. 128) of Boolean :=
      (Character'Pos ('A') => True,
       Character'Pos ('E') => True,
       Character'Pos ('I') => True,
       Character'Pos ('M') => True,
       Character'Pos ('Q') => True,
       Character'Pos ('U') => True,
       Character'Pos ('Y') => True,
       Character'Pos ('c') => True,
       Character'Pos ('g') => True,
       Character'Pos ('k') => True,
       Character'Pos ('o') => True,
       Character'Pos ('s') => True,
       Character'Pos ('w') => True,
       Character'Pos ('0') => True,
       Character'Pos ('4') => True,
       Character'Pos ('8') => True,
       others => False);
   --  Whether the character matches the Base64Binary definitions

   function Image (Value : Long_Long_Float) return String;
   --  Return a string version of Value

   -----------
   -- Image --
   -----------

   function Image (Value : Long_Long_Float) return String is
      Str : constant String := Long_Long_Float'Image (Value);
      E   : constant Integer := Index (Str, "E");
   begin
      if E < Str'First then
         for J in reverse Str'Range loop
            if Str (J) /= '0' then
               return Str (Str'First .. J);
            end if;
         end loop;
      else
         for J in reverse Str'First .. E - 1 loop
            if Str (J) /= '0' then
               return Str (Str'First .. J) & Str (E .. Str'Last);
            end if;
         end loop;
      end if;
      return Str;
   end Image;

   ------------------------------------
   --  Facets used for ranged values --
   ------------------------------------

   generic
      Type_Name : String;
      type T is private;
      with function Value (Ch : Unicode.CES.Byte_Sequence) return T is <>;
      with function Image (T1 : T) return Unicode.CES.Byte_Sequence is <>;
      with function "<=" (T1, T2 : T) return Boolean is <>;
      with function "<" (T1, T2 : T) return Boolean is <>;
      with function ">=" (T1, T2 : T) return Boolean is <>;
      with function ">" (T1, T2 : T) return Boolean is <>;
   package Generic_Range_Facets is
      type Range_Facets_Description is new Common_Facets_Description with
         record
            Max_Inclusive  : T;
            Min_Inclusive  : T;
            Max_Exclusive  : T;
            Min_Exclusive  : T;
         end record;
   private
      procedure Add_Facet
        (Facets      : in out Range_Facets_Description;
         Facet_Name  : Unicode.CES.Byte_Sequence;
         Facet_Value : Unicode.CES.Byte_Sequence;
         Applied     : out Boolean);
      procedure Check_Facet
        (Facets     : in out Range_Facets_Description;
         Node_Value : Unicode.CES.Byte_Sequence);
      procedure Copy
        (From : Range_Facets_Description;
         To   : in out Facets_Description_Record'Class);
      --  See doc for inherited subprograms
   end Generic_Range_Facets;

   ------------------------------------
   --  Facets used for length values --
   ------------------------------------

   generic
      with function Get_Length
        (Value : Unicode.CES.Byte_Sequence) return Natural;
   package Length_Facets is
      type Length_Facets_Description is new Common_Facets_Description with
         record
            Length      : Natural := Natural'Last;
            Min_Length  : Natural := 0;
            Max_Length  : Natural := Natural'Last;
         end record;
   private
      procedure Add_Facet
        (Facets      : in out Length_Facets_Description;
         Facet_Name  : Unicode.CES.Byte_Sequence;
         Facet_Value : Unicode.CES.Byte_Sequence;
         Applied     : out Boolean);
      procedure Check_Facet
        (Facets : in out Length_Facets_Description;
         Value  : Unicode.CES.Byte_Sequence);
      procedure Copy
        (From : Length_Facets_Description;
         To   : in out Facets_Description_Record'Class);
   end Length_Facets;

   -----------------------
   -- Generic validator --
   -----------------------
   --  This validator can be used to implement several other validators
   --  when they all delegate their work to their facets checker.
   --  It can be used for all types which have no children nodes.

   generic
      type Facets_Type is new Facets_Description_Record with private;
   package Generic_Simple_Validator is
      type Validator_Record is new Any_Simple_XML_Validator_Record with record
         Facets : Facets_Type;
      end record;
      type Validator is access all Validator_Record'Class;

   private
      procedure Validate_Characters
        (Validator     : access Validator_Record;
         Ch            : Unicode.CES.Byte_Sequence;
         Empty_Element : Boolean);
      procedure Add_Facet
        (Validator   : access Validator_Record;
         Facet_Name  : Unicode.CES.Byte_Sequence;
         Facet_Value : Unicode.CES.Byte_Sequence);
      procedure Free (Validator : in out Validator_Record);
      function Get_Facets_Description
        (Validator : access Validator_Record) return Facets_Description;
      --  See doc for inherited subprograms
   end Generic_Simple_Validator;

   --------------------------
   -- Generic_Range_Facets --
   --------------------------

   package body Generic_Range_Facets is

      ----------
      -- Copy --
      ----------

      procedure Copy
        (From : Range_Facets_Description;
         To   : in out Facets_Description_Record'Class)
      is
      begin
         Copy (From => Common_Facets_Description (From), To => To);
         Range_Facets_Description (To).Max_Inclusive := From.Max_Inclusive;
         Range_Facets_Description (To).Min_Inclusive := From.Min_Inclusive;
         Range_Facets_Description (To).Max_Exclusive := From.Max_Exclusive;
         Range_Facets_Description (To).Min_Exclusive := From.Min_Exclusive;
      end Copy;

      -----------------
      -- Check_Facet --
      -----------------

      procedure Check_Facet
        (Facets : in out Range_Facets_Description;
         Node_Value  : Unicode.CES.Byte_Sequence)
      is
         Val : T;
      begin
         Val := Value (Node_Value);

         Check_Facet (Common_Facets_Description (Facets), Node_Value);

         if Facets.Mask (Facet_Max_Exclusive)
           and then Facets.Max_Exclusive <= Val
         then
            Validation_Error
              (Node_Value & " is greater than maxExclusive ("
               & Image (Facets.Max_Exclusive) & ")");
         end if;

         if Facets.Mask (Facet_Max_Inclusive)
           and then Facets.Max_Inclusive < Val
         then
            Validation_Error
              (Node_Value & " is greater than maxInclusive ("
               & Image (Facets.Max_Inclusive) & ")");
         end if;

         if Facets.Mask (Facet_Min_Inclusive)
           and then Facets.Min_Inclusive > Val
         then
            Validation_Error
              (Node_Value & " is smaller than minInclusive ("
               & Image (Facets.Min_Inclusive) & ")");
         end if;

         if Facets.Mask (Facet_Min_Exclusive)
           and then Facets.Min_Exclusive >= Val
         then
            Validation_Error
              (Node_Value & " is smaller than minExclusive ("
               & Image (Facets.Min_Exclusive) & ")");
         end if;
      exception
         when Constraint_Error =>
            Validation_Error
              ("Invalid " & Type_Name & ": """ & Node_Value & """");
      end Check_Facet;

      ---------------
      -- Add_Facet --
      ---------------

      procedure Add_Facet
        (Facets      : in out Range_Facets_Description;
         Facet_Name  : Unicode.CES.Byte_Sequence;
         Facet_Value : Unicode.CES.Byte_Sequence;
         Applied     : out Boolean) is
      begin
         Add_Facet
           (Common_Facets_Description (Facets), Facet_Name, Facet_Value,
            Applied);
         if Applied then
            null;
         elsif Facet_Name = "maxInclusive" then
            Facets.Max_Inclusive := Value (Facet_Value);
            Facets.Mask (Facet_Max_Inclusive) := True;
            Applied := True;
         elsif Facet_Name = "maxExclusive" then
            Facets.Max_Exclusive := Value (Facet_Value);
            Facets.Mask (Facet_Max_Exclusive) := True;
            Applied := True;
         elsif Facet_Name = "minInclusive" then
            Facets.Min_Inclusive := Value (Facet_Value);
            Facets.Mask (Facet_Min_Inclusive) := True;
            Applied := True;
         elsif Facet_Name = "minExclusive" then
            Facets.Min_Exclusive := Value (Facet_Value);
            Facets.Mask (Facet_Min_Exclusive) := True;
            Applied := True;
         else
            Applied := False;
         end if;

      exception
         when Constraint_Error =>
            Validation_Error
              ("Invalid " & Facet_Name & ": """ & Facet_Value & """");
      end Add_Facet;
   end Generic_Range_Facets;

   -------------------
   -- Length_Facets --
   -------------------

   package body Length_Facets is

      ----------
      -- Copy --
      ----------

      procedure Copy
        (From : Length_Facets_Description;
         To   : in out Facets_Description_Record'Class) is
      begin
         Copy (Common_Facets_Description (From), To);
         Length_Facets_Description (To).Length     := From.Length;
         Length_Facets_Description (To).Min_Length := From.Min_Length;
         Length_Facets_Description (To).Max_Length := From.Max_Length;
      end Copy;

      ---------------
      -- Add_Facet --
      ---------------

      procedure Add_Facet
        (Facets      : in out Length_Facets_Description;
         Facet_Name  : Unicode.CES.Byte_Sequence;
         Facet_Value : Unicode.CES.Byte_Sequence;
         Applied     : out Boolean) is
      begin
         Add_Facet (Common_Facets_Description (Facets), Facet_Name,
                    Facet_Value, Applied);
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

      -----------------
      -- Check_Facet --
      -----------------

      procedure Check_Facet
        (Facets : in out Length_Facets_Description;
         Value  : Unicode.CES.Byte_Sequence)
      is
         Length : Integer;
      begin
         Check_Facet (Common_Facets_Description (Facets), Value);

         if Facets.Mask (Facet_Length)
           or else Facets.Mask (Facet_Min_Length)
           or else Facets.Mask (Facet_Max_Length)
         then
            Length := Get_Length (Value);

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
   end Length_Facets;

   ------------------------------
   -- Generic_Simple_Validator --
   ------------------------------

   package body Generic_Simple_Validator is

      -------------------------
      -- Validate_Characters --
      -------------------------

      procedure Validate_Characters
        (Validator     : access Validator_Record;
         Ch            : Unicode.CES.Byte_Sequence;
         Empty_Element : Boolean)
      is
         pragma Unreferenced (Empty_Element);
      begin
         if Debug then
            Debug_Output ("Validate_Characters for --" & Ch & "--"
                          & Get_Name (Validator));
         end if;
         Check_Facet (Validator.Facets, Ch);
      end Validate_Characters;

      ---------------
      -- Add_Facet --
      ---------------

      procedure Add_Facet
        (Validator   : access Validator_Record;
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

      procedure Free (Validator : in out Validator_Record) is
      begin
         Free (Validator.Facets);
         Free (Any_Simple_XML_Validator_Record (Validator));
      end Free;

      ----------------------------
      -- Get_Facets_Description --
      ----------------------------

      function Get_Facets_Description
        (Validator : access Validator_Record) return Facets_Description
      is
         --  pragma Unreferenced (Validator);
         Result : Facets_Description;
      begin
         Result := new Facets_Type;
         Copy (From => Validator.Facets, To => Result.all);
         return Result;
      end Get_Facets_Description;

   end Generic_Simple_Validator;

   ------------------
   -- Simple types --
   ------------------

   package Time_Facets_Package is new Generic_Range_Facets ("time", Time_T);
   package Time_Validators is new Generic_Simple_Validator
     (Time_Facets_Package.Range_Facets_Description);

   package Date_Time_Facets_Package is new Generic_Range_Facets
     ("dateTime", Date_Time_T);
   package Date_Time_Validators is new Generic_Simple_Validator
     (Date_Time_Facets_Package.Range_Facets_Description);

   package GDay_Facets_Package is new Generic_Range_Facets
     ("gDay", GDay_T);
   package GDay_Validators is new Generic_Simple_Validator
     (GDay_Facets_Package.Range_Facets_Description);

   package GMonth_Day_Facets_Package is new Generic_Range_Facets
     ("gMonthDay", GMonth_Day_T);
   package GMonth_Day_Validators is new Generic_Simple_Validator
     (GMonth_Day_Facets_Package.Range_Facets_Description);

   package GMonth_Facets_Package is new Generic_Range_Facets
     ("gMonth", GMonth_T);
   package GMonth_Validators is new Generic_Simple_Validator
     (GMonth_Facets_Package.Range_Facets_Description);

   package GYear_Facets_Package is new Generic_Range_Facets
     ("gYear", GYear_T);
   package GYear_Validators is new Generic_Simple_Validator
     (GYear_Facets_Package.Range_Facets_Description);

   package GYear_Month_Facets_Package is new Generic_Range_Facets
     ("gYearMonth", GYear_Month_T);
   package GYear_Month_Validators is new Generic_Simple_Validator
     (GYear_Month_Facets_Package.Range_Facets_Description);

   package Date_Facets_Package is new Generic_Range_Facets
     ("date", Date_T);
   package Date_Validators is new Generic_Simple_Validator
     (Date_Facets_Package.Range_Facets_Description);

   package Duration_Facets_Package is new Generic_Range_Facets
     ("duration", Duration_T);
   package Duration_Validators is new Generic_Simple_Validator
     (Duration_Facets_Package.Range_Facets_Description);

   package Float_Facets_Package is new Generic_Range_Facets
     ("float", Long_Long_Float, Long_Long_Float'Value, Image);
   type Float_Facets_Description is
     new Float_Facets_Package.Range_Facets_Description with null record;
   procedure Check_Facet
     (Facets      : in out Float_Facets_Description;
      Facet_Value : Unicode.CES.Byte_Sequence);
   package Float_Validators is new Generic_Simple_Validator
     (Float_Facets_Description);

   package Decimal_Facets_Package is new Generic_Range_Facets
     ("decimal", Arbitrary_Precision_Number, Value, Image);
   type Decimal_Facets_Description is new
     Decimal_Facets_Package.Range_Facets_Description with
      record
         Total_Digits    : Positive := Positive'Last;
         Fraction_Digits : Natural := Natural'Last;
      end record;
   procedure Add_Facet
     (Facets      : in out Decimal_Facets_Description;
      Facet_Name  : Unicode.CES.Byte_Sequence;
      Facet_Value : Unicode.CES.Byte_Sequence;
      Applied     : out Boolean);
   procedure Copy
     (From : Decimal_Facets_Description;
      To   : in out Facets_Description_Record'Class);
   procedure Check_Facet
     (Facets      : in out Decimal_Facets_Description;
      Facet_Value : Unicode.CES.Byte_Sequence);
   package Decimal_Validators is new Generic_Simple_Validator
     (Decimal_Facets_Description);

   package Integer_Facets_Package is new Generic_Range_Facets
     ("integer",
      Long_Long_Integer, Long_Long_Integer'Value, Long_Long_Integer'Image);
   type Integer_Facets_Description is new
     Integer_Facets_Package.Range_Facets_Description
   with record
      Total_Digits    : Positive := Positive'Last;
   end record;
   procedure Copy
     (From : Integer_Facets_Description;
      To   : in out Facets_Description_Record'Class);
   procedure Add_Facet
     (Facets      : in out Integer_Facets_Description;
      Facet_Name  : Unicode.CES.Byte_Sequence;
      Facet_Value : Unicode.CES.Byte_Sequence;
      Applied     : out Boolean);
   procedure Check_Facet
     (Facets      : in out Integer_Facets_Description;
      Facet_Value : Unicode.CES.Byte_Sequence);
   package Integer_Validators is new Generic_Simple_Validator
     (Integer_Facets_Description);

   type Boolean_Validator_Record is new Any_Simple_XML_Validator_Record with
      record
         Facets : Common_Facets_Description;
      end record;
   procedure Validate_Characters
     (Validator     : access Boolean_Validator_Record;
      Ch            : Unicode.CES.Byte_Sequence;
      Empty_Element : Boolean);
   procedure Add_Facet
     (Validator   : access Boolean_Validator_Record;
      Facet_Name  : Unicode.CES.Byte_Sequence;
      Facet_Value : Unicode.CES.Byte_Sequence);
   procedure Free (Validator : in out Boolean_Validator_Record);
   --   See doc from inherited subprograms

   ----------------------
   -- String_Validator --
   ----------------------

   function String_Get_Length
     (Value : Unicode.CES.Byte_Sequence) return Natural;
   package String_Facets is new Length_Facets (String_Get_Length);
   package String_Validators is new Generic_Simple_Validator
     (String_Facets.Length_Facets_Description);

   function String_List_Get_Length
     (Value : Unicode.CES.Byte_Sequence) return Natural;
   package String_List_Facets is new Length_Facets (String_List_Get_Length);
   package String_List_Validators is new Generic_Simple_Validator
     (String_List_Facets.Length_Facets_Description);

   function HexBinary_Get_Length
     (Value : Unicode.CES.Byte_Sequence) return Natural;
   package HexBinary_Facets is new Length_Facets (HexBinary_Get_Length);
   package HexBinary_Validators is new Generic_Simple_Validator
     (HexBinary_Facets.Length_Facets_Description);

   function Base64Binary_Get_Length
     (Value : Unicode.CES.Byte_Sequence) return Natural;
   function Is_Valid_Base64Binary
     (Value : Unicode.CES.Byte_Sequence) return Boolean;
   package Base64Binary_Facets is new Length_Facets (Base64Binary_Get_Length);
   package Base64Binary_Validators is new Generic_Simple_Validator
     (Base64Binary_Facets.Length_Facets_Description);

   ----------
   -- Copy --
   ----------

   procedure Copy
     (From : Integer_Facets_Description;
      To   : in out Facets_Description_Record'Class) is
   begin
      Integer_Facets_Package.Copy
        (Integer_Facets_Package.Range_Facets_Description (From), To);
      Integer_Facets_Description (To).Total_Digits := From.Total_Digits;
   end Copy;

   -----------------
   -- Check_Facet --
   -----------------

   procedure Check_Facet
     (Facets : in out Integer_Facets_Description;
      Facet_Value  : Unicode.CES.Byte_Sequence)
   is
      use Integer_Facets_Package;
   begin
      Check_Facet (Range_Facets_Description (Facets), Facet_Value);

      if Facets.Mask (Facet_Total_Digits)
        and then Facet_Value'Length > Facets.Total_Digits
      then
         Validation_Error
           ("The maximum number of digits is"
            & Integer'Image (Facets.Total_Digits));
      end if;
   end Check_Facet;

   ---------------
   -- Add_Facet --
   ---------------

   procedure Add_Facet
     (Facets      : in out Integer_Facets_Description;
      Facet_Name  : Unicode.CES.Byte_Sequence;
      Facet_Value : Unicode.CES.Byte_Sequence;
      Applied     : out Boolean)
   is
      use Integer_Facets_Package;
      Val : Integer;
   begin
      Add_Facet
        (Integer_Facets_Package.Range_Facets_Description (Facets), Facet_Name,
         Facet_Value, Applied);
      if Applied then
         null;
      elsif Facet_Name = "totalDigits" then
         Facets.Total_Digits := Integer'Value (Facet_Value);
         Facets.Mask (Facet_Total_Digits) := True;
         Applied := True;
      elsif Facet_Name = "fractionDigits" then
         Val := Integer'Value (Facet_Value);
         if Val /= 0 then
            Validation_Error ("fractionDigits must be 0 for integers");
         end if;
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
     (Facets      : in out Decimal_Facets_Description;
      Facet_Name  : Unicode.CES.Byte_Sequence;
      Facet_Value : Unicode.CES.Byte_Sequence;
      Applied     : out Boolean)
   is
      use Decimal_Facets_Package;
   begin
      Add_Facet
        (Decimal_Facets_Package.Range_Facets_Description (Facets), Facet_Name,
         Facet_Value, Applied);
      if Applied then
         null;
      elsif Facet_Name = "totalDigits" then
         Facets.Total_Digits := Integer'Value (Facet_Value);
         Facets.Mask (Facet_Total_Digits) := True;
         if Facets.Mask (Facet_Fraction_Digits)
           and then Facets.Fraction_Digits > Facets.Total_Digits
         then
            Validation_Error
              ("fractionDigits cannot be greater than totalDigits");
         end if;

         Applied := True;
      elsif Facet_Name = "fractionDigits" then
         Facets.Fraction_Digits := Integer'Value (Facet_Value);
         if Facets.Mask (Facet_Total_Digits)
           and then Facets.Fraction_Digits > Facets.Total_Digits
         then
            Validation_Error
              ("fractionDigits cannot be greater than totalDigits");
         end if;
         Applied := True;
      else
         Applied := False;
      end if;
   exception
      when Constraint_Error =>
         if Debug then
            Debug_Output ("Constraint_Error when setting facet "
                          & Facet_Name & " " & Facet_Value);
         end if;
         Applied := False;
   end Add_Facet;

   ----------
   -- Copy --
   ----------

   procedure Copy
     (From : Decimal_Facets_Description;
      To   : in out Facets_Description_Record'Class) is
   begin
      Decimal_Facets_Package.Copy
        (Decimal_Facets_Package.Range_Facets_Description (From), To);
      Decimal_Facets_Description (To).Total_Digits := From.Total_Digits;
      Decimal_Facets_Description (To).Fraction_Digits := From.Fraction_Digits;
   end Copy;

   -----------------
   -- Check_Facet --
   -----------------

   procedure Check_Facet
     (Facets      : in out Decimal_Facets_Description;
      Facet_Value : Unicode.CES.Byte_Sequence)
   is
      use Decimal_Facets_Package;
   begin
      Check_Digits (Value (Facet_Value), Facets.Fraction_Digits,
                    Facets.Total_Digits);
      Check_Facet (Range_Facets_Description (Facets), Facet_Value);
   end Check_Facet;

   -------------------------
   -- Validate_Characters --
   -------------------------

   procedure Validate_Characters
     (Validator     : access Boolean_Validator_Record;
      Ch            : Unicode.CES.Byte_Sequence;
      Empty_Element : Boolean)
   is
      pragma Unreferenced (Empty_Element);
      First : Integer := Ch'First;
      Index : Integer;
      C : Unicode_Char;
   begin
      if Debug then
         Debug_Output ("Validate_Characters for boolean --" & Ch & "--"
                       & Get_Name (Validator));
      end if;

      if Ch = "" then
         Validation_Error
           ("Invalid value for boolean type: """ & Ch & """");
      end if;

      Check_Facet (Validator.Facets, Ch);

      --  Skip leading spaces
      while First <= Ch'Last loop
         Index := First;
         Encoding.Read (Ch, First, C);
         exit when not Is_White_Space (C);
      end loop;

      if C = Digit_Zero or C = Digit_One then
         if First <= Ch'Last then
            Encoding.Read (Ch, First, C);
         end if;

      elsif Index + True_Sequence'Length - 1 <= Ch'Last
        and then Ch (Index .. Index + True_Sequence'Length - 1) = True_Sequence
      then
         First := Index + True_Sequence'Length;

      elsif Index + False_Sequence'Length - 1 <= Ch'Last
        and then Ch (Index .. Index + False_Sequence'Length - 1) =
          False_Sequence
      then
         First := Index + False_Sequence'Length;
      else
         Validation_Error ("Invalid value for boolean type: """ & Ch & """");
      end if;

      --  Skip trailing spaces

      while First <= Ch'Last loop
         Encoding.Read (Ch, First, C);
         if not Is_White_Space (C) then
            Validation_Error
              ("Invalid value for boolean type: """ & Ch & """");
         end if;
      end loop;
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

   ----------
   -- Free --
   ----------

   procedure Free (Validator : in out Boolean_Validator_Record) is
   begin
      Free (Validator.Facets);
      Free (XML_Validator_Record (Validator));
   end Free;

   -----------------------
   -- String_Get_Length --
   -----------------------

   function String_Get_Length
     (Value : Unicode.CES.Byte_Sequence) return Natural is
   begin
      return Sax.Encodings.Encoding.Length (Value);
   end String_Get_Length;

   ----------------------------
   -- String_List_Get_Length --
   ----------------------------

   function String_List_Get_Length
     (Value : Unicode.CES.Byte_Sequence) return Natural
   is
      Length : Natural := 0;
      C      : Unicode_Char;
      Index  : Natural := Value'First;
   begin
      if Value = "" then
         return 0;
      end if;

      while Index <= Value'Last loop
         Encoding.Read (Value, Index, C);
         while C = Unicode.Names.Basic_Latin.Space loop
            Length := Length + 1;
            Encoding.Read (Value, Index, C);
         end loop;
      end loop;

      return Length + 1;
   end String_List_Get_Length;

   --------------------------
   -- HexBinary_Get_Length --
   --------------------------

   function HexBinary_Get_Length
     (Value : Unicode.CES.Byte_Sequence) return Natural is
   begin
      return Sax.Encodings.Encoding.Length (Value) / 2;
   end HexBinary_Get_Length;

   ---------------------------
   -- Is_Valid_Base64Binary --
   ---------------------------

   function Is_Valid_Base64Binary
     (Value : Unicode.CES.Byte_Sequence) return Boolean
   is
      Index         : Integer := Value'First;
      C             : Unicode_Char;
      Prev_Is_Space : Boolean := False;

      Group         : Natural := 1;
      --  Characters are always by groups of 4, this variable indicates the
      --  index of the current char in the group

      type Char_Categorie is (Char_04, Char_16, Char_64, Char_Equal);
      Chars  : array (1 .. 4) of Char_Categorie;
      --  The various categories that characters can belong two. In the Base64
      --  encoding, we always have groups of 4 characters.

   begin
      while Index <= Value'Last loop
         Sax.Encodings.Encoding.Read (Value, Index, C);

         if C = 16#20# or C = 16#A# then
            if Prev_Is_Space then
               return False;  --  Can never have two spaces in a row
            end if;
            Prev_Is_Space := True;

         elsif C in B04'Range and then B04 (C) then
            Prev_Is_Space := False;
            Chars (Group) := Char_04;
            Group := Group + 1;

         elsif C in B16'Range and then B16 (C) then
            Prev_Is_Space := False;
            Chars (Group) := Char_16;
            Group := Group + 1;

         elsif C in B64'Range and then B64 (C) then
            Prev_Is_Space := False;
            Chars (Group) := Char_64;
            Group := Group + 1;

         elsif C = Character'Pos ('=') then
            Prev_Is_Space := False;
            if Group = 3
              and then Chars (1) <= Char_64
              and then Chars (2) = Char_04
            then
               Chars (Group) := Char_Equal;
               Group := Group + 1;

            elsif Group = 4
              and then Chars (1) <= Char_64
              and then Chars (2) <= Char_64
              and then Chars (3) <= Char_16
            then
               Group := 1;
               exit;  --  Must end now

            elsif Group = 4
              and then Chars (1) <= Char_64
              and then Chars (2) <= Char_04
              and then Chars (3) <= Char_Equal
            then
               Group := 1;
               exit;  --  Must end now

            else
               return False;
            end if;

         else
            return False;
         end if;

         if Group > 4 then
            Group := 1;
         end if;
      end loop;

      --  Cannot finish with a space
      if Prev_Is_Space or Group /= 1 or Index <= Value'Last then
         return False;
      end if;

      return True;
   end Is_Valid_Base64Binary;

   -----------------------------
   -- Base64Binary_Get_Length --
   -----------------------------

   function Base64Binary_Get_Length
     (Value : Unicode.CES.Byte_Sequence) return Natural
   is
      Length : Natural := 0;
      C : Unicode_Char;
      Index : Positive := Value'First;
   begin
      while Index <= Value'Last loop
         Sax.Encodings.Encoding.Read (Value, Index, C);
         if C /= 16#20#
           and then C /= 16#A#
           and then C /= Character'Pos ('=')
         then
            Length := Length + 1;
         end if;
      end loop;
      return Length * 3 / 4;
   end Base64Binary_Get_Length;

   -----------------
   -- Check_Facet --
   -----------------

   procedure Check_Facet
     (Facets      : in out Float_Facets_Description;
      Facet_Value : Unicode.CES.Byte_Sequence)
   is
      use Float_Facets_Package;
   begin
      if Facet_Value = "NaN" then
         if Facets.Mask (Facet_Max_Inclusive)
           or Facets.Mask (Facet_Max_Exclusive)
         then
            Validation_Error
              ("NaN is greater than all numbers, and too big in this context");
         end if;

      elsif Facet_Value = "INF" then
         if Facets.Mask (Facet_Max_Inclusive)
           or Facets.Mask (Facet_Max_Exclusive)
         then
            Validation_Error
              ("INF is greater than maxInclusive and maxExclusive");
         end if;

      elsif Facet_Value = "-INF" then
         if Facets.Mask (Facet_Min_Inclusive)
           or Facets.Mask (Facet_Min_Exclusive)
         then
            Validation_Error
              ("-INF is smaller than minInclusive and minExclusive");
         end if;
      end if;

      Check_Facet
        (Float_Facets_Package.Range_Facets_Description (Facets), Facet_Value);
   end Check_Facet;

   -------------------------------
   -- Register_Predefined_Types --
   -------------------------------

   procedure Register_Predefined_Types (G, XML_G : XML_Grammar_NS) is
      use Integer_Validators;
      use String_Facets, String_List_Facets,
          HexBinary_Facets, Base64Binary_Facets;
      Tmp     : XML_Validator;
      Str     : String_Validators.Validator;
      StrList : String_List_Validators.Validator;
      Hex     : HexBinary_Validators.Validator;
      Base64  : Base64Binary_Validators.Validator;
      Int     : Integer_Validators.Validator;
      Dec     : Decimal_Validators.Validator;
      Created : XML_Type;
   begin
      Tmp := new Boolean_Validator_Record;
      Create_Global_Type (G, "boolean", Tmp);

      Str := new String_Validators.Validator_Record;
      Set_Whitespace (Str.Facets, Preserve);
      Create_Global_Type (G, "string", Str);

      Str := new String_Validators.Validator_Record;
      Set_Implicit_Enumeration (Str.Facets, Is_Valid_QName'Access);
      Create_Global_Type (G, "QName", Str);

      Str := new String_Validators.Validator_Record;
      Set_Whitespace (Str.Facets, Replace);  --  This should be hard-coded ???
      Create_Global_Type (G, "normalizedString", Str);

      Str := new String_Validators.Validator_Record;
      Set_Whitespace (Str.Facets, Collapse);  --  This should be hard-coded ???
      Create_Global_Type (G, "token", Str);

      Str := new String_Validators.Validator_Record;
      Set_Whitespace (Str.Facets, Preserve); --  Inherits from String
      Set_Implicit_Enumeration (Str.Facets, Is_Valid_Language_Name'Access);
      Created := Create_Global_Type (G, "language", Str);
      Create_Global_Attribute (XML_G, "lang", Created);

      Str := new String_Validators.Validator_Record;
      Set_Whitespace (Str.Facets, Collapse);
      Set_Implicit_Enumeration (Str.Facets, Is_Valid_Nmtoken'Access);
      Create_Global_Type (G, "NMTOKEN", Str);

      StrList := new String_List_Validators.Validator_Record;
      Set_Whitespace (StrList.Facets, Collapse);
      Set_Implicit_Enumeration (StrList.Facets, Is_Valid_Nmtokens'Access);
      Create_Global_Type (G, "NMTOKENS", StrList);

      Str := new String_Validators.Validator_Record;
      Set_Whitespace (Str.Facets, Preserve); --  Inherits from String
      Set_Implicit_Enumeration (Str.Facets, Is_Valid_Name'Access);
      Create_Global_Type (G, "Name", Str);

      Str := new String_Validators.Validator_Record;
      Set_Whitespace (Str.Facets, Preserve);  --  Inherits from String
      Set_Implicit_Enumeration (Str.Facets, Is_Valid_NCname'Access);
      Create_Global_Type (G, "NCName", Str);

      Str := new String_Validators.Validator_Record;
      Set_Whitespace (Str.Facets, Preserve);  --  Inherits from String
      Set_Implicit_Enumeration (Str.Facets, Is_Valid_NCname'Access);
      Create_Global_Type (G, "ID", Str);

      Str := new String_Validators.Validator_Record;
      Set_Whitespace (Str.Facets, Preserve);  --  Inherits from String
      Set_Implicit_Enumeration (Str.Facets, Is_Valid_NCname'Access);
      Create_Global_Type (G, "IDREF", Str);

      Str := new String_Validators.Validator_Record;
      Set_Whitespace (Str.Facets, Preserve);  --  Inherits from String
      Set_Implicit_Enumeration (Str.Facets, Is_Valid_NCnames'Access);
      Create_Global_Type (G, "IDREFS", Str);

      Str := new String_Validators.Validator_Record;
      Set_Whitespace (Str.Facets, Preserve); --  Inherits from String
      Set_Implicit_Enumeration (Str.Facets, Is_Valid_NCname'Access);
      Create_Global_Type (G, "ENTITY", Str);

      Str := new String_Validators.Validator_Record;
      Set_Whitespace (Str.Facets, Preserve); --  Inherits from String
      Set_Implicit_Enumeration (Str.Facets, Is_Valid_NCnames'Access);
      Create_Global_Type (G, "ENTITIES", Str);

      Str := new String_Validators.Validator_Record;
      Set_Implicit_Enumeration (Str.Facets, Is_Valid_URI'Access);
      Create_Global_Type (G, "anyURI", Str);

      Hex := new HexBinary_Validators.Validator_Record;
      Set_Implicit_Enumeration (Hex.Facets, Is_Valid_HexBinary'Access);
      Create_Global_Type (G, "hexBinary", Hex);

      Base64 := new Base64Binary_Validators.Validator_Record;
      Set_Implicit_Enumeration (Base64.Facets, Is_Valid_Base64Binary'Access);
      Create_Global_Type (G, "base64Binary", Base64);

      Dec := new Decimal_Validators.Validator_Record;
      Create_Global_Type (G, "decimal", Dec);

      Dec := new Decimal_Validators.Validator_Record;
      Dec.Facets.Mask := (Facet_Fraction_Digits => True,
                          Facet_Max_Inclusive   => True,
                          Facet_Min_Inclusive   => True,
                          others                => False);
      Dec.Facets.Fraction_Digits := 0;
      Dec.Facets.Max_Inclusive := Value ("+18446744073709551615");
      Dec.Facets.Min_Inclusive := Value ("0");
      Create_Global_Type (G, "unsignedLong", Dec);

      Dec := new Decimal_Validators.Validator_Record;
      Dec.Facets.Mask := (Facet_Fraction_Digits => True,
                          others                => False);
      Dec.Facets.Fraction_Digits := 0;
      Create_Global_Type (G, "integer", Dec);

      Dec := new Decimal_Validators.Validator_Record;
      Dec.Facets.Mask := (Facet_Min_Inclusive   => True,
                          others                => False);
      Dec.Facets.Min_Inclusive := Value ("0");
      Create_Global_Type (G, "nonNegativeInteger", Dec);

      Dec := new Decimal_Validators.Validator_Record;
      Dec.Facets.Mask := (Facet_Min_Inclusive   => True,
                          others                => False);
      Dec.Facets.Min_Inclusive := Value ("1");
      Create_Global_Type (G, "positiveInteger", Dec);

      Dec := new Decimal_Validators.Validator_Record;
      Dec.Facets.Mask := (Facet_Max_Inclusive   => True,
                          others                => False);
      Dec.Facets.Max_Inclusive := Value ("0");
      Create_Global_Type (G, "nonPositiveInteger", Dec);

      Dec := new Decimal_Validators.Validator_Record;
      Dec.Facets.Mask := (Facet_Max_Inclusive   => True,
                          others                => False);
      Dec.Facets.Max_Inclusive   := Value ("-1");
      Create_Global_Type (G, "negativeInteger", Dec);

      Int := new Integer_Validators.Validator_Record;
      Int.Facets.Mask := (Facet_Max_Inclusive   => True,
                          Facet_Min_Inclusive   => True,
                          others                => False);
      Int.Facets.Max_Inclusive   := +9_223_372_036_854_775_807;
      Int.Facets.Min_Inclusive   := -9_223_372_036_854_775_808;
      Create_Global_Type (G, "long", Int);

      Int := new Integer_Validators.Validator_Record;
      Int.Facets.Mask := (Facet_Max_Inclusive   => True,
                          Facet_Min_Inclusive   => True,
                          others                => False);
      Int.Facets.Max_Inclusive := +2_147_483_647;
      Int.Facets.Min_Inclusive := -2_147_483_648;
      Create_Global_Type (G, "int", Int);

      Int := new Integer_Validators.Validator_Record;
      Int.Facets.Mask := (Facet_Max_Inclusive   => True,
                          Facet_Min_Inclusive   => True,
                          others                => False);
      Int.Facets.Max_Inclusive := +32_767;
      Int.Facets.Min_Inclusive := -32_768;
      Create_Global_Type (G, "short", Int);

      Int := new Integer_Validators.Validator_Record;
      Int.Facets.Mask := (Facet_Max_Inclusive   => True,
                          Facet_Min_Inclusive   => True,
                          others                => False);
      Int.Facets.Max_Inclusive := +127;
      Int.Facets.Min_Inclusive := -128;
      Create_Global_Type (G, "byte", Int);

      Int := new Integer_Validators.Validator_Record;
      Int.Facets.Mask := (Facet_Min_Inclusive   => True,
                          Facet_Max_Inclusive   => True,
                          others                => False);
      Int.Facets.Max_Inclusive := +4_294_967_295;
      Int.Facets.Min_Inclusive := 0;
      Create_Global_Type (G, "unsignedInt", Int);

      Int := new Integer_Validators.Validator_Record;
      Int.Facets.Mask := (Facet_Min_Inclusive   => True,
                          Facet_Max_Inclusive   => True,
                          others                => False);
      Int.Facets.Max_Inclusive := +65_535;
      Int.Facets.Min_Inclusive := 0;
      Create_Global_Type (G, "unsignedShort", Int);

      Int := new Integer_Validators.Validator_Record;
      Int.Facets.Mask := (Facet_Min_Inclusive   => True,
                          Facet_Max_Inclusive   => True,
                          others                => False);
      Int.Facets.Max_Inclusive := +255;
      Int.Facets.Min_Inclusive := 0;
      Create_Global_Type (G, "unsignedByte", Int);

      Tmp := new Float_Validators.Validator_Record;
      Create_Global_Type (G, "float", Tmp);

      Tmp := new Float_Validators.Validator_Record;
      Create_Global_Type (G, "double", Tmp);

      Tmp := new Time_Validators.Validator_Record;
      Create_Global_Type (G, "time", Tmp);

      Tmp := new Date_Time_Validators.Validator_Record;
      Create_Global_Type (G, "dateTime", Tmp);

      Tmp := new GDay_Validators.Validator_Record;
      Create_Global_Type (G, "gDay", Tmp);

      Tmp := new GMonth_Day_Validators.Validator_Record;
      Create_Global_Type (G, "gMonthDay", Tmp);

      Tmp := new GMonth_Validators.Validator_Record;
      Create_Global_Type (G, "gMonth", Tmp);

      Tmp := new GYear_Month_Validators.Validator_Record;
      Create_Global_Type (G, "gYearMonth", Tmp);

      Tmp := new GYear_Validators.Validator_Record;
      Create_Global_Type (G, "gYear", Tmp);

      Tmp := new Date_Validators.Validator_Record;
      Create_Global_Type (G, "date", Tmp);

      Tmp := new Duration_Validators.Validator_Record;
      Create_Global_Type (G, "duration", Tmp);

      Tmp := Restriction_Of (G, Lookup (G, "anySimpleType"));
      Add_Facet (Tmp, "whiteSpace", "collapse");
      Create_Global_Type (G, "uriReference", Tmp);

   end Register_Predefined_Types;

   ----------------------------
   -- Validate_Start_Element --
   ----------------------------

   procedure Validate_Start_Element
     (Validator             : access Any_Simple_XML_Validator_Record;
      Local_Name             : Unicode.CES.Byte_Sequence;
      Namespace_URI          : Unicode.CES.Byte_Sequence;
      NS                     : XML_Grammar_NS;
      Data                   : Validator_Data;
      Grammar                : XML_Grammar;
      Element_Validator      : out XML_Element)
   is
      pragma Unreferenced (Validator, Data, Namespace_URI, NS, Grammar);
   begin
      Validation_Error
        ("Must be a simple type, no <" & Local_Name & "> child allowed");
      Element_Validator := No_Element;
   end Validate_Start_Element;

   --------------------------
   -- Validate_End_Element --
   --------------------------

   procedure Validate_End_Element
     (Validator      : access Any_Simple_XML_Validator_Record;
      Local_Name     : Unicode.CES.Byte_Sequence;
      Data           : Validator_Data)
   is
      pragma Unreferenced (Validator, Local_Name, Data);
   begin
      null;
   end Validate_End_Element;

   ---------------
   -- Add_Union --
   ---------------

   procedure Add_Union
     (Validator : access XML_Union_Record;
      Part      : XML_Type) is
   begin
      Append
        (Validator.Unions, XML_Particle'
           (Typ        => Particle_XML_Type,
            Type_Descr => Part,
            Next       => null,
            Min_Occurs => 1,
            Max_Occurs => 1));
   end Add_Union;

   -------------------------
   -- Validate_Characters --
   -------------------------

   procedure Validate_Characters
     (Union         : access XML_Union_Record;
      Ch            : Unicode.CES.Byte_Sequence;
      Empty_Element : Boolean)
   is
      Iter : Particle_Iterator;
      Valid : XML_Validator;
   begin
      if Debug then
         Debug_Output ("Validate_Characters for union --" & Ch & "--"
                       & Get_Name (Union));
      end if;

      if Union.Unions = null then
         if Empty_Element then
            return;
         else
            Validation_Error ("No content allowed for this union");
         end if;
      end if;

      Iter := Start (Union.Unions);
      while Get (Iter) /= null loop
         begin
            Valid := Get_Validator (Get (Iter).Type_Descr);
            if Valid /= null then
               Validate_Characters (Valid, Ch, Empty_Element);
            end if;

            --  No error ? => Everything is fine
            Free (Iter);
            return;

         exception
            when XML_Validation_Error =>
               null;
         end;

         Next (Iter);
      end loop;

      Free (Iter);
      Validation_Error ("Invalid value """ & Ch & """");
   end Validate_Characters;

   ----------------------------
   -- Get_Facets_Description --
   ----------------------------

   function Get_Facets_Description
     (Validator : access Any_Simple_XML_Validator_Record)
      return Facets_Description
   is
      pragma Unreferenced (Validator);
   begin
      return new Common_Facets_Description;
   end Get_Facets_Description;

   ------------------------
   -- Check_Content_Type --
   ------------------------

   procedure Check_Content_Type
     (Validator        : access Any_Simple_XML_Validator_Record;
      Should_Be_Simple : Boolean)
   is
      pragma Unreferenced (Validator);
   begin
      if not Should_Be_Simple then
         Validation_Error
           ("Expecting simple type, got complex type");
      end if;
   end Check_Content_Type;

   -------------------------
   -- Validate_Characters --
   -------------------------

   procedure Validate_Characters
     (Validator      : access Any_Simple_XML_Validator_Record;
      Ch             : Unicode.CES.Byte_Sequence;
      Empty_Element  : Boolean) is
   begin
      if Debug then
         Debug_Output ("Validate_Character for Any_Simple_XML_Validator "
                       & Get_Name (Validator) & ' '
                       & Ch & ' ' & Boolean'Image (Empty_Element));
      end if;
   end Validate_Characters;

   ----------
   -- Free --
   ----------

   procedure Free (Union : in out XML_Union_Record) is
   begin
      Free (Union.Unions);
      Free (Any_Simple_XML_Validator_Record (Union));
   end Free;

end Schema.Validators.Simple_Types;
