with Schema.Validators.Facets; use Schema.Validators.Facets;
with Sax.Encodings;            use Sax.Encodings;
with Sax.Utils;                use Sax.Utils;
with Schema.Date_Time;         use Schema.Date_Time;

package body Schema.Validators.Simple_Types is

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
      --  See doc for inherited subprograms
   end Generic_Range_Facets;

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
      end Add_Facet;

   end Generic_Range_Facets;

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
         Debug_Output ("Validate_Characters for --" & Ch & "--"
                       & Get_Name (Validator));
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
      end Free;

      ----------------------------
      -- Get_Facets_Description --
      ----------------------------

      function Get_Facets_Description
        (Validator : access Validator_Record) return Facets_Description
      is
         pragma Unreferenced (Validator);
      begin
         return new Facets_Type;
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

   package Float_Facets_Package is new Generic_Range_Facets
     ("float", Long_Long_Float, Long_Long_Float'Value, Long_Long_Float'Image);
   type Float_Facets_Description is
     new Float_Facets_Package.Range_Facets_Description with null record;
   procedure Check_Facet
     (Facets      : in out Float_Facets_Description;
      Facet_Value : Unicode.CES.Byte_Sequence);
   package Float_Validators is new Generic_Simple_Validator
     (Float_Facets_Description);

   package Integer_Facets_Package is new Generic_Range_Facets
     ("integer",
      Long_Long_Integer, Long_Long_Integer'Value, Long_Long_Integer'Image);
   type Integer_Facets_Description is new
     Integer_Facets_Package.Range_Facets_Description
   with record
      Total_Digits    : Positive := Positive'Last;
      Fraction_Digits : Natural  := Natural'Last;
   end record;
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

   type String_Facets_Description is new Common_Facets_Description with record
      Length      : Natural            := Natural'Last;
      Min_Length  : Natural            := 0;
      Max_Length  : Natural            := Natural'Last;
   end record;
   procedure Add_Facet
     (Facets      : in out String_Facets_Description;
      Facet_Name  : Unicode.CES.Byte_Sequence;
      Facet_Value : Unicode.CES.Byte_Sequence;
      Applied     : out Boolean);
   procedure Check_Facet
     (Facets : in out String_Facets_Description;
      Value  : Unicode.CES.Byte_Sequence);
   --  See doc for inherited subprograms

   type String_Validator_Record is new Any_Simple_XML_Validator_Record with
      record
         Facets : String_Facets_Description;
      end record;
   type String_Validator is access all String_Validator_Record'Class;
   procedure Validate_Characters
     (Validator     : access String_Validator_Record;
      Ch            : Unicode.CES.Byte_Sequence;
      Empty_Element : Boolean);
   procedure Add_Facet
     (Validator   : access String_Validator_Record;
      Facet_Name  : Unicode.CES.Byte_Sequence;
      Facet_Value : Unicode.CES.Byte_Sequence);
   procedure Free (Validator : in out String_Validator_Record);
   function Get_Facets_Description
     (Validator : access String_Validator_Record)
      return Facets_Description;
   --  See doc from inherited subprograms

   -----------------
   -- Check_Facet --
   -----------------

   procedure Check_Facet
     (Facets : in out Integer_Facets_Description;
      Facet_Value  : Unicode.CES.Byte_Sequence)
   is
      use Integer_Facets_Package;
      Val    : Long_Long_Integer;
      ValF   : Long_Long_Float;
      pragma Unreferenced (Val);
   begin
      Check_Facet (Range_Facets_Description (Facets), Facet_Value);

      if Facets.Mask (Facet_Total_Digits)
        and then Facet_Value'Length /= Facets.Total_Digits
      then
         Validation_Error
           ("The maximum number of digits is"
            & Integer'Image (Facets.Total_Digits));
      end if;

      if Facets.Mask (Facet_Fraction_Digits) then
         for V in Facet_Value'Range loop
            if Facet_Value (V) = '.' then
               if Facet_Value'Last - V > Facets.Fraction_Digits then
                  Validation_Error ("Too many digits in the fractional part");
               end if;
            end if;
         end loop;

         if Facets.Fraction_Digits = 0 then
            begin
               Val := Long_Long_Integer'Value (Facet_Value);
            exception
               when Constraint_Error =>
                  Validation_Error ("Value must be an integer");
            end;
         else
            begin
               ValF := Long_Long_Float'Value (Facet_Value);
               Val  := Long_Long_Integer (ValF);
            exception
               when Constraint_Error =>
                  Validation_Error ("Must have a decimal value");
            end;
         end if;

      else
         begin
            ValF := Long_Long_Float'Value (Facet_Value);
            Val  := Long_Long_Integer (ValF);
         exception
            when Constraint_Error =>
               Validation_Error ("Must have a decimal value");
         end;
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
         Facets.Fraction_Digits := Integer'Value (Facet_Value);
         Facets.Mask (Facet_Fraction_Digits) := True;
         Applied := True;
      else
         Applied := False;
      end if;
   exception
      when Constraint_Error =>
         Applied := False;
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
     (Validator : access String_Validator_Record)
      return Facets_Description
   is
      pragma Unreferenced (Validator);
   begin
      return new String_Facets_Description;
   end Get_Facets_Description;

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

   procedure Free (Validator : in out String_Validator_Record) is
   begin
      Free (Validator.Facets);
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
      Check_Facet (Common_Facets_Description (Facets), Value);

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
      Add_Facet (Common_Facets_Description (Facets), Facet_Name, Facet_Value,
                 Applied);
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
      Tmp     : XML_Validator;
      Str     : String_Validator;
      Int     : Integer_Validators.Validator;
      Created : XML_Type;
   begin
      Tmp := new Boolean_Validator_Record;
      Create_Global_Type (G, "boolean", Tmp);

      Str := new String_Validator_Record;
      Create_Global_Type (G, "string", Str);

      Str := new String_Validator_Record;
      Set_Implicit_Enumeration (Str.Facets, Is_Valid_QName'Access);
      Create_Global_Type (G, "QName", Str);

      Str := new String_Validator_Record;
      Set_Whitespace (Str.Facets, Replace);
      Create_Global_Type (G, "normalizeString", Str);

      Str := new String_Validator_Record;
      Set_Whitespace (Str.Facets, Collapse);
      Create_Global_Type (G, "token", Str);

      Str := new String_Validator_Record;
      Set_Whitespace (Str.Facets, Collapse);
      Set_Implicit_Enumeration (Str.Facets, Is_Valid_Language_Name'Access);
      Created := Create_Global_Type (G, "language", Str);
      Create_Global_Attribute (XML_G, "lang", Created);

      Str := new String_Validator_Record;
      Set_Whitespace (Str.Facets, Collapse);
      Set_Implicit_Enumeration (Str.Facets, Is_Valid_Nmtoken'Access);
      Create_Global_Type (G, "NMTOKEN", Str);

      Str := new String_Validator_Record;
      Set_Whitespace (Str.Facets, Collapse);
      Set_Implicit_Enumeration (Str.Facets, Is_Valid_Name'Access);
      Create_Global_Type (G, "Name", Str);

      Str := new String_Validator_Record;
      Set_Whitespace (Str.Facets, Collapse);
      Set_Implicit_Enumeration (Str.Facets, Is_Valid_NCname'Access);
      Create_Global_Type (G, "NCName", Str);

      Str := new String_Validator_Record;
      Set_Whitespace (Str.Facets, Collapse);
      Set_Implicit_Enumeration (Str.Facets, Is_Valid_NCname'Access);
      Create_Global_Type (G, "ID", Str);

      Str := new String_Validator_Record;
      Set_Whitespace (Str.Facets, Collapse);
      Set_Implicit_Enumeration (Str.Facets, Is_Valid_NCname'Access);
      Create_Global_Type (G, "IDREF", Str);

      Str := new String_Validator_Record;
      Set_Whitespace (Str.Facets, Collapse);
      Set_Implicit_Enumeration (Str.Facets, Is_Valid_NCname'Access);
      Create_Global_Type (G, "ENTITY", Str);

      Int := new Integer_Validators.Validator_Record;
      Create_Global_Type (G, "decimal", Int);

      Int := new Integer_Validators.Validator_Record;
      Int.Facets.Mask := (Facet_Fraction_Digits => True,
                          others                => False);
      Int.Facets.Fraction_Digits := 0;
      Create_Global_Type (G, "integer", Int);

      Int := new Integer_Validators.Validator_Record;
      Int.Facets.Mask := (Facet_Fraction_Digits => True,
                          Facet_Max_Inclusive   => True,
                          others                => False);
      Int.Facets.Fraction_Digits := 0;
      Int.Facets.Max_Inclusive := 0;
      Create_Global_Type (G, "nonPositiveInteger", Int);

      Int := new Integer_Validators.Validator_Record;
      Int.Facets.Mask := (Facet_Fraction_Digits => True,
                          Facet_Max_Inclusive   => True,
                          others                => False);
      Int.Facets.Fraction_Digits := 0;
      Int.Facets.Max_Inclusive   := -1;
      Create_Global_Type (G, "negativeInteger", Int);

      Int := new Integer_Validators.Validator_Record;
      Int.Facets.Mask := (Facet_Fraction_Digits => True,
                          Facet_Max_Inclusive   => True,
                          Facet_Min_Inclusive   => True,
                          others                => False);
      Int.Facets.Fraction_Digits := 0;
      Int.Facets.Max_Inclusive   := +9_223_372_036_854_775_807;
      Int.Facets.Min_Inclusive   := -9_223_372_036_854_775_808;
      Create_Global_Type (G, "long", Int);

      Int := new Integer_Validators.Validator_Record;
      Int.Facets.Mask := (Facet_Fraction_Digits => True,
                          Facet_Max_Inclusive   => True,
                          Facet_Min_Inclusive   => True,
                          others                => False);
      Int.Facets.Fraction_Digits := 0;
      Int.Facets.Max_Inclusive := +2_147_483_647;
      Int.Facets.Min_Inclusive := -2_147_483_648;
      Create_Global_Type (G, "int", Int);

      Int := new Integer_Validators.Validator_Record;
      Int.Facets.Mask := (Facet_Fraction_Digits => True,
                          Facet_Max_Inclusive   => True,
                          Facet_Min_Inclusive   => True,
                          others                => False);
      Int.Facets.Fraction_Digits := 0;
      Int.Facets.Max_Inclusive := +32_767;
      Int.Facets.Min_Inclusive := -32_768;
      Create_Global_Type (G, "short", Int);

      Int := new Integer_Validators.Validator_Record;
      Int.Facets.Mask := (Facet_Fraction_Digits => True,
                          Facet_Max_Inclusive   => True,
                          Facet_Min_Inclusive   => True,
                          others                => False);
      Int.Facets.Fraction_Digits := 0;
      Int.Facets.Max_Inclusive := +127;
      Int.Facets.Min_Inclusive := -128;
      Create_Global_Type (G, "byte", Int);

      Int := new Integer_Validators.Validator_Record;
      Int.Facets.Mask := (Facet_Fraction_Digits => True,
                          Facet_Min_Inclusive   => True,
                          others                => False);
      Int.Facets.Fraction_Digits := 0;
      Int.Facets.Min_Inclusive := 0;
      Create_Global_Type (G, "nonNegativeInteger", Int);

      Int := new Integer_Validators.Validator_Record;
      Int.Facets.Mask := (Facet_Fraction_Digits => True,
                          Facet_Min_Inclusive   => True,
                          others                => False);
      Int.Facets.Fraction_Digits := 0;
      Int.Facets.Min_Inclusive := 1;
      Create_Global_Type (G, "positiveInteger", Int);

--        Tmp := new Common_Simple_XML_Validator;
--        Tmp.Facets.Settable := Integer_Facets;
--        Tmp.Facets.Mask (Facet_Fraction_Digits) := True;
--        Tmp.Facets.Fraction_Digits := 0;
--        Tmp.Facets.Mask (Facet_Max_Inclusive) := True;
--        Tmp.Facets.Max_Inclusive := +18_446_744_073_709_551_615;
--        Tmp.Facets.Mask (Facet_Min_Inclusive) := True;
--        Tmp.Facets.Min_Inclusive := 0;
--        Create_Global_Type (G, Create_Type ("unsignedLong", Tmp));

      Int := new Integer_Validators.Validator_Record;
      Int.Facets.Mask := (Facet_Fraction_Digits => True,
                          Facet_Min_Inclusive   => True,
                          Facet_Max_Inclusive   => True,
                          others                => False);
      Int.Facets.Fraction_Digits := 0;
      Int.Facets.Max_Inclusive := +4_294_967_295;
      Int.Facets.Min_Inclusive := 0;
      Create_Global_Type (G, "unsignedInt", Int);

      Int := new Integer_Validators.Validator_Record;
      Int.Facets.Mask := (Facet_Fraction_Digits => True,
                          Facet_Min_Inclusive   => True,
                          Facet_Max_Inclusive   => True,
                          others                => False);
      Int.Facets.Fraction_Digits := 0;
      Int.Facets.Max_Inclusive := +65_535;
      Int.Facets.Min_Inclusive := 0;
      Create_Global_Type (G, "unsignedShort", Int);

      Int := new Integer_Validators.Validator_Record;
      Int.Facets.Mask := (Facet_Fraction_Digits => True,
                          Facet_Min_Inclusive   => True,
                          Facet_Max_Inclusive   => True,
                          others                => False);
      Int.Facets.Fraction_Digits := 0;
      Int.Facets.Max_Inclusive := +255;
      Int.Facets.Min_Inclusive := 0;
      Create_Global_Type (G, "unsignedByte", Int);

      Tmp := new Time_Validators.Validator_Record;
      Create_Global_Type (G, "time", Tmp);

      Tmp := new Float_Validators.Validator_Record;
      Create_Global_Type (G, "float", Tmp);

      Tmp := new Date_Time_Validators.Validator_Record;
      Create_Global_Type (G, "dateTime", Tmp);

      --  ??? Incorrect below

      Tmp := new Time_Validators.Validator_Record;
      Create_Global_Type (G, "date", Tmp);

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
      Schema_Target_NS       : XML_Grammar_NS;
      Element_Validator      : out XML_Element)
   is
      pragma Unreferenced
        (Validator, Data, Namespace_URI, NS, Schema_Target_NS);
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
      Debug_Output ("Validate_Characters for union --" & Ch & "--"
                    & Get_Name (Union));

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
            return;

         exception
            when XML_Validation_Error =>
               null;
         end;

         Next (Iter);
      end loop;

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

end Schema.Validators.Simple_Types;
