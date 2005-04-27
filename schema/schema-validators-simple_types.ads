with Schema.Validators.Facets; use Schema.Validators.Facets;
with Ada.Calendar;

private package Schema.Validators.Simple_Types is

   -----------------------
   -- Integer_Validator --
   -----------------------

   type Integer_Facets_Names is (Facet_Total_Digits,
                                 Facet_Fraction_Digits,
                                 Facet_Min_Inclusive,
                                 Facet_Min_Exclusive,
                                 Facet_Max_Exclusive,
                                 Facet_Max_Inclusive);
   type Integer_Facets_Mask is array (Integer_Facets_Names) of Boolean;
   pragma Pack (Integer_Facets_Mask);

   type Integer_Facets_Description is new Facets_Description_Record with record
      Facets          : Common_Facets_Description;
      Mask            : Integer_Facets_Mask       := (others => False);
      Total_Digits    : Positive                  := Positive'Last;
      Fraction_Digits : Natural        := Natural'Last;
      Max_Inclusive   : Long_Long_Integer;
      Max_Exclusive   : Long_Long_Integer;
      Min_Inclusive   : Long_Long_Integer;
      Min_Exclusive   : Long_Long_Integer;
   end record;
   procedure Add_Facet
     (Facets      : in out Integer_Facets_Description;
      Facet_Name  : Unicode.CES.Byte_Sequence;
      Facet_Value : Unicode.CES.Byte_Sequence;
      Applied     : out Boolean);
   procedure Check_Facet
     (Facets : in out Integer_Facets_Description;
      Value  : Unicode.CES.Byte_Sequence);
   procedure Free (Facets : in out Integer_Facets_Description);
   --  See doc for inherited subprograms

   type Integer_Validator_Record is new Any_Simple_XML_Validator_Record with
      record
         Facets : Integer_Facets_Description;
      end record;
   type Integer_Validator is access all Integer_Validator_Record'Class;
   procedure Validate_Characters
     (Validator     : access Integer_Validator_Record;
      Ch            : Unicode.CES.Byte_Sequence;
      Empty_Element : Boolean);
   procedure Add_Facet
     (Validator   : access Integer_Validator_Record;
      Facet_Name  : Unicode.CES.Byte_Sequence;
      Facet_Value : Unicode.CES.Byte_Sequence);
   procedure Free (Validator : in out Integer_Validator_Record);
   function Get_Facets_Description
     (Validator : access Integer_Validator_Record)
      return Facets_Description;
   --  See doc from inherited subprograms

   ----------------------
   -- String_Validator --
   ----------------------

   type String_Facets_Names is (Facet_Length,
                                Facet_Min_Length,
                                Facet_Max_Length);
   type String_Facets_Mask is array (String_Facets_Names) of Boolean;
   pragma Pack (String_Facets_Mask);

   type String_Facets_Description is new Facets_Description_Record with record
      Facets     : Common_Facets_Description;
      Mask       : String_Facets_Mask := (others => False);
      Length     : Natural            := Natural'Last;
      Min_Length : Natural            := 0;
      Max_Length : Natural            := Natural'Last;
   end record;
   procedure Add_Facet
     (Facets      : in out String_Facets_Description;
      Facet_Name  : Unicode.CES.Byte_Sequence;
      Facet_Value : Unicode.CES.Byte_Sequence;
      Applied     : out Boolean);
   procedure Check_Facet
     (Facets : in out String_Facets_Description;
      Value  : Unicode.CES.Byte_Sequence);
   procedure Free (Facets : in out String_Facets_Description);
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

   -----------------------
   -- Boolean_Validator --
   -----------------------

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

   --------------------
   -- Time_Validator --
   --------------------

   type Time_Facets_Names is (Facet_Max_Inclusive,
                              Facet_Min_Inclusive,
                              Facet_Max_Exclusive,
                              Facet_Min_Exclusive);
   type Time_Facets_Mask is array (Time_Facets_Names) of Boolean;
   pragma Pack (Time_Facets_Mask);

   type Time_Facets_Description is new Facets_Description_Record with record
      Facets         : Common_Facets_Description;
      Mask           : Time_Facets_Mask       := (others => False);
      Max_Inclusive  : Ada.Calendar.Day_Duration;
      Min_Inclusive  : Ada.Calendar.Day_Duration;
      Max_Exclusive  : Ada.Calendar.Day_Duration;
      Min_Exclusive  : Ada.Calendar.Day_Duration;
   end record;
   procedure Add_Facet
     (Facets      : in out Time_Facets_Description;
      Facet_Name  : Unicode.CES.Byte_Sequence;
      Facet_Value : Unicode.CES.Byte_Sequence;
      Applied     : out Boolean);
   procedure Check_Facet
     (Facets : in out Time_Facets_Description;
      Value  : Unicode.CES.Byte_Sequence);
   procedure Free (Facets : in out Time_Facets_Description);
   --  See doc for inherited subprograms

   type Time_Validator_Record is new Any_Simple_XML_Validator_Record with
      record
         Facets : Time_Facets_Description;
      end record;
   procedure Validate_Characters
     (Validator     : access Time_Validator_Record;
      Ch            : Unicode.CES.Byte_Sequence;
      Empty_Element : Boolean);
   procedure Add_Facet
     (Validator   : access Time_Validator_Record;
      Facet_Name  : Unicode.CES.Byte_Sequence;
      Facet_Value : Unicode.CES.Byte_Sequence);
   procedure Free (Validator : in out Time_Validator_Record);
   function Get_Facets_Description
     (Validator : access Time_Validator_Record)
      return Facets_Description;

   ---------------------
   -- Float_Validator --
   ---------------------

   type Float_Facets_Names is (Facet_Min_Inclusive,
                               Facet_Min_Exclusive,
                               Facet_Max_Exclusive,
                               Facet_Max_Inclusive);
   type Float_Facets_Mask is array (Float_Facets_Names) of Boolean;
   pragma Pack (Float_Facets_Mask);

   type Float_Facets_Description is new Facets_Description_Record with record
      Common          : Common_Facets_Description;
      Mask            : Float_Facets_Mask       := (others => False);
      Max_Inclusive   : Long_Long_Float;
      Max_Exclusive   : Long_Long_Float;
      Min_Inclusive   : Long_Long_Float;
      Min_Exclusive   : Long_Long_Float;
   end record;
   procedure Add_Facet
     (Facets      : in out Float_Facets_Description;
      Facet_Name  : Unicode.CES.Byte_Sequence;
      Facet_Value : Unicode.CES.Byte_Sequence;
      Applied     : out Boolean);
   procedure Check_Facet
     (Facets : in out Float_Facets_Description;
      Value  : Unicode.CES.Byte_Sequence);
   procedure Free (Facets : in out Float_Facets_Description);
   --  See doc for inherited subprograms

   type Float_Validator_Record is new Any_Simple_XML_Validator_Record with
      record
         Facets : Float_Facets_Description;
      end record;
   type Float_Validator is access all Float_Validator_Record'Class;
   procedure Validate_Characters
     (Validator     : access Float_Validator_Record;
      Ch            : Unicode.CES.Byte_Sequence;
      Empty_Element : Boolean);
   procedure Add_Facet
     (Validator   : access Float_Validator_Record;
      Facet_Name  : Unicode.CES.Byte_Sequence;
      Facet_Value : Unicode.CES.Byte_Sequence);
   procedure Free (Validator : in out Float_Validator_Record);
   function Get_Facets_Description
     (Validator : access Float_Validator_Record)
      return Facets_Description;
   --  See doc from inherited subprograms

end Schema.Validators.Simple_Types;
