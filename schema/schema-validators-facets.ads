with GNAT.Regpat;  use GNAT.Regpat;
with Ada.Unchecked_Deallocation;

private package Schema.Validators.Facets is

   -------------------------
   --  Byte_Sequence_List --
   -------------------------

   type Byte_Sequence_List is array (Natural range <>)
      of Unicode.CES.Byte_Sequence_Access;
   type Byte_Sequence_List_Access is access Byte_Sequence_List;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Byte_Sequence_List, Byte_Sequence_List_Access);

   procedure Free (List : in out Byte_Sequence_List_Access);
   --  Free the contents of List, including contained sequences

   procedure Append
     (List  : in out Byte_Sequence_List_Access;
      Value : Unicode.CES.Byte_Sequence);
   --  Append a new value to List

   ------------
   -- Facets --
   ------------

   type Pattern_Matcher_Access is access GNAT.Regpat.Pattern_Matcher;
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Pattern_Matcher, Pattern_Matcher_Access);

   type Whitespace_Restriction is (Preserve, Replace, Collapse);

   type Value_Validator is access function
     (Str : Unicode.CES.Byte_Sequence) return Boolean;
   --  Return True if Str is a valid value.
   --  Str is encoded with Sax.Encodings.Encoding

   pragma Warnings (Off);
   type All_Facets is (Facet_Whitespace,
                       Facet_Pattern,
                       Facet_Enumeration,
                       Facet_Implicit_Enumeration,
                       Facet_Length,
                       Facet_Min_Length,
                       Facet_Max_Length,
                       Facet_Total_Digits,
                       Facet_Fraction_Digits,
                       Facet_Min_Inclusive,
                       Facet_Min_Exclusive,
                       Facet_Max_Exclusive,
                       Facet_Max_Inclusive);
   pragma Warnings (On);

   type Facets_Mask is array (All_Facets) of Boolean;
   pragma Pack (Facets_Mask);

   No_Facets : constant Facets_Mask := (others => False);

   String_Facets : constant Facets_Mask :=
     (Facet_Whitespace | Facet_Pattern | Facet_Enumeration
      | Facet_Length | Facet_Min_Length | Facet_Max_Length => True,
      others => False);
   Integer_Facets : constant Facets_Mask :=
     (Facet_Whitespace | Facet_Pattern | Facet_Total_Digits
      | Facet_Fraction_Digits | Facet_Min_Inclusive
      | Facet_Max_Inclusive | Facet_Min_Exclusive
      | Facet_Max_Exclusive => True,
      others => False);

   type Facets_Value is record
      Settable             : Facets_Mask := No_Facets;
      --  List of facets than can be set

      Mask                 : Facets_Mask := No_Facets;
      Whitespace           : Whitespace_Restriction    := Preserve;
      Pattern              : Pattern_Matcher_Access    := null;
      Pattern_String       : Unicode.CES.Byte_Sequence_Access      := null;
      Enumeration          : Byte_Sequence_List_Access := null;
      --  ??? Could use a htable here for faster access
      Implicit_Enumeration : Value_Validator           := null;
      Length               : Natural                   := Natural'Last;
      Min_Length           : Natural                   := 0;
      Max_Length           : Natural                   := Natural'Last;
      Total_Digits         : Positive                  := Positive'Last;
      Fraction_Digits      : Natural                   := Natural'Last;
      Max_Inclusive        : Long_Long_Integer;
      Max_Exclusive        : Long_Long_Integer;
      Min_Inclusive        : Long_Long_Integer;
      Min_Exclusive        : Long_Long_Integer;
   end record;



   procedure Free (Facets : in out Facets_Value);
   --  Free the contents of the facets

   procedure Check_Facet
     (Facets : in out Facets_Value; Value : Unicode.CES.Byte_Sequence);
   --  Check whether Value matches all the facets.
   --  raises XML_Validation_Error in case of error

   procedure Add_Facet
     (Facets      : in out Facets_Value;
      Facet_Name  : Unicode.CES.Byte_Sequence;
      Facet_Value : Unicode.CES.Byte_Sequence);
   --  Raises Invalid_Restriction in case of error

end Schema.Validators.Facets;
