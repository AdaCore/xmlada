with Unicode.CES;     use Unicode.CES;
with Sax.Attributes;  use Sax.Attributes;
with Sax.Encodings;
with GNAT.Regpat;     use GNAT.Regpat;
with Ada.Unchecked_Deallocation;
with Ada.Exceptions;  use Ada.Exceptions;
with Sax.Utils;       use Sax.Utils;
with GNAT.IO;         use GNAT.IO;

package body Schema.Validators is

   type Whitespace_Restriction is (Preserve, Replace, Collapse);

   procedure Validation_Error (Message : String);
   --  Raise Validation_Error with a proper error message.

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Validator_Data_Record'Class, Validator_Data);

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

   ------------------------------
   -- Attribute_Validator_List --
   ------------------------------

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Attribute_Validator_List, Attribute_Validator_List_Access);

   procedure Free (List : in out Attribute_Validator_List_Access);
   --  Free the contents of List, including contained

   procedure Append
     (List      : in out Attribute_Validator_List_Access;
      Validator : access Attribute_Validator_Record'Class);
   --  Append a new value to List

   ----------
   -- Misc --
   ----------

   type Value_Validator is access function
     (Str : Byte_Sequence) return Boolean;
   --  Return True if Str is a valid value.
   --  Str is encoded with Sax.Encodings.Encoding

--     type Element_Declaration_Record;
--     type Element_Declaration is access Element_Declaration_Record;
--
--     type Element_Declaration_Record is record
--        Name : GNAT.OS_Lib.String_Access;
--        --  Local part of the name
--
--        Abstract_Element : Boolean := False;
--        --  If True, such elements can appear in the content models only when
--     --  substitution is allowed. This declaration cannot be used to validate
--        --  element content.
--
--        --  Block :
--
--        Default : GNAT.OS_Lib.String_Access;
--        --
   --     end record;

   -------------------------------
   -- Any_Simple_Type_Validator --
   -------------------------------

   type Any_Simple_Type_Validator_Record is new Type_Validator_Record
   with null record;
   --  Validates a "SimpleType" XML datatype, ie accepts any contents but
   --  elements and attributes

   procedure Validate_Start_Element
     (Validator         : access Any_Simple_Type_Validator_Record;
      Local_Name        : Unicode.CES.Byte_Sequence;
      Data              : Validator_Data;
      Element_Validator : out Type_Validator);
   procedure Validate_End_Element
     (Validator  : access Any_Simple_Type_Validator_Record;
      Local_Name : Unicode.CES.Byte_Sequence;
      Data       : Validator_Data);

   -----------------------------------
   --  Common_Simple_Type_Validator --
   -----------------------------------
   --  For all simple types with pattern, enumeration and whitespace faces

   type Pattern_Matcher_Access is access GNAT.Regpat.Pattern_Matcher;
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Pattern_Matcher, Pattern_Matcher_Access);

   type Common_Simple_Type_Validator is new Any_Simple_Type_Validator_Record
   with record
      Pattern              : Pattern_Matcher_Access    := null;
      Enumeration          : Byte_Sequence_List_Access := null;
      --  ??? Could use a htable here for faster access

      Implicit_Enumeration : Value_Validator  := null;
      Whitespace           : Whitespace_Restriction    := Preserve;
   end record;

   procedure Validate_Characters
     (Validator   : access Common_Simple_Type_Validator;
      Ch          : Unicode.CES.Byte_Sequence;
      Data        : Validator_Data);
   procedure Free
     (Validator : in out Common_Simple_Type_Validator;
      Deep      : Boolean := False);
   procedure Add_Restriction
     (Validator         : access Common_Simple_Type_Validator;
      Restriction_Name  : Unicode.CES.Byte_Sequence;
      Restriction_Value : Unicode.CES.Byte_Sequence);
   --  See doc from inherited subprogram

   ----------------------
   -- String_Validator --
   ----------------------

   type String_Validator_Record is new Common_Simple_Type_Validator with record
      Length               : Natural                   := Natural'Last;
      Min_Length           : Natural                   := 0;
      Max_Length           : Natural                   := Natural'Last;
   end record;

   procedure Validate_Characters
     (Validator   : access String_Validator_Record;
      Ch          : Unicode.CES.Byte_Sequence;
      Data        : Validator_Data);
   --   See doc from inherited subprograms

   -----------------------
   -- Integer_Validator --
   -----------------------

   type Integer_Validator_Record is new Common_Simple_Type_Validator with
      record
         Total_Digits    : Positive      := Positive'Last;
         Fraction_Digits : Natural       := Natural'Last;
         Max_Inclusive   : Long_Float  := Long_Float'Last;
         Max_Exclusive   : Long_Float  := Long_Float'Last;
         Min_Inclusive   : Long_Float  := Long_Float'First;
         Min_Exclusive   : Long_Float  := Long_Float'First;
      end record;

   procedure Validate_Characters
     (Validator   : access Integer_Validator_Record;
      Ch          : Unicode.CES.Byte_Sequence;
      Data        : Validator_Data);
   --   See doc from inherited subprograms

   ----------------------
   -- Validation_Error --
   ----------------------

   procedure Validation_Error (Message : String) is begin
      Raise_Exception (XML_Validation_Error'Identity, Message);
   end Validation_Error;

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

   ----------
   -- Free --
   ----------

   procedure Free (List : in out Attribute_Validator_List_Access) is
   begin
      if List /= null then
         for L in List'Range loop
            Free (List (L));
         end loop;
         Unchecked_Free (List);
      end if;
   end Free;

   ------------
   -- Append --
   ------------

   procedure Append
     (List      : in out Attribute_Validator_List_Access;
      Validator : access Attribute_Validator_Record'Class)
   is
      L : Attribute_Validator_List_Access := List;
   begin
      if List /= null then
         L := new Attribute_Validator_List'
           (List.all & Attribute_Validator (Validator));
         Unchecked_Free (List);
         List := L;
      else
         List := new Attribute_Validator_List'
           (1 => Attribute_Validator (Validator));
      end if;
   end Append;

   ----------
   -- Free --
   ----------

   procedure Free (Validator : in out Attribute_Validator_Record) is
   begin
      Free (Validator.Local_Name);
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Validator : in out Attribute_Validator) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Attribute_Validator_Record'Class, Attribute_Validator);
   begin
      Free (Validator.all);
      Unchecked_Free (Validator);
   end Free;

   -------------------
   -- Add_Attribute --
   -------------------

   procedure Add_Attribute
     (Validator : access Type_Validator_Record;
      Attribute : access Attribute_Validator_Record'Class) is
   begin
      Append (Validator.Attributes, Attribute);
   end Add_Attribute;

   ----------------------------
   -- Validate_Start_Element --
   ----------------------------

   procedure Validate_Start_Element
     (Validator         : access Any_Simple_Type_Validator_Record;
      Local_Name        : Unicode.CES.Byte_Sequence;
      Data              : Validator_Data;
      Element_Validator : out Type_Validator)
   is
      pragma Unreferenced (Validator, Local_Name, Data);
   begin
      Validation_Error
        ("Must be a simple type, no <element> child allowed");
      Element_Validator := null;
   end Validate_Start_Element;

   ----------------------------
   -- Validate_Start_Element --
   ----------------------------

   procedure Validate_Start_Element
     (Validator         : access Type_Validator_Record;
      Local_Name        : Unicode.CES.Byte_Sequence;
      Data              : Validator_Data;
      Element_Validator : out Type_Validator)
   is
      pragma Unreferenced (Validator, Data, Local_Name);
   begin
      Element_Validator := null;
   end Validate_Start_Element;

   -------------------------
   -- Validate_Attributes --
   -------------------------

   procedure Validate_Attributes
     (Validator      : access Type_Validator_Record;
      Atts           : Sax.Attributes.Attributes'Class;
      Data           : Validator_Data)
   is
      pragma Unreferenced (Data);
      Length : constant Natural := Get_Length (Atts);
   begin
      if Validator.Attributes = null then
         for A in 0 .. Length - 1 loop
            --  ??? Should test the namespace
            if Get_URI (Atts, A) = "" then
               Validation_Error
                 ("Must be a simple type, no attributes allowed");
            end if;
         end loop;

      else
         for A in 0 .. Length - 1 loop
            for VA in Validator.Attributes'Range loop
               --  ??? Should check namespace as well
               if Get_Local_Name (Atts, A) =
                 Validator.Attributes (VA).Local_Name.all
               then
                  Validate_Attribute
                    (Validator.Attributes (VA).all, Get_Value (Atts, A));
                  exit;
               end if;
            end loop;
         end loop;
      end if;
   end Validate_Attributes;

   --------------------------
   -- Validate_End_Element --
   --------------------------

   procedure Validate_End_Element
     (Validator      : access Any_Simple_Type_Validator_Record;
      Local_Name     : Unicode.CES.Byte_Sequence;
      Data           : Validator_Data)
   is
      pragma Unreferenced (Validator, Local_Name, Data);
   begin
      null;
   end Validate_End_Element;

   --------------------------
   -- Validate_End_Element --
   --------------------------

   procedure Validate_End_Element
     (Validator      : access Type_Validator_Record;
      Local_Name     : Unicode.CES.Byte_Sequence;
      Data           : Validator_Data)
   is
      pragma Unreferenced (Validator, Local_Name, Data);
   begin
      null;
   end Validate_End_Element;

   -------------------------
   -- Validate_Characters --
   -------------------------

   procedure Validate_Characters
     (Validator      : access Type_Validator_Record;
      Ch             : Unicode.CES.Byte_Sequence;
      Data           : Validator_Data)
   is
      pragma Unreferenced (Validator, Ch, Data);
   begin
      null;
   end Validate_Characters;

   -------------------------
   -- Validate_Characters --
   -------------------------

   procedure Validate_Characters
     (Validator   : access Common_Simple_Type_Validator;
      Ch          : Unicode.CES.Byte_Sequence;
      Data        : Validator_Data)
   is
      pragma Unreferenced (Data);
      Found  : Boolean;
   begin
      if Validator.Pattern /= null
        and then not Match (Validator.Pattern.all, String (Ch))
      then
         Validation_Error ("string pattern not matched");
      end if;

      if Validator.Enumeration /= null then
         Found := False;
         for E in Validator.Enumeration'Range loop
            if Ch = Validator.Enumeration (E).all then
               Found := True;
            end if;
         end loop;

         if not Found then
            Validation_Error ("Element's value not in the enumeration set");
         end if;
      end if;

      case Validator.Whitespace is
         when Preserve =>
            null; --  Always valid

         when Replace =>
            for C in Ch'Range loop
               if Ch (C) = ASCII.HT
                 or else Ch (C) = ASCII.LF
                 or else Ch (C) = ASCII.CR
               then
                  Validation_Error ("HT, LF and CR characters not allowed");
               end if;
            end loop;

         when Collapse =>
            for C in Ch'Range loop
               if Ch (C) = ASCII.HT
                 or else Ch (C) = ASCII.LF
                 or else Ch (C) = ASCII.CR
               then
                  Validation_Error ("HT, LF and CR characters not allowed");

               elsif Ch (C) = ' '
                 and then C < Ch'Last
                 and then Ch (C + 1) = ' '
               then
                  Validation_Error ("Duplicate space characters not allowed");
               end if;
            end loop;

            --  Leading or trailing white spaces are also forbidden
            if Ch'Length /= 0 then
               if Ch (Ch'First) = ' ' then
                  Validation_Error ("Leading whitespaces not allowed");
               elsif Ch (Ch'Last) = ' ' then
                  Validation_Error ("Trailing whitespaces not allowed");
               end if;
            end if;
      end case;

      if Validator.Implicit_Enumeration /= null
        and then not Validator.Implicit_Enumeration (Ch)
      then
         Validation_Error ("Element's value not in the enumeration set");
      end if;
   end Validate_Characters;

   -------------------------
   -- Validate_Characters --
   -------------------------

   procedure Validate_Characters
     (Validator   : access String_Validator_Record;
      Ch          : Unicode.CES.Byte_Sequence;
      Data        : Validator_Data)
   is
      Length : Natural;
   begin
      if Validator.Length /= Natural'Last
        or else Validator.Min_Length /= 0
        or else Validator.Max_Length /= Natural'Last
      then
         Length := Sax.Encodings.Encoding.Length (Ch);

         if Validator.Length /= Natural'Last
           and then Length /= Validator.Length
         then
            Validation_Error ("Invalid length");
         end if;

         if Length < Validator.Min_Length then
            Validation_Error ("Invalid min_length");
         end if;

         if Length > Validator.Max_Length then
            Validation_Error ("Invalid max_length");
         end if;
      end if;

      Validate_Characters
        (Common_Simple_Type_Validator (Validator.all)'Access, Ch, Data);
   end Validate_Characters;

   -------------------------
   -- Validate_Characters --
   -------------------------

   procedure Validate_Characters
     (Validator   : access Integer_Validator_Record;
      Ch          : Unicode.CES.Byte_Sequence;
      Data        : Validator_Data)
   is
      Value : Integer;
   begin
      begin
         Value := Integer'Value (Ch);
      exception
         when Constraint_Error =>
            Validation_Error ("Value must be an integer");
      end;

      if Validator.Total_Digits /= Positive'Last
        and then Ch'Length /= Validator.Total_Digits
      then
         Validation_Error
           ("The maximum number of digits is"
            & Integer'Image (Validator.Total_Digits));
      end if;

      if Validator.Fraction_Digits /= 0 then
         Validation_Error
           ("??? Don't know how to handle fractionDigits facet yet");
      end if;

      if Validator.Max_Inclusive < Long_Float (Value) then
         Validation_Error
           ("maxInclusive is set to"
            & Long_Float'Image (Validator.Max_Inclusive));
      end if;

      if Validator.Max_Exclusive <= Long_Float (Value) then
         Validation_Error
           ("maxExclusive is set to"
            & Long_Float'Image (Validator.Max_Exclusive));
      end if;

      if Validator.Min_Inclusive > Long_Float (Value) then
         Validation_Error
           ("minInclusive is set to"
            & Long_Float'Image (Validator.Min_Inclusive));
      end if;

      if Validator.Min_Exclusive >= Long_Float (Value) then
         Validation_Error
           ("minExclusive is set to"
            & Long_Float'Image (Validator.Min_Exclusive));
      end if;

      Validate_Characters
        (Common_Simple_Type_Validator (Validator.all)'Access, Ch, Data);
   end Validate_Characters;

   -------------
   -- List_Of --
   -------------

   function List_Of
     (Validator : access Type_Validator_Record) return Type_Validator
   is
   begin
      --  ??? Needs to be implemented
      return Type_Validator (Validator);
   end List_Of;

   ---------------
   -- Add_Union --
   ---------------

   procedure Add_Union
     (Validator : access Type_Validator_Record;
      Part      : access Type_Validator_Record'Class)
   is
      pragma Unreferenced (Validator, Part);
   begin
      --  ??? Needs to be implemented
      null;
   end Add_Union;

   ---------------------
   -- Add_Restriction --
   ---------------------

   procedure Add_Restriction
     (Validator         : access Type_Validator_Record;
      Restriction_Name  : Unicode.CES.Byte_Sequence;
      Restriction_Value : Unicode.CES.Byte_Sequence)
   is
      pragma Unreferenced (Validator, Restriction_Value);
   begin
      Raise_Exception
        (Invalid_Restriction'Identity,
         "Invalid restriction: " & Restriction_Name);
   end Add_Restriction;

   ---------------------
   -- Add_Restriction --
   ---------------------

   procedure Add_Restriction
     (Validator         : access Common_Simple_Type_Validator;
      Restriction_Name  : Unicode.CES.Byte_Sequence;
      Restriction_Value : Unicode.CES.Byte_Sequence) is
   begin
      if Restriction_Name = "enumeration" then
         Append (Validator.Enumeration, Restriction_Value);
      else
         Add_Restriction
           (Any_Simple_Type_Validator_Record (Validator.all)'Access,
            Restriction_Name, Restriction_Value);
      end if;
   end Add_Restriction;

   --------------------------------
   -- Create_Attribute_Validator --
   --------------------------------

   function Create_Attribute_Validator
     (Name           : Unicode.CES.Byte_Sequence;
      Attribute_Type : Type_Validator;
      Attribute_Form : Attribute_Form_Type       := Qualified;
      Attribute_Use  : Attribute_Use_Type        := Required;
      Value          : Unicode.CES.Byte_Sequence := "")
      return Attribute_Validator
   is
   begin
      return new Attribute_Validator_Record'
        (Local_Name     => new Unicode.CES.Byte_Sequence'(Name),
         Attribute_Type => Attribute_Type,
         Attribute_Form => Attribute_Form,
         Attribute_Use  => Attribute_Use,
         Value          => new Unicode.CES.Byte_Sequence'(Value));
   end Create_Attribute_Validator;

   ------------------------
   -- Validate_Attribute --
   ------------------------

   procedure Validate_Attribute
     (Validator : Attribute_Validator_Record;
      Value     : Unicode.CES.Byte_Sequence)
   is
      pragma Unreferenced (Validator, Value);
   begin
      --  ??? Should do some actual checks
      null;
   end Validate_Attribute;

   -----------
   -- Clone --
   -----------

   function Clone (Validator : Type_Validator_Record) return Type_Validator is
   begin
      --  ??? Incorrect, should also duplicate strings, ...
      return new Type_Validator_Record'Class'
        (Type_Validator_Record'Class (Validator));
   end Clone;

   ----------
   -- Free --
   ----------

   procedure Free
     (Validator : in out Common_Simple_Type_Validator;
      Deep      : Boolean := False) is
   begin
      if Deep then
         Unchecked_Free (Validator.Pattern);
         Free (Validator.Enumeration);
      end if;
      Free (Any_Simple_Type_Validator_Record (Validator), Deep);
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free
     (Validator : in out Type_Validator_Record;
      Deep      : Boolean := False)
   is
      pragma Unreferenced (Deep);
   begin
      Free (Validator.Attributes);
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free
     (Validator : in out Type_Validator;
      Deep      : Boolean := False)
   is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Type_Validator_Record'Class, Type_Validator);
   begin
      Free (Validator.all, Deep);
      Unchecked_Free (Validator);
   end Free;

   -----------------
   -- Lookup_Type --
   -----------------

   function Lookup_Type
     (Grammar : XML_Grammar;
      Qname   : Unicode.CES.Byte_Sequence) return Type_Validator is
   begin
      return Types_Htable.Get (Grammar.Types.all, Qname).Of_Type;
   end Lookup_Type;

   --------------------
   -- Lookup_Element --
   --------------------

   function Lookup_Element
     (Grammar : XML_Grammar;
      Qname   : Unicode.CES.Byte_Sequence) return Type_Validator is
   begin
      return Elements_Htable.Get (Grammar.Elements.all, Qname).Of_Type;
   end Lookup_Element;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Grammar : in out XML_Grammar) is
      Builtin_String            : String_Validator_Record :=
        (Any_Simple_Type_Validator_Record with
         Length      => Natural'Last,
         Min_Length  => 0,
         Max_Length  => Natural'Last,
         Pattern     => null,
         Enumeration => null,
         Implicit_Enumeration => null,
         Whitespace  => Collapse);
      Builtin_NString           : String_Validator_Record;
      Builtin_Token             : String_Validator_Record;
      Builtin_Language          : String_Validator_Record;
      Builtin_Nmtoken           : String_Validator_Record;
      Builtin_Name              : String_Validator_Record;
      Builtin_NCName            : String_Validator_Record;
      Builtin_ID                : String_Validator_Record;
      Builtin_IDREF             : String_Validator_Record;
      Builtin_Entity            : String_Validator_Record;

      Builtin_Integer           : Integer_Validator_Record;
      Builtin_NPI               : Integer_Validator_Record;

   begin
      Grammar.Types    := new Types_Htable.HTable (1023);
      Grammar.Elements := new Elements_Htable.HTable (1023);

      Register_Type
        (Grammar, Create_Type ("anyType", new Type_Validator_Record));

      Builtin_NString := Builtin_String;
      Builtin_NString.Whitespace := Replace;

      Builtin_Token := Builtin_NString;
      Builtin_Token.Whitespace := Collapse;

      Builtin_Language := Builtin_Token;
      Builtin_Language.Implicit_Enumeration := Is_Valid_Language_Name'Access;

      Builtin_Nmtoken  := Builtin_Token;
      Builtin_Nmtoken.Implicit_Enumeration := Is_Valid_Nmtoken'Access;

      Builtin_Name := Builtin_Token;
      Builtin_Name.Implicit_Enumeration := Is_Valid_Name'Access;

      Builtin_NCName := Builtin_Name;
      Builtin_NCName.Implicit_Enumeration := Is_Valid_NCname'Access;

      Builtin_ID := Builtin_NCName;

      Builtin_IDREF := Builtin_NCName;

      Builtin_Entity := Builtin_NCName;

      Register_Type
        (Grammar,
         Create_Type ("string", new String_Validator_Record'(Builtin_String)));
      Register_Type
        (Grammar,
         Create_Type ("normalizeString",
                      new String_Validator_Record'(Builtin_NString)));
      Register_Type
        (Grammar,
         Create_Type ("token", new String_Validator_Record'(Builtin_Token)));
      Register_Type
        (Grammar,
         Create_Type ("language",
                      new String_Validator_Record'(Builtin_Language)));
      Register_Type
        (Grammar,
         Create_Type ("NMTOKEN",
                      new String_Validator_Record'(Builtin_Nmtoken)));
      Register_Type
        (Grammar,
         Create_Type ("Name", new String_Validator_Record'(Builtin_Name)));
      Register_Type
        (Grammar,
         Create_Type ("NCName", new String_Validator_Record'(Builtin_NCName)));
      Register_Type
        (Grammar,
         Create_Type ("ID", new String_Validator_Record'(Builtin_ID)));
      Register_Type
        (Grammar,
         Create_Type ("IDREF", new String_Validator_Record'(Builtin_IDREF)));
      Register_Type
        (Grammar,
         Create_Type ("ENTITY", new String_Validator_Record'(Builtin_Entity)));

      Builtin_Integer.Fraction_Digits := 0;
      Register_Type
        (Grammar,
         Create_Type ("integer",
                      new Integer_Validator_Record'(Builtin_Integer)));

      Builtin_NPI := Builtin_Integer;
      Builtin_NPI.Max_Inclusive := 0.0;
      Register_Type
        (Grammar, Create_Type
           ("nonPositiveInteger", new Integer_Validator_Record'(Builtin_NPI)));

      Builtin_NPI := Builtin_Integer;
      Builtin_NPI.Max_Inclusive := -1.0;
      Register_Type
        (Grammar, Create_Type
           ("negativeInteger", new Integer_Validator_Record'(Builtin_NPI)));

      Builtin_NPI := Builtin_Integer;
      Builtin_NPI.Max_Inclusive := +9_223_372_036_854_775_807.0;
      Builtin_NPI.Min_Inclusive := -9_223_372_036_854_775_808.0;
      Register_Type
        (Grammar, Create_Type
           ("long", new Integer_Validator_Record'(Builtin_NPI)));

      Builtin_NPI := Builtin_Integer;
      Builtin_NPI.Max_Inclusive := +2_147_483_647.0;
      Builtin_NPI.Min_Inclusive := -2_147_483_648.0;
      Register_Type
        (Grammar, Create_Type
           ("int", new Integer_Validator_Record'(Builtin_NPI)));

      Builtin_NPI := Builtin_Integer;
      Builtin_NPI.Max_Inclusive := +32_767.0;
      Builtin_NPI.Min_Inclusive := -32_768.0;
      Register_Type
        (Grammar, Create_Type
           ("short", new Integer_Validator_Record'(Builtin_NPI)));

      Builtin_NPI := Builtin_Integer;
      Builtin_NPI.Max_Inclusive := +127.0;
      Builtin_NPI.Min_Inclusive := -128.0;
      Register_Type
        (Grammar, Create_Type
           ("byte", new Integer_Validator_Record'(Builtin_NPI)));

      Builtin_NPI := Builtin_Integer;
      Builtin_NPI.Min_Inclusive := 0.0;
      Register_Type
        (Grammar, Create_Type
           ("nonNegativeInteger", new Integer_Validator_Record'(Builtin_NPI)));

      Builtin_NPI := Builtin_Integer;
      Builtin_NPI.Min_Inclusive := 1.0;
      Register_Type
        (Grammar, Create_Type
           ("positiveInteger", new Integer_Validator_Record'(Builtin_NPI)));

      Builtin_NPI := Builtin_Integer;
      Builtin_NPI.Max_Inclusive := 18_446_744_073_709_551_615.0;
      Builtin_NPI.Min_Inclusive := 0.0;
      Register_Type
        (Grammar, Create_Type
           ("unsignedLong", new Integer_Validator_Record'(Builtin_NPI)));

      Builtin_NPI := Builtin_Integer;
      Builtin_NPI.Max_Inclusive := 4_294_967_295.0;
      Builtin_NPI.Min_Inclusive := 0.0;
      Register_Type
        (Grammar, Create_Type
           ("unsignedInt", new Integer_Validator_Record'(Builtin_NPI)));

      Builtin_NPI := Builtin_Integer;
      Builtin_NPI.Max_Inclusive := 65_535.0;
      Builtin_NPI.Min_Inclusive := 0.0;
      Register_Type
        (Grammar, Create_Type
           ("unsignedShort", new Integer_Validator_Record'(Builtin_NPI)));

      Builtin_NPI := Builtin_Integer;
      Builtin_NPI.Max_Inclusive := 255.0;
      Builtin_NPI.Min_Inclusive := 0.0;
      Register_Type
        (Grammar, Create_Type
           ("unsignedByte", new Integer_Validator_Record'(Builtin_NPI)));
   end Initialize;

   ----------------------
   -- Register_Element --
   ----------------------

   procedure Register_Element
     (Grammar   : XML_Grammar;
      Element   : XML_Element) is
   begin
      Elements_Htable.Set (Grammar.Elements.all, Element);
   end Register_Element;

   -------------------
   -- Register_Type --
   -------------------

   procedure Register_Type
     (Grammar   : XML_Grammar;
      Typ       : XML_Type) is
   begin
      Types_Htable.Set (Grammar.Types.all, Typ);
   end Register_Type;

   ----------
   -- Free --
   ----------

   procedure Free (Grammar : in out XML_Grammar) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Types_Htable.HTable, Types_Htable_Access);
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Elements_Htable.HTable, Elements_Htable_Access);
   begin
      if Grammar.Types /= null then
         Elements_Htable.Reset (Grammar.Elements.all);
         Unchecked_Free (Grammar.Elements);
         Types_Htable.Reset (Grammar.Types.all);
         Unchecked_Free (Grammar.Types);
      end if;
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Typ : in out XML_Type) is
   begin
      Free (Typ.Of_Type);
      Free (Typ.Qname);
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Element : in out XML_Element) is
   begin
      --  ??? Should free Of_Type only if it isn't a named type
      Free (Element.Qname);
   end Free;

   -------------
   -- Get_Key --
   -------------

   function Get_Key
     (Element : XML_Element) return Unicode.CES.Byte_Sequence is
   begin
      return Element.Qname.all;
   end Get_Key;

   -------------
   -- Get_Key --
   -------------

   function Get_Key
     (Typ : XML_Type) return Unicode.CES.Byte_Sequence is
   begin
      return Typ.Qname.all;
   end Get_Key;

   ----------
   -- Hash --
   ----------

   function Hash
     (Key : Unicode.CES.Byte_Sequence) return Interfaces.Unsigned_32
   is
      type Uns is mod 2 ** 32;
      function Rotate_Left (Value : Uns; Amount : Natural) return Uns;
      pragma Import (Intrinsic, Rotate_Left);

      Tmp : Uns := 0;
   begin
      for J in Key'Range loop
         Tmp := Rotate_Left (Tmp, 1) + Character'Pos (Key (J));
      end loop;

      return Interfaces.Unsigned_32 (Tmp);
   end Hash;

   --------------------
   -- Create_Element --
   --------------------

   function Create_Element
     (Qname   : Unicode.CES.Byte_Sequence;
      Of_Type : access Type_Validator_Record'Class) return XML_Element is
   begin
      return XML_Element'
        (Qname       => new Unicode.CES.Byte_Sequence'(Qname),
         Of_Type     => Type_Validator (Of_Type));
   end Create_Element;

   -----------------
   -- Create_Type --
   -----------------

   function Create_Type
     (Qname   : Unicode.CES.Byte_Sequence;
      Of_Type : Type_Validator) return XML_Type is
   begin
      return XML_Type'
        (Qname   => new Unicode.CES.Byte_Sequence'(Qname),
         Of_Type => Of_Type);
   end Create_Type;

   ---------------------------
   -- Create_Validator_Data --
   ---------------------------

   function Create_Validator_Data
     (Validator : access Sequence_Record) return Validator_Data
   is
      pragma Unreferenced (Validator);
   begin
      return new Sequence_Data'
        (Group_Model_Data_Record with
         Current           => null,
         Num_Occurs        => 0);
   end Create_Validator_Data;

   ----------------------------
   -- Validate_Start_Element --
   ----------------------------

   procedure Validate_Start_Element
     (Validator         : access Sequence_Record;
      Local_Name        : Unicode.CES.Byte_Sequence;
      Data              : Validator_Data;
      Element_Validator : out Type_Validator)
   is
      D   : constant Sequence_Data_Access := Sequence_Data_Access (Data);
   begin
      --  If we have a nested group_model somewhere, it is its responsability
      --  to check for the current item
      if D.Nested /= null then
         Put_Line ("MANU: Nested in process");
         Validate_Start_Element
           (D.Nested, Local_Name, D.Nested_Data, Element_Validator);
         return;
      end if;

      --  Initialize the sequence if needed

      if D.Current = null then
         D.Current    := Validator.Particles.First;
         D.Num_Occurs := D.Num_Occurs + 1;

         if D.Current = null then
            Validation_Error
              ("No child authorized for this sequence");
         end if;

         if D.Num_Occurs > Validator.Max_Occurs then
            Validation_Error
              ("Too many occurrences of sequence. Expecting at most"
               & Integer'Image (Validator.Max_Occurs));
         end if;
      end if;

      --  Check the current particle

      case D.Current.Particle.Typ is
         when Item_Element =>
            if D.Current.Particle.Element.Qname.all /= Local_Name then
               Validation_Error
                 ("Expecting " & D.Current.Particle.Element.Qname.all
                  & ", found " & Local_Name);
            end if;

            Element_Validator := D.Current.Particle.Element.Of_Type;
            Nested_Group_Terminated (Validator, Data);

         when Item_Nested =>
            D.Nested      := D.Current.Particle.Validator;
            D.Nested_Data := Create_Validator_Data (D.Nested);
            Group_Model_Data (D.Nested_Data).Parent := Group_Model (Validator);
            Group_Model_Data (D.Nested_Data).Parent_Data := Data;
            Put_Line ("MANU Recursing for validation");
            Validate_Start_Element
              (D.Nested, Local_Name, D.Nested_Data, Element_Validator);
      end case;
   end Validate_Start_Element;

   -----------------------------
   -- Nested_Group_Terminated --
   -----------------------------

   procedure Nested_Group_Terminated
     (Group : access Sequence_Record; Data  : Validator_Data)
   is
      D   : constant Sequence_Data_Access := Sequence_Data_Access (Data);
   begin
      Put_Line ("MANU: Terminated nested of sequence");
      Nested_Group_Terminated (Group_Model_Record (Group.all)'Access, Data);

      D.Current := D.Current.Next;

      if D.Parent /= null
        and then D.Current = null
        and then D.Num_Occurs >= Group.Min_Occurs
        and then D.Num_Occurs <= Group.Max_Occurs
      then
         Nested_Group_Terminated (D.Parent, D.Parent_Data);
      end if;
   end Nested_Group_Terminated;

   --------------------------
   -- Validate_End_Element --
   --------------------------

   procedure Validate_End_Element
     (Validator         : access Sequence_Record;
      Local_Name        : Unicode.CES.Byte_Sequence;
      Data              : Validator_Data)
   is
      pragma Unreferenced (Local_Name);
   begin
      if Sequence_Data (Data.all).Current /= null then
         Validation_Error
           ("Unexpected end of sequence, missing elements");
      end if;

      if Sequence_Data (Data.all).Num_Occurs < Validator.Min_Occurs then
         Validation_Error
           ("Not enough occurrences of sequence, expecting at least"
            & Integer'Image (Validator.Min_Occurs));
      end if;
   end Validate_End_Element;

   -------------------------
   -- Validate_Characters --
   -------------------------

   procedure Validate_Characters
     (Validator      : access Group_Model_Record;
      Ch             : Unicode.CES.Byte_Sequence;
      Data           : Validator_Data)
   is
      pragma Unreferenced (Validator, Ch, Data);
   begin
      Validation_Error
        ("No character data allowed by content model");
   end Validate_Characters;

   ------------
   -- Append --
   ------------

   procedure Append (List : in out Item_List; Item : XML_Item) is
   begin
      if List.First = null then
         List.First := new Item_List_Record'
           (Particle => Item, Next => null);
         List.Last  := List.First;
      else
         List.Last.Next := new Item_List_Record'
           (Particle => Item, Next => null);
         List.Last := List.Last.Next;
      end if;
   end Append;

   ---------------------
   -- Create_Sequence --
   ---------------------

   function Create_Sequence
     (Min_Occurs : Natural := 1;
      Max_Occurs : Natural := 1) return Sequence is
   begin
      return new Sequence_Record'
        (Group_Model_Record with
         Max_Occurs => Max_Occurs,
         Min_Occurs => Min_Occurs);
   end Create_Sequence;

   ------------------
   -- Add_Sequence --
   ------------------

   procedure Add_Sequence (Seq : access Sequence_Record; Item : XML_Element) is
   begin
      Append (Seq.Particles, XML_Item'(Item_Element, Item));
   end Add_Sequence;

   ------------------
   -- Add_Sequence --
   ------------------

   procedure Add_Sequence (Seq : access Sequence_Record; Item : Sequence) is
   begin
      Append (Seq.Particles, XML_Item'(Item_Nested, Group_Model (Item)));
   end Add_Sequence;

   ------------------
   -- Add_Sequence --
   ------------------

   procedure Add_Sequence (Seq : access Sequence_Record; Item : Choice) is
   begin
      Append (Seq.Particles, XML_Item'(Item_Nested, Group_Model (Item)));
   end Add_Sequence;

   ----------------------------
   -- Validate_Start_Element --
   ----------------------------

   procedure Validate_Start_Element
     (Validator         : access Choice_Record;
      Local_Name        : Unicode.CES.Byte_Sequence;
      Data              : Validator_Data;
      Element_Validator : out Type_Validator)
   is
      D     : constant Choice_Data_Access := Choice_Data_Access (Data);
      Item  : Item_List_Access := Validator.Particles.First;
   begin
      --  If we have a nested group_model somewhere, it is its responsability
      --  to check for the current item
      if D.Nested /= null then
         Put_Line ("MANU: Evaluate nested of choice");
         Validate_Start_Element
           (D.Nested, Local_Name, D.Nested_Data, Element_Validator);
         return;
      end if;

      --  Check whether the current item is valid

      Put_Line ("MANU: Choice, testing " & Local_Name);

      while Item /= null loop
         case Item.Particle.Typ is
            when Item_Element =>
               if Item.Particle.Element.Qname.all = Local_Name then
                  Element_Validator := Item.Particle.Element.Of_Type;
                  exit;
               end if;

            when Item_Nested =>
               if Applies_To_Tag (Item.Particle.Validator, Local_Name) then
                  exit;
               end if;
         end case;

         Item := Item.Next;
      end loop;

      if Item = null then
         Validation_Error ("Invalid choice: " & Local_Name);
      end if;

      D.Num_Occurs := D.Num_Occurs + 1;
      if D.Num_Occurs > Validator.Max_Occurs then
         Validation_Error ("Too many occurrences of choice, expecting at most"
                           & Integer'Image (Validator.Max_Occurs));
         Element_Validator := null;
      end if;

      if Item.Particle.Typ = Item_Nested then
         D.Nested      := Item.Particle.Validator;
         D.Nested_Data := Create_Validator_Data (D.Nested);
         Group_Model_Data (D.Nested_Data).Parent := Group_Model (Validator);
         Group_Model_Data (D.Nested_Data).Parent_Data := Data;
         Put_Line ("MANU Recursing for validation in choice");
         Validate_Start_Element
           (D.Nested, Local_Name, D.Nested_Data, Element_Validator);
      end if;

      if D.Parent /= null
        and then D.Num_Occurs >= Validator.Min_Occurs
        and then D.Num_Occurs <= Validator.Max_Occurs
      then
         Put_Line ("MANU: Done repeating choice, moving to parent");
         Nested_Group_Terminated (D.Parent, D.Parent_Data);
      end if;
   end Validate_Start_Element;

   -----------------------------
   -- Nested_Group_Terminated --
   -----------------------------

   procedure Nested_Group_Terminated
     (Group : access Choice_Record;
      Data  : Validator_Data) is
   begin
      Put_Line ("MANU: Terminated nested of choice");
      Nested_Group_Terminated (Group_Model_Record (Group.all)'Access, Data);
   end Nested_Group_Terminated;

   --------------------------
   -- Validate_End_Element --
   --------------------------

   procedure Validate_End_Element
     (Validator         : access Choice_Record;
      Local_Name        : Unicode.CES.Byte_Sequence;
      Data              : Validator_Data)
   is
      pragma Unreferenced (Local_Name);
   begin
      if Choice_Data (Data.all).Num_Occurs < Validator.Min_Occurs then
         Validation_Error
           ("Not enough occurrences of choice, expecting at least"
            & Integer'Image (Validator.Min_Occurs));
      end if;
   end Validate_End_Element;

   ---------------------------
   -- Create_Validator_Data --
   ---------------------------

   function Create_Validator_Data
     (Validator : access Choice_Record) return Validator_Data
   is
      pragma Unreferenced (Validator);
   begin
      return new Choice_Data'(Group_Model_Data_Record with Num_Occurs => 0);
   end Create_Validator_Data;

   ----------
   -- Free --
   ----------

   procedure Free (Data : in out Validator_Data_Record) is
      pragma Unreferenced (Data);
   begin
      null;
   end Free;

   ---------------------------
   -- Create_Validator_Data --
   ---------------------------

   function Create_Validator_Data
     (Validator : access Type_Validator_Record) return Validator_Data
   is
      pragma Unreferenced (Validator);
   begin
      return null;
   end Create_Validator_Data;

   -------------------
   -- Create_Choice --
   -------------------

   function Create_Choice
     (Min_Occurs : Natural := 1;
      Max_Occurs : Natural := 1) return Choice is
   begin
      return new Choice_Record'
        (Group_Model_Record with
         Max_Occurs => Max_Occurs,
         Min_Occurs => Min_Occurs);
   end Create_Choice;

   ----------------
   -- Add_Choice --
   ----------------

   procedure Add_Choice (C : access Choice_Record; Item : XML_Element) is
   begin
      Append (C.Particles, XML_Item'(Item_Element, Item));
   end Add_Choice;

   ----------------
   -- Add_Choice --
   ----------------

   procedure Add_Choice (C : access Choice_Record; Item : Sequence) is
   begin
      Append (C.Particles, XML_Item'(Item_Nested, Group_Model (Item)));
   end Add_Choice;

   ----------------
   -- Add_Choice --
   ----------------

   procedure Add_Choice (C : access Choice_Record; Item : Choice) is
   begin
      Append (C.Particles, XML_Item'(Item_Nested, Group_Model (Item)));
   end Add_Choice;

   -----------------------------
   -- Nested_Group_Terminated --
   -----------------------------

   procedure Nested_Group_Terminated
     (Group : access Group_Model_Record;
      Data  : Validator_Data)
   is
      pragma Unreferenced (Group);
      D : constant Group_Model_Data := Group_Model_Data (Data);
   begin
      D.Nested := null;

      if D.Nested_Data /= null then
         Free (D.Nested_Data.all);
         Unchecked_Free (D.Nested_Data);
      end if;
   end Nested_Group_Terminated;

   --------------------
   -- Applies_To_Tag --
   --------------------

   function Applies_To_Tag
     (Group      : access Group_Model_Record;
      Local_Name : Unicode.CES.Byte_Sequence) return Boolean
   is
      pragma Unreferenced (Group, Local_Name);
   begin
      return False;
   end Applies_To_Tag;

   --------------------
   -- Applies_To_Tag --
   --------------------

   function Applies_To_Tag
     (Group      : access Sequence_Record;
      Local_Name : Unicode.CES.Byte_Sequence) return Boolean is
   begin
      if Group.Particles.First = null then
         return False;
      else
         case Group.Particles.First.Particle.Typ is
            when Item_Element =>
               return Group.Particles.First.Particle.Element.Qname.all
                 = Local_Name;
            when Item_Nested =>
               return Applies_To_Tag
                 (Group.Particles.First.Particle.Validator, Local_Name);
         end case;
      end if;
   end Applies_To_Tag;

   --------------------
   -- Applies_To_Tag --
   --------------------

   function Applies_To_Tag
     (Group      : access Choice_Record;
      Local_Name : Unicode.CES.Byte_Sequence) return Boolean
   is
      Tmp : Item_List_Access := Group.Particles.First;
   begin
      while Tmp /= null loop
         case Tmp.Particle.Typ is
            when Item_Element =>
               if Tmp.Particle.Element.Qname.all = Local_Name then
                  return True;
               end if;
            when Item_Nested =>
               if Applies_To_Tag (Tmp.Particle.Validator, Local_Name) then
                  return True;
               end if;
         end case;

         Tmp := Tmp.Next;
      end loop;
      return False;
   end Applies_To_Tag;

end Schema.Validators;
