with Unicode.CES;     use Unicode.CES;
with Sax.Attributes;  use Sax.Attributes;
with Sax.Encodings;
with GNAT.Regpat;     use GNAT.Regpat;
with Ada.Unchecked_Deallocation;
with Ada.Exceptions;  use Ada.Exceptions;
with Sax.Utils;       use Sax.Utils;
with GNAT.IO; use GNAT.IO;
with Ada.Tags; use Ada.Tags;

package body Schema.Validators is

   type Whitespace_Restriction is (Preserve, Replace, Collapse);

   procedure Validation_Error (Message : String);
   --  Raise Validation_Error with a proper error message.

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Element_List, Element_List_Access);

   procedure Initialize_Sequence
     (Seq  : access Sequence_Record'Class;
      Data : Sequence_Data_Access);
   --  Reset the sequence to point to its first item

   function Get_Name
     (Validator : access XML_Validator_Record'Class) return String;
   --  Return a string "(rule "name")" if the name of the validator is defined

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

   ---------------------
   -- Debug_Validator --
   ---------------------
   --  This accepts anything, but prints error messages
   type Debug_Validator_Record is new XML_Validator_Record with null record;
   --  Validates a "SimpleType" XML datatype, ie accepts any contents but
   --  elements and attributes

   procedure Validate_Start_Element
     (Validator         : access Debug_Validator_Record;
      Local_Name        : Unicode.CES.Byte_Sequence;
      Data              : Validator_Data;
      Element_Validator : out XML_Type);
   procedure Validate_Characters
     (Validator   : access Debug_Validator_Record;
      Ch          : Unicode.CES.Byte_Sequence;
      Data        : Validator_Data);


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

   function Check_Substitution_Groups
     (Element    : XML_Element;
      Local_Name : Unicode.CES.Byte_Sequence) return XML_Element;
   --  Check whether any element in the substitution group of Validator can
   --  be used to match Local_Name.

   type Value_Validator is access function
     (Str : Byte_Sequence) return Boolean;
   --  Return True if Str is a valid value.
   --  Str is encoded with Sax.Encodings.Encoding

   -------------------------------
   -- Any_Simple_XML_Validator --
   -------------------------------

   type Any_Simple_XML_Validator_Record is new XML_Validator_Record
   with null record;
   --  Validates a "SimpleType" XML datatype, ie accepts any contents but
   --  elements and attributes

   procedure Validate_Start_Element
     (Validator         : access Any_Simple_XML_Validator_Record;
      Local_Name        : Unicode.CES.Byte_Sequence;
      Data              : Validator_Data;
      Element_Validator : out XML_Type);
   procedure Validate_End_Element
     (Validator  : access Any_Simple_XML_Validator_Record;
      Local_Name : Unicode.CES.Byte_Sequence;
      Data       : Validator_Data);

   -----------------------------------
   --  Common_Simple_XML_Validator --
   -----------------------------------
   --  For all simple types with pattern, enumeration and whitespace faces

   type Pattern_Matcher_Access is access GNAT.Regpat.Pattern_Matcher;
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Pattern_Matcher, Pattern_Matcher_Access);

   type Common_Simple_XML_Validator is new Any_Simple_XML_Validator_Record
   with record
      Pattern              : Pattern_Matcher_Access    := null;
      Enumeration          : Byte_Sequence_List_Access := null;
      --  ??? Could use a htable here for faster access

      Implicit_Enumeration : Value_Validator  := null;
      Whitespace           : Whitespace_Restriction    := Preserve;
   end record;

   procedure Validate_Characters
     (Validator   : access Common_Simple_XML_Validator;
      Ch          : Unicode.CES.Byte_Sequence;
      Data        : Validator_Data);
   procedure Free
     (Validator : in out Common_Simple_XML_Validator);
   procedure Add_Restriction
     (Validator         : access Common_Simple_XML_Validator;
      Restriction_Name  : Unicode.CES.Byte_Sequence;
      Restriction_Value : Unicode.CES.Byte_Sequence);
   --  See doc from inherited subprogram

   ----------------------
   -- String_Validator --
   ----------------------

   type String_Validator_Record is new Common_Simple_XML_Validator with record
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
   -- Boolean_Validator --
   -----------------------

   type Boolean_Validator_Record is new Common_Simple_XML_Validator with
     null record;
   procedure Validate_Characters
     (Validator   : access Boolean_Validator_Record;
      Ch          : Unicode.CES.Byte_Sequence;
      Data        : Validator_Data);
   procedure Add_Restriction
     (Validator         : access Boolean_Validator_Record;
      Restriction_Name  : Unicode.CES.Byte_Sequence;
      Restriction_Value : Unicode.CES.Byte_Sequence);
   --   See doc from inherited subprograms

   ------------------------------
   -- Extension_XML_Validator --
   ------------------------------

   type Extension_XML_Validator is new XML_Validator_Record with record
      Base      : XML_Type;
      Extension : XML_Validator;
   end record;
   type Extension_Type is access Extension_XML_Validator'Class;

   ------------------------------
   -- Extension_XML_Validator --
   ------------------------------

   type Restriction_XML_Validator is new XML_Validator_Record with record
      Base        : XML_Type;
      Restriction : XML_Validator;
   end record;
   type Restriction_Type is access Restriction_XML_Validator'Class;
   type Restriction_Data is new Validator_Data_Record with record
      Restriction_Data : Validator_Data;
   end record;
   type Restriction_Data_Access is access all Restriction_Data'Class;

   procedure Free (Data : in out Restriction_Data);
   function Create_Validator_Data
     (Validator : access Restriction_XML_Validator) return Validator_Data;
   procedure Validate_Start_Element
     (Validator         : access Restriction_XML_Validator;
      Local_Name        : Unicode.CES.Byte_Sequence;
      Data              : Validator_Data;
      Element_Validator : out XML_Type);
   procedure Validate_Characters
     (Validator   : access Restriction_XML_Validator;
      Ch          : Unicode.CES.Byte_Sequence;
      Data        : Validator_Data);
   procedure Validate_Attributes
     (Validator         : access Restriction_XML_Validator;
      Atts              : Sax.Attributes.Attributes'Class;
      Data              : Validator_Data);
   procedure Add_Attribute
     (Validator : access Restriction_XML_Validator;
      Attribute : access Attribute_Validator_Record'Class);
   --  See doc from inherited subprograms

   -----------------------
   -- Integer_Validator --
   -----------------------

   type Integer_Validator_Record is new Common_Simple_XML_Validator with
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
      L : Attribute_Validator_List_Access;
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

   ------------
   -- Append --
   ------------

   procedure Append
     (List    : in out Element_List_Access;
      Element : XML_Element)
   is
      L : Element_List_Access;
   begin
      if List /= null then
         L := new Element_List'(List.all & Element);
         Unchecked_Free (List);
         List := L;
      else
         List := new Element_List'(1 => Element);
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
     (Validator : access XML_Validator_Record;
      Attribute : access Attribute_Validator_Record'Class) is
   begin
      Append (Validator.Attributes, Attribute);
   end Add_Attribute;

   --------------
   -- Get_Name --
   --------------

   function Get_Name
     (Validator : access XML_Validator_Record'Class) return String is
   begin
      if Validator.Debug_Name = null then
         return "(rule tag=" & External_Tag (Validator'Tag) & ")";
      else
         return "(rule debug=""" & Validator.Debug_Name.all & """)";
      end if;
   end Get_Name;

   ----------------------------
   -- Validate_Start_Element --
   ----------------------------

   procedure Validate_Start_Element
     (Validator         : access Any_Simple_XML_Validator_Record;
      Local_Name        : Unicode.CES.Byte_Sequence;
      Data              : Validator_Data;
      Element_Validator : out XML_Type)
   is
      pragma Unreferenced (Validator, Local_Name, Data);
   begin
      Validation_Error
        ("Must be a simple type, no <element> child allowed");
      Element_Validator := No_Type;
   end Validate_Start_Element;

   ----------------------------
   -- Validate_Start_Element --
   ----------------------------

   procedure Validate_Start_Element
     (Validator         : access Debug_Validator_Record;
      Local_Name        : Unicode.CES.Byte_Sequence;
      Data              : Validator_Data;
      Element_Validator : out XML_Type)
   is
      pragma Unreferenced (Validator, Data);
   begin
      Put_Line ("****** Start_Element: DebugType validator for "
                & Local_Name);
      Element_Validator := No_Type;
   end Validate_Start_Element;

   -------------------------
   -- Validate_Characters --
   -------------------------

   procedure Validate_Characters
     (Validator   : access Debug_Validator_Record;
      Ch          : Unicode.CES.Byte_Sequence;
      Data        : Validator_Data)
   is
      pragma Unreferenced (Validator, Data);
   begin
      Put_Line ("****** Charactes: DebugType validator for " & Ch);
   end Validate_Characters;

   ----------------------------
   -- Validate_Start_Element --
   ----------------------------

   procedure Validate_Start_Element
     (Validator         : access XML_Validator_Record;
      Local_Name        : Unicode.CES.Byte_Sequence;
      Data              : Validator_Data;
      Element_Validator : out XML_Type)
   is
      pragma Unreferenced (Validator, Data);
   begin
      Put_Line ("!!!! Accepted by default: " & Local_Name);
      Element_Validator := null;
   end Validate_Start_Element;

   -------------------------
   -- Validate_Attributes --
   -------------------------

   procedure Validate_Attributes
     (Validator      : access XML_Validator_Record;
      Atts           : Sax.Attributes.Attributes'Class;
      Data           : Validator_Data)
   is
      pragma Unreferenced (Data);
      Length : constant Natural := Get_Length (Atts);
   begin
      Put_Line ("MANU Validate_Attributes " & Get_Name (Validator));
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
     (Validator      : access Any_Simple_XML_Validator_Record;
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
     (Validator      : access XML_Validator_Record;
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
     (Validator      : access XML_Validator_Record;
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
     (Validator   : access Common_Simple_XML_Validator;
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
        (Common_Simple_XML_Validator (Validator.all)'Access, Ch, Data);
   end Validate_Characters;

   -------------------------
   -- Validate_Characters --
   -------------------------

   procedure Validate_Characters
     (Validator   : access Boolean_Validator_Record;
      Ch          : Unicode.CES.Byte_Sequence;
      Data        : Validator_Data) is
   begin
      if Ch /= "true"
        and then Ch /= "false"
        and then Ch /= "0"
        and then Ch /= "1"
      then
         Validation_Error ("Invalid value for boolean type: " & Ch);
      end if;
      Validate_Characters
        (Common_Simple_XML_Validator (Validator.all)'Access, Ch, Data);
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
        (Common_Simple_XML_Validator (Validator.all)'Access, Ch, Data);
   end Validate_Characters;

   -------------
   -- List_Of --
   -------------

   function List_Of (Typ : XML_Type) return XML_Type is
   begin
      --  ??? Needs to be implemented
      return Typ;
   end List_Of;

   ---------------
   -- Add_Union --
   ---------------

   procedure Add_Union
     (Validator : access XML_Validator_Record;
      Part      : XML_Type)
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
     (Validator         : access XML_Validator_Record;
      Restriction_Name  : Unicode.CES.Byte_Sequence;
      Restriction_Value : Unicode.CES.Byte_Sequence)
   is
      pragma Unreferenced (Validator);
   begin
      if Restriction_Name = "whiteSpace" then
         if Restriction_Value /= "collapse" then
            Raise_Exception
              (Invalid_Restriction'Identity,
               "Invalid value for restriction whiteSpace: "
               & Restriction_Value);
         end if;
      else
         Raise_Exception
           (Invalid_Restriction'Identity,
            "Invalid restriction: " & Restriction_Name);
      end if;
   end Add_Restriction;

   ---------------------
   -- Add_Restriction --
   ---------------------

   procedure Add_Restriction
     (Validator         : access Common_Simple_XML_Validator;
      Restriction_Name  : Unicode.CES.Byte_Sequence;
      Restriction_Value : Unicode.CES.Byte_Sequence) is
   begin
      if Restriction_Name = "enumeration" then
         Append (Validator.Enumeration, Restriction_Value);

      elsif Restriction_Name = "whiteSpace" then
         if Restriction_Value = "preserve" then
            Validator.Whitespace := Preserve;
         elsif Restriction_Value = "replace" then
            Validator.Whitespace := Replace;
         elsif Restriction_Value = "collapse" then
            Validator.Whitespace := Collapse;
         else
            Raise_Exception
              (Invalid_Restriction'Identity,
               "Invalid value for restriction whiteSpace: "
               & Restriction_Value);
         end if;

      elsif Restriction_Name = "pattern" then
         Raise_Exception
           (Invalid_Restriction'Identity,
            "pattern restriction not fully handled yet");

      else
         Add_Restriction
           (Any_Simple_XML_Validator_Record (Validator.all)'Access,
            Restriction_Name, Restriction_Value);
      end if;
   end Add_Restriction;

   ---------------------
   -- Add_Restriction --
   ---------------------

   procedure Add_Restriction
     (Validator         : access Boolean_Validator_Record;
      Restriction_Name  : Unicode.CES.Byte_Sequence;
      Restriction_Value : Unicode.CES.Byte_Sequence)
   is
   begin
      if Restriction_Name /= "whiteSpace"
        and then Restriction_Name /= "pattern"
      then
         Raise_Exception
           (Invalid_Restriction'Identity,
            "Invalid restriction for boolean type: " & Restriction_Name);
      else
         Add_Restriction
           (Common_Simple_XML_Validator (Validator.all)'Access,
            Restriction_Name, Restriction_Value);
      end if;
   end Add_Restriction;

   ----------------------
   -- Create_Attribute --
   ----------------------

   function Create_Attribute
     (Name           : Unicode.CES.Byte_Sequence;
      Attribute_Type : XML_Type                  := No_Type;
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
   end Create_Attribute;

   ------------------------
   -- Validate_Attribute --
   ------------------------

   procedure Validate_Attribute
     (Validator : Attribute_Validator_Record;
      Value     : Unicode.CES.Byte_Sequence)
   is
      pragma Unreferenced (Validator);
   begin
      --  ??? Should do some actual checks
      Put_Line ("MANU: No check done for attribute " & Value);
   end Validate_Attribute;

   -----------
   -- Clone --
   -----------

   function Clone (Validator : XML_Validator) return XML_Validator is
      C : XML_Validator;
   begin
      if Validator = null then
         return null;
      else
         C := new XML_Validator_Record'Class'
           (XML_Validator_Record'Class (Validator.all));

         C.Debug_Name := null;
         Clone (Validator, C.all);
         return C;
      end if;
   end Clone;

   -----------
   -- Clone --
   -----------

   function Clone
     (List : Attribute_Validator_List_Access)
      return Attribute_Validator_List_Access is
   begin
      if List = null then
         return null;
      else
         return new Attribute_Validator_List'(List.all);
      end if;
   end Clone;

   -----------
   -- Clone --
   -----------

   procedure Clone
     (Validator : access XML_Validator_Record;
      Into      : in out XML_Validator_Record'Class) is
   begin
      Into.Attributes := Clone (Validator.Attributes);
   end Clone;

   -----------
   -- Clone --
   -----------

   procedure Clone
     (Validator : access Group_Model_Record;
      Into      : in out XML_Validator_Record'Class) is
   begin
      Clone (XML_Validator_Record (Validator.all)'Access, Into);
--      Into.Particles := Clone (Validator.Particles);
   end Clone;

   ----------
   -- Free --
   ----------

   procedure Free (Validator : in out Common_Simple_XML_Validator) is
   begin
      Unchecked_Free (Validator.Pattern);
      Free (Validator.Enumeration);
      Free (Any_Simple_XML_Validator_Record (Validator));
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Validator : in out XML_Validator_Record) is
   begin
      Free (Validator.Debug_Name);
      Free (Validator.Attributes);
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Typ : in out XML_Type) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (XML_Type_Record, XML_Type);
   begin
      if Typ /= null then
         Free (Typ.Qname);
         Unchecked_Free (Typ);
      end if;
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Validator : in out XML_Validator) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (XML_Validator_Record'Class, XML_Validator);
   begin
      Free (Validator.all);
      Unchecked_Free (Validator);
   end Free;

   ------------
   -- Lookup --
   ------------

   function Lookup
     (Grammar : XML_Grammar;
      Qname   : Unicode.CES.Byte_Sequence) return XML_Type
   is
      Result : XML_Type := Types_Htable.Get (Grammar.Types.all, Qname);
   begin
      if Result = No_Type then
         Result := Register_Forward (Grammar, Qname);
      end if;
      return Result;
   end Lookup;

   --------------------
   -- Lookup_Element --
   --------------------

   function Lookup_Element
     (Grammar : XML_Grammar;
      Qname   : Unicode.CES.Byte_Sequence) return XML_Element
   is
      Result : XML_Element :=
        Elements_Htable.Get (Grammar.Elements.all, Qname);
   begin
      if Result = No_Element then
         Result := Register_Forward (Grammar, Qname);
      end if;
      return Result;
   end Lookup_Element;

   ------------------
   -- Lookup_Group --
   ------------------

   function Lookup_Group
     (Grammar : XML_Grammar;
      Qname   : Unicode.CES.Byte_Sequence) return XML_Group
   is
      Result : XML_Group := Groups_Htable.Get (Grammar.Groups.all, Qname);
   begin
      if Result = No_Group then
         Result := Register_Forward (Grammar, Qname);
      end if;
      return Result;
   end Lookup_Group;

   --------------
   -- Get_Type --
   --------------

   function Get_Type (Element : XML_Element) return XML_Type is
   begin
      if Element = null then
         return null;
      else
         return Element.Of_Type;
      end if;
   end Get_Type;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Grammar : in out XML_Grammar) is
      Builtin_String            : String_Validator_Record :=
        (Any_Simple_XML_Validator_Record with
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
      QName                     : String_Validator_Record;

      Builtin_Integer           : Integer_Validator_Record;
      Builtin_NPI               : Integer_Validator_Record;
      Tmp : XML_Validator;

   begin
      Grammar.Types    := new Types_Htable.HTable (1023);
      Grammar.Elements := new Elements_Htable.HTable (1023);
      Grammar.Groups   := new Groups_Htable.HTable (1023);

      Tmp := new Debug_Validator_Record;
      Register (Grammar, Create_Type ("debug", Tmp));

      Tmp := new XML_Validator_Record;
      Register (Grammar, Create_Type ("anyType", Tmp));

      Tmp := new Any_Simple_XML_Validator_Record;
      Register (Grammar, Create_Type ("anySimpleType", Tmp));

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

      QName := Builtin_String;
      QName.Implicit_Enumeration := Is_Valid_QName'Access;
      Tmp := new String_Validator_Record'(QName);
      Register (Grammar, Create_Type ("QName", Tmp));

      Tmp := new Debug_Validator_Record;
      Register (Grammar, Create_Type ("@@invalid@@", Tmp));

      Tmp := new String_Validator_Record'(Builtin_String);
      Register (Grammar, Create_Type ("string", Tmp));

      Tmp := new String_Validator_Record'(Builtin_NString);
      Register (Grammar, Create_Type ("normalizeString", Tmp));

      Tmp := new String_Validator_Record'(Builtin_Token);
      Register (Grammar, Create_Type ("token", Tmp));

      Tmp := new String_Validator_Record'(Builtin_Language);
      Register (Grammar, Create_Type ("language", Tmp));

      Tmp := new String_Validator_Record'(Builtin_Nmtoken);
      Register (Grammar, Create_Type ("NMTOKEN", Tmp));

      Tmp := new String_Validator_Record'(Builtin_Name);
      Register (Grammar, Create_Type ("Name", Tmp));

      Tmp := new String_Validator_Record'(Builtin_NCName);
      Register (Grammar, Create_Type ("NCName", Tmp));

      Tmp := new String_Validator_Record'(Builtin_ID);
      Register (Grammar, Create_Type ("ID", Tmp));

      Tmp := new String_Validator_Record'(Builtin_IDREF);
      Register (Grammar, Create_Type ("IDREF", Tmp));

      Tmp := new String_Validator_Record'(Builtin_Entity);
      Register (Grammar, Create_Type ("ENTITY", Tmp));

      Builtin_Integer.Fraction_Digits := 0;
      Tmp := new Integer_Validator_Record'(Builtin_Integer);
      Register (Grammar, Create_Type ("integer", Tmp));

      Builtin_NPI := Builtin_Integer;
      Builtin_NPI.Max_Inclusive := 0.0;
      Tmp := new Integer_Validator_Record'(Builtin_NPI);
      Register (Grammar, Create_Type ("nonPositiveInteger", Tmp));

      Builtin_NPI := Builtin_Integer;
      Builtin_NPI.Max_Inclusive := -1.0;
      Tmp := new Integer_Validator_Record'(Builtin_NPI);
      Register (Grammar, Create_Type ("negativeInteger", Tmp));

      Builtin_NPI := Builtin_Integer;
      Builtin_NPI.Max_Inclusive := +9_223_372_036_854_775_807.0;
      Builtin_NPI.Min_Inclusive := -9_223_372_036_854_775_808.0;
      Tmp := new Integer_Validator_Record'(Builtin_NPI);
      Register (Grammar, Create_Type ("long", Tmp));

      Builtin_NPI := Builtin_Integer;
      Builtin_NPI.Max_Inclusive := +2_147_483_647.0;
      Builtin_NPI.Min_Inclusive := -2_147_483_648.0;
      Tmp := new Integer_Validator_Record'(Builtin_NPI);
      Register (Grammar, Create_Type ("int", Tmp));

      Builtin_NPI := Builtin_Integer;
      Builtin_NPI.Max_Inclusive := +32_767.0;
      Builtin_NPI.Min_Inclusive := -32_768.0;
      Tmp := new Integer_Validator_Record'(Builtin_NPI);
      Register (Grammar, Create_Type ("short", Tmp));

      Builtin_NPI := Builtin_Integer;
      Builtin_NPI.Max_Inclusive := +127.0;
      Builtin_NPI.Min_Inclusive := -128.0;
      Tmp := new Integer_Validator_Record'(Builtin_NPI);
      Register (Grammar, Create_Type ("byte", Tmp));

      Builtin_NPI := Builtin_Integer;
      Builtin_NPI.Min_Inclusive := 0.0;
      Tmp := new Integer_Validator_Record'(Builtin_NPI);
      Register (Grammar, Create_Type ("nonNegativeInteger", Tmp));

      Builtin_NPI := Builtin_Integer;
      Builtin_NPI.Min_Inclusive := 1.0;
      Tmp := new Integer_Validator_Record'(Builtin_NPI);
      Register (Grammar, Create_Type ("positiveInteger", Tmp));

      Builtin_NPI := Builtin_Integer;
      Builtin_NPI.Max_Inclusive := 18_446_744_073_709_551_615.0;
      Builtin_NPI.Min_Inclusive := 0.0;
      Tmp := new Integer_Validator_Record'(Builtin_NPI);
      Register (Grammar, Create_Type ("unsignedLong", Tmp));

      Builtin_NPI := Builtin_Integer;
      Builtin_NPI.Max_Inclusive := 4_294_967_295.0;
      Builtin_NPI.Min_Inclusive := 0.0;
      Tmp := new Integer_Validator_Record'(Builtin_NPI);
      Register (Grammar, Create_Type ("unsignedInt", Tmp));

      Builtin_NPI := Builtin_Integer;
      Builtin_NPI.Max_Inclusive := 65_535.0;
      Builtin_NPI.Min_Inclusive := 0.0;
      Tmp := new Integer_Validator_Record'(Builtin_NPI);
      Register (Grammar, Create_Type ("unsignedShort", Tmp));

      Builtin_NPI := Builtin_Integer;
      Builtin_NPI.Max_Inclusive := 255.0;
      Builtin_NPI.Min_Inclusive := 0.0;
      Tmp := new Integer_Validator_Record'(Builtin_NPI);
      Register (Grammar, Create_Type ("unsignedByte", Tmp));
   end Initialize;

   --------------
   -- Register --
   --------------

   procedure Register (Grammar : XML_Grammar; Element : XML_Element) is
      Old : constant XML_Element :=
        Elements_Htable.Get (Grammar.Elements.all, Element.Qname.all);
   begin
      if Old /= No_Element
        and then Old.Of_Type /= Lookup (Grammar, "@@invalid@@")
      then
         Raise_Exception
           (Program_Error'Identity,
            "Element has already been registered previously: "
            & Element.Qname.all);
      end if;

      Elements_Htable.Set (Grammar.Elements.all, Element);
   end Register;

   --------------
   -- Register --
   --------------

   procedure Register (Grammar : XML_Grammar; Typ : XML_Type) is
      Old : constant XML_Type := Types_Htable.Get
        (Grammar.Types.all, Typ.Qname.all);
   begin
      if Old /= No_Type
        and then Old /= Lookup (Grammar, "@@invalid@@")
      then
         Raise_Exception
           (Program_Error'Identity,
            "Type has already been registered previously: "
            & Typ.Qname.all);
      end if;

      Types_Htable.Set (Grammar.Types.all, Typ);
   end Register;

   --------------
   -- Register --
   --------------

   procedure Register (Grammar : XML_Grammar; Group : XML_Group) is
      Old : constant XML_Group :=
        Groups_Htable.Get (Grammar.Groups.all, Group.Qname.all);
   begin
      if Old /= No_Group then
         Raise_Exception
           (Program_Error'Identity,
            "Group has already been registered previously: "
            & Group.Qname.all);
      end if;
      Groups_Htable.Set (Grammar.Groups.all, Group);
   end Register;

   ----------------------
   -- Register_Forward --
   ----------------------

   function Register_Forward
     (Grammar : XML_Grammar;
      Qname   : Unicode.CES.Byte_Sequence) return XML_Type
   is
      Typ : constant XML_Type := Lookup (Grammar, "@@invalid@@");
   begin
      Put_Line ("MANU: Forward type: " & Qname);
      Types_Htable.Set (Grammar.Types.all, Typ);
      return Typ;
   end Register_Forward;

   ----------------------
   -- Register_Forward --
   ----------------------

   function Register_Forward
     (Grammar : XML_Grammar;
      Qname   : Unicode.CES.Byte_Sequence) return XML_Element
   is
      Typ : constant XML_Type := Lookup (Grammar, "@@invalid@@");
      Elem : constant XML_Element := Create_Element (Qname, Typ);
   begin
      Put_Line ("MANU: Forward element: " & Qname);
      Register (Grammar, Elem);
      return Elem;
   end Register_Forward;

   ----------------------
   -- Register_Forward --
   ----------------------

   function Register_Forward
     (Grammar : XML_Grammar;
      Qname   : Unicode.CES.Byte_Sequence) return XML_Group
   is
      Gr : constant XML_Group := Create_Group (Qname);
   begin
      Put_Line ("MANU: Forward group (not supported yet!!!): " & Qname);
      Register (Grammar, Gr);
      return Gr;
   end Register_Forward;

   ----------
   -- Free --
   ----------

   procedure Free (Grammar : in out XML_Grammar) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Types_Htable.HTable, Types_Htable_Access);
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Elements_Htable.HTable, Elements_Htable_Access);
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Groups_Htable.HTable, Groups_Htable_Access);
   begin
      if Grammar.Types /= null then
         Elements_Htable.Reset (Grammar.Elements.all);
         Unchecked_Free (Grammar.Elements);
         Types_Htable.Reset (Grammar.Types.all);
         Unchecked_Free (Grammar.Types);
         Groups_Htable.Reset (Grammar.Groups.all);
         Unchecked_Free (Grammar.Groups);
      end if;
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Element : in out XML_Element) is
   begin
      --  ??? Should free Of_Type only if it isn't a named type
      Put_Line ("MANU Freeing element " & Element.Qname.all);
      Free (Element.Qname);
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Group : in out XML_Group) is
   begin
      Put_Line ("MANU Freeing group " & Group.Qname.all);
      Free (Group.Qname);
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

   -------------
   -- Get_Key --
   -------------

   function Get_Key
     (Group : XML_Group) return Unicode.CES.Byte_Sequence is
   begin
      return Group.Qname.all;
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
      Of_Type : XML_Type) return XML_Element is
   begin
      return new XML_Element_Record'
        (Qname       => new Unicode.CES.Byte_Sequence'(Qname),
         Of_Type     => Of_Type);
   end Create_Element;

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
         Current               => No_Iter,
         Num_Occurs_Of_Current => 0);
   end Create_Validator_Data;

   -------------------------------
   -- Check_Substitution_Groups --
   -------------------------------

   function Check_Substitution_Groups
     (Element    : XML_Element;
      Local_Name : Unicode.CES.Byte_Sequence) return XML_Element
   is
      Groups : constant Element_List_Access :=
        Get_Validator (Element.Of_Type).Substitution_Groups;
      Result : XML_Element;
   begin
      if Groups /= null then
         for S in Groups'Range loop
            if Groups (S).Qname.all = Local_Name then
               return Groups (S);
            end if;

            Result := Check_Substitution_Groups (Groups (S), Local_Name);
            if Result /= No_Element then
               return Result;
            end if;

         end loop;
      end if;
      return No_Element;
   end Check_Substitution_Groups;

   -------------------------
   -- Initialize_Sequence --
   -------------------------

   procedure Initialize_Sequence
     (Seq  : access Sequence_Record'Class;
      Data : Sequence_Data_Access) is
   begin
      Data.Current    := Start (Seq.Particles);

      Data.Num_Occurs := Data.Num_Occurs + 1;
      Put_Line ("MANU: Reset sequence " & Get_Name (Seq)
                  & Data.Num_Occurs'Img);

      if Get (Data.Current) = null then
         Validation_Error ("No child authorized for this sequence");
      end if;

      if Seq.Max_Occurs /= Unbounded
        and then Data.Num_Occurs > Seq.Max_Occurs
      then
         Validation_Error
           ("Too many occurrences of sequence. Expecting at most"
            & Integer'Image (Seq.Max_Occurs) & Get_Name (Seq));
      end if;
   end Initialize_Sequence;

   ----------------------------
   -- Validate_Start_Element --
   ----------------------------

   procedure Validate_Start_Element
     (Validator         : access Sequence_Record;
      Local_Name        : Unicode.CES.Byte_Sequence;
      Data              : Validator_Data;
      Element_Validator : out XML_Type)
   is
      D         : constant Sequence_Data_Access := Sequence_Data_Access (Data);
      Start_Elem : XML_Particle_Access;
      First_Iter : Boolean := True;
      Valid      : Boolean;
      Elem       : XML_Element;
      Curr       : XML_Particle_Access;
   begin
      --  If we have a nested group_model somewhere, it is its responsability
      --  to check for the current item
      if D.Nested /= null then
         Put_Line ("Into nested of " & Get_Name (Validator)
                   & ": " & Get_Name (D.Nested));
         Validate_Start_Element
           (D.Nested, Local_Name, D.Nested_Data, Element_Validator);
         return;
      end if;

      --  Initialize the sequence if needed

      Start_Elem := Get (D.Current);
      Valid      := False;

      Put_Line ("MANU: Start seq " & Get_Name (Validator));
      while First_Iter or else Get (D.Current) /= Start_Elem loop
         First_Iter := False;

         if Get (D.Current) = null then
            Initialize_Sequence (Validator, D);
         end if;

         Curr := Get (D.Current);

         case Curr.Typ is
            when Particle_Element =>
               Put_Line ("++ Testing element "
                         & Curr.Element.Qname.all);
               if Curr.Element.Qname.all = Local_Name then
                  Element_Validator := Curr.Element.Of_Type;
                  Valid := True;
               else
                  Elem := Check_Substitution_Groups (Curr.Element, Local_Name);
                  Valid := Elem /= No_Element;
                  if Valid then
                     Element_Validator := Elem.Of_Type;
                  end if;
               end if;

               if Valid then
                  Nested_Group_Terminated (Validator, Data);
                  Put_Line ("++ element Matched");
                  exit;
               end if;

            when Particle_Nested =>
               Put_Line ("++ Testing nested " & Get_Name (Curr.Validator));
               if Applies_To_Tag (Curr.Validator, Local_Name) then
                  D.Nested      := Curr.Validator;
                  D.Nested_Data := Create_Validator_Data (D.Nested);
                  Group_Model_Data (D.Nested_Data).Parent :=
                    Group_Model (Validator);
                  Group_Model_Data (D.Nested_Data).Parent_Data := Data;
                  Validate_Start_Element
                    (D.Nested, Local_Name, D.Nested_Data, Element_Validator);
                  --  Do not move to next, this will be done when the nested
                  --  terminates, through a call to Nested_Group_Terminated
                  Put_Line ("++ nested Matched");
                  exit;
               end if;

            when Particle_Group =>
               --  Not possible, since the iterator doesn't return those
               raise Program_Error;
         end case;

         if D.Num_Occurs_Of_Current < Get_Min_Occurs (D.Current) then
            Validation_Error
              ("Expecting at least"
               & Integer'Image (Get_Min_Occurs (D.Current))
               & " occurrences of current particle");
            return;
         end if;

         --  The current element was in fact optional

         D.Num_Occurs_Of_Current := -1;
         Nested_Group_Terminated (Validator, Data);
      end loop;
   end Validate_Start_Element;

   -----------------------------
   -- Nested_Group_Terminated --
   -----------------------------

   procedure Nested_Group_Terminated
     (Group : access Sequence_Record; Data  : Validator_Data)
   is
      D   : constant Sequence_Data_Access := Sequence_Data_Access (Data);
      Curr : XML_Particle_Access;
   begin
      Nested_Group_Terminated (Group_Model_Record (Group.all)'Access, Data);

      D.Num_Occurs_Of_Current := D.Num_Occurs_Of_Current + 1;

      Curr := Get (D.Current);
      if D.Num_Occurs_Of_Current = 0
        or else
          (Get_Max_Occurs (D.Current) /= Unbounded
           and then D.Num_Occurs_Of_Current >= Get_Max_Occurs (D.Current))
      then
         Put_Line ("MANU: End_Of_Nested: "
                   & Get_Name (Group)
                   & D.Num_Occurs_Of_Current'Img
                   & ' ' & Get_Max_Occurs (D.Current)'Img);

         Next (D.Current);
         D.Num_Occurs_Of_Current := 0;

         if Get (D.Current) = null then
            if D.Parent /= null
              and then D.Num_Occurs >= Group.Min_Occurs
              and then (Group.Max_Occurs /= Unbounded
                        and then D.Num_Occurs <= Group.Max_Occurs)
            then
               Put_Line ("MANU: Terminated nested");
               Nested_Group_Terminated (D.Parent, D.Parent_Data);
            end if;

            Put_Line ("MANU: Current is now null");
         end if;
      else
         Put_Line ("MANU: End_Of_Nested: continuing current for "
                   & Get_Name (Group)
                   & D.Num_Occurs_Of_Current'Img
                   & ' ' & Get_Max_Occurs (D.Current)'Img);
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
      D   : constant Sequence_Data_Access := Sequence_Data_Access (Data);
   begin
      --  If the only remaining elements are optional, ignore them

      if Get (D.Current) /= null then
         if D.Num_Occurs_Of_Current >= Get_Min_Occurs (D.Current) then
            loop
               Next (D.Current);
               exit when Get (D.Current) = null
                 or else Get_Min_Occurs (D.Current) /= 0;
            end loop;
         end if;
      end if;

      if Get (D.Current) /= null then
         Validation_Error
           ("Unexpected end of sequence " & Get_Name (Validator));
      end if;

      if D.Num_Occurs < Validator.Min_Occurs then
         Validation_Error
           ("Not enough occurrences of sequence, expecting at least"
            & Integer'Image (Validator.Min_Occurs)
            & ", got" & Integer'Image (D.Num_Occurs) & Get_Name (Validator));
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

   procedure Append
     (List       : in out Particle_List;
      Item       : XML_Particle) is
   begin
      pragma Assert (List /= null, "List was never created");

      if List.First = null then
         List.First := new XML_Particle'(Item);
         List.First.Next := null;
         List.Last  := List.First;
      else
         List.Last.Next := new XML_Particle'(Item);
         List.Last := List.Last.Next;
      end if;
   end Append;

   ---------------------
   -- Create_Sequence --
   ---------------------

   function Create_Sequence
     (Min_Occurs : Natural := 1;
      Max_Occurs : Integer := 1) return Sequence
   is
      Seq : constant Sequence := new Sequence_Record;
   begin
      Seq.Max_Occurs := Max_Occurs;
      Seq.Min_Occurs := Min_Occurs;
      return Seq;
   end Create_Sequence;

   ------------------
   -- Add_Particle --
   ------------------

   procedure Add_Particle
     (Seq : access Sequence_Record; Item : XML_Element;
      Min_Occurs : Natural := 1; Max_Occurs : Integer := 1) is
   begin
      if Item.Qname = null then
         Raise_Exception
           (Program_Error'Identity,
            "Adding empty element to a sequence");
      end if;

      Append
        (Seq.Particles, XML_Particle'
           (Typ        => Particle_Element,
            Element    => Item,
            Next       => null,
            Min_Occurs => Min_Occurs,
            Max_Occurs => Max_Occurs));
   end Add_Particle;

   ------------------
   -- Add_Particle --
   ------------------

   procedure Add_Particle
     (Seq        : access Sequence_Record;
      Item       : Sequence;
      Min_Occurs : Natural := 1; Max_Occurs : Integer := 1) is
   begin
      Append (Seq.Particles, XML_Particle'
                (Typ        => Particle_Nested,
                 Validator  => Group_Model (Item),
                 Next       => null,
                 Min_Occurs => Min_Occurs,
                 Max_Occurs => Max_Occurs));
   end Add_Particle;

   ------------------
   -- Add_Particle --
   ------------------

   procedure Add_Particle
     (Seq        : access Sequence_Record;
      Item       : Choice;
      Min_Occurs : Natural := 1; Max_Occurs : Integer := 1) is
   begin
      Append (Seq.Particles, XML_Particle'
                (Typ        => Particle_Nested,
                 Validator  => Group_Model (Item),
                 Next       => null,
                 Min_Occurs => Min_Occurs,
                 Max_Occurs => Max_Occurs));
   end Add_Particle;

   ------------------
   -- Add_Particle --
   ------------------

   procedure Add_Particle
     (Seq : access Sequence_Record; Item : XML_Group;
      Min_Occurs : Natural := 1; Max_Occurs : Integer := 1) is
   begin
      Append (Seq.Particles, XML_Particle'
                (Typ        => Particle_Group,
                 Group      => Item,
                 Next       => null,
                 Min_Occurs => Min_Occurs,
                 Max_Occurs => Max_Occurs));
   end Add_Particle;

   ----------------------------
   -- Validate_Start_Element --
   ----------------------------

   procedure Validate_Start_Element
     (Validator         : access Choice_Record;
      Local_Name        : Unicode.CES.Byte_Sequence;
      Data              : Validator_Data;
      Element_Validator : out XML_Type)
   is
      D     : constant Choice_Data_Access := Choice_Data_Access (Data);
      Item  : Particle_Iterator := Start (Validator.Particles);
      It    : XML_Particle_Access;
   begin
      --  If we have a nested group_model somewhere, it is its responsability
      --  to check for the current item
      if D.Nested /= null then
         Validate_Start_Element
           (D.Nested, Local_Name, D.Nested_Data, Element_Validator);
         return;
      end if;

      Put_Line ("MANU Start choice " & Get_Name (Validator));

      --  Check whether the current item is valid

      while Get (Item) /= null loop
         It := Get (Item);
         case It.Typ is
            when Particle_Element =>
               Put_Line ("++ Choice Testing element " & It.Element.Qname.all);
               if It.Element.Qname.all = Local_Name then
                  Element_Validator := It.Element.Of_Type;
                  Put_Line ("++ element matched in choice");
                  exit;
               else
                  Element_Validator := Get_Type (Check_Substitution_Groups
                    (It.Element, Local_Name));
                  if Element_Validator /= No_Type then
                     Put_Line ("++ substitute element matched in choice");
                     exit;
                  end if;
               end if;

            when Particle_Nested =>
               Put_Line ("++ Choice Testing nested "
                         & Get_Name (It.Validator));
               if Applies_To_Tag (It.Validator, Local_Name) then
                  Put_Line ("++ nested matched in choice");
                  exit;
               end if;

            when Particle_Group =>
               --  Not possible, since the iterator hides these
               raise Program_Error;
         end case;

         Next (Item);
      end loop;

      if Get (Item) = null then
         Validation_Error ("Invalid choice: " & Local_Name);
      end if;

      D.Num_Occurs := D.Num_Occurs + 1;
      if Validator.Max_Occurs /= Unbounded
        and then D.Num_Occurs > Validator.Max_Occurs
      then
         Validation_Error ("Too many occurrences of choice, expecting at most"
                           & Integer'Image (Validator.Max_Occurs));
         Element_Validator := null;
      end if;

      if It.Typ = Particle_Nested then
         Put_Line ("Nested in " & Get_Name (It.Validator));
         D.Nested      := It.Validator;
         D.Nested_Data := Create_Validator_Data (D.Nested);
         Group_Model_Data (D.Nested_Data).Parent := Group_Model (Validator);
         Group_Model_Data (D.Nested_Data).Parent_Data := Data;
         Validate_Start_Element
           (D.Nested, Local_Name, D.Nested_Data, Element_Validator);
      end if;

      if D.Parent /= null
        and then D.Num_Occurs >= Validator.Min_Occurs
        and then (Validator.Max_Occurs /= Unbounded
                  and then D.Num_Occurs >= Validator.Max_Occurs)
      then
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
      return new Choice_Data;
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
     (Validator : access XML_Validator_Record) return Validator_Data
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
      Max_Occurs : Integer := 1) return Choice
   is
      C : constant Choice := new Choice_Record;
   begin
      C.Max_Occurs := Max_Occurs;
      C.Min_Occurs := Min_Occurs;
      return C;
   end Create_Choice;

   ------------------
   -- Add_Particle --
   ------------------

   procedure Add_Particle
     (C          : access Choice_Record;
      Item       : XML_Element;
      Min_Occurs : Natural := 1; Max_Occurs : Integer := 1) is
   begin
      if Item.Qname = null then
         Raise_Exception
           (Program_Error'Identity,
            "Adding unnamed element to choice");
      end if;
      Append (C.Particles, XML_Particle'
                (Typ        => Particle_Element,
                 Element    => Item,
                 Next       => null,
                 Min_Occurs => Min_Occurs,
                 Max_Occurs => Max_Occurs));
   end Add_Particle;

   ------------------
   -- Add_Particle --
   ------------------

   procedure Add_Particle
     (C : access Choice_Record; Item : Sequence;
      Min_Occurs : Natural := 1; Max_Occurs : Integer := 1) is
   begin
      Append (C.Particles, XML_Particle'
                (Typ        => Particle_Nested,
                 Validator  => Group_Model (Item),
                 Next       => null,
                 Min_Occurs => Min_Occurs,
                 Max_Occurs => Max_Occurs));
   end Add_Particle;

   ------------------
   -- Add_Particle --
   ------------------

   procedure Add_Particle
     (C : access Choice_Record; Item : Choice;
      Min_Occurs : Natural := 1; Max_Occurs : Integer := 1) is
   begin
      Append (C.Particles, XML_Particle'
                (Typ        => Particle_Nested,
                 Validator  => Group_Model (Item),
                 Next       => null,
                 Min_Occurs => Min_Occurs,
                 Max_Occurs => Max_Occurs));
   end Add_Particle;

   ------------------
   -- Add_Particle --
   ------------------

   procedure Add_Particle
     (C : access Choice_Record; Item : XML_Group;
      Min_Occurs : Natural := 1; Max_Occurs : Integer := 1) is
   begin
      Append (C.Particles, XML_Particle'
                (Typ        => Particle_Group,
                 Group      => Item,
                 Next       => null,
                 Min_Occurs => Min_Occurs,
                 Max_Occurs => Max_Occurs));
   end Add_Particle;

   ----------
   -- Free --
   ----------

   procedure Free (Data : in out Validator_Data) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Validator_Data_Record'Class, Validator_Data);
   begin
      if Data /= null then
         Free (Data.all);
         Unchecked_Free (Data);
      end if;
   end Free;

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
      Free (D.Nested_Data);
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
      Local_Name : Unicode.CES.Byte_Sequence) return Boolean
   is
      Tmp : XML_Element;
      Item : XML_Particle_Access;
   begin
      Put_Line ("MANU Applies_To_Tag Seq "
                & Local_Name & ' ' & Get_Name (Group));

      Item := Get (Start (Group.Particles));

      if Item = null then
         Put_Line ("MANU   => empty group");
         return False;
      else
         Put_Line ("MANU   => " & Item.Typ'Img);
         case Item.Typ is
            when Particle_Element =>
               if Item.Element.Qname.all = Local_Name then
                  return True;
               else
                  Tmp := Check_Substitution_Groups (Item.Element, Local_Name);
                  return Tmp /= No_Element;
               end if;

            when Particle_Nested =>
               return Applies_To_Tag (Item.Validator, Local_Name);

            when Particle_Group =>
               --  Not possible since hidden by the iterator
               raise Program_Error;
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
      T   : XML_Element;
      Item : Particle_Iterator := Start (Group.Particles);
      It   : XML_Particle_Access;
   begin
      Put_Line ("MANU Applies_To_Tag Choice "
                & Local_Name & ' ' & Get_Name (Group));
      while Get (Item) /= null loop
         It := Get (Item);
         case It.Typ is
            when Particle_Element =>
               if It.Element.Qname = null then
                  Put_Line ("MANU: Element in choice has no Qname");
               end if;

               if It.Element.Qname.all = Local_Name then
                  return True;
               else
                  T := Check_Substitution_Groups (It.Element, Local_Name);
                  if T /= No_Element then
                     return True;
                  end if;
               end if;

            when Particle_Nested =>
               if Applies_To_Tag (It.Validator, Local_Name) then
                  return True;
               end if;

            when Particle_Group =>
               --  Not possible since hidden by the iterator
               raise Program_Error;
         end case;

         Next (Item);
      end loop;
      return False;
   end Applies_To_Tag;

   ----------------------------
   -- Set_Substitution_Group --
   ----------------------------

   procedure Set_Substitution_Group
     (Element : XML_Element; Head : XML_Element) is
   begin
      Append (Get_Validator (Head.Of_Type).Substitution_Groups, Element);
   end Set_Substitution_Group;

   ------------------
   -- Extension_Of --
   ------------------

   function Extension_Of
     (Base      : XML_Type;
      Extension : access XML_Validator_Record'Class) return XML_Validator
   is
      Result : constant Extension_Type := new Extension_XML_Validator;
   begin
      Result.Base      := Base;
      Result.Extension := XML_Validator (Extension);
      return XML_Validator (Result);
   end Extension_Of;

   ------------------
   -- Extension_Of --
   ------------------

   function Extension_Of
     (Base       : XML_Type;
      Group      : XML_Group;
      Min_Occurs : Natural := 1;
      Max_Occurs : Integer := 1) return XML_Validator
   is
      Result : constant Extension_Type := new Extension_XML_Validator;
      C      : Choice;
   begin
      Result.Base      := Base;
      C := Create_Choice (Min_Occurs => Min_Occurs, Max_Occurs => Max_Occurs);
      Add_Particle (C, Group);
      Result.Extension := XML_Validator (C);
      return XML_Validator (Result);
   end Extension_Of;

   --------------------
   -- Restriction_Of --
   --------------------

   function Restriction_Of
     (Base        : XML_Type;
      Restriction : access XML_Validator_Record'Class) return XML_Validator
   is
      Result : constant Restriction_Type := new Restriction_XML_Validator;
   begin
      Result.Base        := Base;
      Result.Restriction := XML_Validator (Restriction);
      return XML_Validator (Result);
   end Restriction_Of;

   -------------------------
   -- Empty_Particle_List --
   -------------------------

   function Empty_Particle_List return Particle_List is
   begin
      return new Particle_List_Record;
   end Empty_Particle_List;

   ------------------
   -- Create_Group --
   ------------------

   function Create_Group
     (Qname : Unicode.CES.Byte_Sequence) return XML_Group is
   begin
      return new XML_Group_Record'
        (Qname     => new String'(Qname),
         Particles => Empty_Particle_List);
   end Create_Group;

   -----------------
   -- Create_Type --
   -----------------

   function Create_Type
     (Qname     : Unicode.CES.Byte_Sequence;
      Validator : access XML_Validator_Record'Class) return XML_Type is
   begin
      return new XML_Type_Record'
        (Qname     => new String'(Qname),
         Validator => XML_Validator (Validator));
   end Create_Type;

   -------------------
   -- Get_Validator --
   -------------------

   function Get_Validator (Typ : XML_Type) return XML_Validator is
   begin
      return Typ.Validator;
   end Get_Validator;

   ---------------------
   -- Add_Restriction --
   ---------------------

   procedure Add_Restriction
     (Typ               : XML_Type;
      Restriction_Name  : Unicode.CES.Byte_Sequence;
      Restriction_Value : Unicode.CES.Byte_Sequence) is
   begin
      Add_Restriction
        (Get_Validator (Typ), Restriction_Name, Restriction_Value);
   end Add_Restriction;

   ------------------
   -- Add_Particle --
   ------------------

   procedure Add_Particle
     (Group : in out XML_Group; Particle : access Group_Model_Record'Class) is
   begin
      Append
        (Group.Particles, XML_Particle'
           (Typ        => Particle_Nested,
            Validator  => Group_Model (Particle),
            Next       => null,
            Min_Occurs => 1,
            Max_Occurs => 1));
   end Add_Particle;

   ----------------------------
   -- Validate_Start_Element --
   ----------------------------

   procedure Validate_Start_Element
     (Validator         : access Restriction_XML_Validator;
      Local_Name        : Unicode.CES.Byte_Sequence;
      Data              : Validator_Data;
      Element_Validator : out XML_Type)
   is
      D : constant Restriction_Data_Access := Restriction_Data_Access (Data);
   begin
      Put_Line ("---- Start_Element: In restriction: " & Get_Name (Validator)
                & "--" & Get_Name (Validator.Restriction));
      Validate_Start_Element
        (Validator.Restriction, Local_Name,
         D.Restriction_Data, Element_Validator);
   end Validate_Start_Element;

   -------------------------
   -- Validate_Characters --
   -------------------------

   procedure Validate_Characters
     (Validator   : access Restriction_XML_Validator;
      Ch          : Unicode.CES.Byte_Sequence;
      Data        : Validator_Data)
   is
      D : constant Restriction_Data_Access := Restriction_Data_Access (Data);
   begin
      Put_Line ("---- Characters: In restriction: base="
                & Get_Name (Get_Validator (Validator.Base)) & " ch=" & Ch);
      Validate_Characters (Validator.Restriction, Ch, D.Restriction_Data);
   end Validate_Characters;

   -------------------------
   -- Validate_Attributes --
   -------------------------

   procedure Validate_Attributes
     (Validator         : access Restriction_XML_Validator;
      Atts              : Sax.Attributes.Attributes'Class;
      Data              : Validator_Data)
   is
      D : constant Restriction_Data_Access := Restriction_Data_Access (Data);
      Length : constant Natural := Get_Length (Atts);
   begin
      Put_Line ("---- Attributes: In restriction");

      --  Check restrictions introduced on the use of attributes (there are no
      --  new attributes, only prohibited or mandatory ones

      if Validator.Restriction.Attributes /= null then
         for A in 0 .. Length - 1 loop
            for VA in Validator.Restriction.Attributes'Range loop
               Validate_Attribute
                 (Validator.Restriction.Attributes (VA).all,
                  Get_Value (Atts, A));
               exit;
            end loop;
         end loop;
      end if;

      --  Now validate with base, for the remaining attributes
      --  ??? Problem: some of the checks are done twice as a result, not very
      --  nice.
      Validate_Attributes
        (Get_Validator (Validator.Base), Atts, D.Restriction_Data);
   end Validate_Attributes;

   ----------
   -- Free --
   ----------

   procedure Free (Data : in out Restriction_Data) is
   begin
      Free (Data.Restriction_Data);
   end Free;

   ---------------------------
   -- Create_Validator_Data --
   ---------------------------

   function Create_Validator_Data
     (Validator : access Restriction_XML_Validator) return Validator_Data
   is
      D : constant Restriction_Data_Access := new Restriction_Data;
   begin
      D.Restriction_Data := Create_Validator_Data (Validator.Restriction);
      return Validator_Data (D);
   end Create_Validator_Data;

   -------------------
   -- Add_Attribute --
   -------------------

   procedure Add_Attribute
     (Validator : access Restriction_XML_Validator;
      Attribute : access Attribute_Validator_Record'Class) is
   begin
      Add_Attribute (Validator.Restriction, Attribute);
   end Add_Attribute;

   --------------------
   -- Set_Debug_Name --
   --------------------

   procedure Set_Debug_Name
     (Typ : access XML_Validator_Record'Class; Name : String) is
   begin
      Free (Typ.Debug_Name);
      Typ.Debug_Name := new Unicode.CES.Byte_Sequence'(Name);
   end Set_Debug_Name;

   -----------
   -- Start --
   -----------

   function Start (List : Particle_List) return Particle_Iterator is
   begin
      if List.First /= null
        and then List.First.Typ = Particle_Group
      then
         return Particle_Iterator'
           (Current  => List.First,
            In_Group => List.First.Group.Particles.First);
      else
         return Particle_Iterator'
           (Current  => List.First,
            In_Group => null);
      end if;
   end Start;

   ----------
   -- Next --
   ----------

   procedure Next (Iter : in out Particle_Iterator) is
   begin
      if Iter.In_Group /= null then
         Iter.In_Group := Iter.In_Group.Next;
         if Iter.In_Group = null then
            Put_Line ("---> End of group");
         end if;
      end if;

      if Iter.In_Group = null then
         Iter.Current := Iter.Current.Next;

         if Iter.Current /= null
           and then Iter.Current.Typ = Particle_Group
         then
            Put_Line ("---> in group " & Iter.Current.Group.Qname.all);
            Iter.In_Group := Iter.Current.Group.Particles.First;
         end if;
      end if;
   end Next;

   ---------
   -- Get --
   ---------

   function Get (Iter : Particle_Iterator) return XML_Particle_Access is
   begin
      if Iter.In_Group /= null then
         return Iter.In_Group;
      else
         return Iter.Current;
      end if;
   end Get;

   --------------------
   -- Get_Min_Occurs --
   --------------------

   function Get_Min_Occurs (Iter : Particle_Iterator) return Natural is
   begin
      return Iter.Current.Min_Occurs;
   end Get_Min_Occurs;

   --------------------
   -- Get_Max_Occurs --
   --------------------

   function Get_Max_Occurs (Iter : Particle_Iterator) return Integer is
   begin
      return Iter.Current.Max_Occurs;
   end Get_Max_Occurs;

end Schema.Validators;
