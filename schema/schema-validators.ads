with Unicode.CES;
with Sax.Attributes;
with Sax.HTable;
with Interfaces;

package Schema.Validators is

   Invalid_Restriction : exception;
   --  Raised when an invalid restriction is set on a type

   XML_Validation_Error : exception;
   --  Raised in case of error in the validation process. The exception message
   --  contains the error, but not its location

   type Type_Validator_Record is tagged private;
   type Type_Validator is access all Type_Validator_Record'Class;
   --  A new validator is typically created every time a new element starts,
   --  and is in charge of checking the contents and attributes of that
   --  element.
   --  The default implementation always validates.

   --------------------
   -- Validator_Data --
   --------------------
   --  Most validators, when they are performing validation, need to keep some
   --  data while their node is active. Such data might be used to store
   --  counters, current items,...

   type Validator_Data_Record is abstract tagged null record;
   type Validator_Data is access all Validator_Data_Record'Class;

   procedure Free (Data : in out Validator_Data_Record);
   --  Free the memory associated with the data.
   --  By default, this does nothing

   -------------------------
   -- Attribute_Validator --
   -------------------------

   type Attribute_Validator_Record is tagged private;
   type Attribute_Validator is access all Attribute_Validator_Record'Class;

   type Attribute_Use_Type is
     (Prohibited, Optional, Required, Default, Fixed);
   type Attribute_Form_Type is (Qualified, Unqualified);

   function Create_Attribute_Validator
     (Name           : Unicode.CES.Byte_Sequence;
      Attribute_Type : Type_Validator;
      Attribute_Form : Attribute_Form_Type       := Qualified;
      Attribute_Use  : Attribute_Use_Type        := Required;
      Value          : Unicode.CES.Byte_Sequence := "")
      return Attribute_Validator;
   --  Create a new attribute validator

   procedure Validate_Attribute
     (Validator : Attribute_Validator_Record;
      Value     : Unicode.CES.Byte_Sequence);
   --  Return True if Value is valid for this attribute.
   --  Raise XML_Validation_Error in case of error

   procedure Free (Validator : in out Attribute_Validator_Record);
   procedure Free (Validator : in out Attribute_Validator);
   --  Free the memory occupied by the validator

   ---------------------
   -- Type validators --
   ---------------------
   --  Such validators are build to validate specific parts of an XML
   --  document (a whole element).

   function Clone (Validator : Type_Validator_Record) return Type_Validator;
   --  Return a clone of Validator.
   --  The default implementation should be suitable in most cases. It
   --  duplicates Validator, but not a deep clone (ie internal access types
   --  will still point to the same actual memory chunk as Validator. This is
   --  compatible with a call to Free with Deep => False.

   procedure Free
     (Validator : in out Type_Validator_Record; Deep : Boolean := False);
   procedure Free
     (Validator : in out Type_Validator; Deep : Boolean := False);
   --  Free the memory occupied by Validator

   function Create_Validator_Data
     (Validator : access Type_Validator_Record) return Validator_Data;
   --  Create an instance of the data that the validator needs to check its
   --  associated node. This data will be automatically freed when the node
   --  ends. This function is called once when the element starts.
   --  By default, this returns null, and no data is associated.

   procedure Validate_Start_Element
     (Validator         : access Type_Validator_Record;
      Local_Name        : Unicode.CES.Byte_Sequence;
      Data              : Validator_Data;
      Element_Validator : out Type_Validator);
   --  Check whether this Start_Element event is valid in the context of the
   --  validator. Data is the result of Create_Validator_Data.
   --
   --  Element_Validator is set, on exit, to the validator that should be used
   --  to validate the next element. It can safely be left to null if the
   --  element is declared at a global level in the Schema
   --  Raise XML_Validation_Error in case of error

   procedure Validate_Attributes
     (Validator         : access Type_Validator_Record;
      Atts              : Sax.Attributes.Attributes'Class;
      Data              : Validator_Data);
   --  Check whether this list of attributes is valid for elements associated
   --  with this validator. By default, this simply check whether the list of
   --  attributes registered through Add_Attribute matches Atts.

   procedure Validate_End_Element
     (Validator      : access Type_Validator_Record;
      Local_Name     : Unicode.CES.Byte_Sequence;
      Data           : Validator_Data);
   --  Check whether this End_Element event is valid.
   --  This is called for the node associated with the validator itself, not
   --  for the child, as opposed to what is done for Validate_Start_Element.
   --  Raise XML_Validation_Error in case of error

   procedure Validate_Characters
     (Validator      : access Type_Validator_Record;
      Ch             : Unicode.CES.Byte_Sequence;
      Data           : Validator_Data);
   --  Check whether this Characters event is valid in the context of
   --  Validator. Multiple calls to the SAX event Characters are grouped before
   --  calling this subprogram.

--     function Validate_Ignorable_Whitespace
--       (Validator      : access Type_Validator_Record;
--        Ch             : Unicode.CES.Byte_Sequence)
--        return Validator_Status;
   --  Check whether this Ignorable_Whitespace event is valid in the context of
--  Validator.

   procedure Add_Restriction
     (Validator         : access Type_Validator_Record;
      Restriction_Name  : Unicode.CES.Byte_Sequence;
      Restriction_Value : Unicode.CES.Byte_Sequence);
   --  Add a restriction to the set of possible values for Validator.
   --  The valid list of restrictions and their values depends on the type
   --  of Validator.
   --  By default, an error is reported through Invalid_Restriction

   procedure Add_Attribute
     (Validator : access Type_Validator_Record;
      Attribute : access Attribute_Validator_Record'Class);
   --  Add a valid attribute to Validator.

   procedure Add_Union
     (Validator : access Type_Validator_Record;
      Part      : access Type_Validator_Record'Class);
   --  Add a new element to the union in Validator

   function List_Of
     (Validator : access Type_Validator_Record) return Type_Validator;
   --  Return a new type validator that checks for a list of values valid for
   --  Validator.

   --------------
   -- Elements --
   --------------

   type XML_Element is private;

   function Create_Element
     (Qname   : Unicode.CES.Byte_Sequence;
      Of_Type : access Type_Validator_Record'Class) return XML_Element;
   --  Create a new element, with a specific type

   type XML_Type is private;

   function Create_Type
     (Qname   : Unicode.CES.Byte_Sequence;
      Of_Type : Type_Validator) return XML_Type;
   --  Create a new named type

   ------------
   -- Groups --
   ------------

   type Group_Model_Record is abstract new Type_Validator_Record with private;
   type Group_Model is access all Group_Model_Record'Class;

   type Sequence_Record is new Group_Model_Record with private;
   type Sequence is access all Sequence_Record'Class;

   type Choice_Record is new Group_Model_Record with private;
   type Choice is access all Choice_Record'Class;


   function Create_Sequence
     (Min_Occurs, Max_Occurs : Natural := 1) return Sequence;
   --  Create a new empty sequence
   --  (Min_Occurs, Max_Occurs) indicate the number of repetition allowed for
   --  that sequence.

   procedure Add_Sequence (Seq : access Sequence_Record; Item : XML_Element);
   procedure Add_Sequence (Seq : access Sequence_Record; Item : Sequence);
   procedure Add_Sequence (Seq : access Sequence_Record; Item : Choice);
--   procedure Add_Sequence (Sequence : access Sequence_Record; Item : Group);
--   procedure Add_Sequence (Sequence : access Sequence_Record; Item : Any);
   --  Add a new item to the sequence


   function Create_Choice
     (Min_Occurs, Max_Occurs : Natural := 1) return Choice;
   --  Create a new empty choice.
   --  (Min_Occurs, Max_Occurs) indicate the number of repetition allowed for
   --  that choice.

   procedure Add_Choice (C : access Choice_Record; Item : XML_Element);
   procedure Add_Choice (C : access Choice_Record; Item : Sequence);
   procedure Add_Choice (C : access Choice_Record; Item : Choice);
--   procedure Add_Choice (Choice : access Choice_Record; Item : Group);
--   procedure Add_Choice (Choice : access Choice_Record; Item : Any);
   --  Add a new item to the list of choices

   --------------
   -- Grammars --
   --------------

   type XML_Grammar is private;

   function Lookup_Element
     (Grammar : XML_Grammar;
      Qname   : Unicode.CES.Byte_Sequence) return Type_Validator;
   function Lookup_Type
     (Grammar : XML_Grammar;
      Qname   : Unicode.CES.Byte_Sequence) return Type_Validator;
   --  Return the type validator to use for elements with name Qname.
   --  "null" is returned if there is no validator defined for these elements.
   --  The returned value must be freed by the user

   procedure Register_Type
     (Grammar   : XML_Grammar;
      Typ       : XML_Type);
   --  Register a new type in the grammar

   procedure Register_Element
     (Grammar   : XML_Grammar;
      Element   : XML_Element);
   --  Register a new element in the grammar

   procedure Initialize (Grammar : in out XML_Grammar);
   --  Initialize the internal structure of the grammar

   procedure Free (Grammar : in out XML_Grammar);
   --  Free the memory occupied by the grammar


private
   type XML_Element is record
      Qname   : Unicode.CES.Byte_Sequence_Access;
      Of_Type : Type_Validator;
   end record;
   No_Element : constant XML_Element := (null, null);

   type XML_Type is record
      Qname   : Unicode.CES.Byte_Sequence_Access;
      Of_Type : Type_Validator;
   end record;
   No_Type : constant XML_Type := (null, null);

   type Attribute_Validator_List
     is array (Natural range <>) of Attribute_Validator;
   type Attribute_Validator_List_Access is access Attribute_Validator_List;

   type Type_Validator_Record is tagged record
      Attributes : Attribute_Validator_List_Access;
      --  The list of valid attributes registered for this validator.
      --  ??? Could be implemented more efficiently through a htable
   end record;

   type Attribute_Validator_Record is tagged record
      Local_Name     : Unicode.CES.Byte_Sequence_Access;
      Attribute_Type : Type_Validator;
      Attribute_Form : Attribute_Form_Type;
      Attribute_Use  : Attribute_Use_Type;
      Value          : Unicode.CES.Byte_Sequence_Access;
   end record;

   procedure Free (Typ : in out XML_Type);
   function Get_Key (Typ : XML_Type) return Unicode.CES.Byte_Sequence;
   function Hash
     (Key : Unicode.CES.Byte_Sequence) return Interfaces.Unsigned_32;

   package Types_Htable is new Sax.HTable
     (Element       => XML_Type,
      Empty_Element => No_Type,
      Free          => Free,
      Key           => Unicode.CES.Byte_Sequence,
      Get_Key       => Get_Key,
      Hash          => Hash,
      Equal         => "=");
   type Types_Htable_Access is access Types_Htable.HTable;

   procedure Free (Element : in out XML_Element);
   function Get_Key
     (Element : XML_Element) return Unicode.CES.Byte_Sequence;

   package Elements_Htable is new Sax.HTable
     (Element       => XML_Element,
      Empty_Element => No_Element,
      Free          => Free,
      Key           => Unicode.CES.Byte_Sequence,
      Get_Key       => Get_Key,
      Hash          => Hash,
      Equal         => "=");
   type Elements_Htable_Access is access Elements_Htable.HTable;

   type XML_Grammar is record
      Types    : Types_Htable_Access;
      Elements : Elements_Htable_Access;
   end record;

   type XML_Item_Type is (Item_Element, Item_Nested);
   type XML_Item (Typ : XML_Item_Type := Item_Element) is record
      case Typ is
         when Item_Element => Element   : XML_Element;
         when Item_Nested  => Validator : Group_Model;
      end case;
   end record;

   type Item_List_Record;
   type Item_List_Access is access Item_List_Record;
   type Item_List_Record is record
      Particle : XML_Item;
      Next     : Item_List_Access;
   end record;
   type Item_List is record
      First, Last : Item_List_Access;
   end record;
   Empty_Item_List : constant Item_List := (null, null);

   procedure Append (List : in out Item_List; Item : XML_Item);
   --  Append a new element to the list


   type Group_Model_Record is abstract new Type_Validator_Record with record
      Particles   : Item_List := Empty_Item_List;
   end record;
   type Group_Model_Data_Record is new Validator_Data_Record with record
      Nested : Group_Model := null;
      Parent : Group_Model := null;
      --  If a group_model is nested inside another (a sequence within a
      --  a sequence for instance), then Nested will point to the nested
      --  group_model while it is being processed. Parent will point to its
      --  parent, so that at the end of the evaluation of the nested group, we
      --  can go back to the parent.

      Nested_Data : Validator_Data := null;
      Parent_Data : Validator_Data := null;
      --  The data used to evaluate Nested or Parent
   end record;
   type Group_Model_Data is access all Group_Model_Data_Record'Class;

   procedure Nested_Group_Terminated
     (Group : access Group_Model_Record;
      Data  : Validator_Data);
   --  Called when the nested group terminated. By default, this resets the
   --  nested pointer in Data.

   function Applies_To_Tag
     (Group      : access Group_Model_Record;
      Local_Name : Unicode.CES.Byte_Sequence) return Boolean;
   --  Whether Group can process Local_Name. This is used for group_models
   --  nested in a choice, so that we can find out which one should be applied
   --  (given the restrictions in schema, only one of them can apply).

   procedure Validate_Characters
     (Validator      : access Group_Model_Record;
      Ch             : Unicode.CES.Byte_Sequence;
      Data           : Validator_Data);
   --  See doc for inherited subprograms


   type Sequence_Record is new Group_Model_Record with record
      Max_Occurs : Natural := 1;
      Min_Occurs : Natural := 1;
   end record;
   type Sequence_Data is new Group_Model_Data_Record with record
      Current      : Item_List_Access;
      Num_Occurs   : Natural := 0;
   end record;
   type Sequence_Data_Access is access all Sequence_Data'Class;
   procedure Validate_Start_Element
     (Validator         : access Sequence_Record;
      Local_Name        : Unicode.CES.Byte_Sequence;
      Data              : Validator_Data;
      Element_Validator : out Type_Validator);
   procedure Validate_End_Element
     (Validator         : access Sequence_Record;
      Local_Name        : Unicode.CES.Byte_Sequence;
      Data              : Validator_Data);
   function Create_Validator_Data
     (Validator : access Sequence_Record) return Validator_Data;
   procedure Nested_Group_Terminated
     (Group : access Sequence_Record;
      Data  : Validator_Data);
   function Applies_To_Tag
     (Group      : access Sequence_Record;
      Local_Name : Unicode.CES.Byte_Sequence) return Boolean;
   --  See doc for inherited subprograms


   type Choice_Record is new Group_Model_Record with record
      Max_Occurs : Natural := 1;
      Min_Occurs : Natural := 1;
   end record;
   type Choice_Data is new Group_Model_Data_Record with record
      Num_Occurs : Natural := 0;
   end record;
   type Choice_Data_Access is access all Choice_Data'Class;
   procedure Validate_Start_Element
     (Validator         : access Choice_Record;
      Local_Name        : Unicode.CES.Byte_Sequence;
      Data              : Validator_Data;
      Element_Validator : out Type_Validator);
   procedure Validate_End_Element
     (Validator         : access Choice_Record;
      Local_Name        : Unicode.CES.Byte_Sequence;
      Data              : Validator_Data);
   function Create_Validator_Data
     (Validator : access Choice_Record) return Validator_Data;
   procedure Nested_Group_Terminated
     (Group : access Choice_Record;
      Data  : Validator_Data);
   function Applies_To_Tag
     (Group      : access Choice_Record;
      Local_Name : Unicode.CES.Byte_Sequence) return Boolean;
   --  See doc for inherited subprograms

end Schema.Validators;
