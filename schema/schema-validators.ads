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

   type XML_Validator_Record is tagged private;
   type XML_Validator is access all XML_Validator_Record'Class;
   --  A new validator is typically created every time a new element starts,
   --  and is in charge of checking the contents and attributes of that
   --  element.
   --  The default implementation always validates.

   Unbounded : constant Integer := -1;
   --  To indicate that a Max_Occurs is set to unbounded

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

   procedure Free (Data : in out Validator_Data);
   --  Free Data and the pointed data

   -----------
   -- Types --
   -----------

   type XML_Type is private;
   No_Type : constant XML_Type;
   --  A type, which can either be named (ie it has been explicitely declared
   --  with a name and stored in the grammar), or anonymous.

   function Create_Type
     (Qname     : Unicode.CES.Byte_Sequence;
      Validator : access XML_Validator_Record'Class) return XML_Type;
   --  Create a new named type.

   function Get_Validator (Typ : XML_Type) return XML_Validator;
   --  Return the validator used for that type

   procedure Add_Restriction
     (Typ               : XML_Type;
      Restriction_Name  : Unicode.CES.Byte_Sequence;
      Restriction_Value : Unicode.CES.Byte_Sequence);
   --  Add a new restriction to the type (in fact, to its validator, so you
   --  should do a Clone first if you do not wish to alter the base type).

   function List_Of (Typ : XML_Type) return XML_Type;
   --  Return a new type validator that checks for a list of values valid for
   --  Validator.

   function Extension_Of
     (Base      : XML_Type;
      Extension : access XML_Validator_Record'Class) return XML_Validator;
   --  Create an extension of Base.
   --  Base doesn't need to be a Clone of some other type, since it isn't
   --  altered

   function Restriction_Of
     (Base        : XML_Type;
      Restriction : access XML_Validator_Record'Class) return XML_Validator;
   --  Create a restriction of Base
   --  Base doesn't need to be a Clone of some other type, since it isn't
   --  altered

   -------------------------
   -- Attribute_Validator --
   -------------------------

   type Attribute_Validator_Record is tagged private;
   type Attribute_Validator is access all Attribute_Validator_Record'Class;

   type Attribute_Use_Type is
     (Prohibited, Optional, Required, Default, Fixed);
   type Attribute_Form_Type is (Qualified, Unqualified);

   function Create_Attribute
     (Name           : Unicode.CES.Byte_Sequence;
      Attribute_Type : XML_Type                  := No_Type;
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

   function Clone (Validator : XML_Validator) return XML_Validator;
   --  Return a clone of Validator

   procedure Clone
     (Validator : access XML_Validator_Record;
      Into      : in out XML_Validator_Record'Class);
   --  Copy all the fields of Validator into Into.

   procedure Free (Validator : in out XML_Validator_Record);
   procedure Free (Validator : in out XML_Validator);
   --  Free the memory occupied by Validator

   function Create_Validator_Data
     (Validator : access XML_Validator_Record) return Validator_Data;
   --  Create an instance of the data that the validator needs to check its
   --  associated node. This data will be automatically freed when the node
   --  ends. This function is called once when the element starts.
   --  By default, this returns null, and no data is associated.

   procedure Validate_Start_Element
     (Validator         : access XML_Validator_Record;
      Local_Name        : Unicode.CES.Byte_Sequence;
      Data              : Validator_Data;
      Element_Validator : out XML_Type);
   --  Check whether this Start_Element event is valid in the context of the
   --  validator. Data is the result of Create_Validator_Data.
   --
   --  Element_Validator is set, on exit, to the validator that should be used
   --  to validate the next element. It can safely be left to null if the
   --  element is declared at a global level in the Schema
   --  Raise XML_Validation_Error in case of error

   procedure Validate_Attributes
     (Validator         : access XML_Validator_Record;
      Atts              : Sax.Attributes.Attributes'Class;
      Data              : Validator_Data);
   --  Check whether this list of attributes is valid for elements associated
   --  with this validator. By default, this simply check whether the list of
   --  attributes registered through Add_Attribute matches Atts.

   procedure Validate_End_Element
     (Validator      : access XML_Validator_Record;
      Local_Name     : Unicode.CES.Byte_Sequence;
      Data           : Validator_Data);
   --  Check whether this End_Element event is valid.
   --  This is called for the node associated with the validator itself, not
   --  for the child, as opposed to what is done for Validate_Start_Element.
   --  Raise XML_Validation_Error in case of error

   procedure Validate_Characters
     (Validator      : access XML_Validator_Record;
      Ch             : Unicode.CES.Byte_Sequence;
      Data           : Validator_Data);
   --  Check whether this Characters event is valid in the context of
   --  Validator. Multiple calls to the SAX event Characters are grouped before
   --  calling this subprogram.

   procedure Add_Restriction
     (Validator         : access XML_Validator_Record;
      Restriction_Name  : Unicode.CES.Byte_Sequence;
      Restriction_Value : Unicode.CES.Byte_Sequence);
   --  Add a restriction to the set of possible values for Validator.
   --  The valid list of restrictions and their values depends on the type
   --  of Validator.
   --  By default, an error is reported through Invalid_Restriction

   procedure Add_Attribute
     (Validator : access XML_Validator_Record;
      Attribute : access Attribute_Validator_Record'Class);
   --  Add a valid attribute to Validator.

   procedure Add_Union
     (Validator : access XML_Validator_Record; Part : XML_Type);
   --  Add a new element to the union in Validator

   --------------
   -- Elements --
   --------------

   type XML_Element is private;

   function Create_Element
     (Qname   : Unicode.CES.Byte_Sequence;
      Of_Type : XML_Type) return XML_Element;
   --  Create a new element, with a specific type

   procedure Set_Substitution_Group
     (Element : XML_Element; Head : XML_Element);
   --  Define a substitution group for Validator, as declared through the
   --  "substitutionGroup" attribute of the XML Schema.
   --  Anywhere Head is referenced, Validator can be used
   --  instead.

   function Get_Type (Element : XML_Element) return XML_Type;
   --  Return the type validator for this element

   ------------
   -- Groups --
   ------------

   type Group_Model_Record is abstract new XML_Validator_Record with private;
   type Group_Model is access all Group_Model_Record'Class;

   type XML_Group is private;
   --  A group of elements

   function Create_Group (Qname : Unicode.CES.Byte_Sequence) return XML_Group;
   --  Return a new empty group

   procedure Add_Particle
     (Group : in out XML_Group; Particle : access Group_Model_Record'Class);
   --  Add a new particle in the group

   function Extension_Of
     (Base       : XML_Type;
      Group      : XML_Group;
      Min_Occurs : Natural := 1;
      Max_Occurs : Integer := 1) return XML_Validator;
   --  Create an extension of Base.
   --  Base doesn't need to be a Clone of some other type, since it isn't
   --  altered

   ---------------
   -- Particles --
   ---------------

   type Sequence_Record is new Group_Model_Record with private;
   type Sequence is access all Sequence_Record'Class;

   type Choice_Record is new Group_Model_Record with private;
   type Choice is access all Choice_Record'Class;


   function Create_Sequence
     (Min_Occurs : Natural := 1;
      Max_Occurs : Integer := 1) return Sequence;
   --  Create a new empty sequence
   --  (Min_Occurs, Max_Occurs) indicate the number of repetition allowed for
   --  that sequence.
   --  If Max_Occurs = Unbounded, the number of repeats is unbounded

   procedure Add_Particle
     (Seq : access Sequence_Record; Item : XML_Element;
      Min_Occurs : Natural := 1; Max_Occurs : Integer := 1);
   procedure Add_Particle
     (Seq : access Sequence_Record; Item : Sequence;
      Min_Occurs : Natural := 1; Max_Occurs : Integer := 1);
   procedure Add_Particle
     (Seq : access Sequence_Record; Item : Choice;
      Min_Occurs : Natural := 1; Max_Occurs : Integer := 1);
   procedure Add_Particle
     (Seq : access Sequence_Record; Item : XML_Group;
      Min_Occurs : Natural := 1; Max_Occurs : Integer := 1);


   function Create_Choice
     (Min_Occurs : Natural := 1;
      Max_Occurs : Integer := 1) return Choice;
   --  Create a new empty choice.
   --  (Min_Occurs, Max_Occurs) indicate the number of repetition allowed for
   --  that choice.
   --  If Max_Occurs = Unbounded, the number of repeats is unbounded

   procedure Add_Particle
     (C : access Choice_Record; Item : XML_Element;
      Min_Occurs : Natural := 1; Max_Occurs : Integer := 1);
   procedure Add_Particle
     (C : access Choice_Record; Item : Sequence;
      Min_Occurs : Natural := 1; Max_Occurs : Integer := 1);
   procedure Add_Particle
     (C : access Choice_Record; Item : Choice;
      Min_Occurs : Natural := 1; Max_Occurs : Integer := 1);
   procedure Add_Particle
     (C : access Choice_Record; Item : XML_Group;
      Min_Occurs : Natural := 1; Max_Occurs : Integer := 1);

   --------------
   -- Grammars --
   --------------

   type XML_Grammar is private;

   function Lookup_Element
     (Grammar : XML_Grammar;
      Qname   : Unicode.CES.Byte_Sequence) return XML_Element;
   function Lookup
     (Grammar : XML_Grammar;
      Qname   : Unicode.CES.Byte_Sequence) return XML_Type;
   function Lookup_Group
     (Grammar : XML_Grammar;
      Qname   : Unicode.CES.Byte_Sequence) return XML_Group;
   --  Return the type validator to use for elements with name Qname.
   --  "null" is returned if there is no validator defined for these elements.
   --  The returned value must be freed by the user.
   --  If you are going to modify the returned value in any way (adding new
   --  restrictions,...), you must clone it, or you will modify the actual
   --  type stored in the grammar.
   --  If the element doesn't exist yet, a forward declaration is created for
   --  it, that must be overriden later on.

   procedure Register (Grammar : XML_Grammar; Typ     : XML_Type);
   procedure Register (Grammar : XML_Grammar; Element : XML_Element);
   procedure Register (Grammar : XML_Grammar; Group   : XML_Group);
   --  Register a new type or element in the grammar

   function Register_Forward
     (Grammar : XML_Grammar;
      Qname   : Unicode.CES.Byte_Sequence) return XML_Type;
   function Register_Forward
     (Grammar : XML_Grammar;
      Qname   : Unicode.CES.Byte_Sequence) return XML_Element;
   function Register_Forward
     (Grammar : XML_Grammar;
      Qname   : Unicode.CES.Byte_Sequence) return XML_Group;
   --  Register a type, the definition of which is not know at that point.
   --  The definition must be provided before the grammar is fully filled, or
   --  this is an error.

   procedure Initialize (Grammar : in out XML_Grammar);
   --  Initialize the internal structure of the grammar

   procedure Free (Grammar : in out XML_Grammar);
   --  Free the memory occupied by the grammar


   procedure Set_Debug_Name
     (Typ : access XML_Validator_Record'Class; Name : String);
   --  Will be removed

private
   --------------
   -- XML_Type --
   --------------

   type XML_Type_Record is record
      Qname     : Unicode.CES.Byte_Sequence_Access;
      Validator : XML_Validator;
   end record;
   type XML_Type is access all XML_Type_Record;
   No_Type : constant XML_Type := null;

   -----------------
   -- XML_Element --
   -----------------

   type XML_Element_Record is record
      Qname   : Unicode.CES.Byte_Sequence_Access;
      Of_Type : XML_Type;
   end record;
   type XML_Element is access all XML_Element_Record;
   No_Element : constant XML_Element := null;

   ------------------
   -- Element_List --
   ------------------

   type Element_List is array (Natural range <>) of XML_Element;
   type Element_List_Access is access Element_List;

   procedure Append
     (List    : in out Element_List_Access; Element : XML_Element);

   -------------------------
   -- Attribute_Validator --
   -------------------------

   type Attribute_Validator_List
     is array (Natural range <>) of Attribute_Validator;
   type Attribute_Validator_List_Access is access Attribute_Validator_List;

   function Clone
     (List : Attribute_Validator_List_Access)
      return Attribute_Validator_List_Access;
   --  Return a copy of the list. The validators themselves have not been
   --  cloned, however

   type Attribute_Validator_Record is tagged record
      Local_Name     : Unicode.CES.Byte_Sequence_Access;
      Attribute_Type : XML_Type;
      Attribute_Form : Attribute_Form_Type;
      Attribute_Use  : Attribute_Use_Type;
      Value          : Unicode.CES.Byte_Sequence_Access;
   end record;

   --------------
   -- XML_Validator --
   --------------

   type XML_Validator_Record is tagged record
      Debug_Name : Unicode.CES.Byte_Sequence_Access;
      --  Temporary, will be removed

      Substitution_Groups : Element_List_Access;
      --  List of all validators in the same substitution group

      Attributes : Attribute_Validator_List_Access;
      --  The list of valid attributes registered for this validator.
      --  ??? Could be implemented more efficiently through a htable
   end record;

   ---------------
   -- XML_Group --
   ---------------

   type XML_Group_Record;
   type XML_Group is access all XML_Group_Record;
   No_Group : constant XML_Group := null;

   ---------------
   -- Particles --
   ---------------

   type Particle_Type is (Particle_Element, Particle_Nested, Particle_Group);
   type XML_Particle;
   type XML_Particle_Access is access XML_Particle;
   type XML_Particle (Typ : Particle_Type) is record
      Min_Occurs : Natural;
      Max_Occurs : Integer;
      Next       : XML_Particle_Access;
      case Typ is
         when Particle_Element => Element   : XML_Element;
         when Particle_Nested  => Validator : Group_Model;
         when Particle_Group   => Group     : XML_Group;
      end case;
   end record;

   type Particle_List_Record is record
      First, Last : XML_Particle_Access;
   end record;
   type Particle_List is access Particle_List_Record;

   function Empty_Particle_List return Particle_List;
   --  Return an empty list

   procedure Append (List : in out Particle_List; Item : XML_Particle);
   --  Append a new element to the list

   -----------------------
   -- Particle_Iterator --
   -----------------------
   --  This iterator iterates over a list of particles, but consider a group
   --  as a set of particles that are also iterated

   type Particle_Iterator is record
      Current  : XML_Particle_Access;
      In_Group : XML_Particle_Access;
   end record;
   No_Iter : constant Particle_Iterator := (null, null);

   function  Start (List : Particle_List) return Particle_Iterator;
   procedure Next  (Iter : in out Particle_Iterator);
   function  Get   (Iter : Particle_Iterator) return XML_Particle_Access;
   function  Get_Min_Occurs (Iter : Particle_Iterator) return Natural;
   function  Get_Max_Occurs (Iter : Particle_Iterator) return Integer;
   --  Iterate over a list of particles. Get returns null at the end of the
   --  iteration

   ----------------------
   -- XML_Group_Record --
   ----------------------

   type XML_Group_Record is record
      Qname     : Unicode.CES.Byte_Sequence_Access;
      Particles : Particle_List;
   end record;

   -------------
   -- Grammar --
   -------------

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
   --  We store a pointer to an XML_Type_Record, since the validator might not
   --  be known when we first reference the type (it is valid in an XML schema
   --  to ref to a type described later on).

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

   procedure Free (Group : in out XML_Group);
   function Get_Key (Group : XML_Group) return Unicode.CES.Byte_Sequence;

   package Groups_Htable is new Sax.HTable
     (Element       => XML_Group,
      Empty_Element => No_Group,
      Free          => Free,
      Key           => Unicode.CES.Byte_Sequence,
      Get_Key       => Get_Key,
      Hash          => Hash,
      Equal         => "=");
   type Groups_Htable_Access is access Groups_Htable.HTable;

   type XML_Grammar is record
      Types    : Types_Htable_Access;
      Elements : Elements_Htable_Access;
      Groups   : Groups_Htable_Access;

      Forward_Type : XML_Type;
      --  Common type to use for all forward declarations. This needs to be
      --  cloned when we are declaring a new type.
   end record;

   ------------------------
   -- Group_Model_Record --
   ------------------------

   type Group_Model_Record is abstract new XML_Validator_Record with record
      Particles  : Particle_List := Empty_Particle_List;
      Max_Occurs : Integer := 1;
      Min_Occurs : Natural := 1;
   end record;
   type Group_Model_Data_Record is new Validator_Data_Record with record
      Num_Occurs   : Natural := 0;

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

   procedure Clone
     (Validator : access Group_Model_Record;
      Into      : in out XML_Validator_Record'Class);
   procedure Validate_Characters
     (Validator      : access Group_Model_Record;
      Ch             : Unicode.CES.Byte_Sequence;
      Data           : Validator_Data);
   --  See doc for inherited subprograms

   ---------------------
   -- Sequence_Record --
   ---------------------

   type Sequence_Record is new Group_Model_Record with null record;
   type Sequence_Data is new Group_Model_Data_Record with record
      Current      : Particle_Iterator;

      Num_Occurs_Of_Current : Integer := 0;
      --  Number of repeats for the current particle of the sequence. This is
      --  set to Unbounded to force transition to the next element in the
      --  sequence.
   end record;
   type Sequence_Data_Access is access all Sequence_Data'Class;
   procedure Validate_Start_Element
     (Validator         : access Sequence_Record;
      Local_Name        : Unicode.CES.Byte_Sequence;
      Data              : Validator_Data;
      Element_Validator : out XML_Type);
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

   -------------------
   -- Choice_Record --
   -------------------

   type Choice_Record is new Group_Model_Record with null record;
   type Choice_Data is new Group_Model_Data_Record with null record;
   type Choice_Data_Access is access all Choice_Data'Class;
   procedure Validate_Start_Element
     (Validator         : access Choice_Record;
      Local_Name        : Unicode.CES.Byte_Sequence;
      Data              : Validator_Data;
      Element_Validator : out XML_Type);
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
