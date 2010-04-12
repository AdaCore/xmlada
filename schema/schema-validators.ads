-----------------------------------------------------------------------
--                XML/Ada - An XML suite for Ada95                   --
--                                                                   --
--                       Copyright (C) 2004-2010, AdaCore            --
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

with Unicode.CES;
with Sax.Attributes;
with Sax.HTable;
with Sax.Pointers;
with Sax.Readers;
with Sax.Utils;

package Schema.Validators is

   XML_Schema_URI : constant Unicode.CES.Byte_Sequence :=
     "http://www.w3.org/2001/XMLSchema";
   XML_URI : constant Unicode.CES.Byte_Sequence :=
     "http://www.w3.org/XML/1998/namespace";
   XML_Instance_URI : constant Unicode.CES.Byte_Sequence :=
     "http://www.w3.org/2001/XMLSchema-instance";

   XML_Validation_Error : exception;
   --  Raised in case of error in the validation process. The exception message
   --  contains the error, but not its location

   XML_Not_Implemented : exception;
   --  Raised when a schema uses features that are not supported by XML/Ada yet

   type XSD_Versions is (XSD_1_0, XSD_1_1);
   --  The version of XSD the parser should support.
   --  The support for 1.1 is only partial at present.

   type XML_Validator_Record is tagged private;
   type XML_Validator is access all XML_Validator_Record'Class;
   --  A new validator is typically created every time a new element starts,
   --  and is in charge of checking the contents and attributes of that
   --  element.
   --  The default implementation always validates.

   type XML_Grammar is private;
   type XML_Grammar_NS_Record is private;
   type XML_Grammar_NS is access all XML_Grammar_NS_Record;
   --  The part of a grammar specialized for a given namespace.
   --  A grammar can contain the definition for multiple namespaces (generally
   --  the standard XML Schema namespace for predefined types, and the
   --  namespace we are defining). Each of these is accessed by a separate
   --  XML_Grammar_NS object
   --  A grammar is a smart pointer, and will take care of freeing memory
   --  automatically when no longer needed.

   procedure Set_XSD_Version
     (Grammar : in out XML_Grammar; XSD_Version : XSD_Versions);
   function Get_XSD_Version (Grammar : XML_Grammar) return XSD_Versions;
   --  Set the version of XSD accepted by this grammar

   procedure Set_System_Id
     (Grammar   : XML_Grammar_NS;
      System_Id : Unicode.CES.Byte_Sequence);
   function Get_System_Id
     (Grammar : XML_Grammar_NS) return Unicode.CES.Byte_Sequence;
   --  The URI from which we loaded the schema

   No_Grammar : constant XML_Grammar;
   --  No Grammar has been defined

   type XML_Element is private;
   No_Element : constant XML_Element;
   --  An element of an XML stream (associated with a start-tag)

   Unbounded : constant Integer := -1;
   --  To indicate that a Max_Occurs is set to unbounded

   type Form_Type is (Qualified, Unqualified);
   --  Whether locally declared elements need to be qualified or whether
   --  qualification is optional (the latter is the default). This does not
   --  apply to global elements, that always need to be qualified (or found in
   --  the default namespace).
   --  Note that elements defined in a <group> are considered local only if
   --  they do not use the "ref" attribute, otherwise they are considered
   --  global and therefore the "form" does not apply to them.

   type Process_Contents_Type is (Process_Strict, Process_Lax, Process_Skip);
   --  When in an element that accepts any children (ur-type, or xsd:any), this
   --  type indicates that should be done to validate the children:
   --     Strict: the children must have a definition in the schema (as a
   --             global element)
   --     Lax:    if the children have a definition, it is used, otherwise they
   --             are just accepted as is.
   --     Skip:   even if the children have a defition, it is ignored, and the
   --             child is processed as a ur-type.

   --------------------
   -- Validator_Data --
   --------------------
   --  Most validators, when they are performing validation, need to keep some
   --  data while their node is active. Such data might be used to store
   --  counters, current items,...

   type Validator_Data_Record is abstract tagged private;
   type Validator_Data is access all Validator_Data_Record'Class;

   procedure Free (Data : in out Validator_Data_Record);
   --  Free the memory associated with the data.
   --  By default, this does nothing

   procedure Free (Data : in out Validator_Data);
   --  Free Data and the pointed data

   ------------
   -- Facets --
   ------------

   type Facets_Description_Record is abstract tagged null record;
   type Facets_Description is access all Facets_Description_Record'Class;

   procedure Add_Facet
     (Facets      : in out Facets_Description_Record;
      Facet_Name  : Unicode.CES.Byte_Sequence;
      Facet_Value : Unicode.CES.Byte_Sequence;
      Applied     : out Boolean) is abstract;
   --  Set the value of a facet.
   --  Applied is set to True if the facet was valid for Facets

   procedure Check_Facet
     (Facets : in out Facets_Description_Record;
      Value  : Unicode.CES.Byte_Sequence) is abstract;
   --  Check whether Value matches Facets. Raises XML_Validator_Error otherwise

   procedure Copy
     (From : Facets_Description_Record;
      To   : in out Facets_Description_Record'Class) is abstract;
   --  Copy all the facets defined in From into To

   procedure Free (Facets : in out Facets_Description_Record) is abstract;
   procedure Free (Facets : in out Facets_Description);
   --  Free the facets;

   -----------
   -- Types --
   -----------

   type XML_Type is private;
   No_Type : constant XML_Type;
   --  A type, which can either be named (ie it has been explicitely declared
   --  with a name and stored in the grammar), or anonymous.

   function Create_Local_Type
     (Grammar    : XML_Grammar_NS;
      Validator  : access XML_Validator_Record'Class) return XML_Type;
   --  Create a new local type.
   --  This type cannot be looked up in the grammar later on. See the function
   --  Create_Global_Type below if you need this capability

   function Get_Validator (Typ : XML_Type) return XML_Validator;
   --  Return the validator used for that type

   function List_Of
     (Grammar : XML_Grammar_NS; Typ : XML_Type) return XML_Type;
   --  Return a new type validator that checks for a list of values valid for
   --  Validator.

   function Is_ID (Typ : XML_Type) return Boolean;
   --  Whether Typ is an ID, ie the values of its attributes must be unique
   --  throughout the document

   function Is_ID (Validator : XML_Validator_Record) return Boolean;
   --  Whether the validator is associated with an ID type

   function Extension_Of
     (G         : XML_Grammar_NS;
      Base      : XML_Type;
      Extension : XML_Validator := null) return XML_Validator;
   --  Create an extension of Base.
   --  Base doesn't need to be a Clone of some other type, since it isn't
   --  altered. See also Is_Extension_Of below

   function Is_Extension_Of
     (Validator : XML_Validator_Record;
      Base      : access XML_Validator_Record'Class) return Boolean;
   function Is_Restriction_Of
     (Validator : XML_Validator_Record;
      Base      : access XML_Validator_Record'Class) return Boolean;
   --  Whether Validator is an extension/restriction of Base

   function Restriction_Of
     (G           : XML_Grammar_NS;
      Base        : XML_Type;
      Restriction : XML_Validator := null) return XML_Validator;
   --  Create a restriction of Base
   --  Base doesn't need to be a Clone of some other type, since it isn't
   --  altered. See also Is_Restriction_Of below

   function Get_Local_Name (Typ : XML_Type) return Unicode.CES.Byte_Sequence;
   --  Return the local name of the type

   procedure Check_Content_Type
     (Typ : XML_Type; Should_Be_Simple : Boolean);
   --  Check whether Typ is a simpleType or a complexType. See the description
   --  of the homonym for validators.
   --  When in doubt, use this one instead of the one for validators, since
   --  this one properly handles No_Type and types whose definition has not yet
   --  been parsed in the Schema.

   function Is_Simple_Type (Typ : XML_Type) return Boolean;
   --  Whether Typ is a simple type

   procedure Set_Block
     (Typ            : XML_Type;
      On_Restriction : Boolean;
      On_Extension   : Boolean);
   function Get_Block_On_Restriction (Typ : XML_Type) return Boolean;
   function Get_Block_On_Extension (Typ : XML_Type) return Boolean;
   --  Set the "block" status of the type.
   --  This can also be done at the element's level

   procedure Set_Final
     (Typ            : XML_Type;
      On_Restriction : Boolean;
      On_Extension   : Boolean);
   function Get_Final_On_Restriction (Typ : XML_Type) return Boolean;
   function Get_Final_On_Extension   (Typ : XML_Type) return Boolean;
   --  Set the final status of the element

   procedure Normalize_Whitespace
     (Typ       : XML_Type;
      Atts      : Sax.Attributes.Attributes'Class;
      Index     : Natural);
   --  Normalizes whitespaces in the attribute, depending on the type
   --  represented by Validator.

   ---------------
   -- ID_Htable --
   ---------------

   type Id_Htable_Access is private;

   type Validation_Context is record
      Id_Table  : Id_Htable_Access;
      --  Mapping of IDs to elements

      Grammar   : XML_Grammar := No_Grammar;

      Context   : Sax.Readers.Element_Access;
      --  Current element being parsed

      Parser    : Sax.Readers.Reader_Access;
   end record;
   type Validation_Context_Access is access all Validation_Context;

   procedure Free (Id_Table : in out Id_Htable_Access);

   -------------------------
   -- Attribute_Validator --
   -------------------------

   type Attribute_Validator_Record is abstract tagged private;
   type Attribute_Validator is access all Attribute_Validator_Record'Class;

   type Attribute_Use_Type is (Prohibited, Optional, Required, Default);

   function Create_Local_Attribute
     (Local_Name     : Unicode.CES.Byte_Sequence;
      NS             : XML_Grammar_NS;
      Attribute_Type : XML_Type                  := No_Type;
      Attribute_Form : Form_Type                 := Unqualified;
      Attribute_Use  : Attribute_Use_Type        := Optional;
      Fixed          : Unicode.CES.Byte_Sequence := "";
      Has_Fixed      : Boolean := False;
      Value          : Unicode.CES.Byte_Sequence := "")
      return Attribute_Validator;
   function Create_Local_Attribute
     (Based_On       : Attribute_Validator;
      Attribute_Form : Form_Type                 := Unqualified;
      Attribute_Use  : Attribute_Use_Type        := Optional;
      Fixed          : Unicode.CES.Byte_Sequence := "";
      Has_Fixed      : Boolean := False;
      Value          : Unicode.CES.Byte_Sequence := "")
      return Attribute_Validator;
   --  Create a new local attribute validator. See also Create_Global_Attribute

   type Namespace_Kind is (Namespace_Other, Namespace_Any, Namespace_List,
                           Namespace_Local);
   --  "Any":   any non-conflicting namespace
   --  "Other": any non-conflicting namespace other than targetNamespace
   --  "Local": any unqualified non-conflicting namespace

   function Create_Any_Attribute
     (Process_Contents : Process_Contents_Type := Process_Strict;
      Kind : Namespace_Kind;
      NS   : XML_Grammar_NS) return Attribute_Validator;
   --  Equivalent of <anyAttribute> in an XML schema.
   --  Validates to true if the attribute's namespace is:
   --    Namespace_Other:  not equal to NS
   --    Namespace_Any:    any Namespace (the second parameter is irrelevant)
   --    Namespace_List:   equal to NS

   procedure Validate_Attribute
     (Validator : Attribute_Validator_Record;
      Atts      : in out Sax.Attributes.Attributes'Class;
      Index     : Natural;
      Context   : in out Validation_Context) is abstract;
   --  Return True if Value is valid for this attribute.
   --  Sets the type of the attribute (per sax-attributes) to ID if we have an
   --  ID attribute.
   --  Raise XML_Validation_Error in case of error

   procedure Free (Validator : in out Attribute_Validator_Record) is abstract;
   --  Free the memory occupied by the validator

   function Is_Equal
     (Attribute : Attribute_Validator_Record;
      Attr2     : Attribute_Validator_Record'Class)
     return Boolean is abstract;
   --  Whether the two are the same

   procedure Set_Type
     (Attr : access Attribute_Validator_Record; Attr_Type : XML_Type);
   function Get_Type
     (Attr : Attribute_Validator_Record) return XML_Type;
   --  Set the type of the attribute

   ----------------------
   -- Attribute groups --
   ----------------------

   type XML_Attribute_Group is private;
   Empty_Attribute_Group : constant XML_Attribute_Group;
   --  Created through calls to Create_Global_Attribute_Group below

   procedure Add_Attribute
     (Group    : in out XML_Attribute_Group;
      Attr     : access Attribute_Validator_Record'Class;
      Is_Local : Boolean := True);
   --  Add a new attribute to the group
   --  Is_Local should be true if the attribute is local, or False if this is
   --  a reference to a global attribute

   procedure Add_Attribute_Group
     (Group : in out XML_Attribute_Group;
      Attr  : XML_Attribute_Group);
   --  Add a new group of attributes

   ---------------------
   -- Type validators --
   ---------------------
   --  Such validators are build to validate specific parts of an XML
   --  document (a whole element).

   procedure Free (Validator : in out XML_Validator_Record);
   --  Free the memory occupied by Validator

   function Create_Validator_Data
     (Validator : access XML_Validator_Record) return Validator_Data;
   --  Create an instance of the data that the validator needs to check its
   --  associated node. This data will be automatically freed when the node
   --  ends. This function is called once when the element starts.
   --  By default, this returns null, and no data is associated.

   procedure Validate_Start_Element
     (Validator              : access XML_Validator_Record;
      Local_Name             : Unicode.CES.Byte_Sequence;
      Namespace_URI          : Unicode.CES.Byte_Sequence;
      NS                     : XML_Grammar_NS;
      Data                   : Validator_Data;
      Grammar                : XML_Grammar;
      Element_Validator      : out XML_Element);
   --  Check whether this Start_Element event is valid in the context of the
   --  validator. Data is the result of Create_Validator_Data.
   --
   --  NS is the *apparent* namespace, ie for local elements it is
   --  resolved to the parent element's namespace automatically.
   --  Namespace_URI is the actual namespace found in the XML file, possibly
   --  the empty string for an unqualified element.
   --
   --  Element_Validator is set, on exit, to the validator that should be used
   --  to validate the next element.
   --  Element_Validator should be set to null if no validation could be
   --  performed and the control should be given back to the caller (for
   --  instance in the case of nested sequences and choices).
   --
   --  Raise XML_Validation_Error in case of error

   procedure Validate_Attributes
     (Validator : access XML_Validator_Record;
      Atts      : in out Sax.Attributes.Attributes'Class;
      Nillable  : Boolean;
      Is_Nil    : out Boolean;
      Context   : in out Validation_Context);
   --  Check whether this list of attributes is valid for elements associated
   --  with this validator. By default, this simply check whether the list of
   --  attributes registered through Add_Attribute matches Atts.
   --
   --  Id_Table is used to ensure that two same Ids are not in the document. It
   --  is passed as an access type, so that in case of exception it is still
   --  properly set on exit.
   --
   --  Nillable indicates whether the xsi:nil attribute should be supported,
   --  even if not explicitely inserted in the list. Is_Nil is set to the value
   --  of this attribute.
   --
   --  Sets the type of the attributes (through Sax.Attributes.Set_Type) to Id
   --  if the corresponding attribute is an id.

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
      Empty_Element  : Boolean;
      Context        : in out Validation_Context);
   --  Check whether this Characters event is valid in the context of
   --  Validator. Multiple calls to the SAX event Characters are grouped before
   --  calling this subprogram.
   --  If Empty_Element is true, this indicates that the element is in fact
   --  empty. This is to distinguish from the empty string:
   --      <tag/>   and <tag></tag>
   --  If Empty_Element is true, then Ch is irrelevant

   function Get_Facets_Description
     (Validator : access XML_Validator_Record) return Facets_Description;
   --  Allocate a new record to store some values for the facets associated
   --  with Validator. This is used in restrictions or extensions of base
   --  types to override some of the facets. The returned description contains
   --  no preset facets.
   --  null is returned if the type doesn't have any facet.
   --  Return value must be freed by the caller

   procedure Add_Facet
     (Validator   : access XML_Validator_Record;
      Facet_Name  : Unicode.CES.Byte_Sequence;
      Facet_Value : Unicode.CES.Byte_Sequence);
   --  Add a restriction to the set of possible values for Validator.
   --  The valid list of restrictions and their values depends on the type
   --  of Validator.
   --  By default, an error is reported through Invalid_Restriction

   procedure Add_Attribute
     (Validator : access XML_Validator_Record;
      Attribute : access Attribute_Validator_Record'Class;
      Is_Local  : Boolean := True);
   --  Add a valid attribute to Validator.
   --  Is_Local should be true if the attribute is local, or False if this is
   --  a reference to a global attribute

   function Has_Attribute
     (Validator  : access XML_Validator_Record;
      NS         : XML_Grammar_NS;
      Local_Name : Unicode.CES.Byte_Sequence) return Boolean;
   --  Whether the given attribute was already added to Validator

   procedure Add_Attribute_Group
     (Validator : access XML_Validator_Record;
      Group     : XML_Attribute_Group);
   --  Add a new group of attributes

   function Is_Wildcard
     (Validator : access XML_Validator_Record) return Boolean;
   --  Whether Validator is a wildcard

   procedure Set_Mixed_Content
     (Validator : access XML_Validator_Record;
      Mixed     : Boolean);
   function Get_Mixed_Content
     (Validator : access XML_Validator_Record) return Boolean;
   --  Whether character data is allowed within that element, in addition to
   --  children nodes

   procedure Check_Replacement
     (Validator         : access XML_Validator_Record;
      Typ               : XML_Type;
      Had_Restriction   : in out Boolean;
      Had_Extension     : in out Boolean);
   --  Check whether Validator is a valid replacement for Typ (either an
   --  extension or a restriction, and not blocked by a "block" attribute).
   --  If there is an error, an XML_Validation_Error is raised.
   --  Otherwise, this function returns the type of replacement that is done.
   --
   --  Had_* Indicate whether a restriction or extension was encountered while
   --  going up the inheritance tree so far.

   procedure Check_Content_Type
     (Validator        : access XML_Validator_Record;
      Should_Be_Simple : Boolean);
   --  Check whether Validator describes a simple Type (or a complex Type with
   --  simpleContent), if Should_Be_Simple is true, or the opposite otherwise.
   --  Raises XML_Validator_Error in case of error.

   ------------
   -- Unions --
   ------------

   function Create_Union (G : XML_Grammar_NS) return XML_Validator;
   --  Create a new empty union

   procedure Add_Union
     (Validator : access XML_Validator_Record'Class; Part : XML_Type);
   --  Add a new element to the union in Validator

   --------------
   -- Elements --
   --------------

   function Create_Local_Element
     (Local_Name : Unicode.CES.Byte_Sequence;
      NS         : XML_Grammar_NS;
      Of_Type    : XML_Type;
      Form       : Form_Type) return XML_Element;
   --  Create a new element, with a specific type.
   --  This element is a local element, that cannot be registered in the
   --  grammar and looked up later on.
   --  See Register below if you want to create a global element

   procedure Set_Substitution_Group
     (Element : XML_Element; Head : XML_Element);
   function Get_Substitution_Group
     (Element : XML_Element) return XML_Element;
   --  Define a substitution group for Validator, as declared through the
   --  "substitutionGroup" attribute of the XML Schema.
   --  Anywhere Head is referenced, Validator can be used
   --  instead.

   function Is_Extension_Of
     (Element : XML_Element; Base : XML_Element) return Boolean;
   function Is_Restriction_Of
     (Element : XML_Element; Base : XML_Element) return Boolean;
   --  Whether Element is an extension/restriction of Base

   function Get_Type  (Element : XML_Element) return XML_Type;
   procedure Set_Type
     (Element      : XML_Element;
      Element_Type : XML_Type;
      Context      : in out Validation_Context);
   --  Return the type validator for this element

   function Get_Local_Name
     (Element : XML_Element) return Unicode.CES.Byte_Sequence;
   --  Return the local name of Element

   procedure Set_Default
     (Element  : XML_Element;
      Default  : Unicode.CES.Byte_Sequence;
      Context  : in out Validation_Context);
   function Has_Default (Element : XML_Element) return Boolean;
   function Get_Default
     (Element : XML_Element) return Unicode.CES.Byte_Sequence_Access;
   --  Manipulation of the "default" attribute.
   --  The value returned by Get_Default mustn't be altered or freed, and
   --  will be null if the attribute wasn't set. We return a pointer for
   --  efficiency only

   procedure Set_Fixed
     (Element  : XML_Element;
      Fixed    : Unicode.CES.Byte_Sequence;
      Context  : in out Validation_Context);
   function Has_Fixed (Element : XML_Element) return Boolean;
   function Get_Fixed
     (Element : XML_Element) return Unicode.CES.Byte_Sequence_Access;
   --  Manipulation of the "fixed" attribute
   --  The value returned by Get_Fixed mustn't be altered or freed, and
   --  will be null if the attribute wasn't set. We return a pointer for
   --  efficiency only

   procedure Set_Abstract (Element : XML_Element; Is_Abstract : Boolean);
   function  Is_Abstract  (Element : XML_Element) return Boolean;
   --  Whether the element is abstract

   procedure Set_Nillable (Element : XML_Element; Nillable : Boolean);
   function  Is_Nillable  (Element : XML_Element) return Boolean;
   --  Whether the element is nillable (this only adds support for the
   --  attribute xsi:nil

   procedure Set_Final
     (Element : XML_Element;
      On_Restriction : Boolean;
      On_Extension   : Boolean);
   --  Set the final status of the element

   procedure Set_Block
     (Element         : XML_Element;
      On_Restriction  : Boolean;
      On_Extension    : Boolean;
      On_Substitution : Boolean);
   function Get_Block_On_Restriction (Element : XML_Element) return Boolean;
   function Get_Block_On_Extension (Element : XML_Element) return Boolean;
   function Get_Block_On_Substitution (Element : XML_Element) return Boolean;
   --  Set the "block" status of the element

   procedure Check_Qualification
     (Grammar       : XML_Grammar;
      Element       : XML_Element;
      Namespace_URI : Unicode.CES.Byte_Sequence);
   --  Check whether the element should have been qualified or not,
   --  depending on its "form" attribute.
   --  Namespace_URI is the namespace as read in the file.

   function Is_Global (Element : XML_Element) return Boolean;
   --  Whether Element is a global element (ie declared at the top-level of
   --  the schema file), as opposed to a local element declared inside a
   --  global element:
   --     <schema>
   --       <element name="global">
   --         <sequence>
   --           <element name="local" />

   -------------
   -- XML_Any --
   -------------

   type XML_Any_Record is new XML_Validator_Record with private;
   type XML_Any is access all XML_Any_Record'Class;

   function Create_Any
     (Process_Contents : Process_Contents_Type := Process_Strict;
      Namespace        : Unicode.CES.Byte_Sequence;
      Target_NS        : XML_Grammar_NS) return XML_Any;
   --  Create a new validator for <any>

   ------------
   -- Groups --
   ------------

   type Group_Model_Record is abstract new XML_Validator_Record with private;
   type Group_Model is access all Group_Model_Record'Class;

   type XML_Group is private;
   No_XML_Group : constant XML_Group;
   --  A group of elements, Create through a call to Create_Global_Group

   procedure Add_Particle
     (Group : in out XML_Group; Particle : access Group_Model_Record'Class;
      Min_Occurs : Natural := 1; Max_Occurs : Natural := 1);
   --  Add a new particle in the group

   function Extension_Of
     (G          : XML_Grammar_NS;
      Base       : XML_Type;
      Group      : XML_Group;
      Min_Occurs : Natural := 1;
      Max_Occurs : Integer := 1) return XML_Validator;
   --  Create an extension of Base.
   --  Base doesn't need to be a Clone of some other type, since it isn't
   --  altered

   function Get_Local_Name
     (Group : XML_Group) return Unicode.CES.Byte_Sequence;
   --  Return the local name of the group

   ---------------
   -- Particles --
   ---------------

   type Sequence_Record is new Group_Model_Record with private;
   type Sequence is access all Sequence_Record'Class;

   type Choice_Record is new Group_Model_Record with private;
   type Choice is access all Choice_Record'Class;

   function Create_Sequence (G : XML_Grammar_NS) return Sequence;
   --  Create a new empty sequence
   --  (Min_Occurs, Max_Occurs) indicate the number of repetition allowed for
   --  that sequence.
   --  If Max_Occurs = Unbounded, the number of repeats is unbounded

   procedure Add_Particle
     (Seq              : access Sequence_Record;
      Item             : XML_Element;
      Min_Occurs       : Natural := 1;
      Max_Occurs       : Integer := 1);
   procedure Add_Particle
     (Seq : access Sequence_Record; Item : Sequence;
      Min_Occurs : Natural := 1; Max_Occurs : Integer := 1);
   procedure Add_Particle
     (Seq : access Sequence_Record; Item : Choice;
      Min_Occurs : Natural := 1; Max_Occurs : Integer := 1);
   procedure Add_Particle
     (Seq : access Sequence_Record; Item : XML_Any;
      Min_Occurs : Integer := 1; Max_Occurs : Integer := 1);
   procedure Add_Particle
     (Seq : access Sequence_Record; Item : XML_Group;
      Min_Occurs : Natural := 1; Max_Occurs : Integer := 1);

   function Create_Choice (G : XML_Grammar_NS) return Choice;
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
     (C : access Choice_Record; Item : XML_Any;
      Min_Occurs : Integer := 1; Max_Occurs : Integer := 1);
   procedure Add_Particle
     (C : access Choice_Record; Item : XML_Group;
      Min_Occurs : Natural := 1; Max_Occurs : Integer := 1);

   -------------
   -- XML_All --
   -------------

   type XML_All_Record is new Group_Model_Record with private;
   type XML_All is access all XML_All_Record'Class;

   function Create_All
     (G          : XML_Grammar_NS;
      Min_Occurs : Natural := 1;
      Max_Occurs : Integer := 1) return XML_All;
   --  Return a new validator that checks that all its elements appear the
   --  right number of time

   subtype Zero_Or_One is Integer range 0 .. 1;
   procedure Add_Particle
     (Validator : access XML_All_Record; Item : XML_Element;
      Min_Occurs : Zero_Or_One := 1; Max_Occurs : Zero_Or_One := 1);
   --  Add a new element to Validator

   --------------
   -- Grammars --
   --------------

   procedure Get_NS
     (Grammar       : in out XML_Grammar;
      Namespace_URI : Unicode.CES.Byte_Sequence;
      Result        : out XML_Grammar_NS;
      Create_If_Needed : Boolean := True);
   --  Return the part of the grammar specialized for a given namespace.
   --  If no such namespace exists yet in the grammar, it is created.

   procedure Set_Target_NS (Grammar : in out XML_Grammar; NS : XML_Grammar_NS);
   function Get_Target_NS (Grammar : XML_Grammar) return XML_Grammar_NS;
   --  Set the target namespace for the grammar. This is the "targetNamespace"
   --  attribute of the <schema> node.

   function Lookup_Element
     (Grammar       : XML_Grammar_NS;
      Local_Name    : Unicode.CES.Byte_Sequence;
      Create_If_Needed : Boolean := True) return XML_Element;
   function Lookup
     (Grammar       : XML_Grammar_NS;
      Local_Name    : Unicode.CES.Byte_Sequence;
      Create_If_Needed : Boolean := True) return XML_Type;
   function Lookup_Group
     (Grammar       : XML_Grammar_NS;
      Local_Name    : Unicode.CES.Byte_Sequence) return XML_Group;
   function Lookup_Attribute
     (Grammar          : XML_Grammar_NS;
      Local_Name       : Unicode.CES.Byte_Sequence;
      Create_If_Needed : Boolean := True) return Attribute_Validator;
   function Lookup_Attribute_Group
     (Grammar       : XML_Grammar_NS;
      Local_Name    : Unicode.CES.Byte_Sequence) return XML_Attribute_Group;
   --  Return the type validator to use for elements with name Local_Name.
   --  "null" is returned if there is no validator defined for these elements.
   --  The returned value must be freed by the user.
   --  If you are going to modify the returned value in any way (adding new
   --  restrictions,...), you must clone it, or you will modify the actual
   --  type stored in the grammar.
   --  If the element doesn't exist yet, a forward declaration is created for
   --  it, that must be overriden later on.

   function Create_Global_Type
     (Grammar    : XML_Grammar_NS;
      Local_Name : Unicode.CES.Byte_Sequence;
      Validator  : access XML_Validator_Record'Class) return XML_Type;
   function Create_Global_Element
     (Grammar    : XML_Grammar_NS;
      Local_Name : Unicode.CES.Byte_Sequence;
      Form       : Form_Type) return XML_Element;
   function Create_Global_Group
     (Grammar    : XML_Grammar_NS;
      Local_Name : Unicode.CES.Byte_Sequence) return XML_Group;
   function Create_Global_Attribute
     (NS             : XML_Grammar_NS;
      Local_Name     : Unicode.CES.Byte_Sequence;
      Attribute_Type : XML_Type;
      Attribute_Form : Form_Type                 := Qualified;
      Attribute_Use  : Attribute_Use_Type        := Optional;
      Fixed          : Unicode.CES.Byte_Sequence := "";
      Has_Fixed      : Boolean := False) return Attribute_Validator;
   function Create_Global_Attribute_Group
     (NS             : XML_Grammar_NS;
      Local_Name     : Unicode.CES.Byte_Sequence) return XML_Attribute_Group;
   --  Register a new type or element in the grammar.

   procedure Create_Global_Type
     (Grammar    : XML_Grammar_NS;
      Local_Name : Unicode.CES.Byte_Sequence;
      Validator  : access XML_Validator_Record'Class);
   procedure Create_Global_Attribute
     (NS             : XML_Grammar_NS;
      Local_Name     : Unicode.CES.Byte_Sequence;
      Attribute_Type : XML_Type);
   --  Same as above, but doesn't return the newly created type. Use Lookup if
   --  you need access to it later on

   function Redefine_Type
     (Grammar    : XML_Grammar_NS;
      Local_Name : Unicode.CES.Byte_Sequence) return XML_Type;
   function Redefine_Group
     (Grammar    : XML_Grammar_NS;
      Local_Name : Unicode.CES.Byte_Sequence) return XML_Group;
   --  Indicate that a given type or element is being redefined inside a
   --  <redefine> tag. The old definition is returned, and all types that
   --  were referencing it will now refer to a new, invalid type. You need to
   --  register the new type or element before using the grammar.

   procedure Set_Block_Default
     (Grammar : XML_Grammar_NS;
      On_Restriction : Boolean;
      On_Extension   : Boolean);
   --  Set the default value for the "block" attribute

   procedure Compute_Blocks
     (Value         : Unicode.CES.Byte_Sequence;
      Restrictions  : out Boolean;
      Extensions    : out Boolean;
      Substitutions : out Boolean);
   --  Compute the list of blocked elements from value. Value is a list similar
   --  to what is used for the "block" attribute of elements in a schema

   procedure Compute_Final
     (Value         : Unicode.CES.Byte_Sequence;
      Restrictions  : out Boolean;
      Extensions    : out Boolean);
   --  Compute the list of final attributes from value. Value is a list similar
   --  to what is used for the "final" attribute of elements in a schema

   procedure Initialize (Grammar : in out XML_Grammar);
   --  Initialize the internal structure of the grammar.
   --  This adds the definition for all predefined types

   procedure Global_Check (Grammar : XML_Grammar);
   --  Perform checks on the grammar, once it has been fully declared. This
   --  must be called before you start using the grammar (see
   --  Schema.Schema_Readers.Set_Created_Grammar), since some validation checks
   --  can only be performed at the end, not while the grammar is being
   --  constructed.

   function Get_Namespace_URI
     (Grammar : XML_Grammar_NS) return Unicode.CES.Byte_Sequence;
   --  Return the namespace URI associated with Grammar

   function URI_Was_Parsed
     (Grammar : XML_Grammar;
      URI     : Unicode.CES.Byte_Sequence) return Boolean;
   --  Return True if the schema at URI was already parsed and included in
   --  Grammar. URI must be an absolute URI.

   procedure Set_Parsed_URI
     (Grammar : in out XML_Grammar; URI : Unicode.CES.Byte_Sequence);
   --  Indicate that the schema found at URI was fully parsed and integrated
   --  into Grammar. It can then be tested through URI_Was_Parsed.

   procedure Debug_Dump (Grammar : XML_Grammar);
   --  Dump the grammar to stdout. This is for debug only

   procedure Set_Debug_Name
     (Typ : access XML_Validator_Record'Class; Name : String);
   --  Will be removed

   procedure Set_Debug_Output (Output : Boolean);
   --  Whether we should output debug traces

   procedure Validation_Error (Message : String);
   --  Raise Validation_Error with a proper error message.

   function To_QName (Namespace_URI, Local_Name : String) return String;
   --  Return the name as it should be displayed in error messages

private

   procedure Debug_Push_Prefix (Append : String);
   procedure Debug_Pop_Prefix;
   --  Append a prefix to the current output

   ---------
   -- Ids --
   ---------

   type Id_Ref is record
      Key : Unicode.CES.Byte_Sequence_Access;
   end record;
   No_Id : constant Id_Ref := (Key => null);

   procedure Free (Id : in out Id_Ref);
   function Get_Key (Id : Id_Ref) return Unicode.CES.Byte_Sequence;
   package Id_Htable is new Sax.HTable
     (Element       => Id_Ref,
      Empty_Element => No_Id,
      Free          => Free,
      Key           => Unicode.CES.Byte_Sequence,
      Get_Key       => Get_Key,
      Hash          => Sax.Utils.Hash,
      Equal         => "=");
   type Id_Htable_Access is access Id_Htable.HTable;
   --  This table is used to store the list of IDs that have been used in the
   --  document so far, and prevent their duplication in the document.

   --------------------
   -- Validator_Data --
   --------------------

   type Validator_Data_Record is abstract tagged null record;

   --------------
   -- XML_Type --
   --------------

   type Content_Type is (Simple_Content, Complex_Content, Unknown_Content);

   type XML_Type_Record is record
      Local_Name : Unicode.CES.Byte_Sequence_Access;
      Validator  : XML_Validator;
      Simple_Type : Content_Type;

      Block_Restriction : Boolean;
      Block_Extension   : Boolean;
      --  The value for the "block" attribute of the type

      Final_Restriction : Boolean;
      Final_Extension   : Boolean;
      --  Whether this element is final for "restriction" or "extension" or
      --  both

      Next : XML_Type;
      --  Next type in the list of allocated types for this grammar.
      --  This list is used for proper memory management, and ensures that all
      --  types are properly freed on exit.
   end record;
   type XML_Type is access all XML_Type_Record;
   No_Type : constant XML_Type := null;

   procedure Register
     (NS  : XML_Grammar_NS;
      Typ : access XML_Type_Record);
   --  Registers a newly allocated type into NS, so that when the latter
   --  is destroyed, the type is properly deallocated.
   --  This does nothing if Typ was already registered.

   ------------------
   -- Element_List --
   ------------------

   type XML_Element_Record;
   type XML_Element_Access is access all XML_Element_Record;

   type Element_List is array (Natural range <>) of XML_Element_Access;
   type Element_List_Access is access Element_List;

   procedure Append
     (List    : in out Element_List_Access; Element : XML_Element);

   -----------------
   -- XML_Element --
   -----------------

   type XML_Element_Record is record
      Local_Name : Unicode.CES.Byte_Sequence_Access;
      NS         : XML_Grammar_NS;
      Of_Type    : XML_Type;
      Substitution_Group : XML_Element := No_Element;

      Default    : Unicode.CES.Byte_Sequence_Access;
      Fixed      : Unicode.CES.Byte_Sequence_Access;

      Is_Abstract : Boolean;
      --  Whether the corresponding type is abstract

      Nillable    : Boolean;
      --  Whether the element is nillable

      Final_Restriction : Boolean;
      Final_Extension   : Boolean;
      --  Whether this element is final for "restriction" or "extension" or
      --  both

      Block_Restriction  : Boolean;
      Block_Extension    : Boolean;
      Block_Substitution : Boolean;
      --  The value for the "block" attribute of the element

      Form : Form_Type;
      --  The value of the "form" attribute of the element

      Is_Global : Boolean;
      --  Whether the element was declared at the toplevel of the <schema>

      Next : XML_Element_Access;
      --  Points to the next element defined in NS, for memory management
      --  purposes
   end record;

   type XML_Element is record
      Elem   : XML_Element_Access;
      Is_Ref : Boolean;
      --  Whether this element was defined through a Lookup_Element, or a
      --  Create_Element call.
   end record;
   No_Element : constant XML_Element := (null, False);

   procedure Register
     (NS      : XML_Grammar_NS;
      Element : access XML_Element_Record);
   --  Registers a newly allocated element into NS, so that when the latter
   --  is destroyed, the element is properly deallocated.
   --  This does nothing if Element was already registered.

   --------------
   -- Grammars --
   --------------

   type Grammar_NS_Array is array (Natural range <>) of XML_Grammar_NS;
   type Grammar_NS_Array_Access is access all Grammar_NS_Array;

   type String_List_Record;
   type String_List is access String_List_Record;
   type String_List_Record is record
      Str  : Unicode.CES.Byte_Sequence_Access;
      Next : String_List;
   end record;
   --  We will use Ada2005 containers when the compiler is more widely
   --  available

   procedure Free (List : in out String_List);
   --  Free the list and its contents

   type Process_Contents_Array is array (Process_Contents_Type) of XML_Element;

   type XML_Grammar_Record is new Sax.Pointers.Root_Encapsulated with record
      Grammars : Grammar_NS_Array_Access;
      --  All the namespaces known for that grammar

      Parsed_Locations : String_List;
      --  List of schema locations that have already been parsed. This is used
      --  in particular to handle cases where a schema imports two others
      --  schemas, that in turn import a common one.

      UR_Type_Elements  : Process_Contents_Array := (others => No_Element);

      XSD_Version : XSD_Versions := XSD_1_0;

      Target_NS : XML_Grammar_NS;
   end record;

   procedure Free (Grammar : in out XML_Grammar_Record);
   --  Free the memory occupied by the grammar

   package XML_Grammars is new Sax.Pointers.Smart_Pointers
     (XML_Grammar_Record);
   type XML_Grammar is new XML_Grammars.Pointer;
   No_Grammar : constant XML_Grammar :=
     XML_Grammar (XML_Grammars.Null_Pointer);

   -------------------------
   -- Attribute_Validator --
   -------------------------

   type Attribute_Validator_Record is abstract tagged record
      NS : XML_Grammar_NS;

      Next : Attribute_Validator;
      --  Points to the next attribute validator in NS, for memory management
      --  purposes
   end record;

   function Is_ID (Attr : Attribute_Validator_Record) return Boolean;
   --  Whether the attribute is an ID

   type Attribute_Or_Group (Is_Group : Boolean := False) is record
      case Is_Group is
         when True  => Group : XML_Attribute_Group;
         when False =>
            Attr     : Attribute_Validator;
            Is_Local : Boolean;
            --  Whether the attribute is local or a ref to a global attribute
      end case;
   end record;
   type Attribute_Validator_List
     is array (Natural range <>) of Attribute_Or_Group;
   type Attribute_Validator_List_Access is access Attribute_Validator_List;

   type Named_Attribute_Validator_Record;
   type Named_Attribute_Validator is access all
     Named_Attribute_Validator_Record'Class;

   type Named_Attribute_Validator_Record is new Attribute_Validator_Record with
      record
         Local_Name     : Unicode.CES.Byte_Sequence_Access;
         Ref_Attr       : Named_Attribute_Validator;
         Attribute_Type : XML_Type;   --  or read from Ref_Attr the first time
         Attribute_Form : Form_Type;
         Attribute_Use  : Attribute_Use_Type;
         Fixed          : Unicode.CES.Byte_Sequence_Access; -- or from Ref_Attr
         Value          : Unicode.CES.Byte_Sequence_Access;
      end record;
   procedure Validate_Attribute
     (Validator : Named_Attribute_Validator_Record;
      Atts      : in out Sax.Attributes.Attributes'Class;
      Index     : Natural;
      Context   : in out Validation_Context);
   procedure Free (Validator : in out Named_Attribute_Validator_Record);
   function Is_Equal
     (Attribute : Named_Attribute_Validator_Record;
      Attr2     : Attribute_Validator_Record'Class)
     return Boolean;
   procedure Set_Type
     (Attr      : access Named_Attribute_Validator_Record;
      Attr_Type : XML_Type);
   function Get_Type
     (Attr : Named_Attribute_Validator_Record) return XML_Type;
   function Is_ID (Attr : Named_Attribute_Validator_Record) return Boolean;

   type Any_Attribute_Validator is new Attribute_Validator_Record with record
      Process_Contents : Process_Contents_Type;
      Kind             : Namespace_Kind;
   end record;
   procedure Validate_Attribute
     (Validator : Any_Attribute_Validator;
      Atts      : in out Sax.Attributes.Attributes'Class;
      Index     : Natural;
      Context   : in out Validation_Context);
   procedure Free (Validator : in out Any_Attribute_Validator);
   function Is_Equal
     (Attribute : Any_Attribute_Validator;
      Attr2     : Attribute_Validator_Record'Class)
     return Boolean;

   procedure Get_Attribute_Lists
     (Validator   : access XML_Validator_Record;
      List        : out Attribute_Validator_List_Access;
      Dependency1 : out XML_Validator;
      Ignore_Wildcard_In_Dep1 : out Boolean;
      Dependency2 : out XML_Validator);
   --  Return the list of attributes that need to be checked for this
   --  validator, and a set of other validators whose list of attributes also
   --  need to be checked (they in turn will be called through this subprogram
   --  to get their own lists of attributes).
   --  There is generally a single list, although restrictions and
   --  extensions will have two or more.
   --  The lists are checked in turn, and any attribute already encountered
   --  will not be checked again.

   procedure Register
     (NS   : XML_Grammar_NS;
      Attr : access Attribute_Validator_Record'Class);
   --  Registers a newly allocated type into NS, so that when the latter
   --  is destroyed, the type is properly deallocated.
   --  This does nothing if Typ was already registered.

   ---------------------
   -- Attribute_Group --
   ---------------------

   type XML_Attribute_Group_Record is record
      Local_Name : Unicode.CES.Byte_Sequence_Access;
      Attributes : Attribute_Validator_List_Access;
      Is_Forward_Decl : Boolean;
   end record;
   type XML_Attribute_Group is access XML_Attribute_Group_Record;
   Empty_Attribute_Group : constant XML_Attribute_Group := null;

   -------------------
   -- XML_Validator --
   -------------------

   type XML_Validator_Record is tagged record
      Debug_Name : Unicode.CES.Byte_Sequence_Access;
      --  Temporary, will be removed

      Attributes : Attribute_Validator_List_Access;
      --  The list of valid attributes registered for this validator.
      --  ??? Could be implemented more efficiently through a htable

      Mixed_Content : Boolean := False;
      --  Whether character data is allowed in addition to children nodes

      Next : XML_Validator;
      --  Next validator in the list of allocated validators for this grammar.
      --  This list is used for proper memory management, and ensures that all
      --  validators are properly freed on exit.
      --  This is not null when validator was already registered in one of the
      --  grammars, and should not be registered again
   end record;

   procedure Register
     (NS        : XML_Grammar_NS;
      Validator : access XML_Validator_Record'Class);
   --  Registers a newly allocated validator into NS, so that when the latter
   --  is destroyed, the validator is properly deallocated.
   --  This does nothing if Validator was already registered.

   ---------------
   -- XML_Group --
   ---------------

   type XML_Group_Record;
   type XML_Group is access all XML_Group_Record;
   No_XML_Group : constant XML_Group := null;

   ---------------
   -- Particles --
   ---------------

   type Particle_Type is (Particle_Element,
                          Particle_Nested,
                          Particle_Group,
                          Particle_Any,
                          Particle_XML_Type);
   type XML_Particle;
   type XML_Particle_Access is access XML_Particle;
   type XML_Particle (Typ : Particle_Type) is record
      Min_Occurs : Natural;
      Max_Occurs : Integer;
      Next       : XML_Particle_Access;
      case Typ is
         when Particle_Element  => Element    : XML_Element;
         when Particle_Nested   => Validator  : Group_Model;
         when Particle_Group    => Group      : XML_Group;
         when Particle_XML_Type => Type_Descr : XML_Type;
         when Particle_Any      => Any        : XML_Any;
      end case;
   end record;

   type Particle_List_Record is record
      First, Last : XML_Particle_Access;
   end record;
   type Particle_List is access Particle_List_Record;

   function Empty_Particle_List return Particle_List;
   --  Return an empty list

   procedure Append (List : Particle_List; Item : XML_Particle);
   --  Append a new element to the list

   procedure Free (List : in out Particle_List);
   --  Free the list

   -----------------------
   -- Particle_Iterator --
   -----------------------
   --  This iterator iterates over a list of particles, but consider a group
   --  as a set of particles that are also iterated

   type Particle_Iterator_Record;
   type Particle_Iterator is access Particle_Iterator_Record;
   type Particle_Iterator_Record is record
      Current : XML_Particle_Access;
      Parent  : Particle_Iterator;
   end record;
   No_Iter : constant Particle_Iterator := null;

   function  Start (List : Particle_List) return Particle_Iterator;
   procedure Next  (Iter : in out Particle_Iterator);
   function  Get   (Iter : Particle_Iterator) return XML_Particle_Access;
   function  Get_Min_Occurs (Iter : Particle_Iterator) return Natural;
   function  Get_Max_Occurs (Iter : Particle_Iterator) return Integer;
   procedure Free (Iter : in out Particle_Iterator);
   --  Iterate over a list of particles. Get returns null at the end of the
   --  iteration

   ----------------------
   -- XML_Group_Record --
   ----------------------

   type XML_Group_Record is record
      Local_Name : Unicode.CES.Byte_Sequence_Access;
      Particles  : Particle_List;
      Is_Forward_Decl : Boolean;
      --  Set to true if the group was defined as a call to Lookup, but never
      --  through Create_Global_Group
   end record;

   -------------
   -- Grammar --
   -------------

   function Get_Key (Typ : XML_Type) return Unicode.CES.Byte_Sequence;
   procedure Do_Nothing (Typ : in out XML_Type);

   package Types_Htable is new Sax.HTable
     (Element       => XML_Type,
      Empty_Element => No_Type,
      Free          => Do_Nothing,
      Key           => Unicode.CES.Byte_Sequence,
      Get_Key       => Get_Key,
      Hash          => Sax.Utils.Hash,
      Equal         => "=");
   type Types_Htable_Access is access Types_Htable.HTable;
   --  We store a pointer to an XML_Type_Record, since the validator might not
   --  be known when we first reference the type (it is valid in an XML schema
   --  to ref to a type described later on).
   --  Memory management is taken care of through a list of XML_Type stored in
   --  the grammar_ns itself, so the hash table should never free a pointer
   --  from this table

   function Get_Key
     (Element : XML_Element_Access) return Unicode.CES.Byte_Sequence;
   procedure Do_Nothing (Element : in out XML_Element_Access);

   package Elements_Htable is new Sax.HTable
     (Element       => XML_Element_Access,
      Empty_Element => null,
      Free          => Do_Nothing,
      Key           => Unicode.CES.Byte_Sequence,
      Get_Key       => Get_Key,
      Hash          => Sax.Utils.Hash,
      Equal         => "=");
   type Elements_Htable_Access is access Elements_Htable.HTable;

   procedure Free (Group : in out XML_Group);
   function Get_Key (Group : XML_Group) return Unicode.CES.Byte_Sequence;

   package Groups_Htable is new Sax.HTable
     (Element       => XML_Group,
      Empty_Element => No_XML_Group,
      Free          => Free,
      Key           => Unicode.CES.Byte_Sequence,
      Get_Key       => Get_Key,
      Hash          => Sax.Utils.Hash,
      Equal         => "=");
   type Groups_Htable_Access is access Groups_Htable.HTable;

   function Get_Key
     (Att : Named_Attribute_Validator) return Unicode.CES.Byte_Sequence;
   procedure Do_Nothing (Att : in out Named_Attribute_Validator);

   package Attributes_Htable is new Sax.HTable
     (Element       => Named_Attribute_Validator,
      Empty_Element => null,
      Free          => Do_Nothing,
      Key           => Unicode.CES.Byte_Sequence,
      Get_Key       => Get_Key,
      Hash          => Sax.Utils.Hash,
      Equal         => "=");
   type Attributes_Htable_Access is access Attributes_Htable.HTable;

   function Get_Key
     (Att : XML_Attribute_Group) return Unicode.CES.Byte_Sequence;
   procedure Free (Att : in out XML_Attribute_Group);

   package Attribute_Groups_Htable is new Sax.HTable
     (Element       => XML_Attribute_Group,
      Empty_Element => Empty_Attribute_Group,
      Free          => Free,
      Key           => Unicode.CES.Byte_Sequence,
      Get_Key       => Get_Key,
      Hash          => Sax.Utils.Hash,
      Equal         => "=");
   type Attribute_Groups_Htable_Access
     is access Attribute_Groups_Htable.HTable;

   type XML_Grammar_NS_Record is record
      Namespace_URI     : Unicode.CES.Byte_Sequence_Access;
      System_ID         : Unicode.CES.Byte_Sequence_Access;
      Types             : Types_Htable_Access;
      Elements          : Elements_Htable_Access;
      Groups            : Groups_Htable_Access;
      Attributes        : Attributes_Htable_Access;
      Attribute_Groups  : Attribute_Groups_Htable_Access;

      Validators_For_Mem : XML_Validator;
      --  List of validators allocated in the context of this grammar, and
      --  that should be freed when the grammar is destroyed. Warning: if an
      --  XML_Grammar_NS_Record is freed before others that depend on it, the
      --  latter might reference already freed memory

      Types_For_Mem  : XML_Type;
      --  List of types allocated in the context of this grammar. This is only
      --  used for memory management.

      Atts_For_Mem   : Attribute_Validator;
      --  List of attribute validators in the context of this grammar, for
      --  memory management purposes.

      Elems_For_Mem  : XML_Element_Access;
      --  List of elements defined in this grammar, for memory management
      --  purposes

      Block_Extension    : Boolean := False;
      Block_Restriction  : Boolean := False;
      Block_Substitution : Boolean := False;
   end record;

   procedure Free (Grammar : in out XML_Grammar_NS);
   --  Free the memory occupied by Grammar

   ------------------------
   -- Group_Model_Record --
   ------------------------

   type Group_Model_Record is abstract new XML_Validator_Record with record
      Particles  : Particle_List := Empty_Particle_List;
   end record;
   type Group_Model_Data_Record is new Validator_Data_Record with record
      Nested : Group_Model := null;
      --  If a group_model is nested inside another (a sequence within a
      --  a sequence for instance), then Nested will point to the nested
      --  group_model while it is being processed.

      Nested_Data : Validator_Data := null;
      --  The data used to evaluate Nested
   end record;
   type Group_Model_Data is access all Group_Model_Data_Record'Class;

   procedure Free (Data : in out Group_Model_Data_Record);
   --  See inherited documentation

   procedure Free_Nested_Group (Data : Group_Model_Data);
   --  Free the nested group and its data, if any

   procedure Applies_To_Tag
     (Group         : access Group_Model_Record;
      Local_Name    : Unicode.CES.Byte_Sequence;
      Namespace_URI : Unicode.CES.Byte_Sequence;
      NS            : XML_Grammar_NS;
      Grammar       : XML_Grammar;
      Applies       : out Boolean;
      Skip_Current  : out Boolean);
   --  Whether Group can process Local_Name. This is used for group_models
   --  nested in a choice, so that we can find out which one should be applied
   --  (given the restrictions in schema, only one of them can apply).
   --  If Group should be called to validate Local_Name, Applies is set to
   --  True.
   --  If Local_Name is not handled by Group, Applies is set to False.
   --  However, if Skip_Current is then set to true, then the group
   --  matches in fact an empty item, and Local_Name should be passed over to
   --  the successor of Group in its parent sequence or choice.

   function Can_Be_Empty
     (Group : access Group_Model_Record) return Boolean;
   --  Whether having no child is acceptable for Group

   function Type_Model
     (Validator  : access Group_Model_Record;
      First_Only : Boolean) return Unicode.CES.Byte_Sequence;
   --  Return the type model described by Validator.
   --  If First_Only is true, then only the first element(s) expected should
   --  be returned. This is mostly to deal with with sequences.

   procedure Free (Data : in out Group_Model_Record);
   --  See inherited documentation

   --------------------
   -- XML_Any_Record --
   --------------------

   type XML_Any_Record is new XML_Validator_Record with record
      Process_Contents : Process_Contents_Type;
      Namespace        : Unicode.CES.Byte_Sequence_Access;
      Target_NS        : XML_Grammar_NS;
   end record;

   procedure Validate_Start_Element
     (Validator              : access XML_Any_Record;
      Local_Name             : Unicode.CES.Byte_Sequence;
      Namespace_URI          : Unicode.CES.Byte_Sequence;
      NS                     : XML_Grammar_NS;
      Data                   : Validator_Data;
      Grammar                : XML_Grammar;
      Element_Validator      : out XML_Element);
   function Get_Namespace_From_Parent_For_Locals
     (Validator : access XML_Any_Record) return Boolean;
   procedure Free (Any : in out XML_Any_Record);
   --  See inherited documentation

   ---------------------
   -- Sequence_Record --
   ---------------------

   type Sequence_Record is new Group_Model_Record with null record;
   type Sequence_Data is new Group_Model_Data_Record with record
      Previous     : XML_Particle_Access;
      --  The previous particle we were matching

      Current      : Particle_Iterator := No_Iter;
      Num_Occurs_Of_Current : Integer := 0;
      --  Number of repeats for the current particle of the sequence. This is
      --  set to Unbounded to force transition to the next element in the
      --  sequence.
   end record;
   type Sequence_Data_Access is access all Sequence_Data'Class;

   procedure Free (Data : in out Sequence_Data);
   --  See inherited documentation

   procedure Validate_Start_Element
     (Validator         : access Sequence_Record;
      Local_Name             : Unicode.CES.Byte_Sequence;
      Namespace_URI          : Unicode.CES.Byte_Sequence;
      NS                     : XML_Grammar_NS;
      Data                   : Validator_Data;
      Grammar                : XML_Grammar;
      Element_Validator      : out XML_Element);
   procedure Validate_End_Element
     (Validator         : access Sequence_Record;
      Local_Name        : Unicode.CES.Byte_Sequence;
      Data              : Validator_Data);
   function Create_Validator_Data
     (Validator : access Sequence_Record) return Validator_Data;
   procedure Applies_To_Tag
     (Group        : access Sequence_Record;
      Local_Name   : Unicode.CES.Byte_Sequence;
      Namespace_URI : Unicode.CES.Byte_Sequence;
      NS            : XML_Grammar_NS;
      Grammar       : XML_Grammar;
      Applies      : out Boolean;
      Skip_Current : out Boolean);
   function Can_Be_Empty (Group : access Sequence_Record) return Boolean;
   function Type_Model
     (Validator  : access Sequence_Record;
      First_Only : Boolean) return Unicode.CES.Byte_Sequence;
   --  See doc for inherited subprograms

   -------------------
   -- Choice_Record --
   -------------------

   type Choice_Record is new Group_Model_Record with null record;
   type Choice_Data is new Group_Model_Data_Record with record
      Current               : Particle_Iterator := No_Iter;
      Num_Occurs_Of_Current : Natural;
   end record;
   type Choice_Data_Access is access all Choice_Data'Class;

   procedure Free (Data : in out Choice_Data);
   --  See inherited documentation

   procedure Validate_Start_Element
     (Validator              : access Choice_Record;
      Local_Name             : Unicode.CES.Byte_Sequence;
      Namespace_URI          : Unicode.CES.Byte_Sequence;
      NS                     : XML_Grammar_NS;
      Data                   : Validator_Data;
      Grammar                : XML_Grammar;
      Element_Validator      : out XML_Element);
   procedure Validate_End_Element
     (Validator         : access Choice_Record;
      Local_Name        : Unicode.CES.Byte_Sequence;
      Data              : Validator_Data);
   function Create_Validator_Data
     (Validator : access Choice_Record) return Validator_Data;
   procedure Applies_To_Tag
     (Group         : access Choice_Record;
      Local_Name    : Unicode.CES.Byte_Sequence;
      Namespace_URI : Unicode.CES.Byte_Sequence;
      NS            : XML_Grammar_NS;
      Grammar       : XML_Grammar;
      Applies       : out Boolean;
      Skip_Current  : out Boolean);
   function Can_Be_Empty (Group : access Choice_Record) return Boolean;
   function Type_Model
     (Validator  : access Choice_Record;
      First_Only : Boolean) return Unicode.CES.Byte_Sequence;
   --  See doc for inherited subprograms

   --------------------
   -- XML_All_Record --
   --------------------

   type Natural_Array is array (Natural range <>) of Natural;

   type XML_All_Record is new Choice_Record with record
      Min_Occurs : Natural;
      Max_Occurs : Integer;
   end record;
   type All_Data (Num_Elements : Integer) is new Group_Model_Data_Record with
      record
         All_Elements : Natural_Array (1 .. Num_Elements);
         Num_Occurs   : Natural;
      end record;
   type All_Data_Access is access all All_Data'Class;

   procedure Validate_Start_Element
     (Validator         : access XML_All_Record;
      Local_Name             : Unicode.CES.Byte_Sequence;
      Namespace_URI          : Unicode.CES.Byte_Sequence;
      NS                     : XML_Grammar_NS;
      Data                   : Validator_Data;
      Grammar                : XML_Grammar;
      Element_Validator      : out XML_Element);
   procedure Validate_End_Element
     (Validator         : access XML_All_Record;
      Local_Name        : Unicode.CES.Byte_Sequence;
      Data              : Validator_Data);
   function Create_Validator_Data
     (Validator : access XML_All_Record) return Validator_Data;
   function Type_Model
     (Validator  : access XML_All_Record;
      First_Only : Boolean) return Unicode.CES.Byte_Sequence;
   function Can_Be_Empty
     (Group : access XML_All_Record) return Boolean;
   --  See doc for inherited subprograms

   procedure Debug_Output (Str : String);
   pragma Inline (Debug_Output);
   --  Display a string for debugging purposes

   function Get_Name
     (Validator : access XML_Validator_Record'Class) return String;
   --  Return a string "(rule "name")" if the name of the validator is defined.
   --  This is for debug purposes only

   procedure Check_Id
     (Context   : in out Validation_Context;
      Validator : access XML_Validator_Record'Class;
      Value     : Unicode.CES.Byte_Sequence);
   --  Check whether Value is a unique ID in the document.
   --  If yes, store it in Id_Table to ensure its future uniqueness.
   --  This does nothing if Validator is not associated with an ID type.

   Debug : Boolean := False;
   --  Whether we are in debug mode

end Schema.Validators;
