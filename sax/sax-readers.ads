-----------------------------------------------------------------------
--                XML/Ada - An XML suite for Ada95                   --
--                                                                   --
--                       Copyright (C) 2001                          --
--                            ACT-Europe                             --
--                       Author: Emmanuel Briot                      --
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
-----------------------------------------------------------------------

with Sax.Attributes;
with Sax.Exceptions;
with Sax.Locators;
with Input_Sources;
with Char_Automaton;
with Unicode.CES;
with GNAT.Spitbol;

package Sax.Readers is

   type Reader is tagged limited private;
   type Reader_Access is access all Reader'Class;

   type Element_Model;
   type Element_Model_Ptr is access Element_Model;
   --  See below for a full declaration

   -------------------------
   -- Recognized features --
   -------------------------
   --  The two strings below reference the two default features that are
   --  recognized by all parsers.

   Namespace_Feature : constant String :=
     "http://www.xml.org/sax/features/namespace";
   --  Controls general namespace processing. If it is true (the default),
   --  namespace URIs will be used in events.
   --  In fact, this is only given for full compatibility with the SAX
   --  standard. As authorized in the standard, this parser will always
   --  report URIs to the Start_Element and End_Element callbacks.
   --
   --  Default is True.

   Namespace_Prefixes_Feature : constant String :=
     "http://www.xml.org/sax/features/namespace-prefixes";
   --  Controls the reporting of qNames and namespace attributes (xmlns*) to
   --  the application.
   --  When this is False (the default), qNames may optionaly be reported,
   --  and namespace attributes must not be reported.

   --  Summary of the above two features:
   --  1: Namespace names
   --  2: Start/endPrefixMapping
   --  3: qNames
   --  4: xmlns* attributes
   --  namespaces namespace-prefixes   1        2       3      4
   --     true          false         YES      YES   unknown   NO
   --     true          true          YES      YES     YES    YES
   --     false         false         (ILLEGAL COMBINATION)
   --     false         true         unknown unknown   YES    YES
   --
   --  Default is False.

   Validation_Feature : constant String :=
     "http://www.xml.org/sax/features/validation";
   --  This attribute is optional, and might be recognized by some parsers
   --  It is true if the associated SAX parser is validating (ie reads the
   --  external subset of the DTD).

   External_General_Entities_Feature : constant String :=
     "http://xml.org/sax/features/external-general-entities";
   --  If True, include all external general text entities.
   --  If False, these are not included, and will be reported with
   --  Content_Handlers.Skipped_Entity.
   --
   --  Default is True

   External_Parameter_Entities_Feature : constant String :=
     "http://xml.org/sax/features/external-parameter-entities";
   --  If True, include all external parameter entities, including the
   --  external DTD subset. Parameter entities are the ones defined in DTDs
   --  and whose name starts with '%'

   Parameter_Entities_Feature : constant String :=
     "http://xml.org/sax/features/lexical-handler/parameter-entities";
   --  True if the SAX parser will reports parameter entities through its
   --  Lexical_Handler.

   -------------------
   -- Error handler --
   -------------------
   --  The following functions are defined in the Error_Handler interface
   --  in the SAX standard.

   procedure Warning
     (Handler : in out Reader;
      Except : Sax.Exceptions.Sax_Parse_Exception'Class);
   --  Receive notification of a warning.
   --  This method is used to report conditions that are not errors or fatal
   --  errors.
   --  The SAX parser must continue to provide normal parsing events after
   --  invoking this method.
   --  Default action is to do nothing.

   procedure Error
     (Handler : in out Reader;
      Except  : Sax.Exceptions.Sax_Parse_Exception'Class);
   --  Receive notification of a recoverable error.
   --  For example, a validating parser would use this callback to report the
   --  violation of a validity constraint. The default behaviour is to take no
   --  Action.
   --  The SAX parser must continue to provide normal parsing events after
   --  invoking this method. If the application cannot do so, then the parser
   --  should report a fatal error.
   --  Default action is to do nothing.

   procedure Fatal_Error
     (Handler : in out Reader;
      Except  : Sax.Exceptions.Sax_Parse_Exception'Class);
   --  Receive notification of a non-recoverable error.
   --  For example, a parser would use this callback to report the violation
   --  of a well-Formedness constraint.
   --  The application must assume that the document is unusable after the
   --  parser has invoked this method.
   --  Default action is to raise an exception Fatal_Error;

   ----------------------
   -- Content Handlers --
   ----------------------
   --  The following functions are defined in the Content_Handler interface
   --  in the SAX standard.
   --  The default for all the subprograms below is to do nothing, unless
   --  otherwise specified.

   procedure Set_Document_Locator
     (Handler : in out Reader;
      Loc     : access Sax.Locators.Locator'Class);
   --  Receive an object for locating the origin of SAX document events.
   --  SAX parsers are strongly encouraged but not required to give this
   --  information. This callback will always be called before any other.

   procedure Start_Document (Handler : in out Reader);
   --  Receive notification of the beginning of a document.
   --  This callback is called only once by the parser, before any other
   --  function in this interface except Set_Document_Locator.

   procedure End_Document (Handler : in out Reader);
   --  Receive notification of the end of a document.
   --  This callback will be called only once once it has reached the end of
   --  the input stream. It won't be called if a Fatal_Error is raised, it is
   --  your responsability to call the callback yourself in this case.

   procedure Start_Prefix_Mapping
     (Handler : in out Reader;
      Prefix  : Unicode.CES.Byte_Sequence;
      URI     : Unicode.CES.Byte_Sequence);
   --  Begin the scope of a prefix-URI mapping.
   --  This callback is not necessarily for normal namespace processing, since
   --  the SAX parser will automatically substitute prefixes for elements and
   --  attributes if XML_Readers.Namespace_Feature is set to True.
   --  However, there are cases where the automatic replacement can not be
   --  safely done, and in this case this callback is invoked.
   --  It is not garanteed that calls to End_Prefix_Mapping will occur in the
   --  same order (or the reverse one) as Start_Prefix_Mapping.

   procedure End_Prefix_Mapping
     (Handler : in out Reader;
      Prefix  : Unicode.CES.Byte_Sequence);
   --  End the scope of a prefix-URI mapping.
   --  This will always occur after the corresponding End_Element event.

   procedure Start_Element
     (Handler       : in out Reader;
      Namespace_URI : Unicode.CES.Byte_Sequence := "";
      Local_Name    : Unicode.CES.Byte_Sequence := "";
      Qname         : Unicode.CES.Byte_Sequence := "";
      Atts          : Sax.Attributes.Attributes'Class);
   --  Receive notification of the beginning of an element.
   --  There will always be a matching call to End_Element, even for empty
   --  elements.
   --  Up to three name components can be given for each element, depending
   --  on the value of the XML_Reader features.
   --  - Namespace_URI and Local_Name are required when Namespace_Feature is
   --    True, but are optional if False. If one is specified, both must be.
   --  - Qname (qualified name) is required if Namespace_Prefixes_Feature is
   --    True, and optional if False. This is basically of the form "Ns:Name"
   --  The attribute list will only contain attributes with explicit values. It
   --  will contain attributes used for namespace declaration (xmlns*) only if
   --  Namespace_Prefixes_Feature is True.

   procedure End_Element
     (Handler : in out Reader;
      Namespace_URI : Unicode.CES.Byte_Sequence := "";
      Local_Name    : Unicode.CES.Byte_Sequence := "";
      Qname         : Unicode.CES.Byte_Sequence := "");
   --  Receive notification of the end of an element.

   procedure Characters
     (Handler : in out Reader;
      Ch      : Unicode.CES.Byte_Sequence);
   --  Receives notification of character data.
   --  XML parsers may return all contiguous character data in a single chunk,
   --  or they may split them into several chunks. However, all of the
   --  characters in any single event must come from the same external entity
   --  so that the Locator provides useful information
   --
   --  Note that some parsers will report (and validating parsers must) report
   --  whitespace in element content using the Ignorable_Whitespace event.

   procedure Ignorable_Whitespace
     (Handler : in out Reader;
      Ch      : Unicode.CES.Byte_Sequence);
   --  Receive notification of ignorable whitespace in element content (ie
   --  for elements whose xml:space attribute is not set to 'preserve', see
   --  XML specifications 2.10)
   --  Validating Parsers must use this method to report each chunk of
   --  whitespace in element content.
   --  SAX parsers may return all contiguous whitespace in a single chunk, or
   --  they may split it into several chunks.

   procedure Processing_Instruction
     (Handler : in out Reader;
      Target  : Unicode.CES.Byte_Sequence;
      Data    : Unicode.CES.Byte_Sequence);
   --  Receive notification of a processing instruction.
   --  A SAX parser must never report an XML declaration (<?xml..?>, 2.8 in
   --  XML specifications) or a text declaration (<?xml?>, 4.3.1 in XML
   --  specifications) using this method.

   procedure Skipped_Entity
     (Handler : in out Reader;
      Name    : Unicode.CES.Byte_Sequence);
   --  Receive notification of a skipped entity.
   --  The Parser will invoke this method once for each entity
   --  skipped. Non-validating processors may skip entities if they have not
   --  seen the declarations (because, for example, the entity was declared in
   --  an external DTD subset). All processors may skip external Entities,
   --  depending on the value of External_General_Entities_Feature and
   --  External_Parameter_Entities_Feature.
   --
   --  Name is the name of the skipped entity. If it is a parameter entity,
   --  the name will begin with '%', and if it is the external DTD subset,
   --  it will be the string "[dtd]".

   ------------------
   -- DTD Handlers --
   ------------------
   --  The following functions are defined in the DTD_Handler interface
   --  in the SAX standard.

   procedure Unparsed_Entity_Decl
     (Handler       : in out Reader;
      Name          : Unicode.CES.Byte_Sequence;
      System_Id     : Unicode.CES.Byte_Sequence;
      Notation_Name : Unicode.CES.Byte_Sequence);
   --  Receive notification of an unparsed entity declaration event.
   --  This is for entities like  "<!ENTITY foo SYSTEM ".." NDATA gif>"

   procedure Notation_Decl
     (Handler       : in out Reader;
      Name          : Unicode.CES.Byte_Sequence;
      Public_Id     : Unicode.CES.Byte_Sequence;
      System_Id     : Unicode.CES.Byte_Sequence);
   --  Receive notification of a notation declaration event.
   --  At least one of publicId and systemId must be non-null. If a system
   --  identifier is present, and it is a URL, the SAX parser must resolve it
   --  fully before passing it to the application through this event.
   --  There is no guarantee that the notation declaration will be reported
   --  before any unparsed entities that use it.

   ---------------------
   -- Entity Resolver --
   ---------------------
   --  The following functions are defined in the Entity_Resolver interface
   --  in the SAX standard.

   ---------------------
   -- Lexical Handler --
   ---------------------
   --  The following functions are defined in the Lexical_Handler interface
   --  in the extended SAX standard. This is not part of the standard itself,
   --  but rather part of the extension for it.
   --  Note that the SAX standard indicates that such extended handlers should
   --  be set through properties, but this is not necessary in this
   --  implementation where you simply have to override the following
   --  subprograms.

   procedure Comment
     (Handler : in out Reader; Ch : Unicode.CES.Byte_Sequence);
   --  Report an XML comment anywhere in the document.
   --  Default behavior is to do nothing.

   procedure Start_Cdata (Handler : in out Reader);
   --  Report the start of a CData section.
   --  The content of the section is reported through the usual Characters
   --  event, this only acts as the boundary.

   procedure End_Cdata (Handler : in out Reader);
   --  Report the end of a CData section

   procedure Start_Entity
     (Handler : in out Reader; Name : Unicode.CES.Byte_Sequence);
   --  Report the beginning of some internal and external XML entities.
   --  Check the feature Parameter_Entities_Feature to know if the handler
   --  will report these events.

   procedure End_Entity
     (Handler : in out Reader;
      Name    : Unicode.CES.Byte_Sequence);
   --  Report the end of an entity

   procedure Start_DTD
     (Handler   : in out Reader;
      Name      : Unicode.CES.Byte_Sequence;
      Public_Id : Unicode.CES.Byte_Sequence := "";
      System_Id : Unicode.CES.Byte_Sequence := "");
   --  Report the start of DTD declarations, if any.
   --  All events reported to a Decl_Handler are reported between a Start_DTD
   --  and an End_DTD event.
   --  Public_Id and System_Id might be the empty string if none was declared.
   --  The events following Start_DTD (and before the matching End_DTD) are
   --  assumed to be part of the internal subset of the DTD, unless they
   --  appear between a Start_Entity and End_Entity events (with "[dtd]" for
   --  the name).

   procedure End_DTD (Handler : in out Reader);
   --  Report the end of a DTD section

   ------------------
   -- Decl Handler --
   ------------------
   --  The following functions are defined in the Decl_Handler interface
   --  in the extended SAX standard. This is not part of the standard itself,
   --  but rather part of the extension for it.

   procedure Internal_Entity_Decl
     (Handler : in out Reader;
      Name    : Unicode.CES.Byte_Sequence;
      Value   : Unicode.CES.Byte_Sequence);
   --  Report an internal entity declaration.
   --  This is for <!ENTITY...> notations in the DTD, where the value is
   --  specified directly as a string.
   --  Only the effective (first) declaration for each entity will be reported.
   --  All parameter entities in the value will be expanded, but general
   --  entities will not.
   --  For Parameter entities, Name will start with '%'

   procedure External_Entity_Decl
     (Handler   : in out Reader;
      Name      : Unicode.CES.Byte_Sequence;
      Public_Id : Unicode.CES.Byte_Sequence;
      System_Id : Unicode.CES.Byte_Sequence);
   --  Report a parsed external entity declaration, ie when their value is
   --  not defined as a string.

   procedure Element_Decl
     (Handler : in out Reader;
      Name    : Unicode.CES.Byte_Sequence;
      Model   : Unicode.CES.Byte_Sequence;
      Parsed_Model : Element_Model_Ptr);
   --  Report an element type declaration.
   --  The content model will consist of the string "EMPTY", the string "ANY",
   --  or a parenthesised group, optionally followed by an occurrence
   --  indicator. The model will be normalized so that all parameter entities
   --  are fully resolved and all whitespace is removed,and will include the
   --  enclosing parentheses.
   --  In addition to the SAX standard, the parsed_model parameter is used to
   --  manipulate directly a pre-parsed formed of Model. You shouldn't keep
   --  a reference to the pointer, since the memory is freed as soon as your
   --  callback ends.

   procedure Attribute_Decl
     (Handler : in out Reader;
      Ename   : Unicode.CES.Byte_Sequence;
      Aname   : Unicode.CES.Byte_Sequence;
      Typ     : Unicode.CES.Byte_Sequence;
      Value_Default : Unicode.CES.Byte_Sequence;
      Value   : Unicode.CES.Byte_Sequence);
   --  Report an attribute type declaration.
   --  Only the first declaration for an attribute will be reported.
   --  Typ will be one of "CDATA", "ID", "IDREF", "IDREFS", "NMTOKEN",
   --  "NMTOKENS", "ENTITY", "ENTITIES", a parenthesized token group with
   --  the separator "|" and all whitespace removed, or the word "NOTATION"
   --  followed by a space followed by a parenthesized token group with all
   --  whitespace removed.
   --  Value_Default is a string representing the attribute default
   --  ("#IMPLIED", "#REQUIRED", or "#FIXED") or "" if none of these applies.
   --  Value is a string representing the attribute's default value, or ""
   --  if there is none

   -------------------------------
   -- Element models in the DTD --
   -------------------------------

   type Content_Spec is
     (Character_Data,   --  Characters, but no child node
      Element_Ref,      --  A specific child
      Any_Of,           --  child is one of many choices
      Sequence,         --  a sequence of elements (order is imposed)
      Repeat,           --  A repeated element
      Empty,            --  Element must be empty
      Anything          --  Content is not described, and can be anything
     );

   type Element_Model_Array is array (Natural range <>) of Element_Model_Ptr;
   type Element_Model_Array_Ptr is access Element_Model_Array;

   type Element_Model (Content : Content_Spec) is record
      case Content is
         when Character_Data | Empty | Anything => null;

         when Element_Ref =>
            Name : Unicode.CES.Byte_Sequence_Access; --  Name of the element

         when Any_Of | Sequence =>
            List : Element_Model_Array_Ptr; --  all the possible choices

         when Repeat =>
            Min : Natural;
            Max : Positive;
            Elem : Element_Model_Ptr;

      end case;
   end record;
   --  Type used to describe the model used for an element, as described in
   --  the DTD (see 3.2.* in XML specifications). For instance, the following
   --  model "(#PCDATA|emph)*" is translated to:
   --     (Content => Repeat,
   --      Min     => 0,
   --      Max     => Positive'Last,
   --      Elem    => (Content => Any_Of,
   --                  Choices => (0 => (Content => Character_Data),
   --                              1 => (Content => Element,
   --                                    Name    => "emp"))))

   function Parse_Element_Model (Model : Unicode.CES.Byte_Sequence)
      return Element_Model_Ptr;
   --  Parse the model, as defined in XML specifications (3.2)
   --  It is your responsability to free the model with Free.
   --  It raised Invalid_Content_Model if Model is invalid

   procedure Free (Model : in out Element_Model_Ptr);
   --  Free the memory allocated for the model.

   function To_String (Model : Element_Model) return Unicode.CES.Byte_Sequence;
   --  Return the string to put in an XML file to describe Model
   --  Invalid_Content_Model is raised if Model can not be described in a
   --  DTD.

   -------------------------
   -- Parsing subprograms --
   -------------------------

   procedure Parse
     (Read : in out Reader; Input : access Input_Sources.Input_Source'Class);
   --  Parse an XML stream, and calls the appropriate callbacks for each
   --  event.

   function Get_Feature (Read : Reader; Name : String) return Boolean;
   --  lookup the value of a feature
   --  Name is a fully qualified URI.
   --  All XML_Readers must recognize the two features Namespace_Feature
   --  and Namespace_Prefix_Feature

   procedure Set_Feature
     (Read : in out Reader; Name : String; Value : Boolean);
   --  Set the state of a feature

   Max_Buffer_Length : constant := 50000;
   --  Length of internal buffer.
   --  This is also the maximum length of tag names.

   XML_Fatal_Error : exception;

   No_Such_Entity : exception;
   --  Raised when an entity could not be found

   Invalid_Content_Model : exception;
   --  Raised by Parse_Element_Model, when the model is invalid

private

   type File_Locator is new Sax.Locators.Locator_Impl with record
      Char_Number : Natural := 1;
   end record;
   type File_Locator_Access is access all File_Locator'Class;

   type XML_NS_Record;
   type XML_NS is access XML_NS_Record;
   type XML_NS_Record is record
      Prefix : Unicode.CES.Byte_Sequence_Access;
      URI    : Unicode.CES.Byte_Sequence_Access;
      Next   : XML_NS;
   end record;

   type Space_Attribute_Enum is (Preserve, Ignorable);

   type Element;
   type Element_Access is access Element;
   type Element is record
      NS           : Unicode.CES.Byte_Sequence_Access;
      --  Prefix associated with the element. The resolution of this prefix
      --  into an URI is left to when the callback is emitted, so that any
      --  xmlns:* attribute in that element can be used.

      Name         : Unicode.CES.Byte_Sequence_Access;
      Parent       : Element_Access;
      Id           : Natural;

      Space_Handling : Space_Attribute_Enum := Ignorable;
      --  This value is set based on the xml:space attribute defined for this
      --  element in the DTD.

      Namespaces   : XML_NS;
      --  Namespaces defined for that element and its children
   end record;

   type Processing_Item is
     (Attr_Value, Comment, Entity, PI, Declaration, None);
   --  What type of entity we are currently processing (in some cases, an
   --  error in the document is in fact because an PI, Comment,... wasn't
   --  correctly finished, and we want to report the error at the beginning
   --  of that entity).

   type Entity_Def is record
      Str : Unicode.CES.Byte_Sequence_Access;
      External : Boolean;

      Already_Read : Boolean := False;
      --  True if the value of the entity was already read. This is used to
      --  avoid entities referencing themselves.
   end record;
   Null_Entity : constant Entity_Def := (null, False, False);
   function Entity_Img (A : Entity_Def) return String;
   function Entity_Equality (A, B : Entity_Def) return Boolean;

   package Entity_Table is new GNAT.Spitbol.Table
     (Value_Type => Entity_Def,
      Null_Value => Null_Entity,
      Img        => Entity_Img,
      "="        => Entity_Equality);

   type Attributes_Ptr is access all Sax.Attributes.Attributes'Class;
   function Attributes_Img (A : Attributes_Ptr) return String;
   package Attributes_Table is new GNAT.Spitbol.Table
     (Value_Type => Attributes_Ptr,
      Null_Value => null,
      Img        => Attributes_Img);

   type Reader is new Char_Automaton.Character_State_Machine with record
      Current_Node : Element_Access;
      Current_NS   : Unicode.CES.Byte_Sequence_Access;
      Attributes   : Sax.Attributes.Attributes_Impl;
      Entities     : Entity_Table.Table (50); --  ??? Hard-coded
      --  ??? Entities should be freed on destruction
      DTD          : Attributes_Table.Table (50); --  ??? Hard-coded
      --  ??? Should be freed

      Num_Items    : Natural := 0;
      --  Number of items at the top-level

      Default_Namespaces : XML_NS;
      --  All the namespaces defined by default

      Num_Elements : Natural := 0;
      --  Number of elements at the top-level (this only includes <...>
      --  elements, not characters).

      Entity_Start : Natural := 0;
      Declaration_Start : Natural := 0;
      --  Memorize the position in Buffer where the current entity started,
      --  so that we can substitue its value at the end.

      Processing_Attribute_Value : Boolean := False;
      --  Set to True if we are processing an attribute value.

      Locator : File_Locator_Access;

      Recursive_Depth : Natural := 1;
      --  Number of nested call to Main_Loop. Main_Loop is called when
      --  parsing the contents of external entities.

      Element_Id : Natural := 0;
      --  Id of the current element. All elements created will have a
      --  different Id

      --  The buffer to memorize the contents of the input file
      Buffer_Length : Natural := 0;
      Buffer        : Unicode.CES.Byte_Sequence (1 .. Max_Buffer_Length);
      --  Internally, all XML documents are represented as UTF8-encoded
      --  strings, so that we can easily compare between any two document.

      Processing : Processing_Item := None;
      --  What entity we are currently processing.

      Processing_Start : File_Locator_Access;
      --  The location of the entity's start. For attribute values, this
      --  is in fact the location of the possible end for the entity.

      Parsing_DTD : Integer := 0;
      --  Positive while we are parsing a <!DOCTYPE...> section.
      --  This is the number of open square brackets that haven't been closed.

      Feature_Namespace                   : Boolean := True;
      Feature_Namespace_Prefixes          : Boolean := False;
      Feature_External_General_Entities   : Boolean := True;
      Feature_External_Parameter_Entities : Boolean := True;
   end record;
end Sax.Readers;
