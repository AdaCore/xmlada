-----------------------------------------------------------------------
--                XML/Ada - An XML suite for Ada95                   --
--                                                                   --
--                       Copyright (C) 2001-2002                     --
--                            ACT-Europe                             --
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

with Input_Sources;
with Interfaces;
with Sax.Locators;
with Sax.Exceptions;
with Sax.Attributes;
with Sax.Models;
with Unicode;
with Unicode.CES;
with Sax.HTable;
pragma Elaborate_All (Sax.HTable);

package Sax.Readers is

   type Reader is tagged private;

   procedure Parse
     (Parser : in out Reader;
      Input  : in out Input_Sources.Input_Source'Class);
   --  Parse an XML stream, and calls the appropriate SAX callbacks for each
   --  event.
   --  This is not re-entrant: you can not call Parse with the same Parser
   --  argument in one of the SAX callbacks. This has undefined behavior.

   function Get_Feature (Parser : Reader; Name : String) return Boolean;
   --  lookup the value of a feature
   --  Name is a fully qualified URI.
   --  All XML_Readers must recognize the two features Namespace_Feature
   --  and Namespace_Prefix_Feature

   procedure Set_Feature
     (Parser : in out Reader; Name : String; Value : Boolean);
   --  Set the state of a feature

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
   --  If True (not the default), a number of additional tests are performed
   --  while parsing the document, most notably that the document matches
   --  the DTD (internal and external subset).

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
   --  parser has invoked this method. Thus, a Program_Error will be raised
   --  if your callback returns. You should always raise an exception.
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
   --  whitespace between elements using the Ignorable_Whitespace event.

   procedure Ignorable_Whitespace
     (Handler : in out Reader;
      Ch      : Unicode.CES.Byte_Sequence);
   --  Receive notification of ignorable whitespace in element content (ie
   --  for elements whose xml:space attribute is not set to 'preserve', see
   --  XML specifications 2.10)
   --  If there is only white spaces between two tags, they are reported via
   --  this callback.
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
      Model   : Sax.Models.Element_Model_Ptr);
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
      Typ     : Sax.Attributes.Attribute_Type;
      Content : Sax.Models.Element_Model_Ptr;
      Value_Default : Sax.Attributes.Default_Declaration;
      Value   : Unicode.CES.Byte_Sequence);
   --  Report an attribute type declaration.
   --  Only the first declaration for an attribute will be reported.
   --  If Typ is Notation or Enumeration, then Content will contain the
   --  description model for the attribute. Otherwise Content is null.
   --  Content might be freed when returning from this call, so you should make
   --  copies of it if needed.
   --  Value_Default represents the attribute default requirements
   --  ("#IMPLIED", "#REQUIRED", or "#FIXED").
   --  Value is a string representing the attribute's default value, or ""
   --  if there is none

   XML_Fatal_Error : exception;

private
   Entities_Table_Size : constant := 50;
   --  Size of the hash-table used to store entities.
   --  This is not a hard limit on the number of entities that can be defined.
   --  However, if this number is too small with regards to the number of
   --  entities, there will be conflicts in the hash-table that will slow
   --  down the lookup.

   Default_Atts_Table_Size : constant := 50;
   --  Size of the hash-table used to store the default attributes

   function Hash (Str : String) return Interfaces.Unsigned_32;
   --  Compute hash function for given String

   --------------
   -- Entities --
   --------------
   --  We need to memorize all the declared entities, so as to do the
   --  substitution ourselves.

   type Entity_Entry is record
      Name         : Unicode.CES.Byte_Sequence_Access;
      Value        : Unicode.CES.Byte_Sequence_Access;
      External     : Boolean;
      Already_Read : Boolean := False;
      --  True if the value of the entity was already read. This is used to
      --  detect entities referencing themselves.
   end record;
   type Entity_Entry_Access is access Entity_Entry;

   procedure Free (Entity : in out Entity_Entry_Access);
   function Get_Key (Entity : Entity_Entry_Access) return String;

   package Entity_Table is new Sax.HTable
     (Element       => Entity_Entry_Access,
      Empty_Element => null,
      Free          => Free,
      Key           => String,
      Get_Key       => Get_Key,
      Hash          => Hash,
      Equal         => Standard."=");

   type Entity_Input_Source;
   type Entity_Input_Source_Access is access Entity_Input_Source;
   type Entity_Input_Source is record
      External : Boolean;
      Next  : Entity_Input_Source_Access;
      Name  : Unicode.CES.Byte_Sequence_Access;
      --  Name of the entity

      Handle_Strings : Boolean := True;
      --  True if " and ' should be recognized as special characters.
      --  This is used so that a string started in one stream isn't terminated
      --  in another entity or stream.

      Id : Natural;
      --  Uniq ID for each input source

      Input    : Input_Sources.Input_Source_Access;
      Save_Loc : Sax.Locators.Locator_Impl;
   end record;

   type Parser_State is record
      Name : String (1 .. 3);
      --  Name of the state (debugging purposes)

      Ignore_Special : Boolean := False;
      --  True if special characters should be ignored (as is the case in
      --  strings).  ??? Could be ignored, duplicates Greater_Special,
      --  Less_Special, ..

      Detect_End_Of_PI : Boolean := False;
      --  Whether ?> should be reported as end of PI

      Greater_Special : Boolean := False;
      --  Whether > is considered a special character

      Less_Special : Boolean := False;
      --  Should be true if < should be reported separately. Note that in that
      --  case it won't even be associated with the following character if
      --  it is '!', '?',...

      Expand_Param_Entities : Boolean := False;
      --  True if %...; param entities should be recognized, as is the case in
      --  the DTD

      Expand_Entities : Boolean := True;
      --  True if &...; should be recognized

      Expand_Character_Ref : Boolean := True;
      --  True if character references &#...; should be recognized

      In_DTD : Boolean := False;
      --  True if we are parsing the DTD, and '['. ']' and '<!' should be
      --  recognized as special tags

      Recognize_External : Boolean := False;
      --  True if PUBLIC, SYSTEM and NDATA should be recognized as special
      --  tokens

      In_Attlist : Boolean := False;
      --  True if we are in an <!ATTLIST, and we should recognize special
      --  keywords like ID, NMTOKEN,...

      Handle_Strings : Boolean := False;
      --  True if " and ' should be recognized as special characters
      --  ??? Should be merged with a In_String field, that would also replace
      --  Ignore_Special.

      In_Tag : Boolean := False;
      --  True if = and : should be recognized as special characters

      Report_Parenthesis : Boolean := False;
      --  True if Opening_Parenthesis should be reported separately
   end record;

   type XML_NS_Record;
   type XML_NS is access XML_NS_Record;
   type XML_NS_Record is record
      Prefix : Unicode.CES.Byte_Sequence_Access;
      URI    : Unicode.CES.Byte_Sequence_Access;
      Next   : XML_NS;
   end record;

   type Element;
   type Element_Access is access Element;
   type Element is record
      NS             : Unicode.CES.Byte_Sequence_Access;
      Name           : Unicode.CES.Byte_Sequence_Access;
      Parent         : Element_Access;
      Start_Id       : Natural;
      --  Id of the Input source for the start tag. End tag must end on the
      --  same entity.
      Namespaces     : XML_NS;
      --  Namespaces defined for that element and its children
   end record;

   type Attributes_Ptr is access all Sax.Attributes.Attributes'Class;
   type Attributes_Entry is record
      Element_Name : Unicode.CES.Byte_Sequence_Access;
      Attributes   : Attributes_Ptr;
   end record;
   Null_Attribute : constant Attributes_Entry := (null, null);

   procedure Free (Att : in out Attributes_Entry);
   function Get_Key (Att : Attributes_Entry) return String;

   package Attributes_Table is new Sax.HTable
     (Element       => Attributes_Entry,
      Empty_Element => Null_Attribute,
      Free          => Free,
      Key           => String,
      Get_Key       => Get_Key,
      Hash          => Hash,
      Equal         => Standard."=");

   type Notation_Entry is record
      Name : Unicode.CES.Byte_Sequence_Access;
   end record;
   Null_Notation : constant Notation_Entry := (Name => null);

   procedure Free (Notation : in out Notation_Entry);
   function Get_Key (Notation : Notation_Entry) return String;

   package Notations_Table is new Sax.HTable
     (Element       => Notation_Entry,
      Empty_Element => Null_Notation,
      Free          => Free,
      Key           => String,
      Get_Key       => Get_Key,
      Hash          => Hash,
      Equal         => Standard."=");
   --  For notations, we simply store whether they have been defined or not,
   --  and then only for validating parsers

   type Reader is tagged record
      Buffer_Length : Natural := 0;
      Buffer        : Unicode.CES.Byte_Sequence_Access;
      Last_Read     : Unicode.Unicode_Char;
      State         : Parser_State;
      Locator       : Sax.Locators.Locator_Impl_Access;
      Current_Node  : Element_Access;

      Inputs        : Entity_Input_Source_Access;
      --  Entities and parameter entities are processed inline (if we
      --  temporarily substitute the input stream with the replacement text
      --  for the entity).
      --  When Inputs is null, the characters are read from the input stream
      --  given in the call to Parser.

      Close_Inputs  : Entity_Input_Source_Access;
      --  List of entities to be closed at the next call to Next_Token

      In_External_Entity : Boolean;
      --  Whether we are parsing an external entity

      Previous_Char_Was_CR : Boolean;
      --  True if the previous character read from the stream was a
      --  Carriage_Return (needed since XML parsers must convert these to
      --  one single Line_Feed).

      Default_Atts : Attributes_Table.HTable (Default_Atts_Table_Size);
      --  This table contains the list of default attributes defined for
      --  each element in the DTD. Index is the name of the elements.
      --  Note that the namespaces haven't been resolved for these default
      --  attributes, since in some cases the namespace itself could be defined
      --  as a default attribute.

      Notations : Notations_Table.HTable (Default_Atts_Table_Size);
      --  List of notations defined in the XML document. This is left empty
      --  if the parser isn't configured to do validation.

      Entities : Entity_Table.HTable (Entities_Table_Size);

      DTD_Name : Unicode.CES.Byte_Sequence_Access;
      --  Name of the DTD, and also name of the root element (in case we have
      --  a validating parser). This is left to null for non-validating
      --  parsers.

      Ignore_State_Special : Boolean;
      --  If True, ignore the State.Ignore_Special flag in the next call
      --  to Next_Token. This is used for handling of special characters
      --  withing strings.

      Default_Namespaces : XML_NS;
      --  All the namespaces defined by default

      Num_Toplevel_Elements : Natural;
      --  Number of elements at the toplevel

      Element_Id : Natural := 0;
      --  Id of the current element. All elements created will have a
      --  different Id

      Feature_Namespace                   : Boolean := True;
      Feature_Namespace_Prefixes          : Boolean := False;
      Feature_External_General_Entities   : Boolean := True;
      Feature_External_Parameter_Entities : Boolean := True;
      Feature_Validation                  : Boolean := False;
   end record;

end Sax.Readers;
