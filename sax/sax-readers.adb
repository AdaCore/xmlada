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

--  The parsing of the XML file is done through a finite-state machine, so
--  that we can create the XML document by reading only one character at a
--  time (useful when reading data from streams when one can not peek
--  forward at characters).
--  The algorithm in this package is inspired from thotlib.

with Ada.Text_IO;               use Ada.Text_IO;
with Ada.Exceptions;            use Ada.Exceptions;
with Unchecked_Conversion;
with Unchecked_Deallocation;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;

with Char_Automaton;            use Char_Automaton;
with Input_Sources;             use Input_Sources;
with Input_Sources.Strings;     use Input_Sources.Strings;
with Input_Sources.File;        use Input_Sources.File;

with Unicode;                   use Unicode;
with Unicode.CES;               use Unicode.CES;
with Unicode.CES.Basic_8bit;    use Unicode.CES.Basic_8bit;
with Unicode.Names.Basic_Latin; use Unicode.Names.Basic_Latin;

with Sax.Attributes;            use Sax.Attributes;
with Sax.Exceptions;            use Sax.Exceptions;
with Sax.Locators;              use Sax.Locators;

with Encodings;                 use Encodings;

package body Sax.Readers is

   use Char_Automaton.Character_Automaton;
   use Entity_Table;
   use Attributes_Table;

   Debug : constant Boolean := False;
   --  Set this to watch all the callbacks

   Top_State : constant Automaton_State := 0;

   Xml_Sequence : constant Byte_Sequence :=
     Encoding.Encode (Latin_Small_Letter_X)
     & Encoding.Encode (Latin_Small_Letter_M)
     & Encoding.Encode (Latin_Small_Letter_L);

   Xmlns_Sequence : constant Byte_Sequence :=
     Xml_Sequence
     & Encoding.Encode (Latin_Small_Letter_N)
     & Encoding.Encode (Latin_Small_Letter_S);

   Lt_Sequence : constant Byte_Sequence :=
     Encoding.Encode (Latin_Small_Letter_L)
     & Encoding.Encode (Latin_Small_Letter_T);

   Gt_Sequence : constant Byte_Sequence :=
     Encoding.Encode (Latin_Small_Letter_G)
     & Encoding.Encode (Latin_Small_Letter_T);

   Amp_Sequence : constant Byte_Sequence :=
     Encoding.Encode (Latin_Small_Letter_A)
     & Encoding.Encode (Latin_Small_Letter_M)
     & Encoding.Encode (Latin_Small_Letter_P);

   Apos_Sequence : constant Byte_Sequence :=
     Encoding.Encode (Latin_Small_Letter_A)
     & Encoding.Encode (Latin_Small_Letter_P)
     & Encoding.Encode (Latin_Small_Letter_O)
     & Encoding.Encode (Latin_Small_Letter_S);

   Quot_Sequence : constant Byte_Sequence :=
     Encoding.Encode (Latin_Small_Letter_Q)
     & Encoding.Encode (Latin_Small_Letter_U)
     & Encoding.Encode (Latin_Small_Letter_O)
     & Encoding.Encode (Latin_Small_Letter_T);

   Cdata_Attlist_Sequence : constant Byte_Sequence :=
     Encoding.Encode (Latin_Capital_Letter_C)
     & Encoding.Encode (Latin_Capital_Letter_D)
     & Encoding.Encode (Latin_Capital_Letter_A)
     & Encoding.Encode (Latin_Capital_Letter_T)
     & Encoding.Encode (Latin_Capital_Letter_A);

   Cdata_Sequence : constant Byte_Sequence :=
     Encoding.Encode (Exclamation_Mark)
     & Encoding.Encode (Left_Square_Bracket)
     & Cdata_Attlist_Sequence
     & Encoding.Encode (Left_Square_Bracket);

   Doctype_Sequence : constant Byte_Sequence :=
     Encoding.Encode (Exclamation_Mark)
     & Encoding.Encode (Latin_Capital_Letter_D)
     & Encoding.Encode (Latin_Capital_Letter_O)
     & Encoding.Encode (Latin_Capital_Letter_C)
     & Encoding.Encode (Latin_Capital_Letter_T)
     & Encoding.Encode (Latin_Capital_Letter_Y)
     & Encoding.Encode (Latin_Capital_Letter_P)
     & Encoding.Encode (Latin_Capital_Letter_E);

   Yes_Sequence : constant Byte_Sequence :=
     Encoding.Encode (Latin_Small_Letter_Y)
     & Encoding.Encode (Latin_Small_Letter_E)
     & Encoding.Encode (Latin_Small_Letter_S);

   No_Sequence : constant Byte_Sequence :=
     Encoding.Encode (Latin_Small_Letter_N)
     & Encoding.Encode (Latin_Small_Letter_O);

   Version_Sequence : constant Byte_Sequence :=
     Encoding.Encode (Latin_Small_Letter_V)
     & Encoding.Encode (Latin_Small_Letter_E)
     & Encoding.Encode (Latin_Small_Letter_R)
     & Encoding.Encode (Latin_Small_Letter_S)
     & Encoding.Encode (Latin_Small_Letter_I)
     & Encoding.Encode (Latin_Small_Letter_O)
     & Encoding.Encode (Latin_Small_Letter_N);

   Encoding_Sequence : constant Byte_Sequence :=
     Encoding.Encode (Latin_Small_Letter_E)
     & Encoding.Encode (Latin_Small_Letter_N)
     & Encoding.Encode (Latin_Small_Letter_C)
     & Encoding.Encode (Latin_Small_Letter_O)
     & Encoding.Encode (Latin_Small_Letter_D)
     & Encoding.Encode (Latin_Small_Letter_I)
     & Encoding.Encode (Latin_Small_Letter_N)
     & Encoding.Encode (Latin_Small_Letter_G);

   Standalone_Sequence : constant Byte_Sequence :=
     Encoding.Encode (Latin_Small_Letter_S)
     & Encoding.Encode (Latin_Small_Letter_T)
     & Encoding.Encode (Latin_Small_Letter_A)
     & Encoding.Encode (Latin_Small_Letter_N)
     & Encoding.Encode (Latin_Small_Letter_D)
     & Encoding.Encode (Latin_Small_Letter_A)
     & Encoding.Encode (Latin_Small_Letter_L)
     & Encoding.Encode (Latin_Small_Letter_O)
     & Encoding.Encode (Latin_Small_Letter_N)
     & Encoding.Encode (Latin_Small_Letter_E);

   System_Sequence : constant Byte_Sequence :=
     Encoding.Encode (Latin_Capital_Letter_S)
     & Encoding.Encode (Latin_Capital_Letter_Y)
     & Encoding.Encode (Latin_Capital_Letter_S)
     & Encoding.Encode (Latin_Capital_Letter_T)
     & Encoding.Encode (Latin_Capital_Letter_E)
     & Encoding.Encode (Latin_Capital_Letter_M);

   Public_Sequence : constant Byte_Sequence :=
     Encoding.Encode (Latin_Capital_Letter_P)
     & Encoding.Encode (Latin_Capital_Letter_U)
     & Encoding.Encode (Latin_Capital_Letter_B)
     & Encoding.Encode (Latin_Capital_Letter_L)
     & Encoding.Encode (Latin_Capital_Letter_I)
     & Encoding.Encode (Latin_Capital_Letter_C);

   Entity_Attlist_Sequence : constant Byte_Sequence :=
     Encoding.Encode (Latin_Capital_Letter_E)
     & Encoding.Encode (Latin_Capital_Letter_N)
     & Encoding.Encode (Latin_Capital_Letter_T)
     & Encoding.Encode (Latin_Capital_Letter_I)
     & Encoding.Encode (Latin_Capital_Letter_T)
     & Encoding.Encode (Latin_Capital_Letter_Y);

   Entity_Sequence : constant Byte_Sequence :=
     Encoding.Encode (Exclamation_Mark) & Entity_Attlist_Sequence;

   Attlist_Sequence : constant Byte_Sequence :=
     Encoding.Encode (Exclamation_Mark)
     & Encoding.Encode (Latin_Capital_Letter_A)
     & Encoding.Encode (Latin_Capital_Letter_T)
     & Encoding.Encode (Latin_Capital_Letter_T)
     & Encoding.Encode (Latin_Capital_Letter_L)
     & Encoding.Encode (Latin_Capital_Letter_I)
     & Encoding.Encode (Latin_Capital_Letter_S)
     & Encoding.Encode (Latin_Capital_Letter_T);

   Notation_Attlist_Sequence : constant Byte_Sequence :=
     Encoding.Encode (Latin_Capital_Letter_N)
     & Encoding.Encode (Latin_Capital_Letter_O)
     & Encoding.Encode (Latin_Capital_Letter_T)
     & Encoding.Encode (Latin_Capital_Letter_A)
     & Encoding.Encode (Latin_Capital_Letter_T)
     & Encoding.Encode (Latin_Capital_Letter_I)
     & Encoding.Encode (Latin_Capital_Letter_O)
     & Encoding.Encode (Latin_Capital_Letter_N);

   Notation_Sequence : constant Byte_Sequence :=
     Encoding.Encode (Exclamation_Mark) & Notation_Attlist_Sequence;

   Element_Sequence : constant Byte_Sequence :=
     Encoding.Encode (Exclamation_Mark)
     & Encoding.Encode (Latin_Capital_Letter_E)
     & Encoding.Encode (Latin_Capital_Letter_L)
     & Encoding.Encode (Latin_Capital_Letter_E)
     & Encoding.Encode (Latin_Capital_Letter_M)
     & Encoding.Encode (Latin_Capital_Letter_E)
     & Encoding.Encode (Latin_Capital_Letter_N)
     & Encoding.Encode (Latin_Capital_Letter_T);

   Ndata_Sequence : constant Byte_Sequence :=
     Encoding.Encode (Latin_Capital_Letter_N)
     & Encoding.Encode (Latin_Capital_Letter_D)
     & Encoding.Encode (Latin_Capital_Letter_A)
     & Encoding.Encode (Latin_Capital_Letter_T)
     & Encoding.Encode (Latin_Capital_Letter_A);

   Comment_End_Sequence : constant Byte_Sequence :=
     Encoding.Encode (Hyphen_Minus)
     & Encoding.Encode (Hyphen_Minus);

   Comment_Start_Sequence : constant Byte_Sequence :=
     Encoding.Encode (Exclamation_Mark) & Comment_End_Sequence;

   Empty_Sequence : constant Byte_Sequence :=
     Encoding.Encode (Latin_Capital_Letter_E)
     & Encoding.Encode (Latin_Capital_Letter_M)
     & Encoding.Encode (Latin_Capital_Letter_P)
     & Encoding.Encode (Latin_Capital_Letter_T)
     & Encoding.Encode (Latin_Capital_Letter_Y);

   Any_Sequence : constant Byte_Sequence :=
     Encoding.Encode (Latin_Capital_Letter_A)
     & Encoding.Encode (Latin_Capital_Letter_N)
     & Encoding.Encode (Latin_Capital_Letter_Y);

   Pcdata_Sequence : constant Byte_Sequence :=
     Encoding.Encode (Pound_Sign)
     & Encoding.Encode (Latin_Capital_Letter_P)
     & Cdata_Attlist_Sequence;

   Id_Sequence : constant Byte_Sequence :=
     Encoding.Encode (Latin_Capital_Letter_I)
     & Encoding.Encode (Latin_Capital_Letter_D);

   Idref_Sequence : constant Byte_Sequence :=
     Id_Sequence
     & Encoding.Encode (Latin_Capital_Letter_R)
     & Encoding.Encode (Latin_Capital_Letter_E)
     & Encoding.Encode (Latin_Capital_Letter_F);

   Idrefs_Sequence : constant Byte_Sequence :=
     Idref_Sequence & Encoding.Encode (Latin_Capital_Letter_S);

   Entities_Sequence : constant Byte_Sequence :=
     Encoding.Encode (Latin_Capital_Letter_E)
     & Encoding.Encode (Latin_Capital_Letter_N)
     & Encoding.Encode (Latin_Capital_Letter_T)
     & Encoding.Encode (Latin_Capital_Letter_I)
     & Encoding.Encode (Latin_Capital_Letter_T)
     & Encoding.Encode (Latin_Capital_Letter_I)
     & Encoding.Encode (Latin_Capital_Letter_E)
     & Encoding.Encode (Latin_Capital_Letter_S);

   Nmtoken_Sequence : constant Byte_Sequence :=
     Encoding.Encode (Latin_Capital_Letter_N)
     & Encoding.Encode (Latin_Capital_Letter_M)
     & Encoding.Encode (Latin_Capital_Letter_T)
     & Encoding.Encode (Latin_Capital_Letter_O)
     & Encoding.Encode (Latin_Capital_Letter_K)
     & Encoding.Encode (Latin_Capital_Letter_E)
     & Encoding.Encode (Latin_Capital_Letter_N);

   Required_Sequence : constant Byte_Sequence :=
     Encoding.Encode (Number_Sign)
     & Encoding.Encode (Latin_Capital_Letter_R)
     & Encoding.Encode (Latin_Capital_Letter_E)
     & Encoding.Encode (Latin_Capital_Letter_Q)
     & Encoding.Encode (Latin_Capital_Letter_U)
     & Encoding.Encode (Latin_Capital_Letter_I)
     & Encoding.Encode (Latin_Capital_Letter_R)
     & Encoding.Encode (Latin_Capital_Letter_E)
     & Encoding.Encode (Latin_Capital_Letter_D);

   Implied_Sequence : constant Byte_Sequence :=
     Encoding.Encode (Number_Sign)
     & Encoding.Encode (Latin_Capital_Letter_I)
     & Encoding.Encode (Latin_Capital_Letter_M)
     & Encoding.Encode (Latin_Capital_Letter_P)
     & Encoding.Encode (Latin_Capital_Letter_L)
     & Encoding.Encode (Latin_Capital_Letter_I)
     & Encoding.Encode (Latin_Capital_Letter_E)
     & Encoding.Encode (Latin_Capital_Letter_D);

   Fixed_Sequence : constant Byte_Sequence :=
     Encoding.Encode (Number_Sign)
     & Encoding.Encode (Latin_Capital_Letter_F)
     & Encoding.Encode (Latin_Capital_Letter_I)
     & Encoding.Encode (Latin_Capital_Letter_X)
     & Encoding.Encode (Latin_Capital_Letter_E)
     & Encoding.Encode (Latin_Capital_Letter_D);

   Nmtokens_Sequence : constant Byte_Sequence :=
     Nmtoken_Sequence & Encoding.Encode (Latin_Capital_Letter_S);

   Invalid_Character : exception;
   --  Raised by Skip_Name

   ---------------------------------------------
   -- Specifications for internal subprograms --
   ---------------------------------------------

   procedure Free (NS : in out XML_NS);
   --  Free the memory occupied by the namespace list (NS and its siblings)

   procedure Free (Elem : in out Element_Access);
   --  Free the contents of the element, but doesn't remove the element itself.

   procedure Free_Memory (Machine : in out Reader'Class);
   --  Free all the memory allocated for the machine.

   procedure Free_NS is new Unchecked_Deallocation (XML_NS_Record, XML_NS);
   procedure Free_Element is new Unchecked_Deallocation
     (Element, Element_Access);

   ---------------------------------------
   -- Redefine the transition functions --
   ---------------------------------------

   type Xml_Transition_Function is access procedure
     (Automaton : in out Reader'Class; Last_Char_Read : Unicode_Char);

   function Conv is new Unchecked_Conversion
     (Xml_Transition_Function, Character_Automaton.Transition_Function);

   ----------
   -- Misc --
   ----------

   function Is_Valid_Name (Str : Unicode.CES.Byte_Sequence) return Boolean;
   --  True if Str matches the rule for names (XML specifications 2.3, [5])

   function Is_Name_Char (C : Unicode.Unicode_Char) return Boolean;
   --  True if C is a NameChar (XML specifications 2.3, [4])

   function Is_Pubid_Char (C : Unicode.Unicode_Char) return Boolean;
   --  True if C is a PubidChar (XML specifications 2.3, [13])

   procedure Add_Namespace
     (Machine : in out Reader'Class;
      Node    : Element_Access;
      Prefix  : Byte_Sequence;
      URI     : Byte_Sequence);
   --  Create a new prefix mapping (an XML namespace). If Node is null, then
   --  the mapping is added as a default namespace

   function Get_Entity_Replacement
     (Machine       : Reader'Class;
      Entity        : Byte_Sequence;
      Is_Std_Entity : access Boolean;
      Def           : access Entity_Def) return Byte_Sequence;
   --  Get the string that should replace an entity.
   --  Is_Std_Entity is set to True if Entity is one of the five predefined
   --  entity in the XML specifications.
   --  Def will contain the entity definition.

   procedure Find_NS
     (Machine : in out Reader'Class;
      Elem    : Element_Access;
      Prefix  : Byte_Sequence := "";
      NS      : out XML_NS);
   --  Search the namespace associated with a given prefix in the scope of
   --  Elem or its parents. Use the empty string to get the default namespace.
   --  Fatal_Error is raised if no such namespace was found (and null is
   --  returned, in case Fatal_Error didn't raise an exception)

   procedure Main_Loop
     (Machine : in out Reader'Class;
      Input   : in out Input_Sources.Input_Source'Class;
      Increment_Locator : Boolean := True);
   --  Execute the main loop (ie read all the characters from the input,
   --  and process them). This procedure finished only when there is no more
   --  character to read from input.
   --  If Increment_Locator is True, then the locator associated with the
   --  Machine is also incremented as characters are read.
   --  If Self_Contained is true, then the text in Input must be
   --  self-contained, ie all tags and entities must be closed at the end.
   --  Otherwise, an error is raised

   procedure Skip_Spaces (Str : Byte_Sequence; Index : in out Natural);
   --  Skip all the spaces that start at Index, and leaves Index at the first
   --  non-white space character.

   procedure Skip_String
     (Str         : Byte_Sequence;
      Index       : in out Natural;
      Machine     : in out Reader'Class;
      Value_Start : out Natural;
      Value_End   : out Natural;
      Error_Msg   : String);
   --  Skip the string that starts at Index (either with single quote or
   --  double quotes). At the end, Index points after the closing quote.
   --  Error_Msg is the error raised if the first character is not a quote,
   --  but no error is raised if Error_Msg is the empty string.

   function Looking_At
     (Machine : Reader'Class; Index : Natural; Str : Byte_Sequence)
      return Boolean;
   --  Return True if a string Str starts at Index in the buffer, and is
   --  either followed by a white space or terminates the buffer.

   procedure Skip_Name
     (Str        : Byte_Sequence;
      Index      : in out Natural;
      Name_Start : out Natural;
      Name_End   : out Natural;
      Is_Nmtoken : Boolean := False);
   --  Skips the Name starting at Index. On return, Index will point to the
   --  first character following the name.
   --  All characters must correct with regard to Is_Name_Char.
   --  If Is_Nmtoken is False, then the first character must be a letter, ie
   --  match the [5] rule in the XML grammar.

   procedure Check_XML_PI_Syntax
     (Machine : in out Reader'Class;
      Target  : Byte_Sequence;
      Data    : Byte_Sequence);
   --  Check the syntax of the <?xml?> processing instruction

   function Qname_From_Name
     (URI : Byte_Sequence_Access;
      Local_Name : Byte_Sequence) return Byte_Sequence;
   --  Create the qualified name from the namespace URI and the local name.

   procedure Fatal_Error
     (Machine : in out Reader'Class;
      Message : Byte_Sequence;
      Loc     : File_Locator_Access := null);
   --  Raise a fatal error exception.
   --  The exception is reported at the location given in Machine. Note that
   --  in some cases (for instance if we were processing a comment), a
   --  different error message might be substituted automatically.
   --  If Loc is null, the location of the error is extracted automatically
   --  either from the current location or the start of the entity we are
   --  currently processing

   procedure Put_In_Buffer
     (Machine : in out Reader'Class; Last_Read : Unicode_Char);
   --  Save the character for a future use

   procedure Put_In_Buffer_No_Multiple_Space
     (Machine : in out Reader'Class; Last_Read : Unicode_Char);
   --  Put the character of a processing instruction into the buffer.
   --  Note that XML parsers are required to strip multiple white-space
   --  characters to a single one in a processing instruction. This procedure
   --  takes care of that.

   procedure Put_In_Buffer_No_Multiple_Space_Nor_Newline
     (Machine : in out Reader'Class; Last_Read : Unicode_Char);
   --  Like above, but also changes newlines to space

   procedure Put_In_Buffer_Space
     (Machine : in out Reader'Class; Last_Read : Unicode.Unicode_Char);
   --  Insert a space and Last_Read in the buffer.

   procedure Put_In_Buffer_Force
     (Machine : in out Reader'Class; Last_Read : Unicode_Char);
   procedure Put_In_Buffer_Force
     (Machine : in out Reader'Class; S : Byte_Sequence);
   pragma Inline (Put_In_Buffer_Force);
   --  Copy a string directly into the buffer

   procedure Check_Cdata_End
     (Machine   : in out Reader'Class; Last_Read : Unicode_Char);
   --  Put Last_Read in the buffer, in the context of Text, but checks that
   --  the invalid sequence ']]>' doesn't appear in the text.

   procedure Put_Question_Mark
     (Machine : in out Reader'Class; Last_Read : Unicode_Char);
   --  Put a question mark character, followed by last_read

   procedure Xml_Text_To_Document (Machine : in out Reader'Class);
   --  Creates a new text element in the XML tree with the contents of the
   --  buffer.

   procedure Xml_Error
     (Machine : in out Reader'Class; Last_Read : Unicode_Char);
   --  An syntactic error was detected while parsing the XML file. This is
   --  not called for semantic errors (different names for opening or closing
   --  tags).

   procedure Must_Have_Space_In_Attr
     (Machine : in out Reader'Class; Last_Read : Unicode_Char);
   --  Attributes must be separated by spaces

   procedure Must_Have_Equal_After_Attr
     (Machine : in out Reader'Class; Last_Read : Unicode_Char);
   --  Attribute names must be followed by equal sign

   procedure Attr_Need_Value
     (Machine : in out Reader'Class; Last_Read : Unicode_Char);
   --  Attributes must have an explicit value

   procedure Debug_Put (Machine : Reader'Class; Str : Byte_Sequence);
   --  Print Str in debug mode, along with location information

   function Resolve_URI (Machine : Reader'Class; URI : Byte_Sequence)
      return Byte_Sequence;
   --  Return a fully resolved URI, based on the system identifier set for
   --  Machine, and URI.

   ---------
   -- DTD --
   ---------

   procedure Start_Of_DTD
     (Machine : in out Reader'Class; Last_Read : Unicode_Char);
   --  Called when the buffer contains the beginning of a DTD declaration,
   --  ie something matching the rule "<!DOCTYPE name (S* ExternalID)?".

   procedure End_Of_Entity_Declaration (Machine : in out Reader'Class);
   --  Called when the buffer contains an entity declaration, starting with
   --  "!ENTITY".

   procedure End_Of_Element_Declaration (Machine : in out Reader'Class);
   --  Called when the buffer contains an element declaration, starting with
   --  "!ELEMENT".

   procedure End_Of_Notation_Declaration (Machine : in out Reader'Class);
   --  Called when the buffer contains a notation, starting with
   --  "!NOTATION".

   procedure End_Of_Attlist_Declaration (Machine : in out Reader'Class);
   --  Called when the buffer contains an element declaration, starting with
   --  "!ATTLIST".

   procedure Parse_External_Id
     (Machine                  : in out Reader'Class;
      Index                    : in out Natural;
      Public_Start, Public_End : out Natural;
      System_Start, System_End : out Natural);
   --  Parses the external id that starts at Index (see rule [75] in XML
   --  specifications).

   procedure Cancel_Entity
     (Machine : in out Reader'Class; Last_Read : Unicode_Char);
   --  If we are processing the declaration of an entity <!ENTITY>, then
   --  cancel the entity reference currently processed. It puts the
   --  characters read back in the buffer.

   procedure Parse_Element_Model
     (Model  : Unicode.CES.Byte_Sequence;
      Index  : in out Natural;
      Result : out Element_Model_Ptr;
      Nmtokens : Boolean := False);
   --  Internal version of Parse_Element_Model. This stops as soon as the
   --  model is parsed, even if there are some characters left. Index is left
   --  on the first character following the model.
   --  If Nmtokens is False, then any enumeration in the model must token
   --  items that match the [5] Name rule in the XML specifications.

   --------------
   -- Entities --
   --------------

   procedure Start_Of_Entity
     (Machine : in out Reader'Class; Last_Read : Unicode_Char);
   --  A character '&' has been encountered.

   procedure End_Of_Entity
     (Machine : in out Reader'Class; Last_Read : Unicode_Char);
   --  End of an Xml entity name.
   --  Search that entity in the entity tables and put the corresponding
   --  content in the input buffer, since a XML processor is supposed to
   --  substitute these values.

   procedure End_Of_Num_Entity
     (Machine   : in out Reader'Class; Last_Read : Unicode.Unicode_Char);
   --  End of a numerical entity.
   --  Convert the string read into a number and put the character
   --  having that code in the input buffer.

   procedure Num_Entity_Char
     (Machine   : in out Reader'Class; Last_Read : Unicode.Unicode_Char);
   --  A character belonging to an XML numerical entity has been read.

   procedure End_Of_Declaration
     (Machine   : in out Reader'Class; Last_Read : Unicode.Unicode_Char);
   --  End of a <!...> has been met

   procedure Start_Of_Declaration
     (Machine   : in out Reader'Class; Last_Read : Unicode.Unicode_Char);
   --  '<!' has just been read in the buffer

   procedure End_Of_PI
     (Machine : in out Reader'Class; Last_Read : Unicode_Char);
   --  End of a <? ..?> processing instruction has been found.
   --  The buffer contains the all declaration, including attributes.

   procedure Start_Of_PI
     (Machine : in out Reader'Class; Last_Read : Unicode.Unicode_Char);
   --  Start of a <?...?> processing instruction has been found

   function Entity_Length (Machine : Reader'Class) return Natural;
   pragma Inline (Entity_Length);
   --  Return the length of the current entity.
   --  This assumes that you are indeed processing an entity

   procedure Delete_Entity (Machine : in out Reader'Class);
   pragma Inline (Delete_Entity);
   --  Remove the current entity from the buffer.

   ----------
   -- Tags --
   ----------

   procedure Start_Of_Tag
     (Machine : in out Reader'Class; Last_Read : Unicode.Unicode_Char);
   --  Beginning of a Xml tag (start or end tag).
   --  Put the preceding text in the Thot document.

   procedure End_Of_Start_Tag
     (Machine   : in out Reader'Class; Last_Read : Unicode.Unicode_Char);
   --  A ">" or a "/" has been read. It indicates the end of a start tag.
   --  This is called only when there was at least a space after the element
   --  type.

   procedure End_Of_Start_Gi
     (Machine : in out Reader'Class; Last_Read : Unicode.Unicode_Char);
   --  The name of an element type has been read in a start tag.
   --  This function is called when there is a list of attributes after the
   --  element type, or at least a space. End_Of_Start_Tag will be called
   --  when > is read.

   procedure End_Of_Prefix
     (Machine : in out Reader'Class; Last_Read : Unicode.Unicode_Char);
   --  End of a tag prefix (namespace handling).
   --  For instance, for a tag name "html:h1", the prefix is "html"

   procedure End_Of_Start_Gi_And_Tag
     (Machine : in out Reader'Class; Last_Read : Unicode.Unicode_Char);
   --  A ">" has been read. It indicates the end of an element name and the
   --  end of a start tag.

   procedure End_Of_Empty_Tag
     (Machine : in out Reader'Class; Last_Read : Unicode.Unicode_Char);
   --  "/>" has just been read for an empty tag.

   procedure End_Of_XML_End_Tag
     (Machine   : in out Reader'Class; Last_Read : Unicode.Unicode_Char);
   --  The end ('>') of an end tag has been read. This is called when
   --  when the name was followed by a space character for instance.

   procedure End_Of_Name_And_Closing_Tag
     (Machine   : in out Reader'Class; Last_Read : Unicode.Unicode_Char);
   --  An element name followed by a '>' has been read in a closing tag.

   procedure End_Of_Closing_Tag_Name
     (Machine   : in out Reader'Class; Last_Read : Unicode.Unicode_Char);
   --  An element name has been read in a closing tag.
   --  Check that it closes the right element.

   ----------------
   -- Attributes --
   ----------------

   procedure End_Of_Attr_Prefix
     (Machine : in out Reader'Class; Last_Read : Unicode.Unicode_Char)
     renames End_Of_Prefix;
   --  The end of an attribute prefix (":") has been read. If the attribute
   --  is "html:src", the prefix is "html".

   procedure End_Of_Attr_Value
     (Machine : in out Reader'Class; Last_Read : Unicode.Unicode_Char);
   --  The end of an attribute value has been found (the text between double
   --  or single quotes). Note that the quotes are not found in the buffer.

   procedure Start_Of_Attr_Value
     (Machine : in out Reader'Class; Last_Read : Unicode.Unicode_Char);
   --  The beginning of an attribute value was found.

   procedure End_Of_Attr_Name_And_Tag
     (Machine : in out Reader'Class; Last_Read : Unicode.Unicode_Char);
   --  A ">" or a "/" (XML) has been read. It indicates the end of an attribute
   --  name and the end of a start tag.

   procedure End_Of_Attr_Name
     (Machine : in out Reader'Class; Last_Read : Unicode.Unicode_Char);
   --  An XML attribute name has been read.

   procedure Attr_Quote_Error
     (Machine : in out Reader'Class; Last_Read : Unicode.Unicode_Char);
   --  An attribute value wasn't properly quoted

   procedure Illegal_Char_In_Attr_Value
     (Machine : in out Reader'Class; Last_Read : Unicode.Unicode_Char);
   --  An illegal character was met in an attribute value

   procedure Invalid_Ampersand
     (Machine : in out Reader'Class; Last_Read : Unicode.Unicode_Char);
   --  Report an error ('&' illegal in attribute value)

   procedure Seen_Quote_In_Attr_Value
     (Machine : in out Reader'Class; Last_Read : Unicode.Unicode_Char);
   --  Insert Last_Read in buffer, and save the current position.
   --  This is called while parsing an attribute value, when we meet a quote
   --  that is different from the opening quote

   --------------
   -- Comments --
   --------------

   procedure End_Of_Comment
     (Machine : in out Reader'Class; Last_Read : Unicode.Unicode_Char);
   --  The end of a comment has been read, the text of the comment is
   --  available in the input buffer.

   procedure Start_Of_Comment
     (Machine : in out Reader'Class; Last_Read : Unicode.Unicode_Char);
   --  Registers the start of a comment

   procedure Double_Dash_In_Comment
     (Machine : in out Reader'Class; Last_Read : Unicode.Unicode_Char);
   --  Error: The sequence '--' is not authorized in comments

   -------------------
   -- The automaton --
   -------------------

   Xml_Automaton : constant Character_Automaton.Automaton_Def :=
     (
      --  State 0: reading character data
      (Top_State,  Less_Than_Sign,    1,   Conv (Start_Of_Tag'Access)),
      (Top_State,  Ampersand,         -22, Conv (Start_Of_Entity'Access)),
      (Top_State,  Percent_Sign,      -22, Conv (Start_Of_Entity'Access)),
      (Top_State,  Greater_Than_Sign, 0,   Conv (Check_Cdata_End'Access)),
      (Top_State,  Any_Char,          0,   Conv (Put_In_Buffer'Access)),

      --  State 1: < has just been read
      (1,  Slash,             3,   null),
      (1,  Ampersand,         -22, Conv (Start_Of_Entity'Access)),
      (1,  Exclamation_Mark,  10,  Conv (Start_Of_Declaration'Access)),
      (1,  Question_Mark,     19,  Conv (Start_Of_PI'Access)),
      (1,  Space,             1,   Conv (Xml_Error'Access)),
      (1,  Less_Than_Sign,    Top_State, Conv (Xml_Error'Access)),
      (1,  Any_Char,          2,   Conv (Put_In_Buffer'Access)),

      --  State 2: reading the element name in a start tag
      (2,  Colon,             2,   Conv (End_Of_Prefix'Access)),
      (2,  Ampersand,         -22, Conv (Start_Of_Entity'Access)),
      (2,  Slash,             18,  Conv (End_Of_Start_Gi_And_Tag'Access)),
      (2,  Greater_Than_Sign, Top_State,
       Conv (End_Of_Start_Gi_And_Tag'Access)),
      (2,  Space,             16,  Conv (End_Of_Start_Gi'Access)),
      (2,  Any_Char,          2,   Conv (Put_In_Buffer'Access)),

      --  State 3: reading the element name in an end tag
      (3,  Colon,             3,   Conv (End_Of_Prefix'Access)),
      (3,  Greater_Than_Sign, Top_State,
       Conv (End_Of_Name_And_Closing_Tag'Access)),
      (3,  Space,             7,   Conv (End_Of_Closing_Tag_Name'Access)),
      (3,  Any_Char,          3,   Conv (Put_In_Buffer'Access)),

      --  State 4: reading an attribute name
      (4,  Colon,             4,   Conv (End_Of_Attr_Prefix'Access)),
      (4,  Equals_Sign,       5,   Conv (End_Of_Attr_Name'Access)),
      (4,  Space,             17,  Conv (End_Of_Attr_Name'Access)),
      (4,  Slash,             18,  Conv (Attr_Need_Value'Access)),
      (4,  Greater_Than_Sign, Top_State,   Conv (Attr_Need_Value'Access)),
      (4,  Any_Char,          4,   Conv (Put_In_Buffer'Access)),

      --  State 5: Begin of attribute value
      (5,  Quotation_Mark,    6,   Conv (Start_Of_Attr_Value'Access)),
      (5,  Apostrophe,        9,   Conv (Start_Of_Attr_Value'Access)),
      (5,  Space,             5,   null),
      (5,  Any_Char,          Top_State,   Conv (Attr_Quote_Error'Access)),

      --  State 6: Reading an attribute value between double quotes
      (6,  Quotation_Mark,    8,   Conv (End_Of_Attr_Value'Access)),
      (6,  Apostrophe,        6,   Conv (Seen_Quote_In_Attr_Value'Access)),
      (6,  Less_Than_Sign,    8,   Conv (Illegal_Char_In_Attr_Value'Access)),
      (6,  Ampersand,         -28, Conv (Start_Of_Entity'Access)),
      (6,  Any_Char,          6,
       Conv (Put_In_Buffer_No_Multiple_Space_Nor_Newline'Access)),

      --  State 7: Reading spaces and expecting end of end tag
      --  No need to signal the end of the tag at this level, it was already
      --  reported when the first space was met.
      (7,  Greater_Than_Sign, Top_State,   null),
      (7,  Space,             7,   null),

      --  State 8: End of attribute value
      (8,  Question_Mark,     18,  Conv (End_Of_Start_Gi_And_Tag'Access)),
      (8,  Slash,             18,  Conv (End_Of_Start_Tag'Access)),
      (8,  Greater_Than_Sign, Top_State, Conv (End_Of_Start_Tag'Access)),
      (8,  Space,             16,  null),
      (8,  Any_Char,         Top_State, Conv (Must_Have_Space_In_Attr'Access)),

      --  State 9: Reading an attribute value between simple quotes
      (9,  Apostrophe,        8,   Conv (End_Of_Attr_Value'Access)),
      (9,  Quotation_Mark,    9,   Conv (Seen_Quote_In_Attr_Value'Access)),
      (9,  Less_Than_Sign,    9,   Conv (Illegal_Char_In_Attr_Value'Access)),
      (9,  Ampersand,         -28, Conv (Start_Of_Entity'Access)),
      (9,  Any_Char,          9,
       Conv (Put_In_Buffer_No_Multiple_Space_Nor_Newline'Access)),

      --  State 10: <! has been read  (could be <![CDATA)
      (10, Hyphen_Minus,      11,  null),
      (10, Opening_Square_Bracket, 25,  Conv (Put_In_Buffer'Access)),
      (10, Greater_Than_Sign, Top_State,   Conv (End_Of_Declaration'Access)),
      (10, Any_Char,          15,  Conv (Put_In_Buffer'Access)),

      --  State 11: <!- has been read, probably a comment
      (11, Hyphen_Minus,      12,  Conv (Start_Of_Comment'Access)),

      --  State 12: reading a comment
      (12, Hyphen_Minus,      13,  null),
      (12, Any_Char,          12,  Conv (Put_In_Buffer'Access)),

      --  State 13: a dash has been read within a comment
      (13, Hyphen_Minus,      14,  null),
      (13, Any_Char,          12,  Conv (Put_In_Buffer'Access)),

      --  State 14: A double dash has been read within a comment
      (14, Greater_Than_Sign, Top_State, Conv (End_Of_Comment'Access)),
      (14, Any_Char,          Top_State, Conv (Double_Dash_In_Comment'Access)),

      --  State 15: Reading the prologue <!X ..., probably a DOCTYPE
      --  or one of the  declarations inside the DTD).
      (15, Opening_Square_Bracket, Top_State,  Conv (Start_Of_DTD'Access)),
      (15, Greater_Than_Sign, Top_State, Conv (End_Of_Declaration'Access)),
      (15, Quotation_Mark,   -29, Conv (Put_In_Buffer'Access)),
      (15, Apostrophe,       -30, Conv (Put_In_Buffer'Access)),
      (15, Any_Char,          15, Conv (Put_In_Buffer'Access)),

      --  State 16: Expecting an attribute name or an end of start tag
      (16, Space,             16,  null),
      (16, Slash,             18,  Conv (End_Of_Start_Tag'Access)),
      (16, Greater_Than_Sign, Top_State,   Conv (End_Of_Start_Tag'Access)),
      (16, Any_Char,          4,   Conv (Put_In_Buffer'Access)),

      --  State 17: Expecting = after an attribute name
      (17, Space,             17,  null),
      (17, Equals_Sign,       5,   null),
      (17, Any_Char,      Top_State, Conv (Must_Have_Equal_After_Attr'Access)),

      --  State 18: a / has been read withing a start tag. except a > which
      --  indicates the end of the start tag for an empty element
      (18, Greater_Than_Sign, Top_State,   Conv (End_Of_Empty_Tag'Access)),
      (18, Any_Char,          Top_State,   Conv (Xml_Error'Access)),

      --  State 19: <? has been read
      (19, Space,             19,  null), --  Ignore spaces at beginning
      (19, Question_Mark,     21,  null),
      (19, Any_Char,          20,  Conv (Put_In_Buffer'Access)),

      --  State 20: Reading a processing instruction
      (20, Question_Mark,     21,  null),
      (20, Any_Char,        20, Conv (Put_In_Buffer_No_Multiple_Space'Access)),

      --  state 21: A ? has been read in a processing instruction
      (21, Greater_Than_Sign, Top_State,   Conv (End_Of_PI'Access)),
      (21, Question_Mark,     21,  Conv (Put_In_Buffer'Access)), -- <? .. ??>
      (21, Any_Char,          20,  Conv (Put_Question_Mark'Access)),

      --  Sub automaton for reading entities in various contexts
      --  State 22: a '&' has been read
      (22, Number_Sign,       24,  Conv (Put_In_Buffer'Access)),
      (22, Space,             Exit_Sub_Automaton,
       Conv (Invalid_Ampersand'Access)),
      (22, Any_Char,          23,  Conv (Put_In_Buffer'Access)),

      --  State 23: reading a name entity
      --  In some cases, we just give up since we didn't encounter the end of
      --  the entity
      (23, Semicolon,         Exit_Sub_Automaton, Conv (End_Of_Entity'Access)),
      (23, Less_Than_Sign,    Exit_Sub_Automaton,
       Conv (Invalid_Ampersand'Access)),
      (23, Greater_Than_Sign, Exit_Sub_Automaton,
       Conv (Invalid_Ampersand'Access)),
      (23, Slash,             Exit_Sub_Automaton,
       Conv (Invalid_Ampersand'Access)),
      (23, Quotation_Mark,    Exit_Sub_Automaton,
       Conv (Invalid_Ampersand'Access)),
      (23, Apostrophe,        Exit_Sub_Automaton,
       Conv (Invalid_Ampersand'Access)),
      (23, Space,             Exit_Sub_Automaton,
       Conv (Invalid_Ampersand'Access)),
      (23, Any_Char,          23,  Conv (Put_In_Buffer'Access)),

      --  State 24: reading a numerical entity
      (24, Semicolon,         Exit_Sub_Automaton,
       Conv (End_Of_Num_Entity'Access)),
      (24, Any_Char,          24,  Conv (Num_Entity_Char'Access)),

      --  States 25, 26 and 27 are used to parse the contents of CDATA

      --  State 25: reading the contents of a <![... section (probably CDATA)
      (25, Closing_Square_Bracket, 26,   Conv (Put_In_Buffer'Access)),
      (25, Any_Char,          25,   Conv (Put_In_Buffer'Access)),

      --  State 26: read ] in a CDATA section
      (26, Closing_Square_Bracket, 27,   Conv (Put_In_Buffer'Access)),
      (26, Any_Char,          25,   Conv (Put_In_Buffer'Access)),

      --  State 27: read ]] in a CDATA section
      (27, Greater_Than_Sign, Top_State,    Conv (End_Of_Declaration'Access)),
      (27, Closing_Square_Bracket, 27,   Conv (Put_In_Buffer'Access)), --  ]]]>
      (27, Any_Char,          25,   Conv (Put_In_Buffer'Access)),

      --  State 28: Reading an entity while in a attribute value context
      --  If we didn't really have an entity, then this is an error since
      --  '&' is not allowed in char context
      --  Note also that named entities are not expanded in attribute values,
      --  since SAX expect string values to be passed as an unparsed value.
      --  (XML specifications 3.3.1). To change this behavior, uncomment the
      --  line below and comment the following ones.
      (28, Number_Sign,       24,  Conv (Put_In_Buffer'Access)),
      (28, Space,             Exit_Sub_Automaton,
       Conv (Invalid_Ampersand'Access)),
      (28, Any_Char,          23,  Conv (Put_In_Buffer'Access)),
--    (28, Semicolon,         Exit_Sub_Automaton, Conv (Put_In_Buffer'Access)),
--        (28, Less_Than_Sign,    Exit_Sub_Automaton,
--         Conv (Invalid_Ampersand'Access)),
--        (28, Greater_Than_Sign, Exit_Sub_Automaton,
--         Conv (Invalid_Ampersand'Access)),
--        (28, Slash,             Exit_Sub_Automaton,
--         Conv (Invalid_Ampersand'Access)),
--        (28, Quotation_Mark,    Exit_Sub_Automaton,
--         Conv (Invalid_Ampersand'Access)),
--        (28, Apostrophe,        Exit_Sub_Automaton,
--         Conv (Invalid_Ampersand'Access)),
--        (28, Any_Char,          28,  Conv (Put_In_Buffer'Access)),

      --  State 29: Parsing a double-quoted string inside the DTD
      (29, Quotation_Mark,   Exit_Sub_Automaton, Conv (Put_In_Buffer'Access)),
      (29, Ampersand,        -31, Conv (Start_Of_Entity'Access)),
      (29, Any_Char,         29,
       Conv (Put_In_Buffer_No_Multiple_Space_Nor_Newline'Access)),

      --  State 30: Parsing a single-quoted string inside the DTD
      (30, Apostrophe,       Exit_Sub_Automaton, Conv (Put_In_Buffer'Access)),
      (30, Ampersand,        -31, Conv (Start_Of_Entity'Access)),
      (30, Any_Char,         30,
       Conv (Put_In_Buffer_No_Multiple_Space_Nor_Newline'Access)),

      --  State 31: Handling character references in entity declarations
      (31, Number_Sign,      24, Conv (Put_In_Buffer_Force'Access)),
      (31, Any_Char,         23, Conv (Cancel_Entity'Access))
     );


   -----------------
   -- Free_Memory --
   -----------------

   procedure Free_Memory (Machine : in out Reader'Class) is
      procedure Free_Internal is new Unchecked_Deallocation
        (File_Locator'Class, File_Locator_Access);
      Elem : Element_Access := Machine.Current_Node;
      Tmp  : Element_Access;
   begin
      Clear (Machine.Attributes);
      Free (Machine.Default_Namespaces);
      Free (Machine.Locator.all);
      Free_Internal (Machine.Locator);
      Free (Machine.Processing_Start.all);
      Free_Internal (Machine.Processing_Start);
      Free (Machine.Current_NS);

      while Elem /= null loop
         Tmp := Elem.Parent;
         Free (Elem);
         Free_Element (Elem);
         Elem := Tmp;
      end loop;
      Reset (Machine);
   end Free_Memory;

   ----------
   -- Free --
   ----------

   procedure Free (Elem : in out Element_Access) is
   begin
      Free (Elem.NS);
      Free (Elem.Name);
      Free (Elem.Namespaces);
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (NS : in out XML_NS) is
      Tmp : XML_NS;
   begin
      while NS /= null loop
         Tmp := NS.Next;
         Free (NS.Prefix);
         Free (NS.URI);
         Free_NS (NS);
         NS := Tmp;
      end loop;
   end Free;

   ---------------
   -- Debug_Put --
   ---------------

   procedure Debug_Put
     (Machine : Reader'Class; Str : Byte_Sequence) is
   begin
      Put_Line ("++("
                & Get_Current_State (Machine)'Img
                & ')' & To_String (Machine.Locator.all) & ' ' & Str & "--");
   end Debug_Put;

   -------------
   -- Find_NS --
   -------------

   procedure Find_NS
     (Machine : in out Reader'Class;
      Elem    : Element_Access;
      Prefix  : Byte_Sequence := "";
      NS      : out XML_NS) is
   begin
      --  Search in the default namespaces
      if Elem = null then
         NS := Machine.Default_Namespaces;
      else
         NS := Elem.Namespaces;
      end if;

      while NS /= null loop
         if NS.Prefix.all = Prefix then
            return;
         end if;
         NS := NS.Next;
      end loop;

      --  Search either in the parent or in the default namespaces
      if Elem /= null then
         Find_NS (Machine, Elem.Parent, Prefix, NS);
         return;
      end if;

      Fatal_Error (Machine, "No such namespace '" & Prefix & "'");
      NS := null;
   end Find_NS;

   ------------------
   -- Is_Name_Char --
   ------------------

   function Is_Name_Char (C : Unicode_Char) return Boolean is
   begin
      return C = Period
        or else C = Hyphen_Minus
        or else C = Spacing_Underscore
        or else C = Colon
        or else Is_Digit (C)
        or else Is_Letter (C)
        or else Is_Combining_Char (C)
        or else Is_Extender (C);
   end Is_Name_Char;

   -------------------
   -- Is_Pubid_Char --
   -------------------

   function Is_Pubid_Char (C : Unicode.Unicode_Char) return Boolean is
   begin
      return C = Space
        or else C = Carriage_Return
        or else C = Line_Feed
        or else C in Latin_Capital_Letter_A .. Latin_Capital_Letter_Z
        or else C in Latin_Small_Letter_A .. Latin_Small_Letter_Z
        or else C in Digit_Zero .. Digit_Nine
        or else C = Hyphen_Or_Minus_Sign
        or else C = Apostrophe
        or else C = Opening_Parenthesis
        or else C = Closing_Parenthesis
        or else C = Plus_Sign
        or else C = Comma
        or else C = Dot
        or else C = Slash
        or else C = Colon
        or else C = Question_Mark
        or else C = Equals_Sign
        or else C = Semicolon
        or else C = Exclamation_Mark
        or else C = Star
        or else C = Pound_Sign
        or else C = Commercial_At
        or else C = Dollar_Sign
        or else C = Spacing_Underscore
        or else C = Percent_Sign;
   end Is_Pubid_Char;

   -------------------
   -- Is_Valid_Name --
   -------------------

   function Is_Valid_Name (Str : Unicode.CES.Byte_Sequence) return Boolean is
      C : Unicode_Char := Encoding.Read (Str, Str'First);
      J : Natural := Str'First;
   begin
      if not (C = Colon
              or else C = Spacing_Underscore
              or else Is_Letter (C))
      then
         return False;
      end if;

      J := J + Encoding.Width (C);
      while J <= Str'Last loop
         C := Encoding.Read (Str, J);
         if not Is_Name_Char (C) then
            return False;
         end if;
         J := J + Encoding.Width (C);
      end loop;
      return True;
   end Is_Valid_Name;

   ----------------
   -- Looking_At --
   ----------------

   function Looking_At
     (Machine : Reader'Class; Index : Natural; Str : Byte_Sequence)
      return Boolean is
   begin
      return Index + Str'Length - 1 <= Machine.Buffer_Length
        and then Machine.Buffer (Index .. Index + Str'Length - 1) = Str
        and then (Index + Str'Length >= Machine.Buffer_Length
                  or else Is_White_Space (Encoding.Read
                         (Machine.Buffer, Index + Str'Length)));
   end Looking_At;

   -----------------
   -- Resolve_URI --
   -----------------

   function Resolve_URI (Machine : Reader'Class; URI : Byte_Sequence)
      return Byte_Sequence
   is
      C : Unicode_Char;
      System_Id : constant Byte_Sequence :=
        Get_System_Id (Machine.Locator.all);
      Index : Natural := System_Id'First;
      Basename_Start : Natural := System_Id'First;
   begin
      pragma Assert (URI /= "");
      --  ??? Only resolve paths for now
      if Encoding.Read (URI, URI'First) /= Slash then
         while Index <= System_Id'Last loop
            C := Encoding.Read (System_Id, Index);
            Index := Index + Encoding.Width (C);
            if C = Slash then
               Basename_Start := Index;
            end if;
         end loop;
      end if;
      return System_Id (System_Id'First .. Basename_Start - 1) & URI;
   end Resolve_URI;

   ---------------------
   -- Qname_From_Name --
   ---------------------

   function Qname_From_Name
     (URI : Byte_Sequence_Access; Local_Name : Byte_Sequence)
      return Byte_Sequence is
   begin
      if URI = null or else URI.all = "" then
         return Local_Name;
      else
         return URI.all & Encoding.Encode (Colon) & Local_Name;
      end if;
   end Qname_From_Name;

   ---------------------
   -- Start_Of_Entity --
   ---------------------

   procedure Start_Of_Entity (Machine   : in out Reader'Class;
                              Last_Read : Unicode.Unicode_Char) is
   begin
      if Debug then
         Debug_Put (Machine, "Start_Of_Entity");
      end if;

      --  '%' can not start an entity except in the DTD.
      if Last_Read = Percent_Sign
        and then Machine.Parsing_DTD <= 0
      then
         Set_Current_State (Machine, Exit_Sub_Automaton);
         Put_In_Buffer (Machine, Last_Read);
         return;
      end if;

      --  (2.1[1], 2.8[27]): References are not allowed in Misc
      --  Entities can not be referenced at the top-level
      if Last_Read = Ampersand
        and then Machine.Current_Node = null
        and then Machine.Parsing_DTD <= 0
      then
         Fatal_Error
           (Machine,
            "(2.1) Entity references can not appear at the top-level");
      end if;

      Machine.Processing := Entity;
      Copy (Machine.Processing_Start.all, Machine.Locator.all);
      Machine.Entity_Start := Machine.Buffer_Length + 1;
      Put_In_Buffer (Machine, Last_Read);
   end Start_Of_Entity;

   -------------------
   -- Entity_Length --
   -------------------

   function Entity_Length (Machine : Reader'Class) return Natural is
   begin
      return Machine.Buffer_Length + 1 - Machine.Entity_Start;
   end Entity_Length;

   -------------------
   -- Delete_Entity --
   -------------------

   procedure Delete_Entity (Machine : in out Reader'Class) is
   begin
      Machine.Buffer_Length := Machine.Entity_Start - 1;
   end Delete_Entity;

   -----------------
   -- Fatal_Error --
   -----------------

   procedure Fatal_Error
     (Machine : in out Reader'Class;
      Message : Byte_Sequence;
      Loc     : File_Locator_Access := null)
   is
      Loc2 : File_Locator_Access := Loc;
   begin
      if Loc2 = null then
         Loc2 := Machine.Processing_Start;
      end if;

      if Debug then
         Debug_Put (Machine, "Fatal_Error: " & Message);
      end if;

      case Machine.Processing is
         when PI =>
            Fatal_Error
              (Machine,
               Create ("(2.6) Processing instruction must end with ?>", Loc2));

         when Comment =>
            Fatal_Error
              (Machine,
               Create ("(2.5) Comment must end with -->", Loc2));

         when Declaration =>
            Fatal_Error
              (Machine,
               Create ("(2.7, 2.8) Declaration must be terminated", Loc2));

         when Attr_Value =>
            Fatal_Error
              (Machine,
               Create ("(2.3) Attribute value unterminated. possible end",
                       Loc2));

         when Entity =>
            Fatal_Error
              (Machine,
               Create ("(2.3, 2.4) Entity is not terminated."
                       & " Did you want to use &amp;", Loc2));

         when others =>
            if Loc = null then
               Fatal_Error
                 (Machine, Create (Message, Machine.Locator));
            else
               Fatal_Error
                 (Machine, Create (Message, Loc));
            end if;
      end case;
   end Fatal_Error;

   ---------------------
   -- Num_Entity_Char --
   ---------------------

   procedure Num_Entity_Char (Machine   : in out Reader'Class;
                              Last_Read : Unicode.Unicode_Char)
   is
      Prefix_Width : constant Natural := Encoding.Width (Ampersand)
        + Encoding.Width (Number_Sign);
   begin
      if (Last_Read >= Digit_Zero and then Last_Read <= Digit_Nine)
        or else
        (Entity_Length (Machine) = Prefix_Width
         and then Last_Read = Latin_Small_Letter_X)
        or else
        (Entity_Length (Machine) > Prefix_Width
         and then Encoding.Read
            (Machine.Buffer, Machine.Entity_Start + Prefix_Width) =
            Latin_Small_Letter_X
         and then
            (Last_Read in Latin_Capital_Letter_A .. Latin_Capital_Letter_F
            or else Last_Read in Latin_Small_Letter_A .. Latin_Small_Letter_F))
      then
         Put_In_Buffer_Force (Machine, Last_Read);
      else
         --  Don't report the error at the beginning of the entity, but at the
         --  current location
         Machine.Processing := None;
         Fatal_Error
           (Machine, "(4.1) Invalid character '"
            & Encoding.Encode (Last_Read) & "'");
      end if;
   end Num_Entity_Char;

   ----------------------------
   -- Get_Entity_Replacement --
   ----------------------------

   function Get_Entity_Replacement
     (Machine       : Reader'Class;
      Entity        : Byte_Sequence;
      Is_Std_Entity : access Boolean;
      Def           : access Entity_Def) return Byte_Sequence is
   begin
      if Entity = Lt_Sequence then
         Is_Std_Entity.all := True;
         return Encoding.Encode (Less_Than_Sign);
      elsif Entity = Gt_Sequence then
         Is_Std_Entity.all := True;
         return Encoding.Encode (Greater_Than_Sign);
      elsif Entity = Amp_Sequence then
         Is_Std_Entity.all := True;
         return Encoding.Encode (Ampersand);
      elsif Entity = Apos_Sequence then
         Is_Std_Entity.all := True;
         return Encoding.Encode (Apostrophe);
      elsif Entity = Quot_Sequence then
         Is_Std_Entity.all := True;
         return Encoding.Encode (Quotation_Mark);
      end if;

      Is_Std_Entity.all := False;

      declare
         Value : Entity_Def := Get (Machine.Entities, Entity);
      begin
         if Value /= Null_Entity then
            Def.all := Value;
            return Value.Str.all;
         end if;
      end;

      if Machine.Feature_External_General_Entities
        and then Machine.Feature_External_Parameter_Entities
      then
         --  ??? Look in the DTD otherwise
         null;
      end if;

      raise No_Such_Entity;
   end Get_Entity_Replacement;

   -------------------
   -- End_Of_Entity --
   -------------------

   procedure End_Of_Entity (Machine   : in out Reader'Class;
                            Last_Read : Unicode.Unicode_Char)
   is
      Initial : Natural := Machine.Entity_Start;
   begin
      --  If first character is '%', keep is in the name
      if Encoding.Read (Machine.Buffer, Initial) = Ampersand then
         Initial := Machine.Entity_Start + Encoding.Width (Ampersand);
      end if;

      Machine.Processing := None;

      declare
         Name : constant Byte_Sequence := Machine.Buffer
           (Initial .. Machine.Buffer_Length);
      begin

         --  Check that the name is correct. We systematically skip the first
         --  character, since it is either Ampersand or Percent_Sign, and is
         --  always invalid in names
         if not Is_Valid_Name
           (Machine.Buffer (Machine.Entity_Start + Encoding.Width (Ampersand)
                            .. Machine.Buffer_Length))
         then
            Fatal_Error
              (Machine, "(4.1) Invalid name '" & Name & "' for an entity");
         end if;

         declare
            Is_Std_Entity : aliased Boolean;
            Def           : aliased Entity_Def;
            Value : constant Byte_Sequence := Get_Entity_Replacement
              (Machine, Name, Is_Std_Entity'Access, Def'Access);
            Input   : String_Input;
            Input_F : File_Input;
            State : constant Automaton_State := Get_Current_State (Machine);
            Current_Id : Natural := 0;
         begin
            Delete_Entity (Machine);

            --  SAX2 gives only one string value for attributes, so we can't
            --  emit a separate event. We simply substibute the entity name
            --  by its value.
            if Machine.Processing_Attribute_Value
              or else Looking_At
               (Machine, Machine.Buffer'First, Attlist_Sequence)
            then
               if Def.External then
                  Fatal_Error
                    (Machine, "(3.1) Attribute values can not reference"
                     & " external entities");
               end if;

               Put_In_Buffer_Force (Machine, Value);

            else
               --  SAX2 says that the characters for the entity must be emitted
               --  as a separate callback, thus we emit what is currently in
               --  the buffer, and then the characters for the entity itself.

               Xml_Text_To_Document (Machine);
               Start_Entity (Machine, Name);

               --  Note that we must also substitute the value of entities
               --  inside the replacement text for the entity. Thus, we call a
               --  recursive main loop that will act on the entity value, but
               --  will not increment the locator of the machine.
               --  This shouldn't be done for standard entities, since the
               --  special meaning of their substitution must be ignored.
               if not Is_Std_Entity then
                  if Debug then
                     Debug_Put (Machine, "Recursive main loop --"
                                & Value & "--");
                  end if;

                  if Machine.Current_Node /= null then
                     Current_Id := Machine.Current_Node.Id;
                  end if;

                  Set_Current_State (Machine, Top_State);

                  if Def.Already_Read then
                     Fatal_Error
                       (Machine, "(4.1) Entity can not reference itself");
                  end if;

                  Def.Already_Read := True;
                  Set (Machine.Entities, Name, Def);

                  if Def.External then
                     --  ??? Should test Feature_External_General_Entities
                     declare
                        URI : constant Byte_Sequence :=
                          Resolve_URI (Machine, Value);
                        Loc : File_Locator;
                        Pub_Id : Byte_Sequence := Get_Public_Id
                          (Machine.Locator.all);
                        Sys_Id : Byte_Sequence := Get_System_Id
                          (Machine.Locator.all);
                     begin
                        Open (URI, Input_F);
                        Copy (Loc, Machine.Locator.all);

                        Set_System_Id (Machine.Locator.all, URI);
                        Set_Public_Id (Machine.Locator.all, URI);
                        Set_Line_Number (Machine.Locator.all, 1);
                        Set_Column_Number (Machine.Locator.all, 1);

                        Machine.Recursive_Depth := Machine.Recursive_Depth + 1;
                        Main_Loop (Machine, Input_F, False);
                        Close (Input_F);

                        Machine.Recursive_Depth := Machine.Recursive_Depth - 1;
                        Copy (Machine.Locator.all, Loc);
                        Set_System_Id (Machine.Locator.all, Sys_Id);
                        Set_Public_Id (Machine.Locator.all, Pub_Id);
                     exception
                        when Name_Error =>
                           Skipped_Entity (Machine, Name);
                           Put_In_Buffer_Force (Machine, Value);
                           Set_Current_State (Machine, State);
                     end;
                  else
                     Open (Value, Encoding, Input);
                     Main_Loop (Machine, Input, False);
                     Close (Input);
                  end if;

                  Def.Already_Read := False;
                  Set (Machine.Entities, Name, Def);

                  if Debug then
                     Debug_Put (Machine, "End of main loop");
                  end if;

                  if Get_Current_State (Machine) /= Top_State
                    or else (Current_Id = 0
                             and then Machine.Current_Node /= null)
                    or else (Machine.Current_Node /= null
                             and then Machine.Current_Node.Id /= Current_Id)
                  then
                     Fatal_Error
                       (Machine, "(4.5) Entity values must be self-contained");
                  end if;

                  Set_Current_State (Machine, State);
               else
                  Put_In_Buffer_Force (Machine, Value);
               end if;

               Xml_Text_To_Document (Machine);
               End_Entity (Machine, Name);
            end if;
         end;

      exception
         when No_Such_Entity =>
            --  ??? Only for standalone documents (WF: Entity declared 4.1)
            --  Name should start with % for parameter entities, or be [dtd]
            --  for the external subset.
            Skipped_Entity (Machine, Name);
            Error (Machine,
                   Create ("(4.1) Undefined entity '" & Name & ''',
                           Machine.Processing_Start));
            Delete_Entity (Machine);
            Put_In_Buffer_Force
              (Machine,
               Encoding.Encode (Ampersand) & Name
               & Encoding.Encode (Semicolon));
      end;
   end End_Of_Entity;

   -----------------------
   -- End_Of_Num_Entity --
   -----------------------

   procedure End_Of_Num_Entity (Machine   : in out Reader'Class;
                                Last_Read : Unicode.Unicode_Char)
   is
      Val      : Unicode_Char := 0;
      C        : Unicode_Char;
      A_Pos    : constant Unicode_Char := Latin_Capital_Letter_A - 10;
      Lc_A_Pos : constant Unicode_Char := Latin_Small_Letter_A - 10;
      Name     : constant Byte_Sequence := Machine.Buffer
        (Machine.Entity_Start + Encoding.Width (Ampersand)
         + Encoding.Width (Number_Sign) .. Machine.Buffer_Length);
      J : Natural := Name'First;

   begin
      if Encoding.Read (Name, J) = Latin_Small_Letter_X then
         J := Name'First + Encoding.Width (Latin_Small_Letter_X);
         while J <= Name'Last loop
            C := Encoding.Read (Name, J);
            J := J + Encoding.Width (C);
            if C in Latin_Capital_Letter_A .. Latin_Capital_Letter_F then
               Val := Val * 16 + C - A_Pos;
            elsif C in Latin_Small_Letter_A .. Latin_Small_Letter_F then
               Val := Val * 16 + C - Lc_A_Pos;
            else
               Val := Val * 16 + C - Digit_Zero;
            end if;
         end loop;
      else
         while J <= Name'Last loop
            C := Encoding.Read (Name, J);
            Val := Val * 10 + C - Digit_Zero;
            J := J + Encoding.Width (C);
         end loop;
      end if;
      Delete_Entity (Machine);

      if Debug then
         Debug_Put
           (Machine,
            "End_Of_Num_Entity --" & Name & "--" & Encoding.Encode (Val));
      end if;

      Put_In_Buffer (Machine, Val);
      Machine.Processing := None;
   end End_Of_Num_Entity;

   --------------------------
   -- Xml_Text_To_Document --
   --------------------------

   procedure Xml_Text_To_Document (Machine : in out Reader'Class) is
      Index : Natural := Machine.Buffer'First;
      Start : Natural := Index;
      Must_Be_Space : Boolean := True;
      C     : Unicode_Char;
   begin
      if Machine.Buffer_Length > 0 then
         if Debug then
            Debug_Put (Machine, "Xml_Text_To_Document "
                       & Machine.Buffer (1 .. Machine.Buffer_Length));
         end if;

         --  If the white spaces must be preserved, we emit one single event.
         --  Otherwise, we have to emit separate events for white space chunks.
         if (Machine.Current_Node /= null and then
             Machine.Current_Node.Space_Handling = Preserve)
           or else Machine.Processing_Attribute_Value
         then
            Characters (Machine, Machine.Buffer (1 .. Machine.Buffer_Length));

         else
            while Index <= Machine.Buffer_Length loop
               C := Encoding.Read (Machine.Buffer, Index);

               if Is_White_Space (C) /= Must_Be_Space then
                  if Index - 1 >= Start then
                     if Must_Be_Space then
                        Ignorable_Whitespace
                          (Machine, Machine.Buffer (Start .. Index - 1));
                     else
                        Characters
                          (Machine, Machine.Buffer (Start .. Index - 1));
                     end if;
                  end if;
                  Must_Be_Space := not Must_Be_Space;
                  Start := Index;
               end if;

               Index := Index + Encoding.Width (C);
            end loop;

            if Index - 1 >= Start then
               if Must_Be_Space then
                  Ignorable_Whitespace
                    (Machine, Machine.Buffer (Start .. Index - 1));
               else
                  Characters (Machine, Machine.Buffer (Start .. Index - 1));
               end if;
            end if;
         end if;

         Machine.Buffer_Length := 0;
      end if;
   end Xml_Text_To_Document;

   ------------------
   -- Start_Of_Tag --
   ------------------

   procedure Start_Of_Tag (Machine   : in out Reader'Class;
                           Last_Read : Unicode.Unicode_Char) is
   begin
      if Debug then
         Debug_Put (Machine, "Start_Of_Tag");
      end if;
      Xml_Text_To_Document (Machine);
   end Start_Of_Tag;

   ----------------------
   -- End_Of_Start_Tag --
   ----------------------

   procedure End_Of_Start_Tag (Machine   : in out Reader'Class;
                               Last_Read : Unicode.Unicode_Char)
   is
      NS : XML_NS;
      Pos : constant Natural := Get_Length (Machine.Attributes);
      Old_Attr : Integer;
      Attr : Attributes_Ptr;
   begin
      if Debug then
         Debug_Put (Machine, "Start_Of_Start_Tag");
      end if;

      --  Report the event
      pragma Assert (Machine.Current_Node /= null);
      pragma Assert (Machine.Current_Node.Name /= null);

      --  Resolve the attributes' namespaces now.
      --  Note that the default namespace doesn't apply to attributes (XML
      --  namespaces specification)
      for J in 0 .. Pos - 1 loop
         if Get_URI (Machine.Attributes, J) = "" then
            Set_Qname
              (Machine.Attributes, J,
               Get_Local_Name (Machine.Attributes, J));

         else
            Find_NS
              (Machine, Machine.Current_Node,
               Get_URI (Machine.Attributes, J), NS);
            Set_URI (Machine.Attributes, J, NS.URI.all);
            Set_Qname
              (Machine.Attributes, J,
               Qname_From_Name
               (NS.Prefix, Get_Local_Name (Machine.Attributes, J)));
         end if;

         --  The attribute name must be unique. Note that we must compare
         --  with the namespaces expanded to their URI (5.3 XML namespaces
         --  specification)
         Old_Attr := Get_Index
           (Machine.Attributes, Get_Qname (Machine.Attributes, J));
         if Old_Attr /= -1 and then Old_Attr < J then
            Fatal_Error
              (Machine, "(3.1) Attributes must have a unique value, for "
               & Get_Qname (Machine.Attributes, J));
         end if;
      end loop;

      --  Resolve the element's namespace now
      Find_NS
        (Machine, Machine.Current_Node, Machine.Current_Node.NS.all, NS);

      --  Add all the default attributes to the element
      --  We shouldn't add an attribute if it was overriden by the user
      Attr := Get (Machine.DTD, Qname_From_Name
                   (NS.Prefix, Machine.Current_Node.Name.all));

      if Attr /= null then
         for J in 0 .. Get_Length (Attr.all) - 1 loop
            --  ??? This could/should be more efficient.
            if Get_Index (Machine.Attributes,
                          Get_URI (Attr.all, J),
                          Get_Local_Name (Attr.all, J)) = -1
            then
               Add_Attribute (Machine.Attributes,
                              Get_URI (Attr.all, J),
                              Get_Local_Name (Attr.all, J),
                              Get_Qname (Attr.all, J),
                              Get_Type (Attr.all, J),
                              Get_Value (Attr.all, J));
            end if;
         end loop;
      end if;

      Start_Element
        (Handler => Machine,
         Namespace_URI => NS.URI.all,
         Local_Name => Machine.Current_Node.Name.all,
         Qname => Qname_From_Name
           (NS.Prefix,  Machine.Current_Node.Name.all),
         Atts => Machine.Attributes);

      Machine.Buffer_Length := 0;
      Clear (Machine.Attributes);
   end End_Of_Start_Tag;

   ---------------------
   -- End_Of_Start_Gi --
   ---------------------

   procedure End_Of_Start_Gi (Machine   : in out Reader'Class;
                              Last_Read : Unicode.Unicode_Char)
   is
      Name : constant Byte_Sequence :=
        Machine.Buffer (1 .. Machine.Buffer_Length);
   begin
      --  (2.3)[5] Check the syntax of the name for the element
      if not Is_Valid_Name (Name) then
         Fatal_Error (Machine, "(2.3) '" & Name & "' is not a valid name");
      end if;

      --  (2.1)[1] A single root element
      if Machine.Current_Node = null then
         Machine.Num_Items := Machine.Num_Items + 1;
         Machine.Num_Elements := Machine.Num_Elements + 1;

         if Machine.Num_Elements /= 1 then
            Fatal_Error
              (Machine,
               "(2.1) Too many children for top-level node, when adding <"
               & Name & ">");
         end if;
      end if;

      --  Create an internal structure to memorize the node
      Machine.Element_Id := Machine.Element_Id + 1;
      Machine.Current_Node := new Element'
        (NS             => Machine.Current_NS,
         Name           => new Byte_Sequence' (Name),
         Namespaces     => null,
         Space_Handling => Ignorable,
         Id             => Machine.Element_Id,
         Parent         => Machine.Current_Node);

      Machine.Buffer_Length := 0;
      Machine.Current_NS := new String' ("");
   end End_Of_Start_Gi;

   -------------------
   -- End_Of_Prefix --
   -------------------

   procedure End_Of_Prefix (Machine   : in out Reader'Class;
                            Last_Read : Unicode.Unicode_Char)
   is
      Name : constant Byte_Sequence :=
        Machine.Buffer (1 .. Machine.Buffer_Length);
   begin
      if Debug then
         Debug_Put (Machine, "End_Of_Prefix " & Name);
      end if;
      Free (Machine.Current_NS);
      Machine.Current_NS := new String' (Name);
      Machine.Buffer_Length := 0;
   end End_Of_Prefix;

   -------------------
   -- Cancel_Entity --
   -------------------

   procedure Cancel_Entity
     (Machine : in out Reader'Class; Last_Read : Unicode_Char)
   is
      Name : constant Byte_Sequence := Machine.Buffer
        (Machine.Entity_Start .. Machine.Buffer_Length);
   begin
      pragma Assert (Machine.Processing = Entity);
      if Looking_At (Machine, Machine.Buffer'First, Entity_Sequence) then
         Delete_Entity (Machine);
         Put_In_Buffer_Force (Machine, Name);
         Put_In_Buffer_Force (Machine, Last_Read);
         Machine.Processing := None;
         Set_Current_State (Machine, Exit_Sub_Automaton);
      else
         Put_In_Buffer_Force (Machine, Last_Read);
      end if;
   end Cancel_Entity;

   -------------------
   -- Put_In_Buffer --
   -------------------

   procedure Put_In_Buffer (Machine   : in out Reader'Class;
                            Last_Read : Unicode.Unicode_Char) is
   begin
      --  Check rule 2.4 about character validity (note that this can not
      --  be done in Main_Loop, since numeric entities have not been
      --  substituted then)
      if not (Is_White_Space (Last_Read)
              or else Last_Read in Space .. 16#D7FF#
              or else Last_Read in 16#E000# .. 16#FFFD#
              or else Last_Read in 16#10000# .. 16#10FFFF#)
      then
         Machine.Processing := None;
         Fatal_Error (Machine, "(2.2) Illegal character (code"
                      & Unicode_Char'Image (Last_Read) & ")");
      end if;

      --  Check the nesting of square brackets in the DTD
      if Machine.Parsing_DTD > 1
        and then Last_Read = Closing_Square_Bracket
      then
         Machine.Parsing_DTD := Machine.Parsing_DTD - 1;

      --  ??? Nesting of [...] isn't authorized in DTDs. This might still
      --  be needed when parsing conditional sections, though
--        elsif Machine.Parsing_DTD > 1
--          and then Last_Read = Opening_Square_Bracket
--        then
--           Machine.Parsing_DTD := Machine.Parsing_DTD + 1;

      --  Only white spaces can be encountered at top-level
      --  And in fact we simply ignore then.
      elsif Machine.Current_Node = null
        and then Get_Current_State (Machine) = Top_State
        and then Machine.Processing /= Entity
      then
         if not Is_White_Space (Last_Read) then
            Machine.Processing := None;
            Fatal_Error (Machine, "(2.1) Non-white space found at top level");
         end if;

      elsif Machine.Parsing_DTD = 1 then
         Machine.Processing := None;
         Fatal_Error (Machine, "(2.8) Invalid character in the DTD");

      else
         Put_In_Buffer_Force (Machine, Encoding.Encode (Last_Read));
      end if;
   end Put_In_Buffer;

   -------------------------
   -- Put_In_Buffer_Space --
   -------------------------

   procedure Put_In_Buffer_Space
     (Machine   : in out Reader'Class;
      Last_Read : Unicode.Unicode_Char)
   is
   begin
      Put_In_Buffer_Force (Machine, Space);
      Put_In_Buffer (Machine, Last_Read);
   end Put_In_Buffer_Space;

   -------------------------
   -- Put_In_Buffer_Force --
   -------------------------

   procedure Put_In_Buffer_Force
     (Machine : in out Reader'Class; Last_Read : Unicode_Char) is
   begin
      --  Check rule 2.4 about character validity (note that this can not
      --  be done in Main_Loop, since numeric entities have not been
      --  substituted then)
      if not (Is_White_Space (Last_Read)
              or else Last_Read in Space .. 16#D7FF#
              or else Last_Read in 16#E000# .. 16#FFFD#
              or else Last_Read in 16#10000# .. 16#10FFFF#)
      then
         Machine.Processing := None;
         Fatal_Error (Machine, "(2.2) Illegal character (code"
                      & Unicode_Char'Image (Last_Read) & ")");
      end if;

      Put_In_Buffer_Force (Machine, Encoding.Encode (Last_Read));
   end Put_In_Buffer_Force;

   -------------------------
   -- Put_In_Buffer_Force --
   -------------------------

   procedure Put_In_Buffer_Force
     (Machine   : in out Reader'Class; S : Byte_Sequence) is
   begin
      --  ??? Should handle buffer overflow
      pragma Assert (Machine.Buffer_Length + S'Length <= Machine.Buffer'Last);
      Machine.Buffer
        (Machine.Buffer_Length + 1 .. Machine.Buffer_Length + S'Length) := S;
      Machine.Buffer_Length := Machine.Buffer_Length + S'Length;
   end Put_In_Buffer_Force;

   ---------------------
   -- Check_Cdata_End --
   ---------------------

   procedure Check_Cdata_End (Machine   : in out Reader'Class;
                              Last_Read : Unicode.Unicode_Char)
   is
      W : constant Natural := Encoding.Width (Right_Square_Bracket);
   begin
      --  Do we have the invalid sequence ]]> outside of a CDATA ?
      if Machine.Buffer_Length >= 2 * W
        and then Last_Read = Greater_Than_Sign
        and then Encoding.Read
         (Machine.Buffer, Machine.Buffer_Length - W + 1) = Right_Square_Bracket
        and then Encoding.Read
           (Machine.Buffer, Machine.Buffer_Length - 2 * W + 1) =
           Right_Square_Bracket
      then
         Fatal_Error (Machine, "(2.4) Text may not contain the literal ']]>'");

      --  Do we have the end of the DTD ?
      elsif Last_Read = Greater_Than_Sign
        and then Machine.Parsing_DTD >= 1
      then
         End_Of_Declaration (Machine, Last_Read);
         return;
      end if;

      Put_In_Buffer (Machine, Last_Read);
   end Check_Cdata_End;

   -------------------------------------------------
   -- Put_In_Buffer_No_Multiple_Space_Nor_Newline --
   -------------------------------------------------

   procedure Put_In_Buffer_No_Multiple_Space_Nor_Newline
     (Machine   : in out Reader'Class;
      Last_Read : Unicode.Unicode_Char) is
   begin
      if Is_White_Space (Last_Read) then
         --  Don't insert at the beginning of the attribute, nor if we already
         --  have a space character already.
         --  ??? This assumes that Space is encoded on a single byte...
         if Machine.Buffer_Length >= Machine.Buffer'First
           and then not Is_White_Space (Encoding.Read
             (Machine.Buffer, Machine.Buffer_Length))
         then
            Put_In_Buffer_Force (Machine, Space);
         end if;
      else
         Put_In_Buffer_Force (Machine, Last_Read);
      end if;

   exception
      --  ??? Temporary kludge when Space is not encoded on a single byte
      when Invalid_Encoding =>
         Put_In_Buffer_Force (Machine, Last_Read);
   end Put_In_Buffer_No_Multiple_Space_Nor_Newline;

   -------------------------------------
   -- Put_In_Buffer_No_Multiple_Space --
   -------------------------------------

   procedure Put_In_Buffer_No_Multiple_Space
     (Machine   : in out Reader'Class;
      Last_Read : Unicode.Unicode_Char) is
   begin
      if Last_Read = Space or else Last_Read = Horizontal_Tabulation then
         if Machine.Buffer_Length >= Machine.Buffer'First
           and then Encoding.Read
              (Machine.Buffer, Machine.Buffer_Length) /= Space
         then
            Put_In_Buffer_Force (Machine, Space);
         end if;
      else
         Put_In_Buffer (Machine, Last_Read);
      end if;
   end Put_In_Buffer_No_Multiple_Space;

   ---------------
   -- Xml_Error --
   ---------------

   procedure Xml_Error (Machine   : in out Reader'Class;
                        Last_Read : Unicode.Unicode_Char) is
   begin
      Fatal_Error
        (Machine, "Invalid character '"
         & Encoding.Encode (Last_Read) & "'");
      --  Could also be because we saw a '<' in a text context, when it should
      --  have been quoted
   end Xml_Error;

   -----------------------------
   -- Must_Have_Space_In_Attr --
   -----------------------------

   procedure Must_Have_Space_In_Attr
     (Machine   : in out Reader'Class;
      Last_Read : Unicode.Unicode_Char) is
   begin
      Fatal_Error (Machine, "(3.1) Attributes must be separated by spaces");
   end Must_Have_Space_In_Attr;

   --------------------------------
   -- Must_Have_Equal_After_Attr --
   --------------------------------

   procedure Must_Have_Equal_After_Attr
     (Machine   : in out Reader'Class;
      Last_Read : Unicode.Unicode_Char) is
   begin
      Fatal_Error (Machine, "(3.1) Attribute names must be followed by '='");
   end Must_Have_Equal_After_Attr;

   ---------------------
   -- Attr_Need_Value --
   ---------------------

   procedure Attr_Need_Value
     (Machine   : in out Reader'Class;
      Last_Read : Unicode.Unicode_Char)
   is
      Name : constant Byte_Sequence :=
        Machine.Buffer (1 .. Machine.Buffer_Length);
   begin
      --  Test whether what we have so far for the name is valid, since we
      --  should always report the first error that happened
      if not Is_Valid_Name (Name) then
         Fatal_Error (Machine, "(2.3) '" & Name
                      & "' is not a valid attribute name");
      end if;

      --  Else there was indeed no value, and we report it
      Fatal_Error (Machine, "(3.1) Attributes must have an explicit value");
   end Attr_Need_Value;

   --------------------------------
   -- Illegal_Char_In_Attr_Value --
   --------------------------------

   procedure Illegal_Char_In_Attr_Value
     (Machine   : in out Reader'Class;
      Last_Read : Unicode.Unicode_Char) is
   begin
      Fatal_Error (Machine, "(2.3) Attribute values may not contain literal '"
                   & Encoding.Encode (Last_Read) & "'");
   end Illegal_Char_In_Attr_Value;

   --------------------------
   -- Invalid_Ampersand --
   --------------------------

   procedure Invalid_Ampersand
     (Machine   : in out Reader'Class;
      Last_Read : Unicode.Unicode_Char) is
   begin
      Fatal_Error
        (Machine,
         "(2.3, 2.4) literal '&' invalid in this context. Use &amp; instead");
   end Invalid_Ampersand;

   ------------------------------
   -- Seen_Quote_In_Attr_Value --
   ------------------------------

   procedure Seen_Quote_In_Attr_Value
     (Machine   : in out Reader'Class;
      Last_Read : Unicode.Unicode_Char) is
   begin
      Put_In_Buffer (Machine, Last_Read);
      Machine.Processing := Attr_Value;
      Copy (Machine.Processing_Start.all, Machine.Locator.all);
   end Seen_Quote_In_Attr_Value;

   ----------------------
   -- Attr_Quote_Error --
   ----------------------

   procedure Attr_Quote_Error (Machine   : in out Reader'Class;
                               Last_Read : Unicode.Unicode_Char) is
   begin
      Fatal_Error (Machine, "(2.3) Attribute values must be quoted");
   end Attr_Quote_Error;

   -----------------------------
   -- End_Of_Start_Gi_And_Tag --
   -----------------------------

   procedure End_Of_Start_Gi_And_Tag (Machine : in out Reader'Class;
                                      Last_Read : Unicode.Unicode_Char) is
   begin
      End_Of_Start_Gi (Machine, Last_Read);
      End_Of_Start_Tag (Machine, Last_Read);
   end End_Of_Start_Gi_And_Tag;

   --------------------
   -- End_Of_Comment --
   --------------------

   procedure End_Of_Comment (Machine   : in out Reader'Class;
                             Last_Read : Unicode.Unicode_Char) is
   begin
      --  Report the comment (and ignore the ! character at the beginning
      --  of the buffer).
      Comment (Machine, Machine.Buffer (2 .. Machine.Buffer_Length));

      Machine.Processing := None;
      Machine.Buffer_Length := 0;

      --  Register the PI if at the top-level, so that we can correctly
      --  report that <?xml?> must be first in the document
      if Machine.Current_Node = null then
         Machine.Num_Items := Machine.Num_Items + 1;
      end if;
   end End_Of_Comment;

   ----------------------
   -- Start_Of_Comment --
   ----------------------

   procedure Start_Of_Comment (Machine   : in out Reader'Class;
                               Last_Read : Unicode.Unicode_Char) is
   begin
      --  Point to the opening < instead of the current <!--, necessarily on
      --  the same line
      Machine.Processing := Comment;
      Copy (Machine.Processing_Start.all, Machine.Locator.all);
      Set_Column_Number (Machine.Processing_Start.all,
                         Get_Column_Number (Machine.Processing_Start.all) - 3);
   end Start_Of_Comment;

   ----------------------------
   -- Double_Dash_In_Comment --
   ----------------------------

   procedure Double_Dash_In_Comment
     (Machine   : in out Reader'Class;
      Last_Read : Unicode.Unicode_Char) is
   begin
      Machine.Processing := None;
      Fatal_Error (Machine, "(2.5) '--' is not valid inside comments");
   end Double_Dash_In_Comment;

   --------------------------
   -- Start_Of_Declaration --
   --------------------------

   procedure Start_Of_Declaration
     (Machine   : in out Reader'Class;
      Last_Read : Unicode.Unicode_Char) is
   begin
      Machine.Declaration_Start := Machine.Buffer_Length + 1;
      Put_In_Buffer (Machine, Last_Read);
      Machine.Processing := Declaration;

      --  Point to the opening < instead of the current !, necessarily on the
      --  same line
      Copy (Machine.Processing_Start.all, Machine.Locator.all);
      Set_Column_Number (Machine.Processing_Start.all,
                         Get_Column_Number (Machine.Processing_Start.all) - 1);
   end Start_Of_Declaration;

   ---------------
   -- Skip_Name --
   ---------------

   procedure Skip_Name
     (Str        : Byte_Sequence;
      Index      : in out Natural;
      Name_Start : out Natural;
      Name_End   : out Natural;
      Is_Nmtoken : Boolean := False)
   is
      C : Unicode_Char := Encoding.Read (Str, Index);
   begin
      Name_Start := Index;
      if not Is_Nmtoken
        and then not (C = Colon
                      or else C = Spacing_Underscore
                      or else Is_Letter (C))
      then
         raise Invalid_Character;
      end if;
      Index := Index + Encoding.Width (C);

      while Index <= Str'Last loop
         C := Encoding.Read (Str, Index);
         exit when not Is_Name_Char (C);
         Index := Index + Encoding.Width (C);
      end loop;

      Name_End := Index - 1;
   end Skip_Name;

   -------------------------
   -- Parse_Element_Model --
   -------------------------

   function Parse_Element_Model (Model : Unicode.CES.Byte_Sequence)
      return Element_Model_Ptr
   is
      Parsed : Element_Model_Ptr;
      Index : Natural := Model'First;
   begin
      Parse_Element_Model (Model, Index, Parsed);

      --  Error if there are some remaining characters at the end
      if Index < Model'Last then
         Free (Parsed);
         raise Invalid_Content_Model;
      end if;

      return Parsed;
   end Parse_Element_Model;

   -------------------------
   -- Parse_Element_Model --
   -------------------------

   procedure Parse_Element_Model
     (Model  : Unicode.CES.Byte_Sequence;
      Index  : in out Natural;
      Result : out Element_Model_Ptr;
      Nmtokens : Boolean := False)
   is
      --  ??? Would be nice to get rid of this hard-coded limitation in stacks
      Stack_Size : constant Natural := 64;
      Operand_Stack : Element_Model_Array (1 .. Stack_Size);
      Operand_Index : Natural := Operand_Stack'First;
      Operator_Stack : array (1 .. Stack_Size) of Unicode_Char;
      Operator_Index : Natural := Operator_Stack'First;
      Num_Items : Positive;
      Current_Item, Current_Operand : Natural;
      Expect_Operator : Boolean := True;
      C : Unicode_Char;
      Start_Sub, End_Sub : Natural;
      M : Element_Model_Ptr;

   begin
      Skip_Spaces (Model, Index);
      C := Encoding.Read (Model, Index);
      if C /= Opening_Parenthesis then
         raise Invalid_Content_Model;
      end if;

      loop
         --  Process the operator
         case C is
            when Opening_Parenthesis =>
               Operator_Stack (Operator_Index) := C;
               Operator_Index := Operator_Index + 1;
               Expect_Operator := False;
               Index := Index + Encoding.Width (C);

            when Closing_Parenthesis =>
               Num_Items := 1;
               Current_Item := Operator_Index - 1;
               Current_Operand := Operand_Index - 1;
               while Current_Item >= Operator_Stack'First
                 and then Operator_Stack (Current_Item) /= Opening_Parenthesis
               loop
                  if Operator_Stack (Current_Item) /= Comma
                    and then Operator_Stack (Current_Item) /= Vertical_Line
                  then
                     raise Invalid_Content_Model;
                  end if;

                  Num_Items := Num_Items + 1;
                  Current_Item := Current_Item - 1;
                  Current_Operand := Current_Operand - 1;
               end loop;
               if Current_Item < Operator_Stack'First then
                  raise Invalid_Content_Model;
               end if;

               if Operator_Stack (Operator_Index - 1) = Comma then
                  M := new Element_Model (Sequence);
               else
                  M := new Element_Model (Any_Of);
               end if;
               M.List := new Element_Model_Array (1 .. Num_Items);
               for J in Current_Operand .. Operand_Index - 1 loop
                  M.List (J - Current_Operand + 1) := Operand_Stack (J);
               end loop;
               Operand_Index := Current_Operand + 1;
               Operand_Stack (Current_Operand) := M;
               Operator_Index := Current_Item;
               Expect_Operator := False;
               Index := Index + Encoding.Width (C);

            when Comma | Vertical_Line =>
               if Operator_Index = Operator_Stack'First
                 or else
                 (Operator_Stack (Operator_Index - 1) /= C
                  and then
                  Operator_Stack (Operator_Index - 1) /= Opening_Parenthesis)
               then
                  raise Invalid_Content_Model;
               end if;
               Operator_Stack (Operator_Index) := C;
               Operator_Index := Operator_Index + 1;
               Expect_Operator := False;
               Index := Index + Encoding.Width (C);

            when Star =>
               if Operand_Index = Operand_Stack'First then
                  raise Invalid_Content_Model;
               end if;
               Operand_Stack (Operand_Index - 1) := new Element_Model'
                 (Repeat, 0, Positive'Last, Operand_Stack (Operand_Index - 1));
               Expect_Operator := False;
               Index := Index + Encoding.Width (C);

            when Plus_Sign =>
               if Operand_Index = Operand_Stack'First then
                  raise Invalid_Content_Model;
               end if;
               Operand_Stack (Operand_Index - 1) := new Element_Model'
                 (Repeat, 1, Positive'Last, Operand_Stack (Operand_Index - 1));
               Expect_Operator := False;
               Index := Index + Encoding.Width (C);

            when Question_Mark =>
               if Operand_Index = Operand_Stack'First then
                  raise Invalid_Content_Model;
               end if;
               Operand_Stack (Operand_Index - 1) := new Element_Model'
                 (Repeat, 0, 1, Operand_Stack (Operand_Index - 1));
               Expect_Operator := False;
               Index := Index + Encoding.Width (C);

            when others =>
               if Expect_Operator then
                  raise Invalid_Content_Model;
               end if;
               Expect_Operator := True;

               --  #PCDATA can only be the first element of a choice list
               --  ??? Note that in the case the Choice model can only be a
               --  list of names, not a parenthesis expression.
               if Operator_Index > Operator_Stack'First
                 and then Operator_Stack (Operator_Index - 1)
                 = Opening_Parenthesis
                 and then Index + Pcdata_Sequence'Length - 1 <= Model'Last
                 and then Model (Index .. Index + Pcdata_Sequence'Length - 1)
                 = Pcdata_Sequence
               then
                  Index := Index + Pcdata_Sequence'Length;
                  Operand_Stack (Operand_Index) :=
                    new Element_Model (Character_Data);
                  Operand_Index := Operand_Index + 1;
               else
                  Skip_Name (Model, Index, Start_Sub, End_Sub, Nmtokens);
                  Operand_Stack (Operand_Index) :=
                    new Element_Model (Element_Ref);
                  Operand_Stack (Operand_Index).Name :=
                    new Byte_Sequence' (Model (Start_Sub .. End_Sub));
                  Operand_Index := Operand_Index + 1;
               end if;
         end case;

         Skip_Spaces (Model, Index);

         exit when Index > Model'Last;

         C := Encoding.Read (Model, Index);
         exit when Operator_Index = Operator_Stack'First
           and then Operand_Index = Operand_Stack'First + 1
           and then C /= Star
           and then C /= Plus_Sign
           and then C /= Question_Mark;
      end loop;

      if Operator_Index /= Operator_Stack'First
        or else Operand_Index /= Operand_Stack'First + 1
      then
         raise Invalid_Content_Model;
      end if;

      Result := Operand_Stack (Operand_Stack'First);

   exception
      when others =>
         for J in Operand_Stack'First .. Operand_Index - 1 loop
            Free (Operand_Stack (J));
         end loop;
         raise;
   end Parse_Element_Model;

   ---------------
   -- To_String --
   ---------------

   function To_String (Model : Element_Model)
      return Unicode.CES.Byte_Sequence
   is
      Str : Unbounded_String;
   begin
      case Model.Content is
         when Character_Data =>
            return Pcdata_Sequence;

         when Empty =>
            return Empty_Sequence;

         when Anything =>
            return Any_Sequence;

         when Element_Ref =>
            return Model.Name.all;

         when Any_Of | Sequence =>
            for J in Model.List'Range loop
               if Model.List (J).Content = Character_Data
                 and then (Model.Content = Sequence
                           or else J /= Model.List'First)
               then
                  raise Invalid_Content_Model;
               end if;

               if Model.List (J).Content = Anything
                 or else Model.List (J).Content = Empty
               then
                  raise Invalid_Content_Model;
               end if;

               Append (Str, To_String (Model.List (J).all));
               if J /= Model.List'Last then
                  if Model.Content = Any_Of then
                     Append (Str, Encoding.Encode (Vertical_Line));
                  else
                     Append (Str, Encoding.Encode (Comma));
                  end if;
               end if;
            end loop;
            return Encoding.Encode (Opening_Parenthesis)
              & To_String (Str) & Encoding.Encode (Closing_Parenthesis);

         when Repeat =>
            if Model.Elem.Content = Anything
              or else Model.Elem.Content = Empty
            then
               raise Invalid_Content_Model;
            end if;

            if Model.Min = 0 and then Model.Max = Positive'Last then
               return To_String (Model.Elem.all) & Encoding.Encode (Star);
            elsif Model.Min = 0 and then Model.Max = 1 then
               return To_String (Model.Elem.all)
                 & Encoding.Encode (Question_Mark);
            elsif Model.Min = 1 and then Model.Max = Positive'Last then
               return To_String (Model.Elem.all) & Encoding.Encode (Plus_Sign);
            else
               raise Invalid_Content_Model;
            end if;
      end case;
   end To_String;

   ----------
   -- Free --
   ----------

   procedure Free (Model : in out Element_Model_Ptr) is
      procedure Free is new Unchecked_Deallocation
        (Element_Model_Array, Element_Model_Array_Ptr);
      procedure Internal is new Unchecked_Deallocation
        (Element_Model, Element_Model_Ptr);
   begin
      case Model.Content is
         when Character_Data | Anything | Empty => null;
         when Element_Ref =>
            Free (Model.Name);
         when Any_Of | Sequence =>
            for J in Model.List'Range loop
               Free (Model.List (J));
            end loop;
            Free (Model.List);
         when Repeat =>
            Free (Model.Elem);
      end case;
      Internal (Model);
   end Free;

   --------------------------------
   -- End_Of_Element_Declaration --
   --------------------------------

   procedure End_Of_Element_Declaration (Machine : in out Reader'Class) is
      Index : Natural := Machine.Buffer'First + Element_Sequence'Length;
      Name_Start, Name_End : Natural;
      Model_Start, Model_End : Natural;
      Model : Element_Model_Ptr;

   begin
      Skip_Spaces (Machine.Buffer, Index);
      Skip_Name (Machine.Buffer (Index .. Machine.Buffer_Length), Index,
                 Name_Start, Name_End);
      if not Is_White_Space (Encoding.Read (Machine.Buffer, Index)) then
         Fatal_Error (Machine, "(4.2) Invalid character in name");
      end if;
      Skip_Spaces (Machine.Buffer, Index);

      --  Is model EMPTY (following by space or end-of-string) ?
      if Looking_At (Machine, Index, Empty_Sequence) then
         Model_Start := Index;
         Index := Index + Empty_Sequence'Length;
         Model_End := Index - 1;
         Model := new Element_Model (Empty);

      --  Is model ANY (following by space or end-of-string) ?
      elsif Looking_At (Machine, Index, Any_Sequence) then
         Model_Start := Index;
         Index := Index + Any_Sequence'Length;
         Model_End := Index - 1;
         Model := new Element_Model (Anything);

      --  Is the model a parenthesis expression ?
      else
         Model := Parse_Element_Model
           (Machine.Buffer (Index .. Machine.Buffer_Length));
      end if;

      if Model.Content = Empty or else Model.Content = Anything then
         Skip_Spaces (Machine.Buffer, Index);
         if Index <= Machine.Buffer_Length then
            Fatal_Error
              (Machine, "(3.2) Unexpected characters in declaration");
         end if;
      end if;

      Element_Decl
        (Machine,
         Machine.Buffer (Name_Start .. Name_End),
         To_String (Model.all),
         Model);
      Free (Model);

   exception
      when Invalid_Content_Model =>
         Fatal_Error
           (Machine, "(3.2) Invalid model for <!ELEMENT> declaration");
   end End_Of_Element_Declaration;

   ---------------------------------
   -- End_Of_Notation_Declaration --
   ---------------------------------

   procedure End_Of_Notation_Declaration (Machine : in out Reader'Class) is
      Index : Natural := Machine.Buffer'First + Notation_Sequence'Length;
      Name_Start, Name_End : Natural;
      System_Start, System_End : Natural;
      Public_Start, Public_End : Natural;
   begin
      Skip_Spaces (Machine.Buffer, Index);
      Skip_Name (Machine.Buffer (Index .. Machine.Buffer_Length), Index,
                 Name_Start, Name_End);
      Parse_External_Id
        (Machine, Index, Public_Start, Public_End, System_Start, System_End);
      Notation_Decl
        (Machine,
         Machine.Buffer (Name_Start .. Name_End),
         Machine.Buffer (Public_Start .. Public_End),
         Machine.Buffer (System_Start .. System_End));
   end End_Of_Notation_Declaration;

   --------------------------------
   -- End_Of_Attlist_Declaration --
   --------------------------------

   procedure End_Of_Attlist_Declaration (Machine : in out Reader'Class) is
      Index : Natural := Machine.Buffer'First + Attlist_Sequence'Length;
      Element_Start, Element_End : Natural;
      Name_Start, Name_End : Natural;
      Type_Start, Type_End : Natural;
      Default_Type_Start, Default_Type_End : Natural;
      Default_Start, Default_End : Natural;
      Model : Element_Model_Ptr;
      Has_Notation : Boolean := False;
      Attr : Attributes_Ptr;
   begin
      Skip_Spaces (Machine.Buffer, Index);
      Skip_Name (Machine.Buffer (Index .. Machine.Buffer_Length), Index,
                 Element_Start, Element_End);

      while Index < Machine.Buffer_Length loop
         Skip_Spaces (Machine.Buffer, Index);
         exit when Index >= Machine.Buffer_Length;

         Skip_Name (Machine.Buffer (Index .. Machine.Buffer_Length), Index,
                    Name_Start, Name_End);
         Skip_Spaces (Machine.Buffer, Index);
         Type_Start := Index;

         if Looking_At (Machine, Index, Cdata_Attlist_Sequence) then
            Index := Index + Cdata_Attlist_Sequence'Length;
         elsif Looking_At (Machine, Index, Idrefs_Sequence) then
            Index := Index + Idrefs_Sequence'Length;
         elsif Looking_At (Machine, Index, Idref_Sequence) then
            Index := Index + Idref_Sequence'Length;
         elsif Looking_At (Machine, Index, Id_Sequence) then
            Index := Index + Id_Sequence'Length;
         elsif Looking_At (Machine, Index, Entities_Sequence) then
            Index := Index + Entities_Sequence'Length;
         elsif Looking_At (Machine, Index, Entity_Attlist_Sequence) then
            Index := Index + Entity_Attlist_Sequence'Length;
         elsif Looking_At (Machine, Index, Nmtokens_Sequence) then
            Index := Index + Nmtokens_Sequence'Length;
         elsif Looking_At (Machine, Index, Nmtoken_Sequence) then
            Index := Index + Nmtoken_Sequence'Length;
         else
            if Looking_At (Machine, Index, Notation_Attlist_Sequence) then
               Index := Index + Notation_Attlist_Sequence'Length;
               Skip_Spaces
                 (Machine.Buffer (Index .. Machine.Buffer_Length), Index);
               Has_Notation := True;
            end if;

            if Encoding.Read (Machine.Buffer, Index) = Opening_Parenthesis then
               Parse_Element_Model
                 (Machine.Buffer (Index .. Machine.Buffer_Length),
                  Index, Model,
                 Nmtokens => not Has_Notation);

               if Model.Content /= Any_Of then
                  Free (Model);
                  Fatal_Error
                    (Machine,
                     "(3.3.1) Expecting an enumeration for attribute");
               end if;

               for J in Model.List'Range loop
                  if Model.List (J).Content /= Element_Ref then
                     Free (Model);
                     Fatal_Error
                       (Machine,
                        "(3.3.1) Expecting names only in an enumeration");
                  end if;
               end loop;

            else
               Fatal_Error (Machine, "(3.3.1) Invalid type for the attribute");
            end if;
         end if;

         Type_End := Index - 1;
         Skip_Spaces (Machine.Buffer, Index);

         Default_Type_Start := Index;
         Default_Start := Index;
         Default_End := Index - 1;
         if Looking_At (Machine, Index, Required_Sequence) then
            Index := Index + Required_Sequence'Length;
            Default_Type_End := Index - 1;
         elsif Looking_At (Machine, Index, Implied_Sequence) then
            Index := Index + Implied_Sequence'Length;
            Default_Type_End := Index - 1;
         else
            if Looking_At (Machine, Index, Fixed_Sequence) then
               Index := Index + Fixed_Sequence'Length;
               Default_Type_End := Index - 1;
               Skip_Spaces (Machine.Buffer, Index);
            else
               Default_Type_End := Default_Type_Start - 1;
            end if;

            Skip_String
              (Machine.Buffer (Index .. Machine.Buffer_Length),
               Index, Machine, Default_Start, Default_End,
               "(3.3.2) Expecting a default value for the attribute");

            --  Normalize the value: ignore any leading white space
            Skip_Spaces (Machine.Buffer (Default_Start .. Default_End),
                         Default_Start);

            --  Remove trailing spaces
            declare
               C : Unicode_Char;
               W : Natural;
               Tmp_Index : Natural := Default_Start;
            begin
               while Tmp_Index <= Default_End loop
                  C := Encoding.Read (Machine.Buffer, Tmp_Index);
                  W := Encoding.Width (C);
                  Tmp_Index := Tmp_Index + W;
               end loop;

               if Tmp_Index >= Default_Start + W
                 and then Is_White_Space (C)
               then
                  Default_End := Default_End - W;
               end if;
            end;
         end if;

         Attr := Get
           (Machine.DTD, Machine.Buffer (Element_Start .. Element_End));
         if Attr = null then
            Attr := new Attributes_Impl;
         end if;

         if Model /= null and then Has_Notation then
            Attribute_Decl
              (Machine,
               Machine.Buffer (Element_Start .. Element_End),
               Machine.Buffer (Name_Start .. Name_End),
               Notation_Attlist_Sequence & Encoding.Encode (Space)
               & To_String (Model.all),
               Machine.Buffer (Default_Type_Start .. Default_Type_End),
               Machine.Buffer (Default_Start .. Default_End));
            if Machine.Buffer (Default_Type_Start .. Default_Type_End) /=
              Implied_Sequence
            then
               Add_Attribute
                 (Attributes_Impl'Class (Attr.all),
                  "", --  ???
                  Machine.Buffer (Name_Start .. Name_End),
                  Machine.Buffer (Name_Start .. Name_End),
                  Notation_Attlist_Sequence & Encoding.Encode (Space)
                  & To_String (Model.all),
                  Machine.Buffer (Default_Start .. Default_End));
            end if;

            Free (Model);

         elsif Model /= null then
            Attribute_Decl
              (Machine,
               Machine.Buffer (Element_Start .. Element_End),
               Machine.Buffer (Name_Start .. Name_End),
               To_String (Model.all),
               Machine.Buffer (Default_Type_Start .. Default_Type_End),
               Machine.Buffer (Default_Start .. Default_End));
            if Machine.Buffer (Default_Type_Start .. Default_Type_End) /=
              Implied_Sequence
            then
               Add_Attribute
                 (Attributes_Impl'Class (Attr.all),
                  "",  --  ???
                  Machine.Buffer (Name_Start .. Name_End),
                  Machine.Buffer (Name_Start .. Name_End),
                  To_String (Model.all),
                  Machine.Buffer (Default_Start .. Default_End));
            end if;
            Free (Model);

         else
            Attribute_Decl
              (Machine,
               Machine.Buffer (Element_Start .. Element_End),
               Machine.Buffer (Name_Start .. Name_End),
               Machine.Buffer (Type_Start .. Type_End),
               Machine.Buffer (Default_Type_Start .. Default_Type_End),
               Machine.Buffer (Default_Start .. Default_End));
            if Machine.Buffer (Default_Type_Start .. Default_Type_End) /=
              Implied_Sequence
            then
               Add_Attribute
                 (Attributes_Impl'Class (Attr.all),
                  "",  --  ???
                  Machine.Buffer (Name_Start .. Name_End),
                  Machine.Buffer (Name_Start .. Name_End),
                  Machine.Buffer (Type_Start .. Type_End),
                  Machine.Buffer (Default_Start .. Default_End));
            end if;
         end if;

         Set (Machine.DTD,
              Machine.Buffer (Element_Start .. Element_End),
              Attr);
      end loop;
   end End_Of_Attlist_Declaration;

   -------------------------------
   -- End_Of_Entity_Declaration --
   -------------------------------

   procedure End_Of_Entity_Declaration (Machine : in out Reader'Class) is
      Index : Natural := Machine.Buffer'First + Entity_Sequence'Length;
      Prefix_Start, Prefix_End : Natural;
      Name_Start, Name_End : Natural;
      Value_Start, Value_End : Natural;
      System_Start, System_End : Natural;
      Ndata_Start, Ndata_End : Natural;
      C : Unicode_Char;
   begin
      Skip_Spaces (Machine.Buffer, Index);
      Prefix_Start := Index;
      if Encoding.Read (Machine.Buffer, Index) = Percent_Sign then
         Index := Index + Encoding.Width (Percent_Sign);
         Prefix_End := Index - 1;
         if not Is_White_Space (Encoding.Read (Machine.Buffer, Index)) then
            Fatal_Error (Machine, "(4.2) Expecting space character after %");
         end if;
         Skip_Spaces (Machine.Buffer, Index);
      else
         Prefix_End := Index - 1;
      end if;

      Skip_Name (Machine.Buffer (Index .. Machine.Buffer_Length), Index,
                 Name_Start, Name_End);
      if not Is_White_Space (Encoding.Read (Machine.Buffer, Index)) then
         Fatal_Error (Machine, "(4.2) Must specify a value for the entity");
      end if;

      Skip_Spaces
        (Machine.Buffer (1 .. Machine.Buffer_Length), Index);
      Skip_String (Machine.Buffer (1 .. Machine.Buffer_Length),
                   Index, Machine, Value_Start, Value_End, "");

      --  Only report the first definition
      if Get (Machine.Entities,
              Machine.Buffer (Prefix_Start .. Prefix_End)
              & Machine.Buffer (Name_Start .. Name_End)) = Null_Entity
      then

         --  ??? Shouldn't report the second declaration of an entity...
         if Value_Start /= Index then
            Internal_Entity_Decl
              (Machine,
               Machine.Buffer (Prefix_Start .. Prefix_End)
               & Machine.Buffer (Name_Start .. Name_End),
               Machine.Buffer (Value_Start .. Value_End));
            Set
              (Machine.Entities,
               Machine.Buffer (Prefix_Start .. Prefix_End)
               & Machine.Buffer (Name_Start .. Name_End),
               (new Byte_Sequence'
                (Machine.Buffer (Value_Start .. Value_End)),
                External => False,
                Already_Read => False));

         else
            Parse_External_Id
              (Machine, Index, Value_Start, Value_End,
               System_Start, System_End);

            if Index + Ndata_Sequence'Length <= Machine.Buffer_Length
              and then Machine.Buffer
              (Index .. Index + Ndata_Sequence'Length - 1) = Ndata_Sequence
            then
               Index := Index + Ndata_Sequence'Length;
               Ndata_Start := Index;
               Skip_Spaces (Machine.Buffer, Index);

               while Index <= Machine.Buffer_Length loop
                  C := Encoding.Read (Machine.Buffer, Index);
                  exit when C = Space;
                  Index := Index + Encoding.Width (C);
               end loop;

               Ndata_End := Index - 1;
               Skip_Spaces (Machine.Buffer, Index);
            else
               Ndata_Start := Index;
               Ndata_End := Index - 1;
            end if;

            if Index <= Machine.Buffer_Length then
               Fatal_Error
                 (Machine, "(4.2) Expecting end of entity declaration");
            end if;

            if Ndata_Start > Ndata_End then
               External_Entity_Decl
                 (Machine,
                  Machine.Buffer (Prefix_Start .. Prefix_End)
                  & Machine.Buffer (Name_Start .. Name_End),
                  Machine.Buffer (Value_Start .. Value_End),
                  Machine.Buffer (System_Start .. System_End));
               Set
                 (Machine.Entities,
                  Machine.Buffer (Prefix_Start .. Prefix_End)
                  & Machine.Buffer (Name_Start .. Name_End),
                  (new Byte_Sequence'
                   (Machine.Buffer (System_Start .. System_End)),
                   External => True,
                   Already_Read => False));
            else
               Unparsed_Entity_Decl
                 (Machine,
                  Machine.Buffer (Prefix_Start .. Prefix_End)
                  & Machine.Buffer (Name_Start .. Name_End),
                  Machine.Buffer (System_Start .. System_End),
                  Machine.Buffer (Ndata_Start .. Ndata_End));
            end if;
         end if;
      end if;

   exception
      when Invalid_Character =>
         Fatal_Error (Machine, "(4.2) Invalid character "
                      & Encoding.Encode (C) & " in name");
   end End_Of_Entity_Declaration;

   -----------------------
   -- Parse_External_Id --
   -----------------------

   procedure Parse_External_Id
     (Machine                  : in out Reader'Class;
      Index                    : in out Natural;
      Public_Start, Public_End : out Natural;
      System_Start, System_End : out Natural)
   is
      procedure Parse_Id (Public_Id : Boolean; Start, Last : out Natural);
      --  Parse the system Id (ie after the SYSTEM keyword, or the second
      --  argument of PUBLIC)

      C : Unicode_Char;

      --------------
      -- Parse_Id --
      --------------

      procedure Parse_Id (Public_Id : Boolean; Start, Last : out Natural) is
         J : Natural;
      begin
         Skip_Spaces (Machine.Buffer (1 .. Machine.Buffer_Length), Index);
         Skip_String
           (Machine.Buffer (1 .. Machine.Buffer_Length), Index, Machine,
            Start, Last,  "(2.3) Invalid SystemId, expecting string");

         if Public_Id then
            J := Start;
            while Index <= Last loop
               C := Encoding.Read (Machine.Buffer, Index);
               if not Is_Pubid_Char (C) then
                  Fatal_Error (Machine, "(2.3) Invalid PublicID");
               end if;
               Index := Index + Encoding.Width (C);
            end loop;
         end if;

         Skip_Spaces (Machine.Buffer (1 .. Machine.Buffer_Length), Index);
      end Parse_Id;

   begin
      Machine.Processing := None;
      Public_Start := Index;
      Public_End := Index - 1;
      System_Start := Index;
      System_End := Index - 1;

      Skip_Spaces (Machine.Buffer (1 .. Machine.Buffer_Length), Index);

      if Looking_At (Machine, Index, System_Sequence) then
         Index := Index + System_Sequence'Length;
         Parse_Id (False, System_Start, System_End);

      elsif Looking_At (Machine, Index, Public_Sequence) then
         Index := Index + Public_Sequence'Length;
         Parse_Id (True, Public_Start, Public_End);
         Parse_Id (False, System_Start, System_End);
      end if;
   end Parse_External_Id;

   ------------------
   -- Start_Of_DTD --
   ------------------

   procedure Start_Of_DTD
     (Machine : in out Reader'Class; Last_Read : Unicode_Char)
   is
      Index : Natural := Doctype_Sequence'Length + Machine.Buffer'First;
      Start_Name, End_Name,
      System_Start, Public_Start,
      System_End, Public_End   : Natural;

   begin
      Skip_Spaces (Machine.Buffer, Index);
      Skip_Name (Machine.Buffer (Index .. Machine.Buffer_Length), Index,
                 Start_Name, End_Name);
      if not Is_White_Space (Encoding.Read (Machine.Buffer, Index)) then
         Fatal_Error (Machine, "(4.2) Invalid character in name");
      end if;
      Parse_External_Id
        (Machine, Index, Public_Start, Public_End, System_Start, System_End);

      if Index <= Machine.Buffer_Length then
         Fatal_Error (Machine, "(4.2.2) Expected '[' character");
      end if;

      Start_DTD
        (Machine,
         Name => Machine.Buffer (Start_Name .. End_Name),
         Public_Id => Machine.Buffer (Public_Start .. Public_End),
         System_Id => Machine.Buffer (System_Start .. System_End));

      if Last_Read = Opening_Square_Bracket then
         Machine.Parsing_DTD := 2;
      else
         Machine.Parsing_DTD := 1;
      end if;
      Copy (Machine.Processing_Start.all, Machine.Locator.all);
      Machine.Buffer_Length := 0;
   end Start_Of_DTD;

   ------------------------
   -- End_Of_Declaration --
   ------------------------

   procedure End_Of_Declaration (Machine   : in out Reader'Class;
                                 Last_Read : Unicode.Unicode_Char)
   is
      S : constant Byte_Sequence :=
        Machine.Buffer (Machine.Declaration_Start .. Machine.Buffer_Length);
   begin
      Machine.Processing := None;

      --  <!CDATA ...> sequence
      if Cdata_Sequence'Length <= Machine.Buffer_Length
        and then Machine.Buffer (1 .. Cdata_Sequence'Length) = Cdata_Sequence
      then
         if Machine.Current_Node = null then
            Fatal_Error (Machine, "(2.7) Invalid placement of CDATA section",
                         Machine.Processing_Start);
         end if;

         Machine.Buffer_Length := Machine.Declaration_Start - 1;
         Xml_Text_To_Document (Machine);
         Start_Cdata (Machine);
         Put_In_Buffer_Force
           (Machine,
            S (S'First + Cdata_Sequence'Length
               .. S'Last - 2 * Encoding.Width (Right_Square_Bracket)));
         Xml_Text_To_Document (Machine);
         End_Cdata (Machine);
         Machine.Processing := None;
         return;

      --  Should we handle the start of a DTD ? This is true only if we
      --  have a DTD with no internal subset (<!DOCTYPE foo SYSTEM "..">),
      --  since otherwise Start_Of_DTD is called independently.
      elsif Looking_At (Machine, Machine.Buffer'First, Doctype_Sequence) then
         if Machine.Parsing_DTD > 0 then
            Fatal_Error (Machine, "Can't have nested DTDs");
         end if;
         Start_Of_DTD (Machine, Last_Read);
      end if;

      --  Check the possible DTD elements

      if Machine.Parsing_DTD > 0 then
         --  <!ENTITY...>
         if Looking_At (Machine, Machine.Buffer'First, Entity_Sequence) then
            End_Of_Entity_Declaration (Machine);

         --  <!ELEMENT...>
         elsif Looking_At
           (Machine, Machine.Buffer'First, Element_Sequence)
         then
            End_Of_Element_Declaration (Machine);

         --  <!ATTLIST...>
         elsif Looking_At
           (Machine, Machine.Buffer'First, Attlist_Sequence)
         then
            End_Of_Attlist_Declaration (Machine);

            --  <!NOTATION...>
         elsif Looking_At
           (Machine, Machine.Buffer'First, Notation_Sequence)
         then
            End_Of_Notation_Declaration (Machine);

         --  Check that there remains exactly one open square bracket
         elsif Machine.Parsing_DTD = 1 then
            End_DTD (Machine);
            Machine.Parsing_DTD := 0;

         else
            Fatal_Error (Machine, "(2.8) Invalid element in DTD declaration");
         end if;

      else
         Fatal_Error
           (Machine, "(2.7) Invalid declaration", Machine.Processing_Start);
      end if;

      Machine.Buffer_Length := 0;
   end End_Of_Declaration;

   ----------------------
   -- End_Of_Empty_Tag --
   ----------------------

   procedure End_Of_Empty_Tag (Machine   : in out Reader'Class;
                               Last_Read : Unicode.Unicode_Char) is
   begin
      End_Of_XML_End_Tag (Machine, Last_Read);
   end End_Of_Empty_Tag;

   -----------------
   -- Start_Of_PI --
   -----------------

   procedure Start_Of_PI (Machine : in out Reader'Class;
                          Last_Read : Unicode.Unicode_Char)
   is
   begin
      if Debug then
         Debug_Put (Machine, "Start_Of_PI");
      end if;
      Machine.Processing := PI;

      --  Point to the opening < instead of the current ? in the location. It
      --  is necessarily on the same line
      Copy (Machine.Processing_Start.all, Machine.Locator.all);
      Set_Column_Number (Machine.Processing_Start.all,
                         Get_Column_Number (Machine.Processing_Start.all) - 1);
      Machine.Processing_Start.Char_Number := Machine.Locator.Char_Number - 1;
   end Start_Of_PI;

   -----------------
   -- Skip_Spaces --
   -----------------

   procedure Skip_Spaces (Str : Byte_Sequence; Index : in out Natural) is
      C : Unicode_Char;
   begin
      while Index <= Str'Last loop
         C := Encoding.Read (Str, Index);
         exit when not Is_White_Space (C);
         Index := Index + Encoding.Width (C);
      end loop;
   end Skip_Spaces;

   -----------------
   -- Skip_String --
   -----------------

   procedure Skip_String
     (Str         : Byte_Sequence;
      Index       : in out Natural;
      Machine     : in out Reader'Class;
      Value_Start : out Natural;
      Value_End   : out Natural;
      Error_Msg   : String)
   is
      Quote, C : Unicode_Char;
   begin
      if Index >= Str'Last then
         Value_Start := Index;
         Value_End := Index - 1;
         return;
      end if;

      --  Current character must be ' or "
      Quote := Encoding.Read (Str, Index);
      if Quote /= Apostrophe and then Quote /= Quotation_Mark then
         if Error_Msg = "" then
            Value_Start := Index;
            Value_End := Index - 1;
            return;
         else
            Fatal_Error (Machine, Error_Msg, Machine.Processing_Start);
         end if;
      end if;

      Index := Index + Encoding.Width (Quote);
      Value_Start := Index;

      --  Find the end quote
      while Index <= Str'Last loop
         C := Encoding.Read (Str, Index);
         exit when C = Quote;
         Index := Index + Encoding.Width (C);
      end loop;

      Value_End := Index - 1;
      Index := Index + Encoding.Width (C);
   end Skip_String;

   -------------------------
   -- Check_XML_PI_Syntax --
   -------------------------

   procedure Check_XML_PI_Syntax
     (Machine : in out Reader'Class;
      Target  : Byte_Sequence;
      Data    : Byte_Sequence)
   is
      procedure Parse_PI_Argument
        (Index : in out Natural;
         Name_End : out Natural;
         Value_Start, Value_End : out Natural);
      --  Check the argument that starts at Index, and return the first
      --  character following it (a space)
      --  Raises an exception if there is an error
      --  Index is left on the first character following the last parsed one.

      procedure Check_Standalone_Value (Value : Byte_Sequence);
      --  Raise an exception if Value is not valid for the standalone parameter

      procedure Check_Encoding_Value (Value : Byte_Sequence);
      --  Raise an exception if Value is not valid for the encoding
      --  parameter

      procedure Check_Version_Value (Value : Byte_Sequence);
      --  Raise an exception if Value is not valid for the version parameter

      -------------------------
      -- Check_Version_Value --
      -------------------------

      procedure Check_Version_Value (Value : Byte_Sequence) is
         C : Unicode_Char;
         J : Natural := Value'First;
      begin
         while J <= Value'Last loop
            C := Encoding.Read (Value, J);
            J := J + Encoding.Width (C);
            if not (C in Latin_Small_Letter_A .. Latin_Small_Letter_Z)
              and then
                 not (C in Latin_Capital_Letter_A .. Latin_Capital_Letter_Z)
              and then not (C in Digit_Zero .. Digit_Nine)
              and then C /= Low_Line
              and then C /= Period
              and then C /= Colon
              and then C /= Hyphen_Minus
            then
               Fatal_Error
                 (Machine,
                  "(2.8) Illegal version number in <?xml?> processing"
                  & " instruction",
                  Machine.Processing_Start);
            end if;
         end loop;
      end Check_Version_Value;

      --------------------------
      -- Check_Encoding_Value --
      --------------------------

      procedure Check_Encoding_Value (Value : Byte_Sequence) is
         C : Unicode_Char;
         J : Natural;
      begin
         if Value'Length = 0 then
            Fatal_Error
              (Machine, "(4.3.3) Empty value for encoding not allowed");
         else
            C := Encoding.Read (Value, Value'First);
            if not (C in Latin_Small_Letter_A .. Latin_Small_Letter_Z)
              and then not
                (C in Latin_Capital_Letter_A .. Latin_Capital_Letter_Z)
            then
               Fatal_Error
                 (Machine,
                  "(4.3.3) Illegal character '" & Data (Value'First)
                  & "' in encoding value",
                  Machine.Processing_Start);
            end if;

            J := Value'First + Encoding.Width (C);
            while J <= Value'Last loop
               C := Encoding.Read (Value, J);
               J := J + Encoding.Width (C);
               if not (C in Latin_Small_Letter_A .. Latin_Small_Letter_Z)
                 and then not
                   (C in Latin_Capital_Letter_A .. Latin_Capital_Letter_Z)
                 and then not (C in Digit_Zero .. Digit_Nine)
                 and then C /= Period
                 and then C /= Low_Line
                 and then C /= Hyphen_Minus
               then
                  Fatal_Error
                    (Machine,
                     "(4.3.3) Illegal character '"
                     & Encoding.Encode (C) & "' in encoding value",
                     Machine.Processing_Start);
               end if;
            end loop;
         end if;
      end Check_Encoding_Value;

      ----------------------------
      -- Check_Standalone_Value --
      ----------------------------

      procedure Check_Standalone_Value (Value : Byte_Sequence) is
      begin
         if Value /= Yes_Sequence and then Value /= No_Sequence then
            Fatal_Error
              (Machine,
               "(2.9 [32]) Invalid value for standalone parameter in <?xml?>",
               Machine.Processing_Start);
         end if;
      end Check_Standalone_Value;

      -----------------------
      -- Parse_PI_Argument --
      -----------------------

      procedure Parse_PI_Argument
        (Index                  : in out Natural;
         Name_End               : out Natural;
         Value_Start, Value_End : out Natural)
      is
         C : Unicode_Char;
         W : Natural := 0;
      begin
         --  Get the target name
         while Index <= Data'Last loop
            C := Encoding.Read (Data, Index);
            exit when C = Equals_Sign or else Is_White_Space (C);
            W := Encoding.Width (C);
            Index := Index + W;
         end loop;

         Name_End := Index - 1;

         --  Skip the spaces if any
         Skip_Spaces (Data, Index);

         --  If we found the end of the string, or a character other than '='
         if Index > Data'Last
           or else Encoding.Read (Data, Index) /= Equals_Sign
         then
            Fatal_Error
              (Machine,
               "(2.8) Badly formed parameter in <?xml?> PI: no '=' sign",
               Machine.Processing_Start);
         end if;

         --  Skip '=' and the spaces, if any
         Index := Index + Encoding.Width (Equals_Sign);
         Skip_Spaces (Data, Index);

         --  Current character should be ' or "
         Skip_String
           (Data, Index, Machine, Value_Start, Value_End,
            "(2.8) Badly formed parameter in <?xml?> PI: no quote sign");

         if Index < Data'Last then
            C := Encoding.Read (Data, Index);
            if C = Greater_Than_Sign then
               Fatal_Error
                 (Machine,
                  "(2.8) Processing instruction <?xml?> must end with '?>'",
                  Machine.Processing_Start);
            end if;

            if not Is_White_Space (C) then
               Fatal_Error
                 (Machine,
                  "(2.8) Parameters in <?xml?> must be separated by spaces",
                  Machine.Processing_Start);
            end if;
         end if;
      end Parse_PI_Argument;

      First_Arg_Index, Second_Arg_Index, Third_Arg_Index : Natural;
      First_End_Name, Second_End_Name, Third_End_Name : Natural;
      Index : Natural := Data'First;
      Value_Start, Value_End : Natural;

   begin
      --  Parse the first argument
      Skip_Spaces (Data, Index);
      First_Arg_Index := Index;
      Parse_PI_Argument (Index, First_End_Name, Value_Start, Value_End);

      --  (2.8) <?xml?> declaration must contain the version="" string
      --  as its first parameter

      if Data (First_Arg_Index .. First_End_Name) /= Version_Sequence then
         Fatal_Error
           (Machine,
            "(2.8) <?xml?> prolog must have the 'version=' string as its "
            & "first parameter",
            Machine.Processing_Start);
      end if;

      --  (2.8) Check the version number string
      Check_Version_Value (Data (Value_Start .. Value_End));

      --  Parse the second argument
      Skip_Spaces (Data, Index);
      if Index <= Data'Last then
         Second_Arg_Index := Index;
         Parse_PI_Argument (Index, Second_End_Name, Value_Start, Value_End);

         --  Check standalone value
         if Data (Second_Arg_Index .. Second_End_Name) =
           Standalone_Sequence
         then
            Check_Standalone_Value (Data (Value_Start .. Value_End));

         --  Check encoding value
         elsif Data (Second_Arg_Index .. Second_End_Name) =
           Encoding_Sequence
         then
            Check_Encoding_Value (Data (Value_Start .. Value_End));

         --  (2.8) Second argument to <?xml?> can be "encoding" or "standalone"
         elsif Data (Second_Arg_Index .. Second_End_Name) /= "" then
            Fatal_Error
              (Machine,
               "(2.8) <?xml?> arguments can only be 'version', 'encoding' or"
               & " 'standalone', in that order",
               Machine.Processing_Start);
         end if;
      end if;

      --  Parse third argument
      Skip_Spaces (Data, Index);
      if Index <= Data'Last then
         Third_Arg_Index := Index;
         Parse_PI_Argument (Index, Third_End_Name, Value_Start, Value_End);

         --  (2.8) Third argument must be "standalone", and then only if the
         --  second was "encoding"

         if Data (Third_Arg_Index .. Third_End_Name) /= "" then
            if Data (Third_Arg_Index .. Third_End_Name) /= Standalone_Sequence
              or else Data (Second_Arg_Index .. Second_End_Name) /=
              Encoding_Sequence
            then
               Fatal_Error
                 (Machine,
                  "(2.8) <?xml..?> arguments can only be 'version', 'encoding'"
                  & " or 'standalone', in that order",
                  Machine.Processing_Start);
            end if;

            --  Check standalone value
            Check_Standalone_Value (Data (Value_Start .. Value_End));
         end if;
      end if;

      --  Do we have any other argument: if yes, this is an error (2.8)
      Skip_Spaces (Data, Index);
      if Index <= Data'Last then
         Fatal_Error
           (Machine,
            "(2.8) <?xml..?> arguments can only be 'version', 'encoding' or"
            & "'standalone', in that order",
            Machine.Processing_Start);
      end if;
   end Check_XML_PI_Syntax;

   ---------------
   -- End_Of_PI --
   ---------------

   procedure End_Of_PI (Machine : in out Reader'Class;
                        Last_Read : Unicode.Unicode_Char)
   is
      Old_Start : constant Natural := Machine.Processing_Start.Char_Number;
      End_Of_Target : Natural;
      Index : Natural := 1;
   begin
      if Debug then
         Debug_Put (Machine, "End_Of_PI");
      end if;

      Machine.Processing := None;

      --  Get the target of the processing instruction
      while Index <= Machine.Buffer_Length
        and then not Is_White_Space (To_Unicode (Machine.Buffer (Index)))
      loop
         Index := Index + Encoding.Width
           (Encoding.Read (Machine.Buffer, Index));
      end loop;
      End_Of_Target := Index - 1;

      --  Check that the processing instruction has a target (2.6)
      if Index = 1 then
         Fatal_Error
           (Machine,
            "(2.6) Processing Instruction must specify a target name",
           Machine.Processing_Start);
      end if;

      --  (2.6)[17] The target must match the Name rule.
      if not Is_Valid_Name (Machine.Buffer (1 .. End_Of_Target)) then
         Fatal_Error
           (Machine, "(2.6) Invalid target name for processing instruction",
            Machine.Processing_Start);
      end if;

      --  (2.8)[23] the <?xml?> declaration must be first in the document
      if Machine.Buffer (1 .. End_Of_Target) = Xml_Sequence then

         --  Ignore this while parsing the contents of external entities
         if Machine.Recursive_Depth = 1 then

            --  (3.1 [43]) <?xml?> declaration can not be within element
            --  contents
            if Machine.Current_Node /= null then
               Fatal_Error
                 (Machine,
                  "(2.8) <?xml?> prolog can not appear within an element",
                  Machine.Processing_Start);
            end if;

            --  Can't have anything before the prolog <?xml ... ?>

            if Machine.Num_Items /= 0 then
               Fatal_Error
                 (Machine,
                  "(2.8) <?xml?> prolog must be first in the document",
                  Machine.Processing_Start);

               --  ??? This test is incorrect since it ignores any BOM.
            elsif Old_Start /= 1 then
               Fatal_Error
                 (Machine,
                  "(2.8) Spaces must not occur before <?xml?> instruction",
                  Machine.Processing_Start);
            end if;
         end if;

         Check_XML_PI_Syntax
           (Machine,
            Machine.Buffer (1 .. End_Of_Target),
            Machine.Buffer (End_Of_Target + 1 .. Machine.Buffer_Length));

      --  (2.6)[17]: Name can not be 'xml' (case insensitive)
      else
         declare
            C : Unicode_Char;
            J : Natural := Machine.Buffer'First;
         begin
            C := Encoding.Read (Machine.Buffer, J);
            J := J + Encoding.Width (C);

            if C = Latin_Small_Letter_X or else C = Latin_Capital_Letter_X then
               C := Encoding.Read (Machine.Buffer, J);
               J := J + Encoding.Width (C);

               if C = Latin_Capital_Letter_M
                 or else C = Latin_Small_Letter_M
               then
                  C := Encoding.Read (Machine.Buffer, J);
                  J := J + Encoding.Width (C);

                  if (C = Latin_Capital_Letter_L
                      or else C = Latin_Small_Letter_L)
                    and then J = End_Of_Target + 1
                  then
                     Fatal_Error
                       (Machine,
                        "(2.6) '" & Machine.Buffer (1 .. J - 1)
                        & "' is not a valid processing instruction target",
                        Machine.Processing_Start);
                  end if;
               end if;
            end if;
         end;
      end if;

      --  Report the event (SAX says that <?xml?> shouldn't be reported)
      if Machine.Buffer (1 .. End_Of_Target) /= Xml_Sequence then
         Processing_Instruction
           (Machine,
            Machine.Buffer (1 .. End_Of_Target),
            Machine.Buffer (End_Of_Target + 2 .. Machine.Buffer_Length));
      end if;

      Machine.Buffer_Length := 0;
      Free (Machine.Current_NS);
      Machine.Current_NS := new String' ("");

      --  Register the PI if at the top-level, so that we can correctly
      --  report that <?xml?> must be first in the document
      if Machine.Current_Node = null then
         Machine.Num_Items := Machine.Num_Items + 1;
      end if;
   end End_Of_PI;

   -----------------------
   -- Put_Question_Mark --
   -----------------------

   procedure Put_Question_Mark (Machine   : in out Reader'Class;
                                Last_Read : Unicode.Unicode_Char)
   is
   begin
      Put_In_Buffer_Force (Machine, Question_Mark);
      Put_In_Buffer (Machine, Last_Read);
   end Put_Question_Mark;

   ----------------------
   -- End_Of_Attr_Name --
   ----------------------

   procedure End_Of_Attr_Name (Machine   : in out Reader'Class;
                               Last_Read : Unicode.Unicode_Char)
   is
      Name : constant Byte_Sequence :=
        Machine.Buffer (1 .. Machine.Buffer_Length);
   begin
      if Debug then
         Debug_Put (Machine, "End_Of_Attr_Name --"
                    & Qname_From_Name (Machine.Current_NS, Name));
      end if;

      --  (2.3)[5] Check the syntax of the attribute name
      if Name = "" or else not Is_Valid_Name (Name) then
         Fatal_Error (Machine, "(2.3) '" & Name & "' is not a valid name");
      end if;

      --  The attribute name must be unique. Note that we must compare with
      --  the namespaces expanded to their URI (5.3 XML namespaces
      --  specification).
      --  Note that this is again tested when emitting Start_Element, once the
      --  namespaces have been expanded. However, we test it here so that we
      --  can report the error at the correct location.
      if (Machine.Current_NS.all /= ""
          or else Machine.Current_Node.Parent = null)
        and then Get_Index
        (Machine.Attributes,
         Qname_From_Name (Machine.Current_NS, Name)) /= -1
      then
         Fatal_Error (Machine, "(3.1) Attributes must have a unique value");
      end if;

      --  Create a new attribute. The value will be specified later on,
      --  when it is known.
      --  ??? The type should be read from the DTD.
      Add_Attribute
        (Machine.Attributes,
         URI => Machine.Current_NS.all,
         Local_Name => Name,
         Qname => Qname_From_Name (Machine.Current_NS, Name),
         Att_Type => "CDATA",
         Value => "");
      Machine.Buffer_Length := 0;
      Free (Machine.Current_NS);
      Machine.Current_NS := new String' ("");
   end End_Of_Attr_Name;

   -------------------------
   -- Start_Of_Attr_Value --
   -------------------------

   procedure Start_Of_Attr_Value (Machine   : in out Reader'Class;
                                  Last_Read : Unicode.Unicode_Char) is
   begin
      if Debug then
         Debug_Put (Machine, "Start_Of_Attr_Value");
      end if;
      Machine.Processing_Attribute_Value := True;
   end Start_Of_Attr_Value;

   -----------------------
   -- End_Of_Attr_Value --
   -----------------------

   procedure End_Of_Attr_Value (Machine   : in out Reader'Class;
                                Last_Read : Unicode.Unicode_Char)
   is
      Pos          : constant Natural := Get_Length (Machine.Attributes) - 1;
      S            : String (1 .. Machine.Buffer_Length);
      S_Index      : Natural := S'First;
      Buffer_Index : Natural := Machine.Buffer'First;
      C            : Unicode_Char;
      W            : Natural := 0;

   begin
      if Debug then
         Debug_Put
           (Machine, "End_Of_Attr_Value --"
            & Machine.Buffer (1 .. Machine.Buffer_Length));
      end if;

      Machine.Processing_Attribute_Value := False;

      --  Normalize the value: ignore any leading white space
      Skip_Spaces (Machine.Buffer, Buffer_Index);

      --  Then merge several spaces into one
      while Buffer_Index <= Machine.Buffer_Length loop
         C := Encoding.Read (Machine.Buffer, Buffer_Index);
         W := Encoding.Width (C);
         if Is_White_Space (C) then
            W := Encoding.Width (Space);
            S (S_Index .. S_Index + W - 1) := Encoding.Encode (Space);
            S_Index := S_Index + W;
            Skip_Spaces (Machine.Buffer, Buffer_Index);
         else
            S (S_Index .. S_Index + W - 1) :=
              Machine.Buffer (Buffer_Index .. Buffer_Index + W - 1);
            S_Index := S_Index + W;
            Buffer_Index := Buffer_Index + W;
         end if;
      end loop;

      --  Then remove the trailing spaces
      if S_Index >= S'First + W and then Is_White_Space (C) then
         S_Index := S_Index - W;
      end if;

      Machine.Buffer_Length := 0;

      --  Is this a namespace declaration ?  (xmlns:prefix="uri")

      if Get_URI (Machine.Attributes, Pos) = Xmlns_Sequence then
         Add_Namespace
           (Machine, Machine.Current_Node,
            Prefix => Get_Local_Name (Machine.Attributes, Pos),
            URI    => S (S'First .. S_Index - 1));

         --  If we shouldn't send events for namespaces attributes
         if not Get_Feature (Machine, Namespace_Prefixes_Feature) then
            Remove_Attribute (Machine.Attributes, Pos);
            return;
         end if;

      --  Is it the declaration of the default namespace (xmlns="uri")
      elsif Get_URI (Machine.Attributes, Pos)'Length = 0
        and then Get_Local_Name (Machine.Attributes, Pos) = Xmlns_Sequence
      then
         Add_Namespace
           (Machine, Machine.Current_Node,
            Prefix => "",
            URI    => S (S'First .. S_Index - 1));

         --  If we shouldn't send events for namespaces attributes
         if not Get_Feature (Machine, Namespace_Prefixes_Feature) then
            Remove_Attribute (Machine.Attributes, Pos);
            return;
         end if;
      end if;

      Set_Value (Machine.Attributes, Pos, S (S'First .. S_Index - 1));
   end End_Of_Attr_Value;

   ------------------------
   -- End_Of_XML_End_Tag --
   ------------------------

   procedure End_Of_XML_End_Tag (Machine   : in out Reader'Class;
                                 Last_Read : Unicode.Unicode_Char)
   is
      Tmp : Element_Access := Machine.Current_Node;
      NS  : XML_NS;
   begin
      --  Note: we already checked, when necessary, that the name we found
      --  in the closing tag was the same as the one associated with the
      --  opening tag.

      --  Close the element
      Find_NS
        (Machine, Machine.Current_Node, Machine.Current_Node.NS.all, NS);
      End_Element
        (Handler => Machine,
         Namespace_URI => NS.URI.all,
         Local_Name => Machine.Current_Node.Name.all,
         Qname => Qname_From_Name (Machine.Current_Node.NS,
                                   Machine.Current_Node.Name.all));

      --  Close all the namespaces
      NS := Machine.Current_Node.Namespaces;
      while NS /= null loop
         End_Prefix_Mapping (Machine, NS.Prefix.all);
         NS := NS.Next;
      end loop;

      --  Move back to the parent node (after freeing the current node)
      Machine.Current_Node := Machine.Current_Node.Parent;
      Free (Tmp);
      Free_Element (Tmp);

      --  Empty the buffer
      Machine.Buffer_Length := 0;
      Free (Machine.Current_NS);
      Machine.Current_NS := new String' ("");
   end End_Of_XML_End_Tag;

   ------------------------------
   -- End_Of_Attr_Name_And_Tag --
   ------------------------------

   procedure End_Of_Attr_Name_And_Tag
     (Machine   : in out Reader'Class;
      Last_Read : Unicode.Unicode_Char) is
   begin
      if Debug then
         Debug_Put (Machine, "End_Of_Attr_Name_And_Tag");
      end if;
      End_Of_Attr_Name (Machine, Last_Read);
      End_Of_Start_Tag (Machine, Last_Read);
   end End_Of_Attr_Name_And_Tag;

   ---------------------------------
   -- End_Of_Name_And_Closing_Tag --
   ---------------------------------

   procedure End_Of_Name_And_Closing_Tag
     (Machine   : in out Reader'Class;
      Last_Read : Unicode.Unicode_Char)
   is
   begin
      if Debug then
         Debug_Put (Machine, "End_Of_Name_And_Closing_Tag --"
                    & Machine.Buffer (1 .. Machine.Buffer_Length));
      end if;
      End_Of_Closing_Tag_Name (Machine, Last_Read);
   end End_Of_Name_And_Closing_Tag;

   -----------------------------
   -- End_Of_Closing_Tag_Name --
   -----------------------------

   procedure End_Of_Closing_Tag_Name
     (Machine   : in out Reader'Class;
      Last_Read : Unicode.Unicode_Char) is
   begin
      if Debug then
         Debug_Put (Machine, "End_Of_Closing_Tag_Name --"
                    & Machine.Buffer (1 .. Machine.Buffer_Length));
      end if;
      if Machine.Current_Node = null
        or else Machine.Current_Node.Name.all /=
        Machine.Buffer (1 .. Machine.Buffer_Length)
      then
         Fatal_Error (Machine, "(3) Names differ for closing tag");
      end if;

      End_Of_XML_End_Tag (Machine, Last_Read);
   end End_Of_Closing_Tag_Name;

   -------------------
   -- Add_Namespace --
   -------------------

   procedure Add_Namespace
     (Machine : in out Reader'Class;
      Node    : Element_Access;
      Prefix  : Byte_Sequence;
      URI     : Byte_Sequence) is
   begin
      if Node = null then
         Machine.Default_Namespaces := new XML_NS_Record'
           (Prefix => new Byte_Sequence' (Prefix),
            URI => new Byte_Sequence' (URI),
            Next => Machine.Default_Namespaces);

      else
         Node.Namespaces := new XML_NS_Record'
           (Prefix => new Byte_Sequence' (Prefix),
            URI => new Byte_Sequence' (URI),
            Next => Node.Namespaces);
      end if;

      --  Report the event, except for the default namespace
      if Prefix /= "" and then Prefix /= "xmlns" then
         Start_Prefix_Mapping
           (Machine,
            Prefix => Prefix,
            URI    => URI);
      end if;
   end Add_Namespace;

   ---------------
   -- Main_Loop --
   ---------------

   procedure Main_Loop
     (Machine : in out Reader'Class;
      Input   : in out Input_Sources.Input_Source'Class;
      Increment_Locator : Boolean := True)
   is
      C         : Unicode_Char;
      Last_Char : Unicode_Char := Nul;
   begin
      while not Eof (Input) loop

         --  Increment now, rather than after reading the character, for proper
         --  handling of force entries (they should still relate to the
         --  previous location)
         if Increment_Locator and then Last_Char /= Nul then
            if Last_Char = Line_Feed then
               Set_Column_Number (Machine.Locator.all, 1);
               Set_Line_Number
                 (Machine.Locator.all,
                  Get_Line_Number (Machine.Locator.all) + 1);
            else
               Set_Column_Number
                 (Machine.Locator.all,
                  Get_Column_Number (Machine.Locator.all) + 1);
            end if;
            Machine.Locator.Char_Number := Machine.Locator.Char_Number + 1;
         end if;

         --  Normalize the end of line characters: CR & LF => LF, CR =>
         --  LF
         Next_Char (Input, C);
         if Debug then
            Debug_Put (Machine, "Main_Loop: Input " & Encoding.Encode (C)
                       & " (" & C'Img & ")");
         end if;

         if Last_Char = Carriage_Return and then C /= Line_Feed then
            Send_Event (Machine, Line_Feed);
         end if;
         if C /= Carriage_Return then
            Send_Event (Machine, C);
         end if;

         Last_Char := C;
      end loop;
   end Main_Loop;

   -----------
   -- Parse --
   -----------

   procedure Parse (Read : in out Reader; Input : access Input_Source'Class) is
   begin
      --  Reinitialize the variables (leave the default namespace first)
      Read.Current_Node := null;
      Read.Locator := new File_Locator;
      Read.Processing_Start := new File_Locator;
      Set_Public_Id (Read.Locator.all, Get_Public_Id (Input.all));
      Set_System_Id (Read.Locator.all, Get_System_Id (Input.all));
      Set_Column_Number (Read.Locator.all, 1 + Prolog_Size (Input.all));

      Compile
        (Read,
         Highest_State  => Xml_Automaton (Xml_Automaton'Last).Initial_State,
         Initial_State  => Top_State,
         Def            => Xml_Automaton);

      Set_Document_Locator (Reader'Class (Read), Read.Locator);
      Start_Document (Reader'Class (Read));

      --  Create the default namespaces (must be done after we have sent the
      --  Start_Document event).
      Add_Namespace
        (Read, null, Xml_Sequence,
         Encodings.From_Utf32
         (To_Utf32 ("http://www.w3.org/XML/1998/namespace")));
      Add_Namespace (Read, null, Xmlns_Sequence, Xmlns_Sequence);
      Add_Namespace (Read, null, "", "");
      Read.Current_NS := new String' ("");

      Main_Loop (Read, Input.all);

      --  Close all the namespaces
      declare
         NS : XML_NS := Read.Default_Namespaces;
      begin
         while NS /= null loop
            if NS.Prefix.all /= "" and then NS.Prefix.all /= "xmlns" then
               End_Prefix_Mapping (Reader'Class (Read), NS.Prefix.all);
            end if;
            NS := NS.Next;
         end loop;
      end;
      End_Document (Reader'Class (Read));

      --  A valid XML document must have a root element to be well-formed (2.1)
      if Read.Num_Elements /= 1 then
         Fatal_Error (Read, "(2.1) No root element specified");
      end if;

      --  All the nodes must have been closed at the end of the document
      if Read.Current_Node /= null then
         Fatal_Error
           (Read, "(2.1) Node <" & Read.Current_Node.Name.all
            & "> is not closed");
      end if;

      --  We should be parsing the DTD
      if Read.Parsing_DTD > 0 then
         Fatal_Error (Read, "(2.8) DTD must be terminated by ']>'");
      end if;

      --  Free the allocated memory that we no longer need
      Free_Memory (Read);

   exception
      when others =>
         Free_Memory (Read);
         raise;
   end Parse;

   -----------------
   -- Get_Feature --
   -----------------

   function Get_Feature (Read : Reader; Name : String) return Boolean is
   begin
      if Name = Namespace_Feature then
         return Read.Feature_Namespace;

      elsif Name = Namespace_Prefixes_Feature then
         return Read.Feature_Namespace_Prefixes;

      elsif Name = External_General_Entities_Feature then
         return Read.Feature_External_General_Entities;

      elsif Name = External_Parameter_Entities_Feature then
         return Read.Feature_External_Parameter_Entities;

      elsif Name = Parameter_Entities_Feature then
         return False;  --  ??? Unsupported for now
      end if;

      return False;
   end Get_Feature;

   -----------------
   -- Set_Feature --
   -----------------

   procedure Set_Feature
     (Read : in out Reader; Name : String; Value : Boolean) is
   begin
      if Name = Namespace_Feature then
         Read.Feature_Namespace := Value;

      elsif Name = Namespace_Prefixes_Feature then
         Read.Feature_Namespace_Prefixes := Value;

      elsif Name = External_General_Entities_Feature then
         Read.Feature_External_General_Entities := Value;

      elsif Name = External_Parameter_Entities_Feature then
         Read.Feature_External_Parameter_Entities := Value;
      end if;
   end Set_Feature;

   -------------
   -- Warning --
   -------------

   procedure Warning
     (Handler : in out Reader; Except : Sax_Parse_Exception'Class) is
   begin
      null;
   end Warning;

   -----------
   -- Error --
   -----------

   procedure Error
     (Handler : in out Reader; Except : Sax_Parse_Exception'Class) is
   begin
      null;
   end Error;

   -----------------
   -- Fatal_Error --
   -----------------

   procedure Fatal_Error
     (Handler : in out Reader; Except : Sax_Parse_Exception'Class) is
   begin
      Raise_Exception
        (XML_Fatal_Error'Identity,
         Get_Message (Except) & ", at " & To_String (Get_Locator (Except)));
   end Fatal_Error;

   --------------------------
   -- Set_Document_Locator --
   --------------------------

   procedure Set_Document_Locator
     (Handler : in out Reader; Loc : access Sax.Locators.Locator'Class) is
   begin
      null;
   end Set_Document_Locator;

   --------------------
   -- Start_Document --
   --------------------

   procedure Start_Document (Handler : in out Reader) is
   begin
      null;
   end Start_Document;

   ------------------
   -- End_Document --
   ------------------

   procedure End_Document (Handler : in out Reader) is
   begin
      null;
   end End_Document;

   --------------------------
   -- Start_Prefix_Mapping --
   --------------------------

   procedure Start_Prefix_Mapping
     (Handler : in out Reader;
      Prefix  : Unicode.CES.Byte_Sequence;
      URI     : Unicode.CES.Byte_Sequence) is
   begin
      null;
   end Start_Prefix_Mapping;

   ------------------------
   -- End_Prefix_Mapping --
   ------------------------

   procedure End_Prefix_Mapping
     (Handler : in out Reader; Prefix : Unicode.CES.Byte_Sequence) is
   begin
      null;
   end End_Prefix_Mapping;

   -------------------
   -- Start_Element --
   -------------------

   procedure Start_Element
     (Handler       : in out Reader;
      Namespace_URI : Unicode.CES.Byte_Sequence := "";
      Local_Name    : Unicode.CES.Byte_Sequence := "";
      Qname         : Unicode.CES.Byte_Sequence := "";
      Atts          : Sax.Attributes.Attributes'Class) is
   begin
      null;
   end Start_Element;

   -----------------
   -- End_Element --
   -----------------

   procedure End_Element
     (Handler : in out Reader;
      Namespace_URI : Unicode.CES.Byte_Sequence := "";
      Local_Name    : Unicode.CES.Byte_Sequence := "";
      Qname         : Unicode.CES.Byte_Sequence := "") is
   begin
      null;
   end End_Element;

   ----------------
   -- Characters --
   ----------------

   procedure Characters
     (Handler : in out Reader;
      Ch      : Unicode.CES.Byte_Sequence) is
   begin
      null;
   end Characters;

   --------------------------
   -- Ignorable_Whitespace --
   --------------------------

   procedure Ignorable_Whitespace
     (Handler : in out Reader; Ch : Unicode.CES.Byte_Sequence) is
   begin
      null;
   end Ignorable_Whitespace;

   ----------------------------
   -- Processing_Instruction --
   ----------------------------

   procedure Processing_Instruction
     (Handler : in out Reader;
      Target  : Unicode.CES.Byte_Sequence;
      Data    : Unicode.CES.Byte_Sequence) is
   begin
      null;
   end Processing_Instruction;

   --------------------
   -- Skipped_Entity --
   --------------------

   procedure Skipped_Entity
     (Handler : in out Reader; Name : Unicode.CES.Byte_Sequence) is
   begin
      null;
   end Skipped_Entity;

   -------------
   -- Comment --
   -------------

   procedure Comment
     (Handler : in out Reader;
      Ch      : Unicode.CES.Byte_Sequence) is
   begin
      null;
   end Comment;

   -----------------
   -- Start_Cdata --
   -----------------

   procedure Start_Cdata (Handler : in out Reader) is
   begin
      null;
   end Start_Cdata;

   ---------------
   -- End_Cdata --
   ---------------

   procedure End_Cdata (Handler : in out Reader) is
   begin
      null;
   end End_Cdata;

   ------------------
   -- Start_Entity --
   ------------------

   procedure Start_Entity
     (Handler : in out Reader;
      Name    : Unicode.CES.Byte_Sequence) is
   begin
      null;
   end Start_Entity;

   ----------------
   -- End_Entity --
   ----------------

   procedure End_Entity
     (Handler : in out Reader;
      Name    : Unicode.CES.Byte_Sequence) is
   begin
      null;
   end End_Entity;

   ---------------
   -- Start_DTD --
   ---------------

   procedure Start_DTD
     (Handler   : in out Reader;
      Name      : Unicode.CES.Byte_Sequence;
      Public_Id : Unicode.CES.Byte_Sequence := "";
      System_Id : Unicode.CES.Byte_Sequence := "") is
   begin
      null;
   end Start_DTD;

   -------------
   -- End_DTD --
   -------------

   procedure End_DTD (Handler : in out Reader) is
   begin
      null;
   end End_DTD;

   --------------------------
   -- Internal_Entity_Decl --
   --------------------------

   procedure Internal_Entity_Decl
     (Handler : in out Reader;
      Name    : Unicode.CES.Byte_Sequence;
      Value   : Unicode.CES.Byte_Sequence) is
   begin
      null;
   end Internal_Entity_Decl;

   --------------------------
   -- External_Entity_Decl --
   --------------------------

   procedure External_Entity_Decl
     (Handler   : in out Reader;
      Name      : Unicode.CES.Byte_Sequence;
      Public_Id : Unicode.CES.Byte_Sequence;
      System_Id : Unicode.CES.Byte_Sequence) is
   begin
      null;
   end External_Entity_Decl;

   --------------------------
   -- Unparsed_Entity_Decl --
   --------------------------

   procedure Unparsed_Entity_Decl
     (Handler       : in out Reader;
      Name          : Unicode.CES.Byte_Sequence;
      System_Id     : Unicode.CES.Byte_Sequence;
      Notation_Name : Unicode.CES.Byte_Sequence) is
   begin
      null;
   end Unparsed_Entity_Decl;

   ------------------
   -- Element_Decl --
   ------------------

   procedure Element_Decl
     (Handler : in out Reader;
      Name    : Unicode.CES.Byte_Sequence;
      Model   : Unicode.CES.Byte_Sequence;
      Parsed_Model : Element_Model_Ptr) is
   begin
      null;
   end Element_Decl;

   -------------------
   -- Notation_Decl --
   -------------------

   procedure Notation_Decl
     (Handler       : in out Reader;
      Name          : Unicode.CES.Byte_Sequence;
      Public_Id     : Unicode.CES.Byte_Sequence;
      System_Id     : Unicode.CES.Byte_Sequence) is
   begin
      null;
   end Notation_Decl;

   --------------------
   -- Attribute_Decl --
   --------------------

   procedure Attribute_Decl
     (Handler : in out Reader;
      Ename   : Unicode.CES.Byte_Sequence;
      Aname   : Unicode.CES.Byte_Sequence;
      Typ     : Unicode.CES.Byte_Sequence;
      Value_Default : Unicode.CES.Byte_Sequence;
      Value   : Unicode.CES.Byte_Sequence) is
   begin
      null;
   end Attribute_Decl;

   ----------------
   -- Entity_Img --
   ----------------

   function Entity_Img (A : Entity_Def) return String is
   begin
      if A.Str /= null then
         return A.Str.all;
      else
         return "<null>";
      end if;
   end Entity_Img;

   ---------------------
   -- Entity_Equality --
   ---------------------

   function Entity_Equality (A, B : Entity_Def) return Boolean is
   begin
      return A.External = B.External
        and then A.Str /= null
        and then B.Str /= null
        and then A.Str.all = B.Str.all;
   end Entity_Equality;

   --------------------
   -- Attributes_Img --
   --------------------

   function Attributes_Img (A : Attributes_Ptr) return String is
   begin
      return "<???>";
   end Attributes_Img;

end Sax.Readers;
