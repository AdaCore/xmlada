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

with Ada.Exceptions;            use Ada.Exceptions;
with Ada.Text_IO;               use Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with Input_Sources.File;        use Input_Sources.File;
with Input_Sources.Strings;     use Input_Sources.Strings;
with Input_Sources;             use Input_Sources;
with Interfaces;                use Interfaces;
with Sax.Attributes;            use Sax.Attributes;
with Sax.Attributes;            use Sax.Attributes;
with Sax.Encodings;             use Sax.Encodings;
with Sax.Exceptions;            use Sax.Exceptions;
with Sax.Locators;              use Sax.Locators;
with Sax.Models;                use Sax.Models;
with Unchecked_Deallocation;
with Unicode.CES;               use Unicode.CES;
with Unicode.CES.Basic_8bit;    use Unicode.CES.Basic_8bit;
with Unicode.Names.Basic_Latin; use Unicode.Names.Basic_Latin;
with Unicode;                   use Unicode;

package body Sax.Readers is

   use Entity_Table, Attributes_Table, Notations_Table;

   Debug_Lexical : constant Boolean := False;
   Debug_Input : constant Boolean := False;
   --  Set to True if you want to debug this package

   Always_Test_Valid_Char : constant Boolean := True;
   --  If True, XML/Ada will check that each character read from the input
   --  streams is valid, which tends to slow done the parser. If False, this is
   --  only tested for character inserted through character references

   Initial_Buffer_Length : constant := 10000;
   --  Initial length of the internal buffer that stores CDATA, tag names,...

   ------------
   -- Tokens --
   ------------

   type Token_Type is
     (Double_String_Delimiter, --  "
      Single_String_Delimiter, --  '
      Comment,                 --  <!--...--> (Data is the comment)
      Start_Of_Tag,            --  <
      Start_Of_End_Tag,        --  </
      End_Of_Start_Tag,        --  />
      Start_Of_PI,             --  <?
      End_Of_PI,               --  ?>
      End_Of_Tag,              --  >
      Equal,                   --  =  (in tags)
      Colon,                   --  :  (in tags)
      Open_Paren,              --  (  (while parsing content model in ATTLIST)
      Internal_DTD_Start,      --  [  (while in DTD)
      Internal_DTD_End,        --  ]  (while in DTD)
      Include,                 --  <![INCLUDE[
      Ignore,                  --  <![IGNORE[
      Start_Conditional,       --  <![
      End_Conditional,         --  ]]>
      Space,                   --  Any number of spaces (Data is the spaces)
      Text,                    --  any text  (Data is the identifier)
      Name,                    --  same as text, but contains only valid
      --  name characters
      Cdata_Section,           --  <![CDATA
      Doctype_Start,           --  <!DOCTYPE
      System,                  --  SYSTEM  (while in DTD)
      Public,                  --  PUBLIC  (while in DTD)
      Ndata,                   --  NDATA   (while in DTD)
      Any,                     --  ANY (while in DTD)
      Empty,                   --  EMPTY (while in DTD)
      Notation,                --  NOTATION (while in DTD or ATTLIST)
      Entity_Def,              --  <!ENTITY (while in DTD)
      Element_Def,             --  <!ELEMENT (while in DTD)
      Attlist_Def,             --  <!ATTLIST (while in DTD)
      Id_Type,                 --  ID (while in ATTLIST)      Data is "ID"
      Idref,                   --  IDREF (while in ATTLIST)   Data is "IDREF"
      Idrefs,                  --  IDREFS (while in ATTLIST)  Data is "IDREFS"
      Cdata,                   --  CDATA (while in ATTLIST)   Data is "CDATA"
      Entity,                  --  ENTITY (while in ATTLIST)  Data is "ENTITY"
      Entities,                --  ENTITIES (while in ATTLIST) Data="ENTITIES"
      Nmtoken,                 --  NMTOKEN (while in ATTLIST) Data="NMTOKEN"
      Nmtokens,                --  NMTOKENS (while in ATTLIST) Data="NMTOKENS"
      Required,                --  REQUIRED (while in ATTLIST) Data="#REQUIRED"
      Implied,                 --  IMPLIED (while in ATTLIST) Data="#IMPLIED"
      Fixed,                   --  FIXED (while in ATTLIST) Data="#FIXED"
      End_Of_Input             --  End of input was seen.
     );

   type Token is record
      Typ : Token_Type;
      First, Last : Natural;  --   Indexes in the buffer
      Line, Column : Natural; --   Line and col within the current stream
      Input_Id : Natural;     --   Id of the input source in which Token was
                              --   read.
   end record;

   Null_Token : constant Token := (End_Of_Input, 1, 0, 0, 0, 0);

   Default_State : constant Parser_State :=
     (Name => "Def",
      Ignore_Special => False,
      Detect_End_Of_PI => False,
      Greater_Special => False,
      Less_Special => False,
      Expand_Param_Entities => False,
      Expand_Entities => True,
      Expand_Character_Ref => True,
      In_DTD => False,
      Recognize_External => False,
      Handle_Strings => False,
      In_Tag => False,
      Report_Parenthesis => False,
      In_Attlist => False);
   Attr_Value_State : constant Parser_State :=
     (Name => "Att",
      Ignore_Special => True,
      Detect_End_Of_PI => False,
      Greater_Special => False,
      Less_Special => True,
      Expand_Param_Entities => False,
      Expand_Entities => True,
      Expand_Character_Ref => True,
      In_DTD => False,
      Recognize_External => False,
      Handle_Strings => True,
      In_Tag => False,
      Report_Parenthesis => False,
      In_Attlist => False);
   Non_Interpreted_String_State : constant Parser_State :=
     (Name => "Str",
      Ignore_Special => True,
      Detect_End_Of_PI => False,
      Greater_Special => False,
      Less_Special => False,
      Expand_Param_Entities => False,
      Expand_Entities => False,
      Expand_Character_Ref => False,
      In_DTD => False,
      Recognize_External => False,
      Handle_Strings => True,
      In_Tag => False,
      Report_Parenthesis => False,
      In_Attlist => False);
   DTD_State : constant Parser_State :=
     (Name => "DTD",
      Ignore_Special => False,
      Detect_End_Of_PI => False,
      Greater_Special => True,
      Less_Special => False,
      Expand_Param_Entities => True,
      Expand_Entities => True,
      Expand_Character_Ref => True,
      In_DTD => True,
      Recognize_External => True,
      Handle_Strings => True,
      In_Tag => False,
      Report_Parenthesis => False,
      In_Attlist => False);
   PI_State : constant Parser_State :=
     (Name => "PI ",
      Ignore_Special => True,
      Detect_End_Of_PI => True,
      Greater_Special => False,
      Less_Special => False,
      Expand_Param_Entities => False,
      Expand_Entities => False,
      Expand_Character_Ref => False,
      In_DTD => False,
      Recognize_External => False,
      Handle_Strings => True,
      In_Tag => False,
      Report_Parenthesis => False,
      In_Attlist => False);
   Entity_Def_State : constant Parser_State :=
     (Name => "Ent",
      Ignore_Special => False,
      Detect_End_Of_PI => False,
      Greater_Special => True,
      Less_Special => False,
      Expand_Param_Entities => False,
      Expand_Entities => False,
      Expand_Character_Ref => True,
      In_DTD => True,
      Recognize_External => True,
      Handle_Strings => True,
      In_Tag => False,
      Report_Parenthesis => False,
      In_Attlist => False);
   Element_Def_State : constant Parser_State :=
     (Name => "Ele",
      Ignore_Special => False,
      Detect_End_Of_PI => False,
      Greater_Special => True,
      Less_Special => False,
      Expand_Param_Entities => True,
      Expand_Entities => False,
      Expand_Character_Ref => True,
      In_DTD => True,
      Recognize_External => True,
      Handle_Strings => True,
      In_Tag => False,
      Report_Parenthesis => True,
      In_Attlist => False);
   Attribute_Def_State : constant Parser_State :=
     (Name => "AtD",
      Ignore_Special => False,
      Detect_End_Of_PI => False,
      Greater_Special => True,
      Less_Special => False,
      Expand_Param_Entities => True,
      Expand_Entities => False,
      Expand_Character_Ref => True,
      In_DTD => True,
      Recognize_External => False,
      Handle_Strings => True,
      In_Tag => True,
      Report_Parenthesis => True,
      In_Attlist => True);
   Attribute_Def_Name_State : constant Parser_State :=
     (Name => "ADN",
      Ignore_Special => False,
      Detect_End_Of_PI => False,
      Greater_Special => True,
      Less_Special => False,
      Expand_Param_Entities => True,
      Expand_Entities => False,
      Expand_Character_Ref => True,
      In_DTD => True,
      Recognize_External => False,
      Handle_Strings => True,
      In_Tag => True,
      Report_Parenthesis => True,
      In_Attlist => False);
   Entity_Str_Def_State : constant Parser_State :=
     (Name => "EtS",
      Ignore_Special => True,
      Detect_End_Of_PI => False,
      Greater_Special => False,
      Less_Special => False,
      Expand_Param_Entities => True,
      Expand_Entities => False,
      Expand_Character_Ref => True,
      In_DTD => True,
      Recognize_External => False,
      Handle_Strings => True,
      In_Tag => False,
      Report_Parenthesis => False,
      In_Attlist => False);
   Attlist_Str_Def_State : constant Parser_State :=
     (Name => "AtS",
      Ignore_Special => True,
      Detect_End_Of_PI => False,
      Greater_Special => False,
      Less_Special => False,
      Expand_Param_Entities => True,
      Expand_Entities => True,
      Expand_Character_Ref => True,
      In_DTD => True,
      Recognize_External => False,
      Handle_Strings => True,
      In_Tag => False,
      Report_Parenthesis => False,
      In_Attlist => False);
   Tag_State : constant Parser_State :=
     (Name => "Tag",
      Ignore_Special => False,
      Greater_Special => True,
      Less_Special => False,
      Detect_End_Of_PI => False,
      Expand_Param_Entities => False,
      Expand_Entities => False,
      Expand_Character_Ref => True,
      In_DTD => False,
      Recognize_External => False,
      Handle_Strings => True,
      In_Tag => True,
      Report_Parenthesis => False,
      In_Attlist => False);

   --------------------------
   -- Internal subprograms --
   --------------------------

   function Debug_Encode (C : Unicode_Char) return Byte_Sequence;
   --  Return an encoded string matching C (matching Sax.Encodins.Encoding)

   function Is_Name_Char (C : Unicode_Char) return Boolean;
   --  Return True if C is a valid character to use in a Name.

   procedure Test_Valid_Char
     (Parser : in out Reader'Class; C : Unicode_Char; Loc : Token);
   --  Raise an error if C is not valid in XML. The error is reported at
   --  location Loc.

   function Is_Pubid_Char (C : Unicode_Char) return Boolean;
   --  Return True if C is a valid character for a Public ID (2.3 specs)

   procedure Test_Valid_Lang
     (Parser : in out Reader'Class; Lang : Byte_Sequence);
   --  Return True if Lang matches the rules for languages

   Input_Ended : exception;

   procedure Next_Char
     (Input   : in out Input_Source'Class;
      Parser  : in out Reader'Class);
   --  Return the next character, and increments the locators
   --  Input_Ended is raised at the end

   procedure Put_In_Buffer
     (Parser : in out Reader'Class; Char : Unicode_Char);
   pragma Inline (Put_In_Buffer);

   procedure Put_In_Buffer
     (Parser : in out Reader'Class; Str : Byte_Sequence);
   pragma Inline (Put_In_Buffer);
   --  Put the last character read in the internal buffer

   procedure Next_Token
     (Input  : in out Input_Sources.Input_Source'Class;
      Parser : in out Reader'Class;
      Id     : out Token;
      Coalesce_Space : Boolean := False);
   --  Return the next identifier in the input stream.
   --  Locator is modified accordingly (line and column).
   --  If Coalesce_Space is True, then all the Name or Text tokens preceded or
   --  followed by Space tokens are grouped together and returned as a single
   --  Text token.

   procedure Next_Token_Skip_Spaces
     (Input  : in out Input_Sources.Input_Source'Class;
      Parser : in out Reader'Class;
      Id     : out Token;
      Must_Have : Boolean := False);
   --  Same as Next_Token, except it skips spaces. If Must_Have is True,
   --  then the first token read must be a space, or an error is raised

   procedure Reset_Buffer
     (Parser : in out Reader'Class; Id : Token := Null_Token);
   --  Clears the internal buffer in Parser.
   --  If Id is not Null_Token, then only the characters starting from
   --  Id.First are removed

   procedure Set_State (Parser : in out Reader'Class; State : Parser_State);
   --  Set the current state for the parser

   function Get_State (Parser : Reader'Class) return Parser_State;
   --  Return the current state.

   procedure Syntactic_Parse
     (Parser : in out Reader'Class;
      Input  : in out Input_Sources.Input_Source'Class);
   --  Internal syntactical parser.

   procedure Find_NS
     (Parser  : in out Reader'Class;
      Elem    : Element_Access;
      Prefix  : Token;
      NS      : out XML_NS;
      Include_Default_NS : Boolean := True);
   --  Search the namespace associated with a given prefix in the scope of
   --  Elem or its parents. Use the empty string to get the default namespace.
   --  Fatal_Error is raised if no such namespace was found (and null is
   --  returned, in case Fatal_Error didn't raise an exception)
   --  The default namespace is not resolved if Include_Default_NS is False.

   procedure Find_NS
     (Parser  : in out Reader'Class;
      Elem    : Element_Access;
      Prefix  : Byte_Sequence;
      NS      : out XML_NS;
      Include_Default_NS : Boolean := True);
   --  Same as above, from a Byte_Sequence

   function Qname_From_Name (Parser : Reader'Class; Prefix, Local_Name : Token)
      return Byte_Sequence;
   --  Create the qualified name from the namespace URI and the local name.

   function Prefix_From_Qname (Qname : Byte_Sequence) return Byte_Sequence;
   --  Return the prefix part of Qname, or the empty string if no explicit
   --  prefix is defined.

   procedure Add_Namespace
     (Parser : in out Reader'Class;
      Node   : Element_Access;
      Prefix, URI_Start, URI_End : Token;
      Report_Event : Boolean := True);
   --  Create a new prefix mapping (an XML namespace). If Node is null, then
   --  the mapping is added as a default namespace

   procedure Add_Namespace
     (Parser : in out Reader'Class;
      Node   : Element_Access;
      Prefix : Byte_Sequence;
      URI    : Byte_Sequence;
      Report_Event : Boolean := True);
   --  Same as above, with strings

   procedure Add_Namespace_No_Event
     (Parser : in out Reader'Class;
      Prefix : Byte_Sequence;
      Str    : Byte_Sequence);
   --  Create a new default namespace in the parser

   procedure Free (NS : in out XML_NS);
   --  Free NS and its successors in the list

   procedure Free (Parser : in out Reader'Class);
   --  Free the memory allocated for the parser, including the namespaces,
   --  entities,...

   procedure Free (Elem : in out Element_Access);
   --  Free the memory of Elem (and its contents). Note that this doesn't free
   --  the parent of Elem).
   --  On Exit, Elem is set to its parent.

   procedure Parse_Element_Model
     (Input   : in out Input_Sources.Input_Source'Class;
      Parser  : in out Reader'Class;
      Result  : out Element_Model_Ptr;
      Attlist : Boolean := False;
      Open_Was_Read : Boolean);
   --  Parse the following characters in the stream so as to create an
   --  element or attribute contents model, ie the tree matching an
   --  expression like "(foo|bar)+".
   --  Nmtokens should be true if the names in the model should follow the
   --  Nmtoken rule in XML specifications rather than the Name rule.
   --  If Open_Was_Read, then the opening parenthesis is considered to have
   --  been read already and is automatically inserted into the stack.
   --  Attlist should be set to true if this is the model in <!ELEMENT>

   procedure Fatal_Error
     (Parser : in out Reader'Class;
      Msg    : String;
      Id     : Token := Null_Token);
   --  Raises a fatal error.
   --  The error is reported at location Id (or the current parser location
   --  if Id is Null_Token).
   --  The user application should not return from this call. Thus, a
   --  Program_Error is raised if it does return.

   procedure Error
     (Parser : in out Reader'Class;
      Msg    : String;
      Id     : Token := Null_Token);
   --  Same as Fatal_Error, but reports an error instead

   procedure Warning
     (Parser : in out Reader'Class;
      Msg    : String;
      Id     : Token := Null_Token);
   --  Same as Fatal_Error, but reports a warning instead

   function Location (Parser : Reader'Class; Id : Token) return Byte_Sequence;
   --  Return the location of the start of Id as a string.

   function Resolve_URI (Parser : Reader'Class; URI : Byte_Sequence)
      return Byte_Sequence;
   --  Return a fully resolved URI, based on the system identifier set for
   --  Machine, and URI.

   function Input_Id (Parser : Reader'Class) return Natural;
   pragma Inline (Input_Id);
   --  Return the current input id.

   procedure Close_Inputs (Parser : in out Reader'Class);
   --  Close the inputs that have been completely read. This should be
   --  called every time one starts an entity, so that calls to
   --  Start_Entity/End_Entity are properly nested, and error messages
   --  point to the right entity.

   ------------------
   -- Debug_Encode --
   ------------------

   function Debug_Encode (C : Unicode_Char) return Byte_Sequence is
      Buffer : Byte_Sequence (1 .. 20);
      Index  : Natural := Buffer'First - 1;
   begin
      Encoding.Encode (C, Buffer, Index);
      return Buffer (Buffer'First .. Index);
   end Debug_Encode;

   --------------
   -- Input_Id --
   --------------

   function Input_Id (Parser : Reader'Class) return Natural is
   begin
      if Parser.Inputs = null then
         return 0;
      else
         return Parser.Inputs.Id;
      end if;
   end Input_Id;

   ----------
   -- Free --
   ----------

   procedure Free (Elem : in out Element_Access) is
      procedure Free_Element is new Unchecked_Deallocation
        (Element, Element_Access);
      Tmp : constant Element_Access := Elem.Parent;
   begin
      Free (Elem.NS);
      Free (Elem.Name);
      Free (Elem.Namespaces);
      Free_Element (Elem);
      Elem := Tmp;
   end Free;

   -----------------
   -- Resolve_URI --
   -----------------

   function Resolve_URI (Parser : Reader'Class; URI : Byte_Sequence)
      return Byte_Sequence
   is
      C : Unicode_Char;
      System_Id : constant Byte_Sequence := Get_System_Id (Parser.Locator.all);
      Index : Natural := System_Id'First;
      Basename_Start : Natural := System_Id'First;
      URI_Index : Positive := URI'First;
   begin
      pragma Assert (URI /= "");
      --  ??? Only resolve paths for now
      Encoding.Read (URI, URI_Index, C);
      if C /= Slash then
         while Index <= System_Id'Last loop
            Encoding.Read (System_Id, Index, C);
            if C = Slash then
               Basename_Start := Index;
            end if;
         end loop;
      end if;
      return System_Id (System_Id'First .. Basename_Start - 1) & URI;
   end Resolve_URI;

   --------------
   -- Location --
   --------------

   function Location (Parser : Reader'Class; Id : Token)
      return Byte_Sequence
   is
      Line : constant Byte_Sequence := Natural'Image (Id.Line);
      Col : constant Byte_Sequence := Natural'Image (Id.Column);
   begin
      if Parser.Close_Inputs = null then
         return Get_Public_Id (Parser.Locator.all) & ':'
           & Line (Line'First + 1 .. Line'Last)
           & ':' & Col (Col'First + 1 .. Col'Last);
      else
         return Get_Public_Id (Parser.Close_Inputs.Input.all) & ':'
           & Line (Line'First + 1 .. Line'Last)
           & ':' & Col (Col'First + 1 .. Col'Last);
      end if;
   end Location;

   -----------------
   -- Fatal_Error --
   -----------------

   procedure Fatal_Error
     (Parser  : in out Reader'Class;
      Msg     : String;
      Id      : Token := Null_Token)
   is
      Id2 : Token := Id;
   begin
      if Id = Null_Token then
         Id2.Line   := Get_Line_Number (Parser.Locator.all);
         Id2.Column := Get_Column_Number (Parser.Locator.all) - 1;
      end if;
      Parser.Buffer_Length := 0;
      Fatal_Error
        (Parser, Create (Location (Parser, Id2) & ": " & Msg,
                         Parser.Locator));
      raise Program_Error;
   end Fatal_Error;

   -----------
   -- Error --
   -----------

   procedure Error
     (Parser  : in out Reader'Class;
      Msg     : String;
      Id      : Token := Null_Token)
   is
      Id2 : Token := Id;
   begin
      if Id = Null_Token then
         Id2.Line := Get_Line_Number (Parser.Locator.all);
         Id2.Column := Get_Column_Number (Parser.Locator.all);
      end if;
      Error (Parser, Create (Location (Parser, Id2) & ": " & Msg,
                             Parser.Locator));
   end Error;

   -------------
   -- Warning --
   -------------

   procedure Warning
     (Parser : in out Reader'Class;
      Msg    : String;
      Id     : Token := Null_Token)
   is
      Id2 : Token := Id;
   begin
      if Id = Null_Token then
         Id2.Line := Get_Line_Number (Parser.Locator.all);
         Id2.Column := Get_Column_Number (Parser.Locator.all);
      end if;
      Warning (Parser, Create (Location (Parser, Id2) & ": " & Msg,
                             Parser.Locator));
   end Warning;

   ---------------
   -- Next_Char --
   ---------------

   procedure Next_Char
     (Input   : in out Input_Source'Class;
      Parser  : in out Reader'Class)
   is
      procedure Internal (Stream : in out Input_Source'Class);

      --------------
      -- Internal --
      --------------

      procedure Internal (Stream : in out Input_Source'Class) is
         C : Unicode_Char;
      begin
         Next_Char (Stream, C);
         if C = Line_Feed then
            Set_Column_Number (Parser.Locator.all, 1);
            Set_Line_Number
              (Parser.Locator.all, Get_Line_Number (Parser.Locator.all) + 1);
         else
            Set_Column_Number
              (Parser.Locator.all, Get_Column_Number (Parser.Locator.all) + 1);
         end if;

         --  XML specs say that #xD#xA must be converted to one single #xA.
         --  A single #xD must be converted to one single #xA

         if C = Carriage_Return then
            Parser.Previous_Char_Was_CR := True;
            Parser.Last_Read := Line_Feed;

         elsif C = Line_Feed and then Parser.Previous_Char_Was_CR then
            Parser.Previous_Char_Was_CR := False;
            Next_Char (Input, Parser);
         else
            Parser.Previous_Char_Was_CR := False;
            Parser.Last_Read := C;

            if Always_Test_Valid_Char then
               Test_Valid_Char (Parser, Parser.Last_Read, Null_Token);
            end if;
         end if;
      end Internal;

      Input_A : Entity_Input_Source_Access;
   begin
      while Parser.Inputs /= null and then Eof (Parser.Inputs.Input.all) loop
         Copy (Parser.Locator.all, Parser.Inputs.Save_Loc);
         Free (Parser.Inputs.Save_Loc);

         if Parser.Inputs.External then
            Parser.In_External_Entity := False;
            --  ??? Should test whether we are still in an external entity.
            --  However, this is only used for the <?xml?> PI, and at this
            --  point we have already read and discarded it, so it doesn't
            --  really matter.
         end if;

         --  Insert the closed input at the end of the Close_Input list, so
         --  that the next call to Next_Token properly closes the entity.
         --  This can not be done here, otherwise End_Entity is called too
         --  early, and the error messages do not point to the right entity.
         if Parser.Close_Inputs = null then
            Parser.Close_Inputs := Parser.Inputs;
         else
            Input_A := Parser.Close_Inputs;
            while Input_A.Next /= null loop
               Input_A := Input_A.Next;
            end loop;
            Input_A.Next := Parser.Inputs;
         end if;

         Input_A := Parser.Inputs;
         Parser.Inputs := Parser.Inputs.Next;
         Input_A.Next := null;
      end loop;

      --  Read the text of the entity if there is any

      if Parser.Inputs /= null then
         Internal (Parser.Inputs.Input.all);

      --  Else read from the initial input stream
      elsif Eof (Input) then
         if Debug_Input then
            Put_Line
              ("++Input " & To_String (Parser.Locator.all) & " END_OF_INPUT");
         end if;
         Parser.Last_Read := 16#FFFF#;
         raise Input_Ended;

      else
         Internal (Input);
      end if;

      if Debug_Input then
         Put ("++Input " & To_String (Parser.Locator.all)
              & "(" & Unicode_Char'Image (Parser.Last_Read) & ")= ");
         if Parser.Last_Read /= Line_Feed then
            Put_Line (Debug_Encode (Parser.Last_Read));
         else
            Put_Line ("Line_Feed");
         end if;
      end if;
   end Next_Char;

   -------------------
   -- Put_In_Buffer --
   -------------------

   procedure Put_In_Buffer
     (Parser : in out Reader'Class; Char : Unicode_Char)
   is
      W : constant Natural := Encoding.Width (Char);
      Tmp : Byte_Sequence_Access;
   begin
      --  Loop until we have enough memory to store the string
      while Parser.Buffer_Length + W > Parser.Buffer'Last loop
         Tmp := Parser.Buffer;
         Parser.Buffer := new Byte_Sequence
           (1 .. Tmp'Length * 2);
         Parser.Buffer (1 .. Tmp'Length) := Tmp.all;
         Free (Tmp);
      end loop;

      Encoding.Encode (Char, Parser.Buffer.all, Parser.Buffer_Length);
   end Put_In_Buffer;

   -------------------
   -- Put_In_Buffer --
   -------------------

   procedure Put_In_Buffer
     (Parser : in out Reader'Class; Str : Byte_Sequence)
   is
      Tmp : Byte_Sequence_Access;
   begin
      --  Loop until we have enough memory to store the string
      while Parser.Buffer_Length + Str'Length > Parser.Buffer'Last loop
         Tmp := Parser.Buffer;
         Parser.Buffer := new Byte_Sequence
           (1 .. Tmp'Length * 2);
         Parser.Buffer (1 .. Tmp'Length) := Tmp.all;
         Free (Tmp);
      end loop;

      Parser.Buffer
        (Parser.Buffer_Length + 1 .. Parser.Buffer_Length + Str'Length) := Str;
      Parser.Buffer_Length := Parser.Buffer_Length + Str'Length;
   end Put_In_Buffer;

   ------------------
   -- Is_Name_Char --
   ------------------

   function Is_Name_Char (C : Unicode_Char) return Boolean is
   begin
      return C = Period
        or else C = Hyphen_Minus
        or else C = Spacing_Underscore
        or else Is_Digit (C)
        or else Is_Letter (C)
        or else Is_Combining_Char (C)
        or else Is_Extender (C);
   end Is_Name_Char;

   ---------------------
   -- Test_Valid_Lang --
   ---------------------

   procedure Test_Valid_Lang
     (Parser : in out Reader'Class; Lang : Byte_Sequence)
   is
      C, C2 : Unicode_Char;
      Index : Natural := Lang'First;
   begin
      Encoding.Read (Lang, Index, C2);

      if not (C2 in Latin_Small_Letter_A .. Latin_Small_Letter_Z
              or else C2 in Latin_Capital_Letter_A .. Latin_Capital_Letter_Z)
        or else Index > Lang'Last
      then
         Fatal_Error (Parser, "[2.12] Invalid language specification");
      end if;

      Encoding.Read (Lang, Index, C);
      if C in Latin_Small_Letter_A .. Latin_Small_Letter_Z
        or else C in Latin_Capital_Letter_A .. Latin_Capital_Letter_Z
      then
         if Index <= Lang'Last then
            Encoding.Read (Lang, Index, C);
         end if;

      elsif C2 /= Latin_Small_Letter_I
        and then C2 /= Latin_Capital_Letter_I
        and then C2 /= Latin_Small_Letter_X
        and then C2 /= Latin_Capital_Letter_X
      then
         Fatal_Error (Parser, "[2.12] Invalid language specification");
      end if;

      if C = Hyphen_Minus and then Index > Lang'Last then
         Fatal_Error (Parser, "[2.12] Invalid language specification");
      end if;

      while Index <= Lang'Last loop
         if C /= Hyphen_Minus
           or else Index > Lang'Last
         then
            Fatal_Error (Parser, "[2.12] Invalid language specification");
         end if;

         loop
            Encoding.Read (Lang, Index, C);

            exit when Index > Lang'Last
              or else not
              (C in Latin_Small_Letter_A .. Latin_Small_Letter_Z
               or else C in Latin_Capital_Letter_A .. Latin_Capital_Letter_Z);
         end loop;
      end loop;
   end Test_Valid_Lang;

   -------------------
   -- Is_Pubid_Char --
   -------------------

   function Is_Pubid_Char (C : Unicode_Char) return Boolean is
   begin
      return C = Unicode.Names.Basic_Latin.Space
        or else C = Line_Feed
        or else C in Latin_Small_Letter_A .. Latin_Small_Letter_Z
        or else C in Latin_Capital_Letter_A .. Latin_Capital_Letter_Z
        or else C in Digit_Zero .. Digit_Nine
        or else C = Hyphen_Minus
        or else C = Apostrophe
        or else C = Opening_Parenthesis
        or else C = Closing_Parenthesis
        or else C = Plus_Sign
        or else C = Comma
        or else C = Dot
        or else C = Slash
        or else C = Unicode.Names.Basic_Latin.Colon
        or else C = Equals_Sign
        or else C = Question_Mark
        or else C = Semicolon
        or else C = Exclamation_Mark
        or else C = Star
        or else C = Number_Sign
        or else C = Commercial_At
        or else C = Dollar_Sign
        or else C = Spacing_Underscore
        or else C = Percent_Sign;
   end Is_Pubid_Char;

   ---------------------
   -- Test_Valid_Char --
   ---------------------

   procedure Test_Valid_Char
     (Parser : in out Reader'Class; C : Unicode_Char; Loc : Token)
   is
      Id : Token;
   begin
      if not (C = 16#9#
              or else C = 16#A#
              or else C = 16#D#
              or else C in Unicode.Names.Basic_Latin.Space .. 16#D7FF#
              or else C in 16#E000# .. 16#FFFD#
              or else C in 16#10000# .. 16#10FFFF#)
      then
         if Loc /= Null_Token then
            Id := Loc;
         else
            Id.Line := Get_Line_Number (Parser.Locator.all);
            Id.Column := Get_Column_Number (Parser.Locator.all) - 1;
         end if;
         Fatal_Error
           (Parser, "[2.2] Invalid character (code"
            & Unicode_Char'Image (C) & ")", Id);
      end if;
   end Test_Valid_Char;

   ----------
   -- Free --
   ----------

   procedure Free (NS : in out XML_NS) is
      Tmp : XML_NS;
      procedure Free_NS is new Unchecked_Deallocation (XML_NS_Record, XML_NS);
   begin
      while NS /= null loop
         Tmp := NS.Next;
         Free (NS.Prefix);
         Free (NS.URI);
         Free_NS (NS);
         NS := Tmp;
      end loop;
   end Free;

   -------------
   -- Find_NS --
   -------------

   procedure Find_NS
     (Parser  : in out Reader'Class;
      Elem    : Element_Access;
      Prefix  : Token;
      NS      : out XML_NS;
      Include_Default_NS : Boolean := True) is
   begin
      Find_NS (Parser, Elem, Parser.Buffer (Prefix.First .. Prefix.Last), NS,
               Include_Default_NS);
   end Find_NS;

   -------------
   -- Find_NS --
   -------------

   procedure Find_NS
     (Parser  : in out Reader'Class;
      Elem    : Element_Access;
      Prefix  : Byte_Sequence;
      NS      : out XML_NS;
      Include_Default_NS : Boolean := True)
   is
      E : Element_Access := Elem;
   begin
      loop
         --  Search in the default namespaces
         if E = null then
            NS := Parser.Default_Namespaces;
         else
            NS := E.Namespaces;
         end if;

         while NS /= null loop
            if (Include_Default_NS
                or else E = null
                or else NS.Prefix.all /= "")
              and then NS.Prefix.all = Prefix
            then
               return;
            end if;
            NS := NS.Next;
         end loop;

         exit when E = null;
         E := E.Parent;
      end loop;

      Fatal_Error
        (Parser,
         "[WF] Prefix '" & Prefix & "' must be declared before its use");
      NS := null;
   end Find_NS;

   ---------------------
   -- Qname_From_Name --
   ---------------------

   function Qname_From_Name (Parser : Reader'Class; Prefix, Local_Name : Token)
      return Byte_Sequence is
   begin
      if Prefix = Null_Token then
         return Parser.Buffer (Local_Name.First .. Local_Name.Last);
      else
         return Parser.Buffer (Prefix.First .. Prefix.Last)
           & Colon_Sequence
           & Parser.Buffer (Local_Name.First .. Local_Name.Last);
      end if;
   end Qname_From_Name;

   -----------------------
   -- Prefix_From_Qname --
   -----------------------

   function Prefix_From_Qname (Qname : Byte_Sequence) return Byte_Sequence is
      Index : Natural := Qname'First;
      C : Unicode_Char;
      Previous : Natural;
   begin
      while Index <= Qname'Last loop
         Previous := Index;
         Encoding.Read (Qname, Index, C);
         if C = Unicode.Names.Basic_Latin.Colon then
            return Qname (Qname'First .. Previous - 1);
         end if;
      end loop;
      return "";
   end Prefix_From_Qname;

   ----------------------------
   -- Add_Namespace_No_Event --
   ----------------------------

   procedure Add_Namespace_No_Event
     (Parser : in out Reader'Class;
      Prefix : Byte_Sequence;
      Str    : Byte_Sequence)
   is
      Pref, URI : Token;
   begin
      Pref.First := Parser.Buffer_Length + 1;
      Put_In_Buffer (Parser, Prefix);
      Pref.Last := Parser.Buffer_Length;
      URI.First := Parser.Buffer_Length + 1;
      Put_In_Buffer (Parser, Str);
      URI.Last := Parser.Buffer_Length;
      Add_Namespace (Parser, null, Pref, URI, URI, Report_Event => False);
      Reset_Buffer (Parser, Pref);
   end Add_Namespace_No_Event;

   -------------------
   -- Add_Namespace --
   -------------------

   procedure Add_Namespace
     (Parser : in out Reader'Class;
      Node   : Element_Access;
      Prefix, URI_Start, URI_End : Token;
      Report_Event : Boolean := True) is
   begin
      Add_Namespace
        (Parser       => Parser,
         Node         => Node,
         Prefix       => Parser.Buffer (Prefix.First .. Prefix.Last),
         URI          => Parser.Buffer (URI_Start.First .. URI_End.Last),
         Report_Event => Report_Event);
   end Add_Namespace;

   -------------------
   -- Add_Namespace --
   -------------------

   procedure Add_Namespace
     (Parser : in out Reader'Class;
      Node   : Element_Access;
      Prefix : Byte_Sequence;
      URI    : Byte_Sequence;
      Report_Event : Boolean := True)
   is
      NS : XML_NS;
   begin
      NS := new XML_NS_Record'
        (Prefix => new Byte_Sequence'(Prefix),
         URI    => new Byte_Sequence'(URI),
         Next   => null);

      if Node = null then
         NS.Next := Parser.Default_Namespaces;
         Parser.Default_Namespaces := NS;
      else
         NS.Next := Node.Namespaces;
         Node.Namespaces := NS;
      end if;

      --  Report the event, except for the default namespace
      if Report_Event then
         Start_Prefix_Mapping
           (Parser,
            Prefix => NS.Prefix.all,
            URI    => NS.URI.all);
      end if;
   end Add_Namespace;

   ------------------
   -- Close_Inputs --
   ------------------

   procedure Close_Inputs (Parser : in out Reader'Class) is
      procedure Free is new Unchecked_Deallocation
        (Entity_Input_Source, Entity_Input_Source_Access);
      procedure Unchecked_Free is new Unchecked_Deallocation
        (Input_Source'Class, Input_Source_Access);
      Input_A : Entity_Input_Source_Access;
   begin
      while Parser.Close_Inputs /= null loop
         --  ??? Could use Input_Sources.Locator.Free
         Close (Parser.Close_Inputs.Input.all);
         Unchecked_Free (Parser.Close_Inputs.Input);

         --  not in string context
         if not Parser.State.Ignore_Special then
            End_Entity (Parser, Parser.Close_Inputs.Name.all);
         end if;

         Input_A := Parser.Close_Inputs;
         Parser.Close_Inputs := Parser.Close_Inputs.Next;
         Free (Input_A.Name);
         Free (Input_A);
      end loop;
   end Close_Inputs;

   ----------------
   -- Next_Token --
   ----------------

   procedure Next_Token
     (Input   : in out Input_Source'Class;
      Parser  : in out Reader'Class;
      Id      : out Token;
      Coalesce_Space : Boolean := False)
   is
      function Looking_At (Str : Byte_Sequence) return Boolean;
      --  True if the next characters read (including the current one) in the
      --  stream match Str. Characters read are stored in the buffer

      procedure Handle_Comments;
      --  <!- has been seen in the buffer, check if this is a comment and
      --  handle it appropriately

      procedure Handle_Character_Ref;
      --  '&#' has been seen in the buffer, check if this is a character
      --  entity reference and handle it appropriately

      procedure Handle_Less_Than_Sign;
      --  Handle '<', '<!', '<!--', '<![',... sequences

      procedure Debug_Print;
      --  Print the returned token

      procedure Handle_Entity_Ref;
      --  '&' has been read (as well as the following character). Skips till
      --  the end of the entity, ie ';'. Saves the name of the entity in the
      --  buffer.
      --  Parser.Last_Read is left to ';', but it is not put in the buffer.

      ----------------
      -- Looking_At --
      ----------------

      function Looking_At (Str : Byte_Sequence) return Boolean is
         C : Unicode_Char;
         Index : Natural := Str'First;
      begin
         while Index <= Str'Last loop
            Encoding.Read (Str, Index, C);

            if C /= Parser.Last_Read or else Eof (Input) then
               return False;
            end if;
            Put_In_Buffer (Parser, Parser.Last_Read);
            Next_Char (Input, Parser);
         end loop;
         return True;
      end Looking_At;

      ---------------------
      -- Handle_Comments --
      ---------------------

      procedure Handle_Comments is
      begin
         if not Eof (Input) then
            Next_Char (Input, Parser);
            if Parser.Last_Read = Hyphen_Minus then
               Id.Typ := Comment;  --  In case we reach the eof in the loop
               --  Note that if the file ends exactly with '<!--', we get
               --  an empty text. But at least we will detect the error.
               --  It also fails if we have a non-terminated comment and the
               --  last character in the file is '-'. Doesn't seem worth
               --  paying the cost for some extra tests to handle this.
               loop
                  Next_Char (Input, Parser);
                  if Parser.Last_Read = Hyphen_Minus then
                     Next_Char (Input, Parser);
                     if Parser.Last_Read = Hyphen_Minus then
                        if not Eof (Input) then
                           Next_Char (Input, Parser);
                           if Parser.Last_Read = Greater_Than_Sign then
                              exit;
                           end if;
                        end if;
                        Parser.Buffer_Length := Id.First - 1;
                        Id.Line := Get_Line_Number (Parser.Locator.all);
                        Id.Column :=
                          Get_Column_Number (Parser.Locator.all) - 3;
                        --  3 = 2 * Hyphen_Minus + Parser.Last_Read
                        Fatal_Error
                          (Parser, "[2.5] '--' cannot appear in comments", Id);
                     else
                        Put_In_Buffer (Parser, Hyphen_Minus);
                        Put_In_Buffer (Parser, Parser.Last_Read);
                     end if;
                  else
                     Put_In_Buffer (Parser, Parser.Last_Read);
                  end if;
               end loop;

               if Input_Id (Parser) /= Id.Input_Id then
                  Fatal_Error
                    (Parser, "[4.5] Entity values must be self-contained", Id);
               end if;

               if not Eof (Input) then
                  Next_Char (Input, Parser);
               end if;
               return;
            end if;
         end if;
         Fatal_Error (Parser, "[WF] Invalid characters '<!-' in stream");
         Id.Typ := End_Of_Input;
      end Handle_Comments;

      --------------------------
      -- Handle_Character_Ref --
      --------------------------

      procedure Handle_Character_Ref is
         Val : Unicode_Char := 0;
      begin
         Id.Typ := Text;

         if Parser.Current_Node = null
           and then Parser.State.Name = Default_State.Name
         then
            Fatal_Error
              (Parser,
               "[2.1] Character references can not appear at top-level",  Id);
         end if;

         Next_Char (Input, Parser);
         if Parser.Last_Read = Latin_Small_Letter_X then
            Next_Char (Input, Parser);

            while Parser.Last_Read /= Semicolon loop
               if Parser.Last_Read in Digit_Zero .. Digit_Nine then
                  Val := Val * 16 + Parser.Last_Read - Digit_Zero;

               elsif Parser.Last_Read in
                 Latin_Capital_Letter_A .. Latin_Capital_Letter_F
               then
                  Val := Val * 16 + Parser.Last_Read - Latin_Capital_Letter_A
                    + 10;

               elsif Parser.Last_Read in
                 Latin_Small_Letter_A .. Latin_Small_Letter_F
               then
                  Val := Val * 16 + Parser.Last_Read - Latin_Small_Letter_A
                    + 10;

               else
                  Id.Line := Get_Line_Number (Parser.Locator.all);
                  Id.Column := Get_Column_Number (Parser.Locator.all) - 1;
                  Fatal_Error
                    (Parser, "[4.1] Invalid character '"
                     & Debug_Encode (Parser.Last_Read) & "' in"
                     & " character reference", Id);
               end if;
               Next_Char (Input, Parser);
            end loop;
         else
            while Parser.Last_Read /= Semicolon loop
               if Parser.Last_Read in Digit_Zero .. Digit_Nine then
                  Val := Val * 10 + Parser.Last_Read - Digit_Zero;
               else
                  Id.Line := Get_Line_Number (Parser.Locator.all);
                  Id.Column := Get_Column_Number (Parser.Locator.all) - 1;
                  Fatal_Error
                    (Parser, "[4.1] Invalid character '"
                     & Debug_Encode (Parser.Last_Read) & "' in"
                     & " character reference", Id);
               end if;
               Next_Char (Input, Parser);
            end loop;
         end if;

         Test_Valid_Char (Parser, Val, Id);
         Put_In_Buffer (Parser, Val);
         Next_Char (Input, Parser);
      end Handle_Character_Ref;

      ---------------------------
      -- Handle_Less_Than_Sign --
      ---------------------------

      procedure Handle_Less_Than_Sign is
         Num_Closing_Bracket : Natural;
         Id2 : Token;
      begin
         Id.Typ := Start_Of_Tag;
         Next_Char (Input, Parser);
         case Parser.Last_Read is
            when Slash =>
               Id.Typ := Start_Of_End_Tag;
               Next_Char (Input, Parser);

            when Exclamation_Mark =>
               Next_Char (Input, Parser);
               if Parser.Last_Read = Hyphen_Minus then
                  Handle_Comments;

               elsif Looking_At (Doctype_Sequence) then
                  Reset_Buffer (Parser, Id);
                  Id.Typ := Doctype_Start;

               elsif Parser.Last_Read = Opening_Square_Bracket then
                  Next_Char (Input, Parser);

                  if Parser.Last_Read = Latin_Capital_Letter_C then

                     if not Looking_At (Cdata_Sequence) then
                        Fatal_Error (Parser, "Invalid declaration", Id);
                     end if;

                     if Parser.Last_Read /= Opening_Square_Bracket then
                        Fatal_Error
                          (Parser,
                           "CDATA must be followed immediately by '['", Id);
                     end if;

                     Reset_Buffer (Parser, Id);
                     Id.Typ := Cdata_Section;
                     Num_Closing_Bracket := 1;
                     loop
                        Next_Char (Input, Parser);
                        Put_In_Buffer (Parser, Parser.Last_Read);

                        if Parser.Last_Read = Closing_Square_Bracket then
                           Num_Closing_Bracket := Num_Closing_Bracket + 1;

                        elsif Parser.Last_Read = Greater_Than_Sign
                          and then Num_Closing_Bracket >= 2
                        then
                           Parser.Buffer_Length := Parser.Buffer_Length
                             - 2 * Encoding.Width (Closing_Square_Bracket)
                             - Encoding.Width (Greater_Than_Sign);
                           exit;

                        else
                           Num_Closing_Bracket := 0;
                        end if;
                     end loop;
                     if Id.Input_Id /= Input_Id (Parser) then
                        Fatal_Error
                          (Parser, "[4.3.2] Entity must be self-contained",
                           Id);
                     end if;

                     if not Eof (Input) then
                        Next_Char (Input, Parser);
                     else
                        Parser.Last_Read := 16#FFFF#;
                     end if;

                  else
                     while Is_White_Space (Parser.Last_Read) loop
                        Next_Char (Input, Parser);
                     end loop;

                     if Parser.Last_Read = Latin_Capital_Letter_I
                       or else Parser.Last_Read = Percent_Sign
                     then
                        Next_Token (Input, Parser, Id2);
                        if Parser.Buffer (Id2.First .. Id2.Last) =
                          Include_Sequence
                        then
                           Reset_Buffer (Parser, Id2);
                           Id.Typ := Include;
                        elsif Parser.Buffer (Id2.First .. Id2.Last) =
                          Ignore_Sequence
                        then
                           Reset_Buffer (Parser, Id2);
                           Id.Typ := Ignore;
                        else
                           Fatal_Error (Parser, "Invalid declaration", Id);
                        end if;

                        if not Parser.State.In_DTD
                          or else not Parser.In_External_Entity
                        then
                           Fatal_Error
                             (Parser, "[3.4] INCLUDE and IGNORE sections only"
                              & " authorized in the external DTD subset", Id);
                        end if;

                        Next_Token_Skip_Spaces (Input, Parser, Id2);
                        if Id2.Typ /= Internal_DTD_Start then
                           Fatal_Error
                             (Parser,
                              "(3.4) Conditional sections need a '[' after the"
                              & " INCLUDE or IGNORE", Id2);
                        end if;

                     elsif Parser.State.In_DTD then
                        Id.Typ := Start_Conditional;
                     else
                        Fatal_Error
                          (Parser,
                           "No declaration starting with '<!' outside of DTD",
                           Id);
                     end if;
                  end if;

               elsif not Parser.State.In_DTD then
                  Fatal_Error
                    (Parser,
                     "No declaration starting with '<!' outside of DTD", Id);
                  Id.Typ := End_Of_Input;

               elsif Looking_At (Attlist_Sequence) then
                  Reset_Buffer (Parser, Id);
                  Id.Typ := Attlist_Def;

               elsif Parser.Last_Read = Latin_Capital_Letter_E then
                  Next_Char (Input, Parser);
                  if Looking_At (Ntity_Sequence) then
                     Reset_Buffer (Parser, Id);
                     Id.Typ := Entity_Def;

                  elsif Looking_At (Element_Sequence) then
                     Reset_Buffer (Parser, Id);
                     Id.Typ := Element_Def;

                  else
                     Fatal_Error (Parser, "[WF] Unknown declaration in DTD");
                  end if;

               elsif Looking_At (Notation_Sequence) then
                  Reset_Buffer (Parser, Id);
                  Id.Typ := Notation;

               else
                  Put_In_Buffer (Parser, Less_Than_Sign);
                  Put_In_Buffer (Parser, Exclamation_Mark);
                  Id.Typ := Text;
               end if;

            when Question_Mark =>
               Id.Typ := Start_Of_PI;
               Next_Char (Input, Parser);

            when others => null;
         end case;
      end Handle_Less_Than_Sign;

      -----------------------
      -- Handle_Entity_Ref --
      -----------------------

      procedure Handle_Entity_Ref is
      begin
         if Is_Letter (Parser.Last_Read)
           or else Parser.Last_Read = Spacing_Underscore
         then
            while Parser.Last_Read /= Semicolon
              and then Is_Name_Char (Parser.Last_Read)
            loop
               Put_In_Buffer (Parser, Parser.Last_Read);
               Next_Char (Input, Parser);
            end loop;

            if Parser.Last_Read /= Semicolon then
               Fatal_Error
                 (Parser, "[4.1] Entity references must end with ';'."
                  & ASCII.LF & "Did you want to use &amp; ?", Id);
            end if;

            if Input_Id (Parser) /= Id.Input_Id then
               Fatal_Error
                 (Parser, "[4.3.2] Entity must be self-contained", Id);
            end if;

         else
            Fatal_Error
              (Parser, "[4.1] Invalid first letter in entity name '"
               & Debug_Encode (Parser.Last_Read) & "'", Id);
         end if;
      end Handle_Entity_Ref;

      -----------------
      -- Debug_Print --
      -----------------

      procedure Debug_Print is
         L : Locator_Impl := Locator_Impl (Parser.Locator.all);
      begin
         Set_Line_Number (L, Id.Line);
         Set_Column_Number (L, Id.Column);
         Put ("++Lex (" & Parser.State.Name & ") at "
              & To_String (L) & " (" & Token_Type'Image (Id.Typ) & ")");
         if Parser.State.Ignore_Special then
            Put (" (in string)");
         end if;

         if Id.Typ = Space then
            declare
               J : Natural := Id.First;
               C : Unicode_Char;
            begin
               Put (" --");
               while J <= Id.Last loop
                  Encoding.Read (Parser.Buffer.all, J, C);
                  Put (Unicode_Char'Image (C));
               end loop;
               Put ("--");
            end;

         elsif Id.Last >= Id.First then
            Put (" --" & Parser.Buffer (Id.First .. Id.Last) & "--");
         end if;

         Put_Line
           (" buffer="
            & Parser.Buffer (Parser.Buffer'First .. Parser.Buffer_Length)
            & "--");
      end Debug_Print;

      type Entity_Ref is (None, Entity, Param_Entity);
      Is_Entity_Ref : Entity_Ref := None;
   begin
      Id.First := Parser.Buffer_Length + 1;
      Id.Last := Parser.Buffer_Length;
      Id.Typ := End_Of_Input;
      Id.Line := Get_Line_Number (Parser.Locator.all);
      Id.Column := Get_Column_Number (Parser.Locator.all) - 1;
      Id.Input_Id := Input_Id (Parser);
      Close_Inputs (Parser);

      if Eof (Input) and then Parser.Last_Read = 16#FFFF# then
         Id.Column := Id.Column + 1;
         return;
      end if;

      if Is_White_Space (Parser.Last_Read) then
         Id.Typ := Space;
         loop
            Put_In_Buffer (Parser, Parser.Last_Read);
            Next_Char (Input, Parser);
            exit when not Is_White_Space (Parser.Last_Read);
         end loop;

      --  If we are ignoring special characters
      elsif Id.Typ = End_Of_Input
        and then not Parser.Ignore_State_Special
        and then Parser.State.Ignore_Special
        and then not Parser.State.Detect_End_Of_PI
      then
         Id.Typ := Text;
         Parser.Ignore_State_Special := True;
         loop
            exit when Parser.Last_Read = Ampersand
              and then (Parser.State.Expand_Entities
                        or else Parser.State.Expand_Character_Ref);
            exit when Parser.Last_Read = Percent_Sign
              and then Parser.State.Expand_Param_Entities;
            exit when (Parser.Last_Read = Apostrophe
                       or else Parser.Last_Read = Quotation_Mark)
              and then Parser.State.Handle_Strings
              and then (Parser.Inputs = null
                        or else Parser.Inputs.Handle_Strings);
            exit when Parser.Last_Read = Less_Than_Sign
              and then Parser.State.Less_Special;
            Put_In_Buffer (Parser, Parser.Last_Read);
            Next_Char (Input, Parser);
         end loop;
      end if;

      --  If we haven't found a non-empty token yet
      if Id.Typ = End_Of_Input
        or else Id.First > Parser.Buffer_Length
      then
         case Parser.Last_Read is
            when Less_Than_Sign =>
               if Parser.State.Less_Special then
                  Id.Typ := Start_Of_Tag;
                  Next_Char (Input, Parser);
               elsif Parser.State.Detect_End_Of_PI then
                  Put_In_Buffer (Parser, Parser.Last_Read);
                  Id.Typ := Text;
                  Next_Char (Input, Parser);
               else
                  Handle_Less_Than_Sign;
               end if;

            when Question_Mark =>
               if Eof (Input) then
                  Put_In_Buffer (Parser, Parser.Last_Read);
                  Id.Typ := Text;
               else
                  Next_Char (Input, Parser);
                  if Parser.Last_Read = Greater_Than_Sign then
                     Id.Typ := End_Of_PI;
                     Next_Char (Input, Parser);
                  elsif Parser.Last_Read = Question_Mark then
                     Put_In_Buffer (Parser, Question_Mark);
                     Id.Typ := Text;
                  else
                     Put_In_Buffer (Parser, Question_Mark);
                     Id.Typ := Text;
                  end if;
               end if;

            when Greater_Than_Sign =>
               if Parser.State.Greater_Special then
                  Id.Typ := End_Of_Tag;
               else
                  Put_In_Buffer (Parser, Parser.Last_Read);
                  Id.Typ := Text;
               end if;
               Next_Char (Input, Parser);

            when Equals_Sign =>
               if Parser.State.In_Tag then
                  Id.Typ := Equal;
               else
                  Put_In_Buffer (Parser, Parser.Last_Read);
                  Id.Typ := Text;
               end if;
               Next_Char (Input, Parser);

            when Unicode.Names.Basic_Latin.Colon =>
               if Parser.State.In_Tag then
                  Id.Typ := Colon;
               else
                  Put_In_Buffer (Parser, Parser.Last_Read);
                  Id.Typ := Text;
               end if;
               Next_Char (Input, Parser);

            when Ampersand =>
               Id.Typ := Text; --  So that eof would at least report an error
               if Eof (Input)
                 and then Parser.State.Expand_Entities
               then
                  Fatal_Error
                    (Parser, "[4.1] Entity references must end with ';'."
                     & ASCII.LF & "Did you want to use &amp; ?", Id);
               end if;

               Next_Char (Input, Parser);
               if Parser.Last_Read = Number_Sign
                 and then Parser.State.Expand_Character_Ref
               then
                  Handle_Character_Ref;
                  if Input_Id (Parser) /= Id.Input_Id then
                     Fatal_Error
                       (Parser, "[4.3.2] Entity must be self-contained",
                        Id);
                  end if;

               elsif Parser.Last_Read /= Number_Sign
                 and then Parser.State.Expand_Entities
               then
                  Handle_Entity_Ref;
                  Is_Entity_Ref := Entity;

               elsif Parser.Last_Read /= Number_Sign
                 and then Parser.State.Ignore_Special   --  string context
                 and then not Parser.State.Detect_End_Of_PI  --  not in PI
               then
                  --  Inside a string (entity value), we still need to check
                  --  that the '&' marks the beginning of an entity reference.
                  Put_In_Buffer (Parser, Ampersand);
                  Handle_Entity_Ref;
                  Put_In_Buffer (Parser, Parser.Last_Read);
                  Next_Char (Input, Parser);

               else
                  Put_In_Buffer (Parser, Ampersand);
               end if;

            when Percent_Sign =>
               Put_In_Buffer (Parser, Parser.Last_Read);
               Id.Typ := Text;

               Next_Char (Input, Parser);
               if Parser.State.Expand_Param_Entities then
                  while Parser.Last_Read /= Semicolon
                    and then Is_Name_Char (Parser.Last_Read)
                  loop
                     Put_In_Buffer (Parser, Parser.Last_Read);
                     Next_Char (Input, Parser);
                  end loop;

                  if Parser.Last_Read /= Semicolon then
                     Fatal_Error (Parser, "[WF] Unterminated entity");
                  end if;
                  Is_Entity_Ref := Param_Entity;
               end if;

            when Quotation_Mark =>
               if Parser.State.Handle_Strings then
                  Id.Typ := Double_String_Delimiter;
                  Next_Char (Input, Parser);
               else
                  Id.Typ := Text;
                  Put_In_Buffer (Parser, Parser.Last_Read);
                  Next_Char (Input, Parser);
               end if;

            when Apostrophe =>
               if Parser.State.Handle_Strings then
                  Id.Typ := Single_String_Delimiter;
                  Next_Char (Input, Parser);
               else
                  Id.Typ := Text;
                  Put_In_Buffer (Parser, Parser.Last_Read);
                  Next_Char (Input, Parser);
               end if;

            when Opening_Square_Bracket =>
               if Parser.State.In_DTD then
                  Id.Typ := Internal_DTD_Start;
               else
                  Put_In_Buffer (Parser, Parser.Last_Read);
                  Id.Typ := Text;
               end if;
               Next_Char (Input, Parser);

            when Closing_Square_Bracket =>
               if Parser.State.In_DTD
                 and then not Parser.In_External_Entity
               then
                  Id.Typ := Internal_DTD_End;
                  loop
                     Next_Char (Input, Parser);
                     exit when Parser.Last_Read = Greater_Than_Sign;
                     if not Is_White_Space (Parser.Last_Read) then
                        Fatal_Error
                          (Parser, "[2.8] Unexpected character between ']'"
                           & " and '>' in the DTD", Id);
                     end if;
                  end loop;
                  Next_Char (Input, Parser);

               --  In string context ?
               elsif Parser.State.Ignore_Special then
                  Id.Typ := Text;
                  Put_In_Buffer (Parser, Parser.Last_Read);
                  Next_Char (Input, Parser);

               else
                  declare
                     Num_Bracket : Natural := 1;
                  begin
                     Id.Typ := Text;

                     loop
                        Put_In_Buffer (Parser, Parser.Last_Read);
                        Next_Char (Input, Parser);

                        if Parser.Last_Read = Closing_Square_Bracket then
                           Num_Bracket := Num_Bracket + 1;

                        elsif Num_Bracket >= 2
                          and Parser.Last_Read = Greater_Than_Sign
                        then
                           if Parser.State.In_DTD
                             and then Parser.In_External_Entity
                           then
                              Id.Typ := End_Conditional;
                              Reset_Buffer (Parser, Id);
                              Next_Char (Input, Parser);
                              exit;
                           else
                              Id.Column := Id.Column + Num_Bracket - 2;
                              Fatal_Error
                                (Parser,
                                 "[2.4] Text may not contain the litteral"
                                 & " ']]>'", Id);
                           end if;
                        else
                           exit;
                        end if;
                     end loop;
                  end;
               end if;

            when Slash =>
               Id.Typ := Text;
               Next_Char (Input, Parser);
               if Parser.State.Greater_Special
                 and then Parser.Last_Read = Greater_Than_Sign
               then
                  Id.Typ := End_Of_Start_Tag;
                  Next_Char (Input, Parser);
               else
                  Put_In_Buffer (Parser, Slash);
               end if;

            when others =>
               if Parser.State.Recognize_External then

                  if Parser.Last_Read = Latin_Capital_Letter_A then
                     if Looking_At (Any_Sequence) then
                        Reset_Buffer (Parser, Id);
                        Id.Typ := Any;
                     else
                        Id.Typ := Name;
                     end if;

                  elsif Parser.Last_Read = Latin_Capital_Letter_E then
                     if Looking_At (Empty_Sequence) then
                        Reset_Buffer (Parser, Id);
                        Id.Typ := Empty;
                     else
                        Id.Typ := Name;
                     end if;

                  elsif Parser.Last_Read = Latin_Capital_Letter_N then
                     if Looking_At (Ndata_Sequence) then
                        Reset_Buffer (Parser, Id);
                        Id.Typ := Ndata;
                     else
                        Id.Typ := Name;
                     end if;

                  elsif Parser.Last_Read = Latin_Capital_Letter_P then
                     if Looking_At (Public_Sequence) then
                        Reset_Buffer (Parser, Id);
                        Id.Typ := Public;
                     else
                        Id.Typ := Name;
                     end if;

                  elsif Parser.Last_Read = Latin_Capital_Letter_S then
                     if Looking_At (System_Sequence) then
                        Reset_Buffer (Parser, Id);
                        Id.Typ := System;
                     else
                        Id.Typ := Name;
                     end if;
                  end if;
               end if;

               if Parser.State.Report_Parenthesis
                 and then Parser.Last_Read = Opening_Parenthesis
               then
                  Reset_Buffer (Parser, Id);
                  Id.Typ := Open_Paren;
                  Next_Char (Input, Parser);
                  return;
               end if;

               if Parser.State.In_Attlist then
                  if Parser.Last_Read = Latin_Capital_Letter_C then
                     if Looking_At (Cdata_Sequence) then
                        Id.Typ := Cdata;
                     else
                        Id.Typ := Name;
                     end if;

                  elsif Parser.Last_Read = Latin_Capital_Letter_E
                    and then Looking_At (Entit_Sequence)
                  then
                     if Looking_At (Ies_Sequence) then
                        Id.Typ := Entities;
                     elsif Parser.Last_Read = Latin_Capital_Letter_Y then
                        Id.Typ := Entity;
                        Put_In_Buffer (Parser, Parser.Last_Read);
                        Next_Char (Input, Parser);
                     else
                        Fatal_Error
                          (Parser, "[WF] Unexpected type in ATTLIST");
                     end if;

                  elsif Parser.Last_Read = Latin_Capital_Letter_I
                    and then Looking_At (Id_Sequence)
                  then
                     if Looking_At (Ref_Sequence) then
                        if Parser.Last_Read = Latin_Capital_Letter_S then
                           Id.Typ := Idrefs;
                           Put_In_Buffer (Parser, Parser.Last_Read);
                           Next_Char (Input, Parser);
                        else
                           Id.Typ := Idref;
                        end if;
                     else
                        Id.Typ := Id_Type;
                     end if;

                  elsif Parser.Last_Read = Latin_Capital_Letter_N then
                     Next_Char (Input, Parser);
                     if Looking_At (Mtoken_Sequence) then
                        if Parser.Last_Read = Latin_Capital_Letter_S then
                           Id.Typ := Nmtokens;
                           Next_Char (Input, Parser);
                        else
                           Id.Typ := Nmtoken;
                        end if;
                     elsif Looking_At (Otation_Sequence) then
                        Id.Typ := Notation;
                     else
                        Fatal_Error
                          (Parser, "[WF] Invalid type for attribute");
                     end if;

                  elsif Parser.Last_Read = Number_Sign then
                     Put_In_Buffer (Parser, Parser.Last_Read);
                     Next_Char (Input, Parser);
                     if Looking_At (Implied_Sequence) then
                        Id.Typ := Implied;
                     elsif Looking_At (Required_Sequence) then
                        Id.Typ := Required;
                     elsif Looking_At (Fixed_Sequence) then
                        Id.Typ := Fixed;
                     else
                        Fatal_Error (Parser, "[WF] Invalid keyword");
                     end if;
                  end if;
               end if;
         end case;

         --  try to coalesce as many things as possible into a single
         --  text event
         if Id.Typ = End_Of_Input then
            if Is_Letter (Parser.Last_Read)
              or else Parser.Last_Read = Spacing_Underscore
            then
               Id.Typ := Name;
               Put_In_Buffer (Parser, Parser.Last_Read);
               Next_Char (Input, Parser);
            else
               Id.Typ := Text;
            end if;
         end if;

         if Id.Typ = Name and then not Coalesce_Space then
            while Is_Name_Char (Parser.Last_Read) loop
               Put_In_Buffer (Parser, Parser.Last_Read);
               Next_Char (Input, Parser);
            end loop;

         elsif Is_Entity_Ref = None
           and then (Id.Typ = Text
                     or else (Coalesce_Space and then Id.Typ = Name))
         then
            while (Coalesce_Space
                   or else not Is_White_Space (Parser.Last_Read))
              and then (not Parser.State.Greater_Special
                        or else Parser.Last_Read /= Greater_Than_Sign)
              and then Parser.Last_Read /= Less_Than_Sign
              and then Parser.Last_Read /= Ampersand
              and then (not Parser.State.Expand_Param_Entities
                        or else Parser.Last_Read /= Percent_Sign)
              and then Parser.Last_Read /= Equals_Sign
              and then Parser.Last_Read /= Quotation_Mark
              and then Parser.Last_Read /= Apostrophe
              and then Parser.Last_Read /= Closing_Square_Bracket
              and then Parser.Last_Read /= Slash
              and then (Parser.Last_Read /= Question_Mark
                        or else not Parser.State.Detect_End_Of_PI)
            loop
               Put_In_Buffer (Parser, Parser.Last_Read);
               Next_Char (Input, Parser);
            end loop;
         end if;

         Parser.Ignore_State_Special := False;
      end if;

      if Coalesce_Space and then Id.Typ = Space then
         --  First character is necessarily not a space, so we'll change the
         --  type of the token to text
         declare
            Save_Length : constant Natural := Parser.Buffer_Length;
         begin
            while (not Parser.State.Greater_Special
                   or else Parser.Last_Read /= Greater_Than_Sign)
              and then Parser.Last_Read /= Less_Than_Sign
              and then Parser.Last_Read /= Ampersand
              and then (not Parser.State.Expand_Param_Entities

                        or else Parser.Last_Read /= Percent_Sign)
              and then Parser.Last_Read /= Equals_Sign
              and then Parser.Last_Read /= Quotation_Mark
              and then Parser.Last_Read /= Closing_Square_Bracket
              and then Parser.Last_Read /= Apostrophe
              and then Parser.Last_Read /= Slash
              and then (Parser.Last_Read /= Question_Mark
                        or else not Parser.State.Detect_End_Of_PI)
            loop
               Put_In_Buffer (Parser, Parser.Last_Read);
               Next_Char (Input, Parser);
            end loop;

            --  Special case for ']': since the parser needs to detect whether
            --  this is the beginning of ']]>', this will be done in the next
            --  call to Next_Token. However, we shouldn't report the spaces as
            --  Ignorable_Whitespace in this case.

            if Parser.Last_Read = Closing_Square_Bracket
              or else Parser.Buffer_Length /= Save_Length
            then
               Id.Typ := Text;
            end if;
         end;
      end if;


      Id.Last := Parser.Buffer_Length;

      if Debug_Lexical then
         Debug_Print;
      end if;

      --  Internal entities should be processes inline

      if Is_Entity_Ref /= None then
         declare
            N : constant Byte_Sequence := Parser.Buffer (Id.First .. Id.Last);
            V : Entity_Entry_Access := Get (Parser.Entities, N);
            Null_Loc : Locator_Impl;
         begin
            Reset_Buffer (Parser, Id);
            if N = Lt_Sequence then
               Put_In_Buffer (Parser, Less_Than_Sign);
               Id.Typ := Text;
               Id.Last := Parser.Buffer_Length;
               Next_Char (Input, Parser);

            elsif N = Gt_Sequence then
               Put_In_Buffer (Parser, Greater_Than_Sign);
               Id.Typ := Text;
               Id.Last := Parser.Buffer_Length;
               Next_Char (Input, Parser);

            elsif N = Amp_Sequence then
               Put_In_Buffer (Parser, Ampersand);
               Id.Typ := Text;
               Id.Last := Parser.Buffer_Length;
               Next_Char (Input, Parser);

            elsif N = Apos_Sequence then
               Put_In_Buffer (Parser, Apostrophe);
               Id.Typ := Text;
               Id.Last := Parser.Buffer_Length;
               Next_Char (Input, Parser);

            elsif N = Quot_Sequence then
               Put_In_Buffer (Parser, Quotation_Mark);
               Id.Typ := Text;
               Id.Last := Parser.Buffer_Length;
               Next_Char (Input, Parser);

            elsif V = null then
               Skipped_Entity (Parser, N);
               Error (Parser, "[4.1] Undefined entity '" & N & ''', Id);
               Id.Typ := Text;
               Id.Last := Id.First - 1;
               Next_Char (Input, Parser);

            else
               if Is_Entity_Ref = Entity
                 and then Parser.Current_Node = null
                 and then not Parser.State.In_DTD
               then
                  Fatal_Error
                    (Parser,
                     "[2.1] Entity references can not appear at top-level",
                     Id);

               --  Else if we are in the internal subset of the DTD, and in
               --  a context other than a declaration
               elsif Is_Entity_Ref = Param_Entity
                 and then not Parser.In_External_Entity
                 and then Parser.State.Name /= DTD_State.Name
               then
                  Fatal_Error
                    (Parser, "[WF PE in internal subset] Parameter entities"
                     & " cannot occur in attribute values", Id);
               end if;

               Close_Inputs (Parser);

               --  not in string context
               if not Parser.State.Ignore_Special then
                  Start_Entity (Parser, N);
               end if;

               if V.Already_Read then
                  Fatal_Error
                    (Parser, "(4.1) Entity can not reference itself", Id);
               end if;

               V.Already_Read := True;

               Parser.Element_Id := Parser.Element_Id + 1;
               Parser.Inputs := new Entity_Input_Source'
                 (External       => V.External,
                  Name           => new Byte_Sequence'(N),
                  Input          => null,
                  Save_Loc       => Null_Loc,
                  Id             => Parser.Element_Id,
                  Handle_Strings => not Parser.State.Ignore_Special,
                  Next           => Parser.Inputs);
               Copy (Parser.Inputs.Save_Loc, Parser.Locator.all);

               if V.External then
                  if Parser.State.Name = Attlist_Str_Def_State.Name
                    or else Parser.State.Name = Attr_Value_State.Name
                  then
                     Fatal_Error
                       (Parser, "[3.1] Attribute values can not reference"
                        & " external entities", Id);
                  end if;

                  declare
                     URI : constant Byte_Sequence :=
                       Resolve_URI (Parser, V.Value.all);
                  begin
                     Parser.Inputs.Input := new File_Input;
                     Open (URI, File_Input (Parser.Inputs.Input.all));
                     Set_System_Id (Parser.Locator.all, URI);
                     Set_Public_Id (Parser.Locator.all, V.Value.all);
                     Parser.In_External_Entity := True;
                  exception
                     when Name_Error =>
                        Error (Parser,
                               "External entity not found: " & URI, Id);
                  end;
               else
                  Parser.Inputs.Input := new String_Input;
                  Open (V.Value, Encoding,
                        String_Input (Parser.Inputs.Input.all));
                  Set_Public_Id (Parser.Locator.all, "entity " & N);
               end if;

               Set_Public_Id
                 (Parser.Inputs.Input.all, Get_Public_Id (Parser.Locator.all));
               Set_Line_Number (Parser.Locator.all, 1);
               Set_Column_Number
                 (Parser.Locator.all,
                  1 + Prolog_Size (Parser.Inputs.Input.all));

               Next_Char (Input, Parser);
               Next_Token (Input, Parser, Id);

               V.Already_Read := False;
            end if;
         end;
      end if;

   exception
      when Input_Ended =>
            --  Make sure we always emit the last characters in the buffer
         Id.Last := Parser.Buffer_Length;
         if Debug_Lexical then
            Debug_Print;
         end if;

         if Id.Typ = Cdata_Section then
            Fatal_Error
              (Parser, "[2.7] CDATA sections must end with ']]>'", Id);
         elsif Id.Typ = Comment then
            Fatal_Error
              (Parser, "[2.5] Comments must end with '-->'", Id);
         end if;
   end Next_Token;

   ----------------------------
   -- Next_Token_Skip_Spaces --
   ----------------------------

   procedure Next_Token_Skip_Spaces
     (Input  : in out Input_Sources.Input_Source'Class;
      Parser : in out Reader'Class;
      Id     : out Token;
      Must_Have : Boolean := False) is
   begin
      Next_Token (Input, Parser, Id);
      if Must_Have and then Id.Typ /= Space then
         Fatal_Error (Parser, "Expecting a space", Id);
      end if;
      while Id.Typ = Space loop
         Reset_Buffer (Parser, Id);
         Next_Token (Input, Parser, Id);
      end loop;
   end Next_Token_Skip_Spaces;

   ------------------
   -- Reset_Buffer --
   ------------------

   procedure Reset_Buffer
     (Parser : in out Reader'Class; Id : Token := Null_Token) is
   begin
      Parser.Buffer_Length := Id.First - 1;
   end Reset_Buffer;

   ---------------
   -- Set_State --
   ---------------

   procedure Set_State
     (Parser : in out Reader'Class; State : Parser_State) is
   begin
      Parser.State := State;
   end Set_State;

   ---------------
   -- Get_State --
   ---------------

   function Get_State (Parser : Reader'Class) return Parser_State is
   begin
      return Parser.State;
   end Get_State;

   -------------------------
   -- Parse_Element_Model --
   -------------------------

   procedure Parse_Element_Model
     (Input   : in out Input_Source'Class;
      Parser  : in out Reader'Class;
      Result  : out Element_Model_Ptr;
      Attlist : Boolean := False;
      Open_Was_Read : Boolean)
   is
      --  ??? Would be nice to get rid of this hard-coded limitation in stacks
      Stack_Size : constant Natural := 64;
      Operand_Stack : Element_Model_Array (1 .. Stack_Size);
      Operand_Index : Natural := Operand_Stack'First;
      Operator_Stack : array (1 .. Stack_Size) of Unicode_Char;
      Operator_Index : Natural := Operator_Stack'First;
      Expect_Operator : Boolean := not Open_Was_Read;

      procedure Parse_Element_Model_From_Entity (Name : Byte_Sequence);
      --  Parse the element model defined in the entity Name, and leave the
      --  contents on the stacks.

      procedure Parse
        (Input         : in out Input_Source'Class;
         Result        : out Element_Model_Ptr;
         Open_Was_Read : Boolean;
         Is_Recursive_Call : Boolean);
      --  Parse the content model read in Input
      --  Is_Recursive_Call should be true when called from itself or from
      --  Parse_Element_Model_From_Entity.

      -------------------------------------
      -- Parse_Element_Model_From_Entity --
      -------------------------------------

      procedure Parse_Element_Model_From_Entity (Name : Byte_Sequence) is
         Loc : Locator_Impl;
         Last : constant Unicode_Char := Parser.Last_Read;
         Input_S : String_Input;
         Val : constant Entity_Entry_Access := Get (Parser.Entities, Name);
         M : Element_Model_Ptr;
      begin
         if Val = null then
            Fatal_Error (Parser, "Unknown entity " & Name);

         elsif Val.Value.all = "" then
            return;

         else
            Copy (Loc, Parser.Locator.all);
            Set_Line_Number (Parser.Locator.all, 1);
            Set_Column_Number (Parser.Locator.all, 1);
            Set_Public_Id (Parser.Locator.all, "entity " & Name);

            Open (Val.Value, Encoding, Input_S);
            Next_Char (Input_S, Parser);
            Parse (Input_S, M, False, True);
            --  Parse_Element_Model (Input_S, Parser, M, Attlist, False);
            Close (Input_S);

            Copy (Parser.Locator.all, Loc);
            Free (Loc);
            Parser.Last_Read := Last;
         end if;
      end Parse_Element_Model_From_Entity;

      -----------
      -- Parse --
      -----------

      procedure Parse
        (Input : in out Input_Source'Class;
         Result : out Element_Model_Ptr;
         Open_Was_Read : Boolean;
         Is_Recursive_Call : Boolean)
      is
         Num_Items : Positive;
         Current_Item, Current_Operand : Natural;
         Start_Sub : Natural;
         M : Element_Model_Ptr;
         Found : Boolean;
         Start_Id : constant Natural := Input_Id (Parser);
         Start_Token : Token;
         Test_Multiplier : Boolean;
         Can_Be_Mixed : Boolean;
         Num_Parenthesis : Integer := 0;

      begin
         Start_Token.Line := Get_Line_Number (Parser.Locator.all);
         Start_Token.Column := Get_Column_Number (Parser.Locator.all) - 1;

         if Open_Was_Read then
            Start_Token.Column := Start_Token.Column - 1;
         end if;

         while Is_White_Space (Parser.Last_Read) loop
            Next_Char (Input, Parser);
         end loop;

         loop
            if Input_Id (Parser) /= Start_Id then
               Fatal_Error
                 (Parser, "[4.5] Entity values must be self-contained",
                  Start_Token);
            end if;

            Test_Multiplier := False;

            --  Process the operator
            case Parser.Last_Read is
               when Opening_Parenthesis =>
                  Operator_Stack (Operator_Index) := Parser.Last_Read;
                  Operator_Index := Operator_Index + 1;
                  Expect_Operator := False;
                  Next_Char (Input, Parser);
                  Num_Parenthesis := Num_Parenthesis + 1;

               when Closing_Parenthesis =>
                  Num_Parenthesis := Num_Parenthesis - 1;
                  Num_Items := 1;
                  Current_Item := Operator_Index - 1;
                  Current_Operand := Operand_Index - 1;
                  Can_Be_Mixed :=  Current_Operand >= Operand_Stack'First
                    and then
                    (Operand_Stack (Current_Operand).Content = Character_Data
                     or else Operand_Stack (Current_Operand).Content
                     = Element_Ref);

                  if Current_Operand >= Operand_Stack'First
                    and then Is_Mixed (Operand_Stack (Current_Operand))
                  then
                     Fatal_Error
                       (Parser, "[3.2.1] Mixed contents can not be used in"
                        & " a list or a sequence");
                  end if;

                  while Current_Item >= Operator_Stack'First
                    and then
                      Operator_Stack (Current_Item) /= Opening_Parenthesis
                  loop
                     if Operator_Stack (Current_Item) /= Comma
                       and then Operator_Stack (Current_Item) /= Vertical_Line
                     then
                        Fatal_Error
                          (Parser, "Invalid content model", Start_Token);
                     end if;

                     Current_Operand := Current_Operand - 1;

                     if Current_Operand < Operand_Stack'First then
                        Fatal_Error
                          (Parser, "Invalid content model", Start_Token);
                     end if;

                     if Operand_Stack (Current_Operand).Content
                       /= Character_Data and then
                       Operand_Stack (Current_Operand).Content /= Element_Ref
                     then
                        Can_Be_Mixed := False;
                     end if;

                     if Is_Mixed (Operand_Stack (Current_Operand)) then
                        Fatal_Error
                          (Parser, "[3.2.1] Mixed contents can not be used in"
                           & " a list or a sequence");
                     end if;

                     Num_Items := Num_Items + 1;
                     Current_Item := Current_Item - 1;
                  end loop;

                  if Current_Item < Operator_Stack'First then
                     Fatal_Error
                       (Parser, "Invalid content model", Start_Token);
                  end if;
                  if Current_Operand < Operand_Stack'First then
                     Fatal_Error
                       (Parser, "Invalid content model: "
                        & "List of choices cannot be empty", Start_Token);
                  end if;

                  if Operator_Stack (Operator_Index - 1) = Comma then
                     M := new Element_Model (Sequence);
                  else
                     if not Can_Be_Mixed
                       and then Operand_Stack (Current_Operand).Content
                       = Character_Data
                     then
                        Fatal_Error
                          (Parser, "[3.2.2] Nested groups and occurence"
                           & " operators not allowed in mixed content");
                     end if;

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
                  Test_Multiplier := True;
                  Next_Char (Input, Parser);

               when Comma | Vertical_Line =>
                  if Attlist and then Parser.Last_Read = Comma then
                     Fatal_Error
                       (Parser,
                        "[3.3.1] Invalid character ','"
                        & " in ATTLIST enumeration");
                  end if;

                  if Parser.Last_Read = Comma
                    and then Operator_Stack (Operator_Index - 1)
                    = Opening_Parenthesis
                    and then Operand_Stack (Operand_Index - 1).Content
                    = Character_Data
                  then
                     Fatal_Error
                       (Parser,
                        "[3.2.2] #PCDATA can only be used with"
                        & " '|' connectors");
                  end if;

                  if Operator_Index = Operator_Stack'First
                    or else
                    (Operator_Stack (Operator_Index - 1) /= Parser.Last_Read
                     and then
                     Operator_Stack (Operator_Index - 1) /=
                       Opening_Parenthesis)
                  then
                     Fatal_Error
                       (Parser, "Can't mix ',' and '|' in content model");
                  end if;
                  Operator_Stack (Operator_Index) := Parser.Last_Read;
                  Operator_Index := Operator_Index + 1;
                  Expect_Operator := False;
                  Next_Char (Input, Parser);

               when Star | Question_Mark | Plus_Sign =>
                  Fatal_Error
                    (Parser, "[3.2.1] Invalid location '+', '?' or '*' "
                     & "operator", Start_Token);

               when Number_Sign =>
                  if Expect_Operator then
                     Fatal_Error
                       (Parser, "Invalid content model, cannot start with #",
                        Start_Token);
                  end if;
                  Expect_Operator := True;

                  --  #PCDATA can only be the first element of a choice list
                  --  ??? Note that in that case the Choice model can only be a
                  --  list of names, not a parenthesis expression.
                  Start_Sub := Parser.Buffer_Length + 1;

                  Next_Char (Input, Parser);
                  Found := (Parser.Last_Read = Latin_Capital_Letter_P);
                  if Found then
                     Next_Char (Input, Parser);
                     Found := (Parser.Last_Read = Latin_Capital_Letter_C);
                     if Found then
                        Next_Char (Input, Parser);
                        Found := (Parser.Last_Read = Latin_Capital_Letter_D);
                        if Found then
                           Next_Char (Input, Parser);
                           Found := Parser.Last_Read = Latin_Capital_Letter_A;
                           if Found then
                              Next_Char (Input, Parser);
                              Found :=
                                (Parser.Last_Read = Latin_Capital_Letter_T);
                              if Found then
                                 Next_Char (Input, Parser);
                                 Found :=
                                   (Parser.Last_Read = Latin_Capital_Letter_A);
                              end if;
                           end if;
                        end if;
                     end if;
                  end if;

                  if not Found then
                     Fatal_Error
                       (Parser, "[WF] Invalid sequence in content model",
                        Start_Token);
                  end if;

                  if Operator_Stack (Operator_Index - 1)
                    /= Opening_Parenthesis
                  then
                     Fatal_Error
                       (Parser, "[3.2.2] #PCDATA must be first in list");
                  end if;

                  Operand_Stack (Operand_Index) :=
                    new Element_Model (Character_Data);
                  Operand_Index := Operand_Index + 1;
                  Parser.Buffer_Length := Start_Sub - 1;
                  Next_Char (Input, Parser);

               when Percent_Sign =>
                  if not Parser.In_External_Entity
                    and then Parser.State.Name /= DTD_State.Name
                  then
                     Fatal_Error
                       (Parser, "[WF PE in internal subset] Parameter entities"
                        & " cannot occur in attribute values");
                  end if;

                  Start_Sub := Parser.Buffer_Length + 1;

                  while Parser.Last_Read /= Semicolon loop
                     Put_In_Buffer (Parser, Parser.Last_Read);
                     Next_Char (Input, Parser);
                  end loop;

                  Parse_Element_Model_From_Entity
                    (Parser.Buffer (Start_Sub .. Parser.Buffer_Length));
                  Parser.Buffer_Length := Start_Sub - 1;
                  Next_Char (Input, Parser);

               when others =>
                  if Expect_Operator then
                     Fatal_Error
                       (Parser, "Expecting operator in content model");
                  end if;
                  Expect_Operator := True;

                  --  ??? Should test Is_Nmtoken
                  Start_Sub := Parser.Buffer_Length + 1;

                  while Parser.Last_Read = Unicode.Names.Basic_Latin.Colon
                    or else Is_Name_Char (Parser.Last_Read)
                  loop
                     Put_In_Buffer (Parser, Parser.Last_Read);
                     Next_Char (Input, Parser);
                  end loop;

                  if Start_Sub > Parser.Buffer_Length then
                     Fatal_Error (Parser, "Invalid name in content model: "
                                  & Debug_Encode (Parser.Last_Read),
                                  Start_Token);
                  end if;

                  Operand_Stack (Operand_Index) :=
                    new Element_Model (Element_Ref);
                  Operand_Stack (Operand_Index).Name := new Byte_Sequence'
                    (Parser.Buffer (Start_Sub .. Parser.Buffer_Length));
                  Operand_Index := Operand_Index + 1;
                  Parser.Buffer_Length := Start_Sub - 1;
                  Test_Multiplier := True;

            end case;

            if Test_Multiplier then
               case Parser.Last_Read is
                  when Star =>
                     if Operand_Index = Operand_Stack'First then
                        Fatal_Error
                          (Parser, "'*' must follow a name or list");
                     end if;
                     Operand_Stack (Operand_Index - 1) := new Element_Model'
                       (Repeat, 0, Positive'Last,
                        Operand_Stack (Operand_Index - 1));
                     Expect_Operator := True;
                     Next_Char (Input, Parser);

                  when Plus_Sign =>
                     if Operand_Index = Operand_Stack'First then
                        Fatal_Error
                          (Parser, "'+' must follow a name or list");
                     end if;
                     if Is_Mixed (Operand_Stack (Operand_Index - 1)) then
                        Fatal_Error
                          (Parser, "[3.2.2] Occurence on #PCDATA must be '*'");
                     end if;

                     Operand_Stack (Operand_Index - 1) := new Element_Model'
                       (Repeat, 1,
                        Positive'Last, Operand_Stack (Operand_Index - 1));
                     Expect_Operator := True;
                     Next_Char (Input, Parser);

                  when Question_Mark =>
                     if Operand_Index = Operand_Stack'First then
                        Fatal_Error
                          (Parser, "'?' must follow a name or list");
                     end if;
                     if Is_Mixed (Operand_Stack (Operand_Index - 1)) then
                        Fatal_Error
                          (Parser, "[3.2.2] Occurence on #PCDATA must be '*'");
                     end if;
                     Operand_Stack (Operand_Index - 1) := new Element_Model'
                       (Repeat, 0, 1, Operand_Stack (Operand_Index - 1));
                     Expect_Operator := True;
                     Next_Char (Input, Parser);

                  when others => null;
               end case;
            end if;

            exit when Operator_Index = Operator_Stack'First
              and then Operand_Index = Operand_Stack'First + 1;

            while Is_White_Space (Parser.Last_Read) loop
               Next_Char (Input, Parser);
            end loop;
         end loop;

         if not Is_Recursive_Call then
            if Operator_Index /= Operator_Stack'First
              or else Operand_Index /= Operand_Stack'First + 1
            then
               Fatal_Error (Parser, "Invalid content model", Start_Token);
            end if;

            Result := Operand_Stack (Operand_Stack'First);

         elsif Num_Parenthesis /= 0 then
            Fatal_Error (Parser, "[3.2.1] Replacement text for entities must"
                         & " be properly nested", Start_Token);
         end if;

      exception
         when Input_Ended =>
            if not Is_Recursive_Call then
               for J in Operand_Stack'First .. Operand_Index - 1 loop
                  Free (Operand_Stack (J));
               end loop;

            elsif Num_Parenthesis /= 0 then
               Fatal_Error
                 (Parser, "[3.2.1] Replacement text for entities must"
                  & " be properly nested", Start_Token);

            elsif Parser.Buffer_Length >= Start_Sub then
               Operand_Stack (Operand_Index) :=
                 new Element_Model (Element_Ref);
               Operand_Stack (Operand_Index).Name := new Byte_Sequence'
                 (Parser.Buffer (Start_Sub .. Parser.Buffer_Length));
               Operand_Index := Operand_Index + 1;
               Parser.Buffer_Length := Start_Sub - 1;
            end if;

         when others =>
            if not Is_Recursive_Call then
               for J in Operand_Stack'First .. Operand_Index - 1 loop
                  Free (Operand_Stack (J));
               end loop;
            end if;
            raise;
      end Parse;

   begin
      if Open_Was_Read then
         --  Insert the opening parenthesis into the operators stack
         Operator_Stack (Operator_Stack'First) := Opening_Parenthesis;
         Operator_Index := Operator_Index + 1;
      end if;

      Parse (Input, Result, Open_Was_Read, False);
   end Parse_Element_Model;

   ---------------------
   -- Syntactic_Parse --
   ---------------------

   procedure Syntactic_Parse
     (Parser : in out Reader'Class;
      Input  : in out Input_Sources.Input_Source'Class)
   is
      Id  : Token := Null_Token;

      procedure Parse_Start_Tag;
      --  Process an element start and its attributes   <!name name="value"..>

      procedure Parse_End_Tag;
      --  Process an element end   </name>

      procedure Parse_Doctype;
      --  Process the DTD declaration

      procedure Parse_Doctype_Contents;
      --  Process the DTD's contents

      procedure Parse_Entity_Def (Id : in out Token);
      --  Parse an <!ENTITY declaration

      procedure Parse_Element_Def (Id : in out Token);
      --  Parse an <!ELEMENT declaration

      procedure Parse_Notation_Def (Id : in out Token);
      --  Parse an <!NOTATION declaration

      procedure Parse_Attlist_Def (Id : in out Token);
      --  Parse an <!ATTLIST declaration

      procedure Parse_PI (Id : in out Token);
      --  Parse a <?...?> processing instruction

      procedure End_Element (NS_Id, Name_Id : Token);
      --  End the current element. Its namespace prefix and local_name are
      --  given in the parameters.

      procedure Get_String
        (Id : in out Token;
         State : Parser_State;
         Str_Start, Str_End : out Token;
         Normalize : Boolean := False);
      --  Get all the character till the end of the string. Id should contain
      --  the initial quote that starts the string.
      --  On exit, Str_Start is set to the first token of the string, and
      --  Str_End to the last token.

      procedure Get_Name_NS (Id : in out Token; NS_Id, Name_Id : out Token);
      --  Read the next tokens so as to match either a single name or
      --  a "ns:name" name.
      --  Id should initially point to the candidate token for the name, and
      --  will be left on the token following that name.
      --  An error is raised if we can't even match a Name.

      procedure Get_External
        (Id : in out Token;
         System_Start, System_End, Public_Start, Public_End : out Token;
         Allow_Publicid : Boolean := False);
      --  Parse a PUBLIC or SYSTEM definition and its arguments.
      --  Id should initially point to the keyword itself, and will be set to
      --  the first identifier following the full definition
      --  If Allow_Publicid is True, then PUBLIC might be followed by a single
      --  string, as in rule [83] of the XML specifications.

      procedure Check_Standalone_Value (Id : in out Token);
      procedure Check_Encoding_Value (Id : in out Token);
      procedure Check_Version_Value (Id : in out Token);
      --  Check the arguments for the <?xml?> processing instruction.
      --  Each of this procedures gets the arguments from Next_Token, up to,
      --  and including, the following space or End_Of_PI character.
      --  They raise errors appropriately

      procedure Check_Model;
      --  Check that the last element inserted matches the model. This
      --  procedure should not be called for the root element.

      ----------------
      -- Get_String --
      ----------------

      procedure Get_String
        (Id : in out Token;
         State : Parser_State;
         Str_Start, Str_End : out Token;
         Normalize : Boolean := False)
      is
         T : constant Token := Id;
         Saved_State : constant Parser_State := Get_State (Parser);
         Possible_End : Token := Null_Token;
         C : Unicode_Char;
         Index : Natural;
         Last_Space : Natural := 0;
         Had_Space : Boolean := Normalize;  --  Avoid leading spaces

      begin
         Set_State (Parser, State);
         Next_Token (Input, Parser, Id);
         Str_Start := Id;
         Str_End := Id;

         while Id.Typ /= T.Typ and then Id.Typ /= End_Of_Input loop
            Str_End := Id;
            case Id.Typ is
               when Double_String_Delimiter =>
                  Str_End.First := Parser.Buffer_Length + 1;
                  Put_In_Buffer (Parser, Quotation_Mark);
                  Str_End.Last := Parser.Buffer_Length;
                  Possible_End := Str_End;
                  Had_Space := False;
               when Single_String_Delimiter =>
                  Str_End.First := Parser.Buffer_Length + 1;
                  Put_In_Buffer (Parser, Apostrophe);
                  Str_End.Last := Parser.Buffer_Length;
                  Possible_End := Str_End;
                  Had_Space := False;
               when Start_Of_Tag =>
                  if Possible_End = Null_Token then
                     Fatal_Error
                       (Parser, "[2.3] '<' not authorized in attribute values",
                        Id);
                  else
                     Fatal_Error
                       (Parser, "[2.3] '<' not authorized in attribute values."
                        & " Possible end of attribute value at "
                        & Location (Parser, Possible_End), Id);
                  end if;
               when others =>
                  if Normalize then
                     declare
                        Str : constant Byte_Sequence :=
                          Parser.Buffer (Id.First .. Id.Last);
                     begin
                        Reset_Buffer (Parser, Id);
                        Index := Str'First;
                        while Index <= Str'Last loop
                           Encoding.Read (Str, Index, C);
                           if Is_White_Space (C) then
                              if not Had_Space then
                                 Put_In_Buffer
                                   (Parser, Unicode.Names.Basic_Latin.Space);
                              end if;
                              Had_Space := True;
                              Last_Space := Parser.Buffer_Length;
                           else
                              Had_Space := False;
                              Put_In_Buffer (Parser, C);
                           end if;
                        end loop;
                     end;
                     Str_End.Last := Parser.Buffer_Length;
                  end if;
            end case;
            Next_Token (Input, Parser, Id);
         end loop;

         if Normalize and then Had_Space and then Last_Space /= 0 then
            Str_End.Last := Last_Space - 1;
         end if;

         if Id.Typ = End_Of_Input then
            if Possible_End = Null_Token then
               Fatal_Error
                 (Parser, "[2.3] Unterminated string");
            else
               Fatal_Error
                 (Parser, "[2.3] Unterminated string, possible end at "
                  & Location (Parser, Possible_End), T);
            end if;
         end if;
         Set_State (Parser, Saved_State);
      end Get_String;

      ------------------
      -- Get_External --
      ------------------

      procedure Get_External
        (Id : in out Token;
         System_Start, System_End, Public_Start, Public_End : out Token;
         Allow_Publicid : Boolean := False)
      is
         Had_Space : Boolean;
         C : Unicode_Char;
         Index : Natural;
      begin
         System_Start := Null_Token;
         System_End := Null_Token;
         Public_Start := Null_Token;
         Public_End := Null_Token;

         --  Check the arguments for PUBLIC
         if Id.Typ = Public then
            Next_Token_Skip_Spaces (Input, Parser, Id, Must_Have => True);
            if Id.Typ /= Double_String_Delimiter
              and then Id.Typ /= Single_String_Delimiter
            then
               Fatal_Error (Parser, "[WF] Expecting string after PUBLIC");
            else
               Get_String
                 (Id, Non_Interpreted_String_State, Public_Start, Public_End);

               Index := Public_Start.First;
               while Index <= Public_End.Last loop
                  Encoding.Read (Parser.Buffer.all, Index, C);

                  if not Is_Pubid_Char (C) then
                     Fatal_Error
                       (Parser, "Invalid PubID character '"
                        & Debug_Encode (C) & "'", Public_Start);
                  end if;
               end loop;
            end if;

            Next_Token (Input, Parser, Id);
            Had_Space := (Id.Typ = Space);
            if Had_Space then
               Next_Token (Input, Parser, Id);
            elsif Allow_Publicid then
               return;
            end if;

            if Id.Typ /= Double_String_Delimiter
              and then Id.Typ /= Single_String_Delimiter
            then
               if not Allow_Publicid then
                  Fatal_Error (Parser, "[WF] Expecting SystemID after PUBLIC");
               end if;
            else
               if not Had_Space then
                  Fatal_Error
                    (Parser, "[4.2.2] Require whitespace between public and"
                     & " system IDs", Id);
               end if;
               Get_String
                 (Id, Non_Interpreted_String_State, System_Start, System_End);
               Next_Token (Input, Parser, Id);
            end if;

            --  Check the arguments for SYSTEM
         elsif Id.Typ = System then
            Next_Token_Skip_Spaces (Input, Parser, Id, Must_Have => True);
            if Id.Typ /= Double_String_Delimiter
              and then Id.Typ /= Single_String_Delimiter
            then
               Fatal_Error (Parser, "[WF] Expecting string after SYSTEM");
            else
               Get_String
                 (Id, Non_Interpreted_String_State, System_Start, System_End);
               Next_Token (Input, Parser, Id);
            end if;
         end if;
      end Get_External;

      -----------------
      -- Get_Name_NS --
      -----------------

      procedure Get_Name_NS (Id : in out Token; NS_Id, Name_Id : out Token) is
      begin
         Name_Id := Id;

         if Id.Typ = Text then
            Fatal_Error
              (Parser, "[3.1] '" & Parser.Buffer (Id.First .. Id.Last)
               & "' is not a valid name", Id);
         --  An empty namespace ? This seems to be useful only for the XML
         --  conformance suite, so we only handle the case of a single ':'
         --  to mean both an empty prefix and empty local name.
         elsif Name_Id.Typ = Colon then
            Name_Id.Typ := Text;
            NS_Id := Name_Id;
            Next_Token (Input, Parser, Id);

         elsif Id.Typ /= Name then
            Fatal_Error (Parser, "Expecting a name", Id);

         else
            Next_Token (Input, Parser, Id);
            if Id.Typ = Colon then
               NS_Id := Name_Id;
               Next_Token (Input, Parser, Name_Id);
               if Name_Id.Typ /= Name then
                  Fatal_Error (Parser, "[WF] Expecting name after namespace");
               end if;
               Next_Token (Input, Parser, Id);
            else
               NS_Id := Null_Token;
            end if;
         end if;
      end Get_Name_NS;

      ----------------------
      -- Parse_Entity_Def --
      ----------------------

      procedure Parse_Entity_Def (Id : in out Token) is
         Is_Parameter : Token := Null_Token;
         Name_Id : Token;
         Def_Start, Def_End : Token := Null_Token;
         Ndata_Id : Token := Null_Token;
         Public_Start, Public_End : Token := Null_Token;
         System_Start, System_End : Token := Null_Token;
         Had_Space : Boolean;
      begin
         Set_State (Parser, Entity_Def_State);
         Next_Token_Skip_Spaces (Input, Parser, Name_Id, True);

         if Name_Id.Typ = Text
           and then Parser.Buffer (Name_Id.First .. Name_Id.Last) =
           Percent_Sign_Sequence
         then
            Is_Parameter := Name_Id;
            Next_Token_Skip_Spaces (Input, Parser, Name_Id);
         end if;

         if Name_Id.Typ /= Name then
            Fatal_Error (Parser, "[WF] Expecting entity name");
         end if;

         Next_Token_Skip_Spaces (Input, Parser, Id, Must_Have => True);

         if Id.Typ = Public or else Id.Typ = System then
            Get_External
              (Id, System_Start, System_End, Public_Start, Public_End);

            Had_Space := (Id.Typ = Space);
            if Had_Space then
               Next_Token (Input, Parser, Id);
            end if;

            if Id.Typ = Ndata then
               if not Had_Space then
                  Fatal_Error
                    (Parser,
                     "[4.2.2] Expecting space before NDATA declaration", Id);
               end if;

               if Is_Parameter /= Null_Token then
                  Fatal_Error
                    (Parser, "[4.2] NDATA annotation not allowed for parameter"
                     & " entities", Id);
               end if;
               Next_Token_Skip_Spaces (Input, Parser, Ndata_Id, True);
               if Ndata_Id.Typ /= Text and then Ndata_Id.Typ /= Name then
                  Fatal_Error (Parser, "[WF] Expecting string after NDATA");
               else
                  if Parser.Feature_Validation
                    and then Get
                    (Parser.Notations,
                     Parser.Buffer (Ndata_Id.First .. Ndata_Id.Last)) /=
                    Null_Notation
                  then
                     Fatal_Error
                       (Parser, "[VC 4.2.2] Notation '"
                        & Parser.Buffer (Ndata_Id.First .. Ndata_Id.Last)
                        & "' must be declared", Ndata_Id);
                  end if;

                  Next_Token_Skip_Spaces (Input, Parser, Id);
               end if;
            end if;

         elsif Id.Typ = Double_String_Delimiter
           or else Id.Typ = Single_String_Delimiter
         then
            Get_String (Id, Entity_Str_Def_State, Def_Start, Def_End);
            Next_Token_Skip_Spaces (Input, Parser, Id);
         else
            Fatal_Error (Parser, "[WF] Invalid definition for ENTITY");
         end if;

         if Id.Typ /= End_Of_Tag then
            Fatal_Error (Parser, "[WF] Expecting end of ENTITY definition");
         end if;

         --  Only report the first definition
         if Get (Parser.Entities,
                 Parser.Buffer (Is_Parameter.First .. Is_Parameter.Last)
                 & Parser.Buffer (Name_Id.First .. Name_Id.Last)) /= null
         then
            null;

         elsif Def_End /= Null_Token then
            Set (Parser.Entities,
                 new Entity_Entry'
                   (Name => new Byte_Sequence'
                      (Parser.Buffer (Is_Parameter.First .. Is_Parameter.Last)
                       & Parser.Buffer (Name_Id.First .. Name_Id.Last)),
                    Value => new Byte_Sequence'
                      (Parser.Buffer (Def_Start.First .. Def_End.Last)),
                    External     => False,
                    Already_Read => False));
            Internal_Entity_Decl
              (Parser,
               Name => Parser.Buffer (Is_Parameter.First .. Is_Parameter.Last)
               & Parser.Buffer (Name_Id.First .. Name_Id.Last),
               Value => Parser.Buffer (Def_Start.First .. Def_End.Last));

         elsif Ndata_Id /= Null_Token then
            Unparsed_Entity_Decl
              (Parser,
               Name => Parser.Buffer (Is_Parameter.First .. Is_Parameter.Last)
               & Parser.Buffer (Name_Id.First .. Name_Id.Last),
               System_Id =>
                 Parser.Buffer (System_Start.First .. System_End.Last),
               Notation_Name =>
                 Parser.Buffer (Ndata_Id.First .. Ndata_Id.Last));

         else
            Set
              (Parser.Entities,
               new Entity_Entry'
                 (Name => new Byte_Sequence'
                    (Parser.Buffer (Is_Parameter.First .. Is_Parameter.Last)
                     & Parser.Buffer (Name_Id.First .. Name_Id.Last)),
                  Value => new Byte_Sequence'
                  (Parser.Buffer (System_Start.First .. System_End.Last)),
                  External => True,
                  Already_Read => False));
            External_Entity_Decl
              (Parser,
               Name => Parser.Buffer (Is_Parameter.First .. Is_Parameter.Last)
               & Parser.Buffer (Name_Id.First .. Name_Id.Last),
               Public_Id => Parser.Buffer
                 (Public_Start.First .. Public_End.Last),
               System_Id => Parser.Buffer
                 (System_Start.First .. System_End.Last));
         end if;

         if Is_Parameter /= Null_Token then
            Reset_Buffer (Parser, Is_Parameter);
         else
            Reset_Buffer (Parser, Name_Id);
         end if;
         Set_State (Parser, DTD_State);
      end Parse_Entity_Def;

      -----------------------
      -- Parse_Element_Def --
      -----------------------

      procedure Parse_Element_Def (Id : in out Token) is
         Name_Id : Token;
         M : Element_Model_Ptr;
      begin
         Set_State (Parser, Element_Def_State);
         Next_Token_Skip_Spaces (Input, Parser, Name_Id);

         if Name_Id.Typ /= Name then
            Fatal_Error (Parser, "[WF] Expecting element name");
         end if;

         Next_Token_Skip_Spaces (Input, Parser, Id, True);
         case Id.Typ is
            when Empty  => M := new Element_Model (Empty);
            when Any    => M := new Element_Model (Anything);
            when Open_Paren =>
               Parse_Element_Model
                 (Input, Parser, M, Attlist => False, Open_Was_Read => True);
            when others =>
               Fatal_Error (Parser, "[WF] Invalid content model: expecting"
                            & " '(', 'EMPTY' or 'ANY'", Id);
         end case;
         Next_Token_Skip_Spaces (Input, Parser, Id);

         if Id.Typ /= End_Of_Tag then
            Fatal_Error (Parser, "[WF] Expecting end of ELEMENT definition");
         end if;

         Element_Decl
           (Parser, Parser.Buffer (Name_Id.First .. Name_Id.Last), M);
         Free (M);

         Reset_Buffer (Parser, Name_Id);
         Set_State (Parser, DTD_State);
      end Parse_Element_Def;

      ------------------------
      -- Parse_Notation_Def --
      ------------------------

      procedure Parse_Notation_Def (Id : in out Token) is
         Public_Start, Public_End : Token := Null_Token;
         System_Start, System_End : Token := Null_Token;
         Name_Id : Token;
      begin
         Set_State (Parser, Element_Def_State);
         Next_Token_Skip_Spaces (Input, Parser, Name_Id);

         if Name_Id.Typ /= Name then
            Fatal_Error (Parser, "[WF] Expecting notation name");
         end if;

         Next_Token_Skip_Spaces (Input, Parser, Id);

         if Id.Typ = Public or else Id.Typ = System then
            Get_External
              (Id, System_Start, System_End, Public_Start, Public_End, True);
            if Id.Typ = Space then
               Next_Token (Input, Parser, Id);
            end if;
         else
            Fatal_Error (Parser, "[WF] Invalid notation declaration");
         end if;

         if Id.Typ /= End_Of_Tag then
            Fatal_Error (Parser, "[WF] Expecting end of NOTATION definition");
         end if;

         Notation_Decl
           (Parser,
            Name => Parser.Buffer (Name_Id.First .. Name_Id.Last),
            Public_Id => Parser.Buffer (Public_Start.First .. Public_End.Last),
            System_Id =>
              Parser.Buffer (System_Start.First .. System_End.Last));

         if Parser.Feature_Validation then
            Set (Parser.Notations,
                 (Name => new Byte_Sequence'
                    (Parser.Buffer (Name_Id.First .. Name_Id.Last))));
         end if;

         Set_State (Parser, DTD_State);
         Reset_Buffer (Parser, Name_Id);
      end Parse_Notation_Def;

      -----------------------
      -- Parse_Attlist_Def --
      -----------------------

      procedure Parse_Attlist_Def (Id : in out Token) is
         M : Element_Model_Ptr;
         Default_Start, Default_End : Token;
         Ename_Id, Name_Id, NS_Id, Type_Id : Token;
         Default_Id : Token;
         Attr : Attributes_Ptr;
         Default_Decl : Default_Declaration;
         Att_Type : Attribute_Type;
      begin
         Set_State (Parser, Element_Def_State);
         Next_Token_Skip_Spaces (Input, Parser, Ename_Id);

         if Ename_Id.Typ /= Name then
            Fatal_Error (Parser, "[WF] Expecting element name", Ename_Id);
         end if;

         Attr := Get
           (Parser.Default_Atts,
            Parser.Buffer (Ename_Id.First .. Ename_Id.Last)).Attributes;
         if Attr = null then
            Attr := new Sax.Attributes.Attributes;
            Set (Parser.Default_Atts,
                 (Element_Name => new Byte_Sequence'
                    (Parser.Buffer (Ename_Id.First .. Ename_Id.Last)),
                  Attributes => Attr));
         end if;

         loop
            --  Temporarily disable In_Attlist, so that the names like "NAME"
            --  are parsed as names and not as NMTOKEN.
            Set_State (Parser, Attribute_Def_Name_State);
            Next_Token_Skip_Spaces (Input, Parser, Id);
            exit when Id.Typ = End_Of_Tag or else Id.Typ = End_Of_Input;

            Get_Name_NS (Id, NS_Id, Name_Id);

            if Id.Typ /= Space then
               Fatal_Error
                 (Parser, "[3.3] Expecting space between attribute name"
                  & " and type", Id);
            end if;

            Set_State (Parser, Attribute_Def_State);
            Next_Token (Input, Parser, Id);

            Type_Id := Id;
            Default_Start := Null_Token;
            Default_End := Null_Token;
            case Type_Id.Typ is
               when Id_Type  => Att_Type := Sax.Attributes.Id;
               when Idref    => Att_Type := Sax.Attributes.Idref;
               when Idrefs   => Att_Type := Sax.Attributes.Idrefs;
               when Cdata    => Att_Type := Sax.Attributes.Cdata;
               when Nmtoken  => Att_Type := Sax.Attributes.Nmtoken;
               when Nmtokens => Att_Type := Sax.Attributes.Nmtokens;
               when Entity   => Att_Type := Sax.Attributes.Entity;
               when Entities => Att_Type := Sax.Attributes.Entities;
               when Notation =>
                  Att_Type := Notation;
                  Next_Token (Input, Parser, Id);
                  if Id.Typ /= Space then
                     Fatal_Error
                       (Parser,
                        "[3.3.1] Space is required between NOTATION keyword"
                        & " and list of enumerated", Id);
                  end if;
                  Parse_Element_Model (Input, Parser, M, True, False);

                  if Parser.Feature_Validation then
                     for J in M.List'Range loop
                        if Get (Parser.Notations, M.List (J).Name.all) /=
                          Null_Notation
                        then
                           Fatal_Error
                             (Parser,
                              "[VC 3.3.1] Notation '"
                              & M.List (J).Name.all & "' must be defined",
                              Id);
                        end if;
                     end loop;
                  end if;

               when Open_Paren =>
                  Att_Type := Enumeration;
                  Parse_Element_Model (Input, Parser, M, True, True);

               when others =>
                  Fatal_Error (Parser, "[WF] Invalid type for attribute");
            end case;

            Next_Token_Skip_Spaces (Input, Parser, Default_Id, True);
            if Default_Id.Typ = Implied then
               Default_Decl := Sax.Attributes.Implied;
            elsif Default_Id.Typ = Required then
               Default_Decl := Sax.Attributes.Required;
            else
               Id := Default_Id;
               if Default_Id.Typ = Fixed then
                  Next_Token_Skip_Spaces (Input, Parser, Id, True);
                  Default_Decl := Sax.Attributes.Fixed;
               else
                  Default_Decl := Sax.Attributes.Default;
               end if;

               if Id.Typ = Double_String_Delimiter
                 or else Id.Typ = Single_String_Delimiter
               then
                  Get_String
                    (Id, Attlist_Str_Def_State, Default_Start, Default_End,
                     Normalize => True);
               else
                  Fatal_Error
                    (Parser, "[WF] Invalid default value for attribute");
               end if;
            end if;

            --  Always report the attribute, even when we know the value
            --  won't be used. We can't do it coherently otherwise, in case
            --  an attribute is seen in the external subset, and then
            --  overriden in the internal subset.
            Attribute_Decl
              (Parser,
               Ename => Parser.Buffer (Ename_Id.First .. Ename_Id.Last),
               Aname => Qname_From_Name (Parser, NS_Id, Name_Id),
               Typ   => Att_Type,
               Content => M,
               Value_Default => Default_Decl,
               Value => Parser.Buffer
                 (Default_Start.First .. Default_End.Last));

            if Get_Index
              (Attr.all,
               Qname => Qname_From_Name (Parser, NS_Id, Name_Id)) = -1
            then
               --  The URI cannot be resolved at this point, since it will
               --  depend on the contents of the document at the place where
               --  the attribute is used.

               Add_Attribute
                 (Attr.all,
                  "",
                  Parser.Buffer (Name_Id.First .. Name_Id.Last),
                  Qname_From_Name (Parser, NS_Id, Name_Id),
                  Att_Type,
                  M,
                  Parser.Buffer (Default_Start.First .. Default_End.Last),
                  Default_Decl);

               --  M will be freed automatically when the Default_Atts field is
               --  freed. However, we need to reset it for the next attribute
               --  in the list.
               M := null;
            else
               Free (M);
            end if;

            if NS_Id /= Null_Token then
               Reset_Buffer (Parser, NS_Id);
            else
               Reset_Buffer (Parser, Name_Id);
            end if;
            Set_State (Parser, Element_Def_State);
         end loop;

         if Id.Typ /= End_Of_Tag then
            Fatal_Error (Parser, "[WF] Expecting end of ATTLIST definition");
         end if;

         Set_State (Parser, DTD_State);
         Reset_Buffer (Parser, Ename_Id);
      end Parse_Attlist_Def;

      -----------------
      -- Check_Model --
      -----------------

      procedure Check_Model is
      begin
         null;
      end Check_Model;

      ---------------------
      -- Parse_Start_Tag --
      ---------------------

      procedure Parse_Start_Tag is
         Open_Id : constant Token := Id;
         Value_Start, Value_End : Token;
         Elem_Name_Id, Elem_NS_Id : Token;
         Attr_Name_Id, Attr_NS_Id : Token;
         Add_Attr : Boolean;
         Attributes : Sax.Attributes.Attributes;
         NS : XML_NS;
         Attr : Attributes_Ptr;
         Found : Boolean;

      begin
         Set_State (Parser, Tag_State);

         Parser.Current_Node := new Element'
           (NS             => null,
            Name           => null,
            Namespaces     => null,
            Start_Id       => Id.Input_Id,
            Parent         => Parser.Current_Node);

         Next_Token (Input, Parser, Id);
         Get_Name_NS (Id, Elem_NS_Id, Elem_Name_Id);

         Parser.Current_Node.NS := new Byte_Sequence'
           (Parser.Buffer (Elem_NS_Id.First .. Elem_NS_Id.Last));
         Parser.Current_Node.Name := new Byte_Sequence'
           (Parser.Buffer (Elem_Name_Id.First .. Elem_Name_Id.Last));

         if Parser.Current_Node.Parent = null then
            Parser.Num_Toplevel_Elements := Parser.Num_Toplevel_Elements + 1;
            if Parser.Num_Toplevel_Elements > 1 then
               Fatal_Error
                 (Parser, "(2.1) Too many children for top-level node,"
                  & " when adding <"
                  & Qname_From_Name (Parser, Elem_NS_Id, Elem_Name_Id)
                  & ">", Open_Id);
            end if;

            if Parser.Feature_Validation
              and then Parser.DTD_Name = null
            then
               Fatal_Error
                 (Parser, "[VC 2.8] No DTD defined for this document", Id);
            end if;

            if Parser.Feature_Validation
              and then Parser.DTD_Name.all /= Parser.Current_Node.Name.all
            then
               Fatal_Error
                 (Parser, "[VC 2.8] Name of root element doesn't match name"
                  & " of DTD ('" & Parser.DTD_Name.all & "')", Id);
            end if;

         elsif Parser.Feature_Validation then
            Check_Model;
         end if;

         if Id.Typ = Space then
            Next_Token (Input, Parser, Id);
         elsif Id.Typ /= End_Of_Tag
           and then Id.Typ /= End_Of_Start_Tag
         then
            Fatal_Error
              (Parser, "[3] Must have spaces between tag name and attributes",
               Id);
         end if;

         --  ??? Should we support comments in tag_start
         while Id.Typ /= End_Of_Tag
           and then Id.Typ /= End_Of_Input
           and then Id.Typ /= End_Of_Start_Tag
         loop
            Get_Name_NS (Id, Attr_NS_Id, Attr_Name_Id);
            if Id.Typ = Space then
               Next_Token (Input, Parser, Id);
            end if;

            if Get_Index
              (Attributes,
               Qname_From_Name (Parser, Attr_NS_Id, Attr_Name_Id)) /= -1
            then
               Fatal_Error
                 (Parser, "(3.1) Attributes must have a unique name",
                  Attr_Name_Id);
            end if;

            if Id.Typ /= Equal then
               Fatal_Error
                 (Parser, "[3.1] Attributes must have an explicit value", Id);
            end if;

            Next_Token_Skip_Spaces (Input, Parser, Id);
            if Id.Typ /= Double_String_Delimiter
              and then Id.Typ /= Single_String_Delimiter
            then
               Fatal_Error
                 (Parser, "[3.1] Attribute values must be quoted", Id);
            end if;
            Get_String (Id, Attr_Value_State, Value_Start, Value_End,
                        Normalize => True); --  ??? All considered as CDATA

            Add_Attr := True;

            --  Is this a namespace declaration ?
            if Parser.Buffer (Attr_NS_Id.First .. Attr_NS_Id.Last) =
              Xmlns_Sequence
            then
               Add_Namespace
                 (Parser, Parser.Current_Node,
                  Attr_Name_Id, Value_Start, Value_End);
               Add_Attr := Parser.Feature_Namespace_Prefixes;

            --  Is this the declaration of the default namesapce (xmlns="uri")
            elsif Attr_NS_Id = Null_Token
              and then Parser.Buffer (Attr_Name_Id.First .. Attr_Name_Id.Last)
              = Xmlns_Sequence
            then
               Add_Namespace
                 (Parser, Parser.Current_Node,
                  Null_Token, Value_Start, Value_End);
               Add_Attr := Parser.Feature_Namespace_Prefixes;

            else
               --  All attributes must be defined (including xml:lang, that
               --  requires additional testing afterwards
               if Parser.Feature_Validation then
                  declare
                     Atts : constant Attributes_Ptr := Get
                       (Parser.Default_Atts,
                        Parser.Buffer
                        (Elem_Name_Id.First .. Elem_Name_Id.Last)).Attributes;
                     Index : Integer;
                     Att_Type : Attribute_Type;
                  begin
                     if Atts = null then
                        Fatal_Error
                          (Parser, "[VC] No attribute defined for element "
                           & Parser.Buffer
                             (Elem_Name_Id.First .. Elem_Name_Id.Last));
                     end if;

                     --  We must compare with Qnames, since we namespaces
                     --  haven't been resolved for default attributes
                     Index := Get_Index
                       (Atts.all,
                        Qname_From_Name (Parser, Attr_NS_Id, Attr_Name_Id));
                     if Index = -1 then
                        Fatal_Error
                          (Parser, "[VC] Attribute not declared in DTD: "
                           & Qname_From_Name
                           (Parser, Attr_NS_Id, Attr_Name_Id));
                     end if;

                     Att_Type := Get_Type (Atts.all, Index);
                     if (Att_Type = Idrefs or else Att_Type = Nmtokens)
                       and then Value_Start.First > Value_End.Last
                     then
                        Fatal_Error
                          (Parser,
                           "[VC 3.3.1] requires at least one name in IDREFS"
                           & " and NMTOKENS", Value_Start);
                     end if;
                  end;
               end if;

               if Parser.Buffer (Attr_NS_Id.First .. Attr_NS_Id.Last) =
                   Xml_Sequence
                 and then
                 Parser.Buffer (Attr_Name_Id.First .. Attr_Name_Id.Last) =
                   Lang_Sequence
               then
                  Test_Valid_Lang
                    (Parser,
                     Parser.Buffer (Value_Start.First .. Value_End.Last));
               end if;
            end if;

            --  Register the attribute
            --  URI are resolved later on, we currently only store the prefix
            if Add_Attr then
               Add_Attribute
                 (Attributes,
                  URI => Parser.Buffer (Attr_NS_Id.First .. Attr_NS_Id.Last),
                  Local_Name => Parser.Buffer
                    (Attr_Name_Id.First .. Attr_Name_Id.Last),
                  Qname => Qname_From_Name (Parser, Attr_NS_Id, Attr_Name_Id),
                  Att_Type => Sax.Attributes.Cdata,
                  Content => null,
                  Value => Parser.Buffer
                    (Value_Start.First .. Value_End.Last));
            end if;

            if Attr_NS_Id /= Null_Token then
               Reset_Buffer (Parser, Attr_NS_Id);
            else
               Reset_Buffer (Parser, Attr_Name_Id);
            end if;

            Next_Token (Input, Parser, Id);
            if Id.Typ = Space then
               Next_Token (Input, Parser, Id);
            elsif Id.Typ /= End_Of_Tag and then Id.Typ /= End_Of_Start_Tag then
               Fatal_Error
                 (Parser, "[3.1] Attributes must be separated by spaces", Id);
            end if;
         end loop;

         Attr := Get
           (Parser.Default_Atts,
            Qname_From_Name (Parser, Elem_NS_Id, Elem_Name_Id)).Attributes;

         --  Check that all #REQUIRED attributes are defined
         --  and that #FIXED attributes have the defined value
         if Parser.Feature_Validation and then Attr /= null then
            for J in 0 .. Get_Length (Attr.all) - 1 loop
               if Get_Default_Declaration (Attr.all, J) = Required then
                  Found := False;
                  for K in 0 .. Get_Length (Attributes) - 1 loop
                     if Get_Qname (Attributes, K)
                       = Get_Qname (Attr.all, J)
                     then
                        Found := True;
                        exit;
                     end if;
                  end loop;

                  if not Found then
                     Fatal_Error
                       (Parser, "[VC 3.3.2] Required attribute '"
                        & Get_Qname (Attr.all, J) & "' must be defined", Id);
                  end if;

               elsif Get_Default_Declaration (Attr.all, J) = Fixed then
                  for K in 0 .. Get_Length (Attributes) - 1 loop
                     if Get_Qname (Attributes, K)
                       = Get_Qname (Attr.all, J)
                     then
                        if Get_Value (Attributes, K)
                          /= Get_Value (Attr.all, J)
                        then
                           Fatal_Error
                             (Parser, "[VC 3.3.2] Fixed attribute '"
                              & Get_Qname (Attr.all, J) & "' must have the"
                              & " defined value", Id);
                        end if;
                        exit;
                     end if;
                  end loop;
               end if;
            end loop;
         end if;

         --  Add all the default attributes to the element
         --  We shouldn't add an attribute if it was overriden by the user

         if Attr /= null then
            for J in 0 .. Get_Length (Attr.all) - 1 loop
               --  ??? This could/should be more efficient.

               --  We must compare qnames, since namespaces haven't been
               --  resolved in the default attributes.
               if Get_Default_Declaration (Attr.all, J) /=
                   Sax.Attributes.Implied
                 and then Get_Index (Attributes,
                                     Qname => Get_Qname (Attr.all, J)) = -1
               then
                  declare
                     Prefix : constant Byte_Sequence :=
                       Prefix_From_Qname (Get_Qname (Attr.all, J));
                     Is_Xmlns : constant Boolean := Prefix = Xmlns_Sequence;
                  begin
                     if Parser.Feature_Namespace_Prefixes
                       or else not Is_Xmlns
                     then
                        Add_Attribute (Attributes,
                                       Prefix,
                                       Get_Local_Name (Attr.all, J),
                                       Get_Qname (Attr.all, J),
                                       Get_Type (Attr.all, J),
                                       Get_Content (Attr.all, J),
                                       Get_Value (Attr.all, J));
                     end if;

                     --  Is this a namespace declaration ?
                     if Is_Xmlns then
                        --  Following warning is because for parser that don't
                        --  read external DTDs, the behavior would be different
                        --  for the same document.
                        Warning
                          (Parser,
                           "namespace-declaring attribute inserted via "
                           & "DTD defaulting mechanisms are not good style");
                        Add_Namespace
                          (Parser, Parser.Current_Node,
                           Prefix => Get_Local_Name (Attr.all, J),
                           URI    => Get_Value (Attr.all, J));
                     end if;
                  end;
               end if;
            end loop;
         end if;

         --  We now need to resolve all the namespaces for the attribute
         --  namespaces

         for J in 0 .. Get_Length (Attributes) - 1 loop
            Find_NS (Parser, Parser.Current_Node,
                     Get_URI (Attributes, J), NS,
                     Include_Default_NS => False);
            Set_URI (Attributes, J, NS.URI.all);
         end loop;

         --  And report the elements to the callbacks

         Set_State (Parser, Default_State);
         Find_NS (Parser, Parser.Current_Node,  Elem_NS_Id, NS);
         Start_Element
           (Parser,
            Namespace_URI => NS.URI.all,
            Local_Name =>
              Parser.Buffer (Elem_Name_Id.First .. Elem_Name_Id.Last),
            Qname => Qname_From_Name (Parser, Elem_NS_Id, Elem_Name_Id),
            Atts => Attributes);

         Clear (Attributes);

         if Id.Typ = End_Of_Start_Tag then
            End_Element (Elem_NS_Id, Elem_Name_Id);
         end if;

         if Elem_NS_Id /= Null_Token then
            Reset_Buffer (Parser, Elem_NS_Id);
         else
            Reset_Buffer (Parser, Elem_Name_Id);
         end if;

         if Id.Typ = End_Of_Input then
            Fatal_Error (Parser, "[WF] Unexpected end of stream");
         end if;
      end Parse_Start_Tag;

      ----------------------------
      -- Parse_Doctype_Contents --
      ----------------------------

      procedure Parse_Doctype_Contents is
         Start_Id : Natural;

         Num_Include : Natural := 0;
         --  Number of <![INCLUDE[ sections at the top of the external
         --  subset.

         Num_Ignore : Natural := 0;
         --  Number of <![IGNORE[ and <![INCLUDE[ sections, starting at the
         --  first ignore section.
      begin
         loop
            Next_Token_Skip_Spaces (Input, Parser, Id);
            Start_Id := Id.Input_Id;

            if Id.Typ = Ignore then
               Num_Ignore := Num_Ignore + 1;

            elsif Id.Typ = Include or else Id.Typ = Start_Conditional then
               if Num_Ignore > 0 then
                  Num_Ignore := Num_Ignore + 1;
               else
                  Num_Include := Num_Include + 1;
               end if;

            elsif Id.Typ = End_Conditional then
               if Num_Include + Num_Ignore = 0 then
                  Fatal_Error
                    (Parser,
                     "[2.4] Text may not contain the litteral ']]>'", Id);
               elsif Num_Ignore > 0 then
                  Num_Ignore := Num_Ignore - 1;
               else
                  Num_Include := Num_Include  - 1;
               end if;

            elsif Id.Typ = End_Of_Input then
               exit;

            elsif Num_Ignore = 0 then
               case Id.Typ is
                  when End_Of_Tag | Internal_DTD_End =>
                     exit;
                  when Entity_Def => Parse_Entity_Def (Id);
                  when Element_Def => Parse_Element_Def (Id);
                  when Notation => Parse_Notation_Def (Id);
                  when Attlist_Def => Parse_Attlist_Def (Id);
                  when Text | Name =>
                     Fatal_Error
                       (Parser,  "[WF] Unexpected character in the DTD");
                  when Comment =>
                     Comment (Parser, Parser.Buffer (Id.First .. Id.Last));
                     Reset_Buffer (Parser, Id);
                  when Start_Of_PI =>
                     Parse_PI (Id);
                  when others =>
                     Fatal_Error
                       (Parser, "[2.8] Element not allowed in the DTD", Id);
               end case;

            else
               Reset_Buffer (Parser, Id);
            end if;

            if Start_Id /= Id.Input_Id then
               Fatal_Error
                 (Parser, "[4.5] Entity values must be self-contained", Id);
            end if;
         end loop;

         if Num_Ignore + Num_Include /= 0 then
            Fatal_Error
              (Parser, "[3.4] Conditional section must be properly terminated",
               Id);
         end if;
      end Parse_Doctype_Contents;

      -------------------
      -- Parse_Doctype --
      -------------------

      procedure Parse_Doctype is
         Public_Start, Public_End : Token := Null_Token;
         System_Start, System_End : Token := Null_Token;
         Name_Id : Token;
      begin
         Set_State (Parser, DTD_State);

         Next_Token_Skip_Spaces (Input, Parser, Name_Id);
         if Name_Id.Typ /= Name then
            Fatal_Error (Parser, "[WF] Expecting name after <!DOCTYPE");
         end if;

         Next_Token_Skip_Spaces (Input, Parser, Id);
         Get_External (Id, System_Start, System_End, Public_Start, Public_End);
         if Id.Typ = Space then
            Next_Token (Input, Parser, Id);
         end if;
         Start_DTD
           (Parser,
            Name => Parser.Buffer (Name_Id.First .. Name_Id.Last),
            Public_Id => Parser.Buffer (Public_Start.First .. Public_End.Last),
            System_Id =>
              Parser.Buffer (System_Start.First .. System_End.Last));

         if Parser.Feature_Validation then
            Parser.DTD_Name := new Byte_Sequence'
              (Parser.Buffer (Name_Id.First .. Name_Id.Last));
         end if;

         if Id.Typ = Internal_DTD_Start then
            Parse_Doctype_Contents;
            if Id.Typ /= Internal_DTD_End then
               Fatal_Error
                 (Parser, "[2.8] Expecting end of internal subset ']>'", Id);
            end if;
         elsif Id.Typ /= End_Of_Tag then
            Fatal_Error (Parser, "[WF] Expecting end of DTD");
         end if;

         --  Read the external subset if required. This needs to be read
         --  after the internal subset only, so that the latter gets
         --  priority (XML specifications 2.8)
         if System_End.Last >= System_Start.First then
            declare
               Loc : Locator_Impl;
               URI : constant Byte_Sequence := Resolve_URI
                 (Parser,
                  Parser.Buffer (System_Start.First .. System_End.Last));
               In_External : constant Boolean := Parser.In_External_Entity;
               Last  : constant Unicode_Char := Parser.Last_Read;
               Input_F : File_Input;
            begin
               Open (URI, Input_F);
               Copy (Loc, Parser.Locator.all);
               Set_Line_Number (Parser.Locator.all, 1);
               Set_Column_Number
                 (Parser.Locator.all, 1 + Prolog_Size (Input_F));
               Set_System_Id (Parser.Locator.all, URI);
               Set_Public_Id
                 (Parser.Locator.all,
                  Parser.Buffer (System_Start.First .. System_End.Last));
               Reset_Buffer (Parser, Name_Id);

               Parser.In_External_Entity := True;

               Syntactic_Parse (Parser, Input_F);
               Close (Input_F);
               Parser.In_External_Entity := In_External;
               Copy (Parser.Locator.all, Loc);
               Free (Loc);
               Parser.Last_Read := Last;
            exception
               when Name_Error =>
                  Error (Parser,
                         "External subset not found: " & URI, Id);
                  Reset_Buffer (Parser, Name_Id);
            end;
         else
            Reset_Buffer (Parser, Name_Id);
         end if;

         Parser.In_External_Entity := False;
         End_DTD (Parser);
         Set_State (Parser, Default_State);
      end Parse_Doctype;

      -----------------
      -- End_Element --
      -----------------

      procedure End_Element (NS_Id, Name_Id : Token) is
         NS : XML_NS;
      begin
         Find_NS (Parser, Parser.Current_Node, NS_Id, NS);
         End_Element
           (Parser,
            Namespace_URI => NS.URI.all,
            Local_Name => Parser.Current_Node.Name.all,
            Qname => Qname_From_Name (Parser, NS_Id, Name_Id));

         --  Tag must end in the same entity
         if Id.Input_Id /= Parser.Current_Node.Start_Id then
            Fatal_Error
              (Parser, "[4.5] Entity values must be self-contained", Id);
         end if;

         --  Close all the namespaces
         NS := Parser.Current_Node.Namespaces;
         while NS /= null loop
            End_Prefix_Mapping (Parser, NS.Prefix.all);
            NS := NS.Next;
         end loop;

         --  Move back to the parent node (after freeing the current node)
         Free (Parser.Current_Node);
      end End_Element;

      -------------------
      -- Parse_End_Tag --
      -------------------

      procedure Parse_End_Tag is
         Open_Id : constant Token := Id;
         NS_Id, Name_Id : Token := Null_Token;
      begin
         Set_State (Parser, Tag_State);

         Next_Token (Input, Parser, Id);
         Get_Name_NS (Id, NS_Id, Name_Id);
         if Id.Typ = Space then
            Next_Token (Input, Parser, Id);
         end if;

         if Id.Typ /= End_Of_Tag then
            Fatal_Error (Parser, "[3.1] Tags must end with a '>' symbol", Id);
         end if;

         --  Tag must end in the same entity
         if Id.Input_Id /= Parser.Current_Node.Start_Id then
            Fatal_Error
              (Parser, "[4.5] Entity values must be self-contained", Id);
         end if;

         if Parser.Current_Node = null
           or else Parser.Buffer (NS_Id.First .. NS_Id.Last) /=
             Parser.Current_Node.NS.all
           or else Parser.Buffer (Name_Id.First .. Name_Id.Last) /=
             Parser.Current_Node.Name.all
         then
            --  Well-Formedness Constraint: Element Type Match
            Fatal_Error
              (Parser,
               "[WF-Element Type Match] Name differ for closing tag",
               Open_Id);
         end if;

         End_Element (NS_Id, Name_Id);

         Set_State (Parser, Default_State);
         if NS_Id /= Null_Token then
            Reset_Buffer (Parser, NS_Id);
         else
            Reset_Buffer (Parser, Name_Id);
         end if;
      end Parse_End_Tag;

      -------------------------
      -- Check_Version_Value --
      -------------------------

      procedure Check_Version_Value (Id : in out Token) is
         C : Unicode_Char;
         J : Natural;
         Value_Start, Value_End : Token;
      begin
         Next_Token_Skip_Spaces (Input, Parser, Id);
         if Id.Typ /= Equal then
            Fatal_Error (Parser, "Expecting '=' sign", Id);
         end if;

         Next_Token_Skip_Spaces (Input, Parser, Id);
         if Id.Typ /= Double_String_Delimiter
           and then Id.Typ /= Single_String_Delimiter
         then
            Fatal_Error (Parser, "[WF] Expecting version value", Id);
         end if;
         Get_String (Id, Attr_Value_State, Value_Start, Value_End);

         J := Value_Start.First;
         while J <= Value_End.Last loop
            Encoding.Read (Parser.Buffer.all, J, C);
            if not (C in Latin_Small_Letter_A .. Latin_Small_Letter_Z)
              and then
                 not (C in Latin_Capital_Letter_A .. Latin_Capital_Letter_Z)
              and then not (C in Digit_Zero .. Digit_Nine)
              and then C /= Low_Line
              and then C /= Period
              and then C /= Unicode.Names.Basic_Latin.Colon
              and then C /= Hyphen_Minus
            then
               Fatal_Error
                 (Parser, "[2.8] Illegal version number in <?xml?> processing"
                  & " instruction", Value_Start);
            end if;
         end loop;

         Next_Token (Input, Parser, Id);
         if Id.Typ = Space then
            Next_Token (Input, Parser, Id);
         elsif Id.Typ /= End_Of_PI then
            Fatal_Error (Parser, "values must be separated by spaces", Id);
         end if;
      end Check_Version_Value;

      --------------------------
      -- Check_Encoding_Value --
      --------------------------

      procedure Check_Encoding_Value (Id : in out Token) is
         C : Unicode_Char;
         J : Natural;
         Value_Start, Value_End : Token;
         Tmp : Positive;
      begin
         Next_Token_Skip_Spaces (Input, Parser, Id);
         if Id.Typ /= Equal then
            Fatal_Error (Parser, "Expecting '=' sign");
         end if;

         Next_Token_Skip_Spaces (Input, Parser, Id);
         if Id.Typ /= Double_String_Delimiter
           and then Id.Typ /= Single_String_Delimiter
         then
            Fatal_Error (Parser, "[WF] Expecting encoding value");
         end if;
         Get_String (Id, Attr_Value_State, Value_Start, Value_End);

         if Value_End.Last < Value_Start.First then
            Fatal_Error
              (Parser, "[4.3.3] Empty value for encoding not allowed");
         else
            Tmp := Value_Start.First;
            Encoding.Read (Parser.Buffer.all, Tmp, C);
            if not (C in Latin_Small_Letter_A .. Latin_Small_Letter_Z)
              and then not
                (C in Latin_Capital_Letter_A .. Latin_Capital_Letter_Z)
            then
               Fatal_Error
                 (Parser, "[4.3.3] Illegal character '"
                  & Debug_Encode (C) & "' in encoding value", Value_Start);
            end if;

            J := Value_Start.First + Encoding.Width (C);
            while J <= Value_End.Last loop
               Encoding.Read (Parser.Buffer.all, J, C);
               if not (C in Latin_Small_Letter_A .. Latin_Small_Letter_Z)
                 and then not
                   (C in Latin_Capital_Letter_A .. Latin_Capital_Letter_Z)
                 and then not (C in Digit_Zero .. Digit_Nine)
                 and then C /= Period
                 and then C /= Low_Line
                 and then C /= Hyphen_Minus
               then
                  Fatal_Error
                    (Parser, "(4.3.3) Illegal character '"
                     & Debug_Encode (C) & "' in encoding value",
                     Value_Start);
               end if;
            end loop;
         end if;

         --  Check we indeed have a following space

         Next_Token (Input, Parser, Id);
         if Id.Typ = Space then
            Next_Token (Input, Parser, Id);
         elsif Id.Typ /= End_Of_PI then
            Fatal_Error (Parser, "values must be separated by spaces", Id);
         end if;

         --  Change the encoding for the streams, if needed
         Set_Stream_Encoding
           (Input, Parser.Buffer (Value_Start.First .. Value_End.Last));
      end Check_Encoding_Value;

      ----------------------------
      -- Check_Standalone_Value --
      ----------------------------

      procedure Check_Standalone_Value (Id : in out Token) is
         Value_Start, Value_End : Token;
      begin
         Next_Token_Skip_Spaces (Input, Parser, Id);
         if Id.Typ /= Equal then
            Fatal_Error (Parser, "Expecting '=' sign");
         end if;

         Next_Token_Skip_Spaces (Input, Parser, Id);
         if Id.Typ /= Double_String_Delimiter
           and then Id.Typ /= Single_String_Delimiter
         then
            Fatal_Error
              (Parser, "Parameter to 'standalone' must be quoted", Id);
         end if;
         Get_String (Id, Attr_Value_State, Value_Start, Value_End);

         if Parser.Buffer (Value_Start.First .. Value_End.Last) /= Yes_Sequence
           and then Parser.Buffer (Value_Start.First .. Value_End.Last) /=
             No_Sequence
         then
            Fatal_Error
              (Parser,
               "[2.9 [32]] Invalid value for standalone parameter in <?xml?>",
               Value_Start);
         end if;

         Next_Token (Input, Parser, Id);
         if Id.Typ = Space then
            Next_Token (Input, Parser, Id);
         elsif Id.Typ /= End_Of_PI then
            Fatal_Error (Parser, "values must be separated by spaces", Id);
         end if;
      end Check_Standalone_Value;

      --------------
      -- Parse_PI --
      --------------

      procedure Parse_PI (Id : in out Token) is
         State : constant Parser_State := Get_State (Parser);
         Open_Id : constant Token := Id;
         Name_Id, Data_Start : Token;
         Data_End : Token := Null_Token;
      begin
         Set_State (Parser, PI_State);

         Next_Token (Input, Parser, Name_Id);
         if Name_Id.Typ /= Name then
            Fatal_Error
              (Parser,
               "[2.6] Processing Instruction must specify a target name",
               Name_Id);
         end if;

         Next_Token (Input, Parser, Id);
         if Id.Typ /= Space and then Id.Typ /= End_Of_PI then
            Fatal_Error (Parser, "Must have space betwee target and data");
         elsif Id.Typ = Space then
            Next_Token (Input, Parser, Id);
         end if;

         --  Special handling for <?xml?>
         if Parser.Buffer (Name_Id.First .. Name_Id.Last) = Xml_Sequence then

            if Open_Id.Line /= 1
              or else Open_Id.Column /= 1 + Prolog_Size (Input)
              or else (Parser.Inputs /= null
                       and then not Parser.Inputs.External)
            then
               Fatal_Error
                 (Parser,
                  "[2.8] <?xml?> instruction must be first in document",
                  Open_Id);
            end if;

            --  ??? No true for text declaratinos 4.3.1 (external parsed
            --  entities)
            Set_State (Parser, Tag_State);

            if Parser.Buffer (Id.First .. Id.Last) = Version_Sequence then
               Check_Version_Value (Id);
            elsif not Parser.In_External_Entity then
               Fatal_Error
                 (Parser, "'version' must be the first argument to <?xml?>",
                  Id);
            end if;

            if Id.Typ = Name
              and then Parser.Buffer (Id.First .. Id.Last) = Encoding_Sequence
            then
               Check_Encoding_Value (Id);
            elsif Parser.In_External_Entity then
               Fatal_Error
                 (Parser, "'encoding' must be specified for <?xml?> in"
                  & " external entities", Id);
            end if;

            if not Parser.In_External_Entity
              and then Id.Typ = Name
              and then Parser.Buffer (Id.First .. Id.Last) =
                Standalone_Sequence
            then
               Check_Standalone_Value (Id);
            end if;

            if Id.Typ /= End_Of_PI then
               if Parser.In_External_Entity then
                  Fatal_Error
                    (Parser,
                     "Text declarations <?xml?> in external entity can not"
                     & " specify parameters other than 'version' and"
                     & " 'encoding'", Id);
               else
                  Fatal_Error
                    (Parser,
                     "<?xml..?> arguments can only be 'version', 'encoding' or"
                     & " 'standalone', in that order", Id);
               end if;
            end if;

         else
            --  (2.6)[17]: Name can not be 'xml' (case insensitive)
            declare
               C : Unicode_Char;
               J : Natural := Name_Id.First;
            begin
               Encoding.Read (Parser.Buffer.all, J, C);

               if C = Latin_Small_Letter_X
                 or else C = Latin_Capital_Letter_X
               then
                  Encoding.Read (Parser.Buffer.all, J, C);

                  if C = Latin_Capital_Letter_M
                    or else C = Latin_Small_Letter_M
                  then
                     Encoding.Read (Parser.Buffer.all, J, C);

                     if (C = Latin_Capital_Letter_L
                         or else C = Latin_Small_Letter_L)
                       and then J = Name_Id.Last + 1
                     then
                        Fatal_Error
                          (Parser,
                           "[2.6] '"
                           & Parser.Buffer (Name_Id.First .. Name_Id.Last)
                           & "' is not a valid processing instruction target",
                           Name_Id);
                     end if;
                  end if;
               end if;
            end;

            Data_Start := Id;

            while Id.Typ /= End_Of_PI and then Id.Typ /= End_Of_Input loop
               Data_End := Id;
               Next_Token (Input, Parser, Id);
            end loop;

            if Id.Typ = End_Of_Input then
               Fatal_Error
                 (Parser, "[2.6] Processing instruction must end with '?>'",
                  Open_Id);
            end if;

            Processing_Instruction
              (Parser,
               Target => Parser.Buffer (Name_Id.First .. Name_Id.Last),
               Data   => Parser.Buffer (Data_Start.First .. Data_End.Last));
         end if;

         Set_State (Parser, State);
         Reset_Buffer (Parser, Name_Id);
      end Parse_PI;

   begin
      --  Initialize the parser with the first character of the stream.
      if Eof (Input) then
         return;
      end if;
      Next_Char (Input, Parser);

      if Parser.State.In_DTD then
         Parse_Doctype_Contents;
      end if;

      loop
         --  Unless in string, buffer should be empty at this point. Strings
         --  are special-cased just in case we are currently substituting
         --  entities while in a string.
         pragma Assert (Parser.State.Ignore_Special
                        or else Parser.Buffer_Length = 0);

         Next_Token (Input, Parser, Id,
                     Coalesce_Space => Parser.Current_Node /= null);
         exit when Id.Typ = End_Of_Input;

         case Id.Typ is
            when Start_Of_PI =>
               Parse_PI (Id);

            when Cdata_Section =>
               if Parser.Current_Node = null then
                  Fatal_Error
                    (Parser, "[2.1] Non-white space found at top level", Id);
               end if;
               Start_Cdata (Parser);
               Characters (Parser, Parser.Buffer (Id.First .. Id.Last));
               End_Cdata (Parser);
               Reset_Buffer (Parser, Id);

            when Text | Name =>
               if Parser.Current_Node = null then
                  Fatal_Error
                    (Parser, "[2.1] Non-white space found at top level", Id);
               end if;
               Characters (Parser, Parser.Buffer (Id.First .. Id.Last));
               Reset_Buffer (Parser, Id);

            when Sax.Readers.Space =>
               Ignorable_Whitespace
                 (Parser, Parser.Buffer (Id.First .. Id.Last));
               Reset_Buffer (Parser, Id);

            when Comment =>
               Comment (Parser, Parser.Buffer (Id.First .. Id.Last));
               Reset_Buffer (Parser, Id);

            when Start_Of_Tag =>
               Parse_Start_Tag;

            when Start_Of_End_Tag =>
               Parse_End_Tag;

            when Doctype_Start =>
               Parse_Doctype;

            when others =>
               Fatal_Error (Parser, "[WF] Currently ignored: "
                            & Token_Type'Image (Id.Typ));
         end case;
      end loop;
   end Syntactic_Parse;

   ----------
   -- Free --
   ----------

   procedure Free (Parser : in out Reader'Class) is
      Tmp : Element_Access;
      Iter : Attributes_Table.Iterator;
      Length : Natural;
      Model : Element_Model_Ptr;
   begin
      Close_Inputs (Parser);
      Free (Parser.Default_Namespaces);
      Free (Parser.Locator);
      Free (Parser.DTD_Name);
      Free (Parser.Buffer);

      --  Free the nodes, in case there are still some open
      Tmp := Parser.Current_Node;
      while Tmp /= null loop
         Free (Tmp);
      end loop;

      --  Free the content model for the default attributes
      Iter := First (Parser.Default_Atts);
      while Iter /= Attributes_Table.No_Iterator loop
         Length := Get_Length (Current (Iter).Attributes.all);
         for A in 1 .. Length loop
            Model := Get_Content (Current (Iter).Attributes.all, A - 1);
            Free (Model);
            Set_Content (Current (Iter).Attributes.all, A - 1, null);
         end loop;
         Next (Parser.Default_Atts, Iter);
      end loop;

      --  Free the internal tables
      Reset (Parser.Entities);
      Reset (Parser.Default_Atts);
      Reset (Parser.Notations);
   end Free;

   -----------
   -- Parse --
   -----------

   procedure Parse
     (Parser : in out Reader;
      Input  : in out Input_Sources.Input_Source'Class) is
   begin
      Parser.Locator := new Locator_Impl;
      Set_Public_Id (Parser.Locator.all, Get_Public_Id (Input));
      Set_System_Id (Parser.Locator.all, Get_System_Id (Input));
      Set_Column_Number (Parser.Locator.all, 1 + Prolog_Size (Input));
      Set_Line_Number (Parser.Locator.all, 1);
      Parser.Current_Node := null;
      Parser.Num_Toplevel_Elements := 0;
      Parser.Previous_Char_Was_CR := False;
      Parser.Ignore_State_Special := False;
      Parser.In_External_Entity := False;
      Parser.Buffer := new Byte_Sequence (1 .. Initial_Buffer_Length);
      Set_State (Parser, Default_State);

      Add_Namespace_No_Event
        (Parser, Xml_Sequence,
         Encodings.From_Utf32
         (Basic_8bit.To_Utf32 ("http://www.w3.org/XML/1998/namespace")));
      Add_Namespace_No_Event (Parser, Xmlns_Sequence, Xmlns_Sequence);
      Add_Namespace_No_Event (Parser, "", "");

      Set_Document_Locator (Reader'Class (Parser), Parser.Locator);

      Start_Document (Reader'Class (Parser));
      Syntactic_Parse (Parser, Input);

      --  Close all the namespaces
      declare
         NS : XML_NS := Parser.Default_Namespaces;
      begin
         while NS /= null loop
            if NS.Prefix.all /= ""
              and then NS.Prefix.all /= Xmlns_Sequence
            then
               End_Prefix_Mapping (Reader'Class (Parser), NS.Prefix.all);
            end if;
            NS := NS.Next;
         end loop;
      end;

      --  All the nodes must have been closed at the end of the document
      if Parser.Current_Node /= null then
         Fatal_Error
           (Parser, "[2.1] Node <" & Parser.Current_Node.Name.all
            & "> is not closed");
      end if;

      if Parser.Num_Toplevel_Elements = 0 then
         Fatal_Error (Parser, "[2.1] No root element specified");
      end if;

      End_Document (Reader'Class (Parser));

      Free (Parser);

   exception
      when others =>
         Free (Parser);
         raise;
   end Parse;

   ----------
   -- Hash --
   ----------

   function Hash (Str : String) return Unsigned_32 is
      Result : Unsigned_32 := Str'Length;
   begin
      for J in Str'Range loop
         Result := Rotate_Left (Result, 1) +
           Unsigned_32 (Character'Pos (Str (J)));
      end loop;

      return Result;
   end Hash;

   ----------
   -- Free --
   ----------

   procedure Free (Entity : in out Entity_Entry_Access) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Entity_Entry, Entity_Entry_Access);
   begin
      Free (Entity.Name);
      Free (Entity.Value);
      Unchecked_Free (Entity);
   end Free;

   -------------
   -- Get_Key --
   -------------

   function Get_Key (Entity : Entity_Entry_Access) return String is
   begin
      return Entity.Name.all;
   end Get_Key;

   ----------
   -- Free --
   ----------

   procedure Free (Att : in out Attributes_Entry) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Sax.Attributes.Attributes'Class, Attributes_Ptr);
   begin
      Free (Att.Element_Name);
      Clear (Att.Attributes.all);
      Unchecked_Free (Att.Attributes);
   end Free;

   -------------
   -- Get_Key --
   -------------

   function Get_Key (Att : Attributes_Entry) return String is
   begin
      return Att.Element_Name.all;
   end Get_Key;

   ----------
   -- Free --
   ----------

   procedure Free (Notation : in out Notation_Entry) is
   begin
      Free (Notation.Name);
   end Free;

   -------------
   -- Get_Key --
   -------------

   function Get_Key (Notation : Notation_Entry) return String is
   begin
      return Notation.Name.all;
   end Get_Key;

   -----------------
   -- Get_Feature --
   -----------------

   function Get_Feature (Parser : Reader; Name : String) return Boolean is
   begin
      if Name = Namespace_Feature then
         return Parser.Feature_Namespace;

      elsif Name = Namespace_Prefixes_Feature then
         return Parser.Feature_Namespace_Prefixes;

      elsif Name = External_General_Entities_Feature then
         return Parser.Feature_External_General_Entities;

      elsif Name = External_Parameter_Entities_Feature then
         return Parser.Feature_External_Parameter_Entities;

      elsif Name = Validation_Feature then
         return Parser.Feature_Validation;

      elsif Name = Parameter_Entities_Feature then
         return False;  --  ??? Unsupported for now
      end if;

      return False;
   end Get_Feature;

   -----------------
   -- Set_Feature --
   -----------------

   procedure Set_Feature
     (Parser : in out Reader; Name : String; Value : Boolean) is
   begin
      if Name = Namespace_Feature then
         Parser.Feature_Namespace := Value;

      elsif Name = Namespace_Prefixes_Feature then
         Parser.Feature_Namespace_Prefixes := Value;

      elsif Name = External_General_Entities_Feature then
         Parser.Feature_External_General_Entities := Value;

      elsif Name = External_Parameter_Entities_Feature then
         Parser.Feature_External_Parameter_Entities := Value;

      elsif Name = Validation_Feature then
         Parser.Feature_Validation := Value;
      end if;
   end Set_Feature;

   -------------
   -- Warning --
   -------------

   procedure Warning
     (Handler : in out Reader; Except : Sax_Parse_Exception'Class)
   is
      pragma Warnings (Off, Handler);
      pragma Warnings (Off, Except);
   begin
      null;
   end Warning;

   -----------
   -- Error --
   -----------

   procedure Error
     (Handler : in out Reader; Except : Sax_Parse_Exception'Class)
   is
      pragma Warnings (Off, Handler);
      pragma Warnings (Off, Except);
   begin
      null;
   end Error;

   -----------------
   -- Fatal_Error --
   -----------------

   procedure Fatal_Error
     (Handler : in out Reader; Except : Sax_Parse_Exception'Class)
   is
      pragma Warnings (Off, Handler);
   begin
      Raise_Exception
        (XML_Fatal_Error'Identity,
         Get_Message (Except));
   end Fatal_Error;

   --------------------------
   -- Set_Document_Locator --
   --------------------------

   procedure Set_Document_Locator
     (Handler : in out Reader; Loc : access Sax.Locators.Locator'Class)
   is
      pragma Warnings (Off, Handler);
      pragma Warnings (Off, Loc);
   begin
      null;
   end Set_Document_Locator;

   --------------------
   -- Start_Document --
   --------------------

   procedure Start_Document (Handler : in out Reader) is
      pragma Warnings (Off, Handler);
   begin
      null;
   end Start_Document;

   ------------------
   -- End_Document --
   ------------------

   procedure End_Document (Handler : in out Reader) is
      pragma Warnings (Off, Handler);
   begin
      null;
   end End_Document;

   --------------------------
   -- Start_Prefix_Mapping --
   --------------------------

   procedure Start_Prefix_Mapping
     (Handler : in out Reader;
      Prefix  : Unicode.CES.Byte_Sequence;
      URI     : Unicode.CES.Byte_Sequence)
   is
      pragma Warnings (Off, Handler);
      pragma Warnings (Off, Prefix);
      pragma Warnings (Off, URI);
   begin
      null;
   end Start_Prefix_Mapping;

   ------------------------
   -- End_Prefix_Mapping --
   ------------------------

   procedure End_Prefix_Mapping
     (Handler : in out Reader; Prefix : Unicode.CES.Byte_Sequence)
   is
      pragma Warnings (Off, Handler);
      pragma Warnings (Off, Prefix);
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
      Atts          : Sax.Attributes.Attributes'Class)
   is
      pragma Warnings (Off, Handler);
      pragma Warnings (Off, Namespace_URI);
      pragma Warnings (Off, Local_Name);
      pragma Warnings (Off, Qname);
      pragma Warnings (Off, Atts);
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
      Qname         : Unicode.CES.Byte_Sequence := "")
   is
      pragma Warnings (Off, Handler);
      pragma Warnings (Off, Namespace_URI);
      pragma Warnings (Off, Local_Name);
      pragma Warnings (Off, Qname);
   begin
      null;
   end End_Element;

   ----------------
   -- Characters --
   ----------------

   procedure Characters
     (Handler : in out Reader;
      Ch      : Unicode.CES.Byte_Sequence)
   is
      pragma Warnings (Off, Handler);
      pragma Warnings (Off, Ch);
   begin
      null;
   end Characters;

   --------------------------
   -- Ignorable_Whitespace --
   --------------------------

   procedure Ignorable_Whitespace
     (Handler : in out Reader; Ch : Unicode.CES.Byte_Sequence)
   is
      pragma Warnings (Off, Handler);
      pragma Warnings (Off, Ch);
   begin
      null;
   end Ignorable_Whitespace;

   ----------------------------
   -- Processing_Instruction --
   ----------------------------

   procedure Processing_Instruction
     (Handler : in out Reader;
      Target  : Unicode.CES.Byte_Sequence;
      Data    : Unicode.CES.Byte_Sequence)
   is
      pragma Warnings (Off, Handler);
      pragma Warnings (Off, Target);
      pragma Warnings (Off, Data);
   begin
      null;
   end Processing_Instruction;

   --------------------
   -- Skipped_Entity --
   --------------------

   procedure Skipped_Entity
     (Handler : in out Reader; Name : Unicode.CES.Byte_Sequence)
   is
      pragma Warnings (Off, Handler);
      pragma Warnings (Off, Name);
   begin
      null;
   end Skipped_Entity;

   -------------
   -- Comment --
   -------------

   procedure Comment
     (Handler : in out Reader;
      Ch      : Unicode.CES.Byte_Sequence)
   is
      pragma Warnings (Off, Handler);
      pragma Warnings (Off, Ch);
   begin
      null;
   end Comment;

   -----------------
   -- Start_Cdata --
   -----------------

   procedure Start_Cdata (Handler : in out Reader) is
      pragma Warnings (Off, Handler);
   begin
      null;
   end Start_Cdata;

   ---------------
   -- End_Cdata --
   ---------------

   procedure End_Cdata (Handler : in out Reader) is
      pragma Warnings (Off, Handler);
   begin
      null;
   end End_Cdata;

   ------------------
   -- Start_Entity --
   ------------------

   procedure Start_Entity
     (Handler : in out Reader;
      Name    : Unicode.CES.Byte_Sequence)
   is
      pragma Warnings (Off, Handler);
      pragma Warnings (Off, Name);
   begin
      null;
   end Start_Entity;

   ----------------
   -- End_Entity --
   ----------------

   procedure End_Entity
     (Handler : in out Reader;
      Name    : Unicode.CES.Byte_Sequence)
   is
      pragma Warnings (Off, Handler);
      pragma Warnings (Off, Name);
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
      System_Id : Unicode.CES.Byte_Sequence := "")
   is
      pragma Warnings (Off, Handler);
      pragma Warnings (Off, Name);
      pragma Warnings (Off, Public_Id);
      pragma Warnings (Off, System_Id);
   begin
      null;
   end Start_DTD;

   -------------
   -- End_DTD --
   -------------

   procedure End_DTD (Handler : in out Reader) is
      pragma Warnings (Off, Handler);
   begin
      null;
   end End_DTD;

   --------------------------
   -- Internal_Entity_Decl --
   --------------------------

   procedure Internal_Entity_Decl
     (Handler : in out Reader;
      Name    : Unicode.CES.Byte_Sequence;
      Value   : Unicode.CES.Byte_Sequence)
   is
      pragma Warnings (Off, Handler);
      pragma Warnings (Off, Name);
      pragma Warnings (Off, Value);
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
      System_Id : Unicode.CES.Byte_Sequence)
   is
      pragma Warnings (Off, Handler);
      pragma Warnings (Off, Name);
      pragma Warnings (Off, Public_Id);
      pragma Warnings (Off, System_Id);
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
      Notation_Name : Unicode.CES.Byte_Sequence)
   is
      pragma Warnings (Off, Handler);
      pragma Warnings (Off, Name);
      pragma Warnings (Off, System_Id);
      pragma Warnings (Off, Notation_Name);
   begin
      null;
   end Unparsed_Entity_Decl;

   ------------------
   -- Element_Decl --
   ------------------

   procedure Element_Decl
     (Handler : in out Reader;
      Name    : Unicode.CES.Byte_Sequence;
      Model   : Element_Model_Ptr)
   is
      pragma Warnings (Off, Handler);
      pragma Warnings (Off, Name);
      pragma Warnings (Off, Model);
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
      System_Id     : Unicode.CES.Byte_Sequence)
   is
      pragma Warnings (Off, Handler);
      pragma Warnings (Off, Name);
      pragma Warnings (Off, Public_Id);
      pragma Warnings (Off, System_Id);
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
      Typ     : Sax.Attributes.Attribute_Type;
      Content : Sax.Models.Element_Model_Ptr;
      Value_Default : Sax.Attributes.Default_Declaration;
      Value   : Unicode.CES.Byte_Sequence)
   is
      pragma Warnings (Off, Handler);
      pragma Warnings (Off, Ename);
      pragma Warnings (Off, Aname);
      pragma Warnings (Off, Typ);
      pragma Warnings (Off, Content);
      pragma Warnings (Off, Value_Default);
      pragma Warnings (Off, Value);
   begin
      null;
   end Attribute_Decl;
end Sax.Readers;
