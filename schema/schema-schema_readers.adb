-----------------------------------------------------------------------
--                XML/Ada - An XML suite for Ada95                   --
--                                                                   --
--                Copyright (C) 2004-2010, AdaCore                   --
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

pragma Ada_05;

with Ada.Exceptions;    use Ada.Exceptions;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Unicode;           use Unicode;
with Unicode.CES;       use Unicode.CES;
with Sax.Encodings;     use Sax.Encodings;
with Sax.Locators;
with Sax.Readers;       use Sax.Readers;
with Sax.Symbols;       use Sax.Symbols;
with Sax.Utils;         use Sax.Utils;
with Schema.Validators.Lists; use Schema.Validators.Lists;
with Schema.Readers;    use Schema.Readers;
with Ada.Unchecked_Deallocation;

package body Schema.Schema_Readers is
   use Schema_State_Machines, Schema_State_Machines_PP;
   use Type_Tables, Type_HTables, Element_HTables;

   Max_Namespaces_In_Any_Attribute : constant := 50;
   --  Maximum number of namespaces for a <anyAttribute>
   --  This only impacts the parsing of the grammar, so can easily be raised if
   --  need be.

   procedure Free (Self : in out Type_Details_Access);
   --  Free [Self], [Self.Next] and so on

   procedure Free (C : in out Context_Access; Recurse : Boolean);
   --  Free the memory occupied by C

   procedure Get_Grammar_For_Namespace
     (Handler : access Schema_Reader'Class;
      NS      : Sax.Symbols.Symbol;
      Grammar : out XML_Grammar_NS;
      Create_If_Needed : Boolean := True);
   --  Return the grammar matching a given prefix

   procedure Create_NFA (Parser : access Schema_Reader);
   --  Create the state machine from the registered elements and types

   function To_String (Blocks : Block_Status) return String;
   function To_String (Final  : Final_Status) return String;

   procedure Lookup_With_NS
     (Handler : access Schema_Reader'Class;
      Name    : Qualified_Name;
      Result  : out XML_Type);
   procedure Lookup_With_NS
     (Handler : access Schema_Reader'Class;
      Name    : Qualified_Name;
      Result  : out XML_Element);
   procedure Lookup_With_NS
     (Handler : access Schema_Reader'Class;
      QName   : Symbol;
      Result  : out XML_Group);
   procedure Lookup_With_NS
     (Handler    : access Schema_Reader'Class;
      QName      : Symbol;
      Result     : out XML_Attribute_Group);
   --  Lookup a type or element  with a possible namespace specification

   procedure Ensure_Type
     (Handler : access Schema_Reader'Class; C : Context_Access);
   --  Make sure the context (of type Context_Type_Def) has a proper
   --  type validator defined

   function Ada_Name (Element : XML_Element) return String;
   function Ada_Name (Typ : XML_Type)        return String;
   function Ada_Name (C : Context_Access)    return String;
   --  Return the name of an Ada variable suitable to represent Element

   function XML_To_Ada (Str : Byte_Sequence) return Byte_Sequence;
   --  Return a string suitable as an Ada identifier

   function In_Redefine_Context
     (Handler : Schema_Reader'Class) return Boolean;
   --  Whether we are currently processing a <redefine> tag

   function Resolve_QName
     (Handler : access Schema_Reader'Class;
      QName   : Sax.Symbols.Symbol) return Qualified_Name;
   --  Resolve namespaces for QName

   procedure Insert_In_Type
     (Handler : access Schema_Reader'Class;
      Element : Type_Details_Access);
   --  Insert Element in the type definition one level up

   procedure Compute_Blocks
     (Atts    : Sax_Attribute_List;
      Handler : access Schema_Reader'Class;
      Blocks  : out Block_Status;
      Is_Set  : out Boolean;
      Index   : Integer);
   --  Compute the list of blocked elements from the attribute "block".

   function Compute_Final
     (Atts    : Sax_Attribute_List;
      Handler : access Schema_Reader'Class;
      Index   : Integer) return Final_Status;
   --  Compute the list of final attributes from value. Value is a list similar
   --  to what is used for the "final" attribute of elements in a schema

   function Compute_Form
     (Atts      : Sax_Attribute_List;
      Handler   : access Schema_Reader'Class;
      Index     : Integer;
      Default   : Form_Type) return Form_Type;
   --  Parse the given attribute

   procedure Insert_Attribute
     (Handler        : access Schema_Reader'Class;
      In_Context     : Context_Access;
      Attribute      : Attribute_Validator;
      Attribute_Name : Byte_Sequence;
      Is_Local       : Boolean);
   --  Insert attribute at the right location in In_Context.
   --  Attribute_Name is only for debugging purposes

   function Process_Contents_From_Atts
     (Handler : access Schema_Reader'Class;
      Atts    : Sax_Attribute_List) return Process_Contents_Type;
   --  Get the value of processContents from the attributes

   procedure Create_Element
     (Handler : access Schema_Reader'Class; Atts : Sax_Attribute_List);
   procedure Create_Complex_Type
     (Handler : access Schema_Reader'Class; Atts : Sax_Attribute_List);
   procedure Create_Simple_Type
     (Handler : access Schema_Reader'Class; Atts : Sax_Attribute_List);
   procedure Create_Restriction
     (Handler : access Schema_Reader'Class; Atts : Sax_Attribute_List);
   procedure Create_All
     (Handler : access Schema_Reader'Class; Atts : Sax_Attribute_List);
   procedure Create_Sequence
     (Handler : access Schema_Reader'Class; Atts : Sax_Attribute_List);
   procedure Create_Attribute
     (Handler : access Schema_Reader'Class; Atts : Sax_Attribute_List);
   procedure Create_Schema
     (Handler : access Schema_Reader'Class; Atts : Sax_Attribute_List);
   procedure Create_Extension
     (Handler : access Schema_Reader'Class; Atts : Sax_Attribute_List);
   procedure Create_List
     (Handler : access Schema_Reader'Class; Atts : Sax_Attribute_List);
   procedure Create_Union
     (Handler : access Schema_Reader'Class; Atts : Sax_Attribute_List);
   procedure Create_Choice
     (Handler : access Schema_Reader'Class; Atts : Sax_Attribute_List);
   procedure Create_Redefine
     (Handler : access Schema_Reader'Class; Atts : Sax_Attribute_List);
   procedure Create_Include
     (Handler : access Schema_Reader'Class; Atts : Sax_Attribute_List);
   procedure Create_Group
     (Handler : access Schema_Reader'Class; Atts : Sax_Attribute_List);
   procedure Create_Attribute_Group
     (Handler : access Schema_Reader'Class; Atts : Sax_Attribute_List);
   procedure Create_Any
     (Handler : access Schema_Reader'Class; Atts : Sax_Attribute_List);
   procedure Create_Import
     (Handler : access Schema_Reader'Class; Atts : Sax_Attribute_List);
   procedure Create_Any_Attribute
     (Handler : access Schema_Reader'Class; Atts : Sax_Attribute_List);
   --  Create a new context for a specific tag:
   --  resp. <element>, <complexType>, <restriction>, <all>, <sequence>,
   --  <attribute>, <schema>, <extension>, <list>, <union>, <choice>,
   --  <redefine>, <group>, <attributeGroup>, <any>, <import>, <anyAttribute>

   procedure Finish_Element      (Handler : access Schema_Reader'Class);
   procedure Finish_Complex_Type (Handler : access Schema_Reader'Class);
   procedure Finish_Simple_Type  (Handler : access Schema_Reader'Class);
   procedure Finish_Restriction  (Handler : access Schema_Reader'Class);
   procedure Finish_All          (Handler : access Schema_Reader'Class);
   procedure Finish_Attribute    (Handler : access Schema_Reader'Class);
   procedure Finish_Extension    (Handler : access Schema_Reader'Class);
   procedure Finish_Union        (Handler : access Schema_Reader'Class);
   procedure Finish_List         (Handler : access Schema_Reader'Class);
   procedure Finish_Group        (Handler : access Schema_Reader'Class);
   --  Finish the handling of various tags:
   --  resp. <element>, <complexType>, <restriction>, <all>, <sequence>,
   --  <extension>, <union>, <list>, <choice>, <group>

   procedure Get_Occurs
     (Handler : access Schema_Reader'Class;
      Atts    : Sax_Attribute_List;
      Min_Occurs, Max_Occurs : out Integer);
   --  Get the "minOccurs" and "maxOccurs" attributes

--     function Get_Last_State
--       (Handler   : access Schema_Reader'Class;
--        Ctxt      : Context_Access) return State;
--     --  Return the state we should link from, given the context
--
--     procedure Link_To_Previous
--       (Handler   : access Schema_Reader'Class;
--        Ctxt      : Context_Access;
--        S         : State;
--        On_Symbol : Transition_Event;
--        Min_Occurs : Natural := 1;
--        Max_Occurs : Integer := 1);
--     --  Add link in the state machine for S
--
--     procedure Propagate_Last
--       (Handler : access Schema_Reader'Class);
   --  The current context is terminated, we need to update the parent
   --  context's last_state

   function Max_Occurs_From_Value
     (Reader : access Abstract_Validation_Reader'Class;
      Atts   : Sax_Attribute_List;
      Index  : Integer) return Integer;
   --  Return the value of maxOccurs from the attributes'value. This properly
   --  takes into account the "unbounded" case

   procedure Create_Restricted
     (Handler : access Schema_Reader'Class;
      Ctx     : Context_Access);
   --  Applies to a Context_Restriction, ensures that the restriction has been
   --  created appropriately.

   procedure Debug_Dump_Contexts
     (Handler : Schema_Reader'Class; Prefix : String);
   --  List the current contexts

   -------------------------
   -- Debug_Dump_Contexts --
   -------------------------

   procedure Debug_Dump_Contexts
     (Handler : Schema_Reader'Class; Prefix : String)
   is
      C : Context_Access := Handler.Contexts;
   begin
      if Debug then
         while C /= null loop
            case C.Typ is
               when Context_Type_Def =>
                  Debug_Output
                    (Prefix & "=" & C.Typ'Img
                     & " mixed="
                     & Handler.Types.Table (C.Type_Info).Mixed'Img
                     & " simple="
                     & Handler.Types.Table (C.Type_Info).Simple_Content'Img);

               when others =>
                  Debug_Output (Prefix & "=" & C.Typ'Img);
            end case;
            C := C.Next;
         end loop;
      end if;
   end Debug_Dump_Contexts;

   ---------------
   -- To_String --
   ---------------

   function To_String (Blocks : Block_Status) return String is
   begin
      return "restr=" & Blocks (Block_Restriction)'Img
        & " ext=" & Blocks (Block_Extension)'Img
        & " sub=" & Blocks (Block_Substitution)'Img;
   end To_String;

   ---------------
   -- To_String --
   ---------------

   function To_String (Final : Final_Status) return String is
   begin
      return "restr=" & Final (Final_Restriction)'Img
        & " ext=" & Final (Final_Extension)'Img
        & " union=" & Final (Final_Union)'Img
        & " list=" & Final (Final_List)'Img;
   end To_String;

   -------------------------
   -- In_Redefine_Context --
   -------------------------

   function In_Redefine_Context
     (Handler : Schema_Reader'Class) return Boolean
   is
      Tmp : Context_Access := Handler.Contexts;
   begin
      while Tmp /= null loop
         if Tmp.Typ = Context_Redefine then
            return True;
         end if;
         Tmp := Tmp.Next;
      end loop;
      return False;
   end In_Redefine_Context;

   ---------------------------
   -- Max_Occurs_From_Value --
   ---------------------------

   function Max_Occurs_From_Value
     (Reader : access Abstract_Validation_Reader'Class;
      Atts   : Sax_Attribute_List;
      Index  : Integer) return Integer
   is
      Value : constant Symbol := Get_Value (Atts, Index);
   begin
      if Value = Reader.Unbounded then
         return Unbounded;
      else
         declare
            Val : constant Cst_Byte_Sequence_Access := Get (Value);
            Pos : Integer;
            C   : Unicode_Char;
         begin
            return Integer'Value (Val.all);
         exception
            when Constraint_Error =>
               --  Either we have an integer too big to fit in Integer, or we
               --  do not have an integer at all
               Pos := Val'First;
               while Pos <= Val'Last loop
                  Encoding.Read (Val.all, Pos, C);
                  if not Is_Digit (C) then
                     Validation_Error
                       (Reader, "#Value for ""maxOccurs"" must"
                        & " be an integer or ""unbounded""");
                  end if;
               end loop;

               return Integer'Last;
         end;
      end if;
   end Max_Occurs_From_Value;

   ----------------
   -- XML_To_Ada --
   ----------------

   function XML_To_Ada (Str : Byte_Sequence) return Byte_Sequence is
      Str2 : Byte_Sequence (Str'Range);
   begin
      for S in Str'Range loop
         if Str (S) = '-' then
            Str2 (S) := '_';
         else
            Str2 (S) := Str (S);
         end if;
      end loop;
      return Str2;
   end XML_To_Ada;

   --------------
   -- Ada_Name --
   --------------

   function Ada_Name (Element : XML_Element) return String is
   begin
      return "E_" & XML_To_Ada (To_QName (Element));
   end Ada_Name;

   --------------
   -- Ada_Name --
   --------------

   function Ada_Name (Typ : XML_Type) return String is
   begin
      return "T_" & XML_To_Ada (To_QName (Typ));
   end Ada_Name;

   ----------------
   -- Create_NFA --
   ----------------

   procedure Create_NFA (Parser : access Schema_Reader) is
      NFA : constant Schema.Validators.Schema_State_Machines.NFA_Access :=
        Get_NFA (Get_Grammar (Parser.all));
      End_Schema_Choice : State;

      procedure Process_Global_Element (Info : Element_Descr);
      procedure Process_Type (Info : in out Type_Descr);
      procedure Process_Details
        (Details    : Type_Details_Access;
         Start      : State;
         Nested_End : out State);

      function Create_Element_State
        (Info : Element_Descr; Start : State) return State;
      --  Create (and decorate) the node corresponding to an <element>

      procedure Create_Nested_For_Type (Info : in out Type_Descr);
      --  Create a a nested machine (with only the start state) for [Info]

      --------------------------
      -- Create_Element_State --
      --------------------------

      function Create_Element_State
        (Info : Element_Descr; Start : State) return State
      is
         S1        : constant State := NFA.Add_State (Default_User_Data);
         Typ       : Type_Index;
         XML_Typ   : XML_Type;
         Real      : Element_Descr;

      begin
         --  Resolve element references

         if Info.Name = No_Qualified_Name then
            Real := Get (Parser.Global_Elements, Info.Ref);
         else
            Real := Info;
         end if;

         --  Create nested NFA for the type, if needed

         if Real.Typ = No_Qualified_Name then
            Typ := Real.Local_Type;
            if Typ = No_Type_Index then
               Validation_Error
                 (Parser, "Unknown type for element " & To_QName (Real.Name));
            end if;

         else
            Typ := Get (Parser.Global_Types, Real.Typ);
            if Typ /= No_Type_Index then
               --  OK, we have a complexType
               null;

            else
               --  We have a simpleType
               Lookup_With_NS (Parser, Real.Typ, Result => XML_Typ);
               if Get_Validator (XML_Typ) /= null then
                  NFA.Get_Data (S1).Simple_Type := Get_Validator (XML_Typ);
                  NFA.Get_Data (S1).Type_Name := Real.Typ.Local;
               else
                  Validation_Error
                    (Parser, "Unknown type " & To_QName (Real.Typ));
               end if;
            end if;
         end if;

         if Typ /= No_Type_Index
           and then not Parser.Types.Table (Typ).Simple_Content
         then
            NFA.Set_Nested (S1, Parser.Types.Table (Typ).NFA);
         end if;

         --  Link with previous element

         NFA.Add_Transition (Start, S1, (Transition_Symbol, Real.Name));

         return S1;
      end Create_Element_State;

      ----------------------------
      -- Process_Global_Element --
      ----------------------------

      procedure Process_Global_Element (Info : Element_Descr) is
         S1  : constant State := Create_Element_State (Info, Start_State);
      begin
         NFA.Add_Empty_Transition (S1, End_Schema_Choice);
      end Process_Global_Element;

      ---------------------
      -- Process_Details --
      ---------------------

      procedure Process_Details
        (Details    : Type_Details_Access;
         Start      : State;
         Nested_End : out State)
      is
         S : State;
         T : Type_Details_Access;
      begin
         case Details.Kind is
            when Type_Empty =>
               null;

            when Type_Sequence =>
               S := Start;
               T := Details.First_In_Seq;
               while T /= null loop
                  Process_Details (T, S, Nested_End);
                  NFA.Repeat (S, Nested_End, T.Min_Occurs, T.Max_Occurs);
                  S := Nested_End;
                  T := T.Next;
               end loop;

            when Type_Choice =>
               T := Details.First_In_Choice;
               Nested_End := NFA.Add_State (Default_User_Data);
               while T /= null loop
                  Process_Details (T, Start, S);
                  NFA.Repeat (Start, S, T.Min_Occurs, T.Max_Occurs);
                  NFA.Add_Empty_Transition (S, Nested_End);
                  T := T.Next;
               end loop;

            when Type_Element =>
               Nested_End := Create_Element_State (Details.Element, Start);
         end case;
      end Process_Details;

      ----------------------------
      -- Create_Nested_For_Type --
      ----------------------------

      procedure Create_Nested_For_Type (Info : in out Type_Descr) is
         S : State;
      begin
         if not Info.Simple_Content then
            S := NFA.Add_State
              ((Type_Name   => Info.Name,
                Attributes  => Info.Attributes,
                Simple_Type => null));
            Info.NFA := NFA.Create_Nested (S);
         end if;
      end Create_Nested_For_Type;

      ------------------
      -- Process_Type --
      ------------------

      procedure Process_Type (Info : in out Type_Descr) is
         S, S1 : State;
      begin
         if Info.Simple_Content then
            return;
         end if;

         S := Get_Start_State (Info.NFA);

         if Info.Details /= null then
            Process_Details
              (Details    => Info.Details,
               Start      => S,
               Nested_End => S1);
            NFA.Add_Transition
              (S1, Final_State, (Kind => Transition_Close_Nested));
         end if;
      end Process_Type;

      Element_Info : Element_Descr;

   begin
      if Debug then
         Debug_Output ("Create_NFA");
      end if;

      --  Prepare the nested machines for the global types

      for J in Type_Tables.First .. Last (Parser.Types) loop
         Create_Nested_For_Type (Parser.Types.Table (J));
      end loop;

      --  Prepare schema for global elements

      End_Schema_Choice := NFA.Add_State
        ((Type_Name   => No_Symbol,
          Attributes  => null,
          Simple_Type => null));
      NFA.Add_Transition
        (End_Schema_Choice, Final_State, (Kind => Transition_Close_Nested));

      Element_Info := Get_First (Parser.Global_Elements);
      while Element_Info /= No_Element_Descr loop
         Process_Global_Element (Element_Info);
         Element_Info := Get_Next (Parser.Global_Elements);
      end loop;

      --  Finally, complete the definition of complexTypes

      for J in Type_Tables.First .. Last (Parser.Types) loop
         Process_Type (Parser.Types.Table (J));
      end loop;

      --  NFA.Resolve_Aliases;

      if Debug then
         Output_Action ("NFA: " & Dump (NFA, Dump_Dot_Compact));
      end if;
   end Create_NFA;

   -----------
   -- Parse --
   -----------

   procedure Parse
     (Parser            : in out Schema_Reader;
      Input             : in out Input_Sources.Input_Source'Class;
      Default_Namespace : Symbol;
      Do_Global_Check   : Boolean)
   is
      Validate_XSD : constant Boolean := False;
      Grammar      : XML_Grammar := Get_Grammar (Parser);
      URI          : Symbol;
   begin
      if Debug then
         Output_Action
           ("Parsing schema "& Input_Sources.Get_System_Id (Input));
      end if;

      Initialize_Symbols (Parser);

      URI := Find_Symbol (Parser, Input_Sources.Get_System_Id (Input));

      if not URI_Was_Parsed (Grammar, URI) then
         Initialize_Grammar (Parser'Access);
         Set_Grammar (Parser, Grammar); --  In case it was not initialized yet

         if Debug then
            Output_Action ("Get_NS (Grammar, """
                           & Get (Default_Namespace).all
                           & """, Handler.Target_NS)");
         end if;

         Get_NS (Grammar, Default_Namespace,     Parser.Target_NS);
         Get_NS (Grammar, Parser.XML_Schema_URI, Parser.Schema_NS);

         Init (Parser.Types);

         if Debug then
            Output_Action
              ("Get_NS (Handler.Created_Grammar, {"
               & Get (Default_Namespace).all & "}, Handler.Target_NS)");
         end if;

         Set_Feature
           (Parser, Sax.Readers.Schema_Validation_Feature, Validate_XSD);
         Set_Parsed_URI (Parser'Access, Grammar, URI);
         Set_System_Id (Parser.Target_NS, URI);

         Parse (Validating_Reader (Parser), Input);

         Create_NFA (Parser'Access);

--           if Do_Global_Check then
--              Global_Check (Parser'Unchecked_Access, Parser.Target_NS);
--           end if;

         --  ??? Should not free those, they are needed for other namespaces
         --  as well.
         Reset (Parser.Global_Elements);
         Reset (Parser.Global_Types);

         for T in Type_Tables.First .. Last (Parser.Types) loop
            Free (Parser.Types.Table (T).Details);
         end loop;
         Free (Parser.Types);
      end if;

   exception
      when others =>
         Free (Parser.Contexts, Recurse => True);
         raise;
   end Parse;

   ----------
   -- Hash --
   ----------

   function Hash (Name : Qualified_Name) return Header_Num is
      use type Interfaces.Unsigned_32;
   begin
      return (Hash (Name.NS) + Hash (Name.Local)) / 2;
   end Hash;

   ----------
   -- Hash --
   ----------

   function Hash (Name : Sax.Symbols.Symbol) return Header_Num is
      use type Interfaces.Unsigned_32;
   begin
      if Name = No_Symbol then
         return 0;
      else
         return Header_Num
           (Sax.Symbols.Hash (Name)
            mod Interfaces.Unsigned_32 (Header_Num'Last));
      end if;
   end Hash;

   -----------
   -- Parse --
   -----------

   procedure Parse
     (Parser : in out Schema_Reader;
      Input  : in out Input_Sources.Input_Source'Class) is
   begin
      Parse (Parser, Input,
             Default_Namespace => Empty_String, Do_Global_Check => True);
   end Parse;

   -------------------
   -- Resolve_QName --
   -------------------

   function Resolve_QName
     (Handler : access Schema_Reader'Class;
      QName   : Sax.Symbols.Symbol) return Qualified_Name
   is
      Val       : Cst_Byte_Sequence_Access;
      Separator : Integer;
      NS        : XML_NS;
   begin
      if QName = No_Symbol then
         return No_Qualified_Name;
      else
         Val       := Get (QName);
         Separator := Split_Qname (Val.all);

         Get_Namespace_From_Prefix
           (Handler  => Handler.all,
            Prefix   =>
              Find_Symbol (Handler.all, Val (Val'First .. Separator - 1)),
            NS       => NS);

         return
           (NS    => Get_URI (NS),
            Local =>
              Find_Symbol (Handler.all, Val (Separator + 1 .. Val'Last)));
      end if;
   end Resolve_QName;

   --------------------
   -- Lookup_With_NS --
   --------------------

   procedure Lookup_With_NS
     (Handler : access Schema_Reader'Class;
      Name    : Qualified_Name;
      Result  : out XML_Type)
   is
      G    : XML_Grammar_NS;
   begin
      Get_Grammar_For_Namespace
        (Handler, Name.NS, G, Create_If_Needed => False);
      Result := Lookup (G, Handler, Name.Local);
   end Lookup_With_NS;

   --------------------
   -- Lookup_With_NS --
   --------------------

   procedure Lookup_With_NS
     (Handler : access Schema_Reader'Class;
      Name    : Qualified_Name;
      Result  : out XML_Element)
   is
      G    : XML_Grammar_NS;
   begin
      Get_Grammar_For_Namespace (Handler, Name.NS, G);
      Result := Lookup_Element (G, Handler, Name.Local);
   end Lookup_With_NS;

   --------------------
   -- Lookup_With_NS --
   --------------------

   procedure Lookup_With_NS
     (Handler : access Schema_Reader'Class;
      QName   : Symbol;
      Result  : out XML_Group)
   is
      Name : constant Qualified_Name := Resolve_QName (Handler, QName);
      G    : XML_Grammar_NS;
   begin
      Get_Grammar_For_Namespace (Handler, Name.NS, G);
      Result := Lookup_Group (G, Handler, Name.Local);
   end Lookup_With_NS;

   --------------------
   -- Lookup_With_NS --
   --------------------

   procedure Lookup_With_NS
     (Handler    : access Schema_Reader'Class;
      QName      : Symbol;
      Result     : out XML_Attribute_Group)
   is
      Name : constant Qualified_Name := Resolve_QName (Handler, QName);
      G    : XML_Grammar_NS;
   begin
      Get_Grammar_For_Namespace (Handler, Name.NS, G);
      Result := Lookup_Attribute_Group (G, Handler, Name.Local);
   end Lookup_With_NS;

   ----------------
   -- Get_Occurs --
   ----------------

   procedure Get_Occurs
     (Handler : access Schema_Reader'Class;
      Atts    : Sax_Attribute_List;
      Min_Occurs, Max_Occurs : out Integer)
   is
      Min_Occurs_Index : constant Integer :=
        Get_Index (Atts, URI => Empty_String, Local_Name => Handler.MinOccurs);
      Max_Occurs_Index : constant Integer :=
        Get_Index (Atts, URI => Empty_String, Local_Name => Handler.MaxOccurs);
   begin
      Min_Occurs := 1;
      Max_Occurs := 1;

      if Min_Occurs_Index /= -1 then
         Min_Occurs := Max_Occurs_From_Value (Handler, Atts, Min_Occurs_Index);
         if Min_Occurs = Unbounded then
            Validation_Error
              (Handler, "#minOccurs cannot be ""unbounded""");
         end if;
      end if;

      if Max_Occurs_Index /= -1 then
         Max_Occurs := Max_Occurs_From_Value (Handler, Atts, Max_Occurs_Index);
      end if;
   end Get_Occurs;

   ------------------
   -- Create_Group --
   ------------------

   procedure Create_Group
     (Handler : access Schema_Reader'Class;
      Atts    : Sax_Attribute_List)
   is
      Name_Index : constant Integer :=
        Get_Index (Atts, URI => Empty_String, Local_Name => Handler.Name);
      Ref_Index : constant Integer :=
        Get_Index (Atts, URI => Empty_String, Local_Name => Handler.Ref);
      Tmp  : Context_Access;
      Min_Occurs, Max_Occurs : Integer := 1;
      Seq : Sequence;
   begin
      Get_Occurs (Handler, Atts, Min_Occurs, Max_Occurs);

      Handler.Contexts := new Context'
        (Typ             => Context_Group,
         Group           => No_XML_Group,
         Redefined_Group => No_XML_Group,
         Group_Min       => Min_Occurs,
         Group_Max       => Max_Occurs,
         Next            => Handler.Contexts);

      --  Do not use In_Redefine_Context, since this only applies for types
      --  that are redefined
      if Handler.Contexts.Next.Typ = Context_Redefine then
         Handler.Contexts.Redefined_Group := Redefine_Group
           (Handler.Target_NS, Get_Value (Atts, Name_Index));
         if Debug then
            Output_Action (Ada_Name (Handler.Contexts)
                           & " := Redefine_Group (Handler.Target_NS, """
                           & Get (Get_Value (Atts, Name_Index)).all & """);");
         end if;
      end if;

      if Name_Index /= -1 then
         Handler.Contexts.Group := Create_Global_Group
           (Handler.Target_NS, Handler, Get_Value (Atts, Name_Index));
         if Debug then
            Output_Action
              (Ada_Name (Handler.Contexts)
               & " := Create_Global_Group (Handler.Target_NS, """
               & Get (Get_Value (Atts, Name_Index)).all & """);");
         end if;

      elsif Ref_Index /= -1 then
         if In_Redefine_Context (Handler.all) then
            Tmp := Handler.Contexts;
            while Tmp /= null loop
               if Tmp.Typ = Context_Group
                 and then Tmp.Next.Typ = Context_Redefine
                 and then Get_Local_Name (Tmp.Group) =
                 Get_Value (Atts, Ref_Index)
               then
                  Handler.Contexts.Group := Tmp.Redefined_Group;
                  Output_Action
                    (Ada_Name (Handler.Contexts)
                     & " := <old definition of group>;");
                  exit;
               end if;
               Tmp := Tmp.Next;
            end loop;
         end if;

         if Handler.Contexts.Group = No_XML_Group then
            Lookup_With_NS
              (Handler, Get_Value (Atts, Ref_Index),
               Handler.Contexts.Group);
            if Debug then
               Output_Action
                 (Ada_Name (Handler.Contexts) & " := Group;");
            end if;
         end if;
      end if;

      case Handler.Contexts.Next.Typ is
         when Context_Schema | Context_Redefine =>
            null;

         when Context_Type_Def =>
            null;  --  See Finish_Group

         when Context_Sequence =>
            null;

         when Context_Choice =>
            null;

         when Context_Extension =>
            Seq := Create_Sequence (Handler.Target_NS);
            null;
--              if Debug then
--                 Output_Action ("Validator := Create_Sequence;");
--              end if;
--              Add_Particle (Seq, Handler, Handler.Contexts.Group,
--                            Min_Occurs, Max_Occurs);

            Handler.Contexts.Next.Extension := XML_Validator (Seq);

         when Context_Restriction =>
            Seq := Create_Sequence (Handler.Target_NS);
--              Add_Particle (Seq, Handler, Handler.Contexts.Group,
--                            Min_Occurs, Max_Occurs);
            Handler.Contexts.Next.Restriction := XML_Validator (Seq);

         when others =>
            if Debug then
               Output_Action ("Can't handle nested group decl");
            end if;
            Raise_Exception
              (XML_Not_Implemented'Identity,
               "Unsupported: ""group"" in this context");
      end case;
   end Create_Group;

   ------------------
   -- Finish_Group --
   ------------------

   procedure Finish_Group (Handler : access Schema_Reader'Class) is
      Seq : Sequence;
   begin
      case Handler.Contexts.Next.Typ is
         when Context_Type_Def =>
            Seq := Create_Sequence (Handler.Target_NS);
--              Add_Particle (Seq, Handler, Handler.Contexts.Group,
--                            Handler.Contexts.Group_Min,
--                            Handler.Contexts.Group_Max);

            Handler.Contexts.Next.Type_Validator := Restriction_Of
              (Handler.Target_NS, Handler,
               Lookup (Handler.Schema_NS, Handler, Handler.Anytype),
               XML_Validator (Seq));
            if Debug then
               Output_Action
                 ("Validator := Restriction_Of (Lookup (Handler.Schema.NS,"
                  & """anytype""), " & Ada_Name (Handler.Contexts));
            end if;

         when others =>
            null;
      end case;
   end Finish_Group;

   ----------------------------
   -- Create_Attribute_Group --
   ----------------------------

   procedure Create_Attribute_Group
     (Handler : access Schema_Reader'Class;
      Atts    : Sax_Attribute_List)
   is
      Name_Index : constant Integer :=
        Get_Index (Atts, URI => Empty_String, Local_Name => Handler.Name);
      Ref_Index : constant Integer :=
        Get_Index (Atts, URI => Empty_String, Local_Name => Handler.Ref);
      In_Redefine : constant Boolean := In_Redefine_Context (Handler.all);
   begin
      Handler.Contexts := new Context'
        (Typ            => Context_Attribute_Group,
         Attr_Group     => Empty_Attribute_Group,
         Next           => Handler.Contexts);

      if In_Redefine then
         --  <redefine><attributeGroup>
         --     <attributeGroup ref="foo" />
         --     <attribute name="bar" />
         --  </attributeGroup></redefine>    <!--  xsd003b.xsd test -->

         if Handler.Contexts.Next.Typ = Context_Attribute_Group then
            --  Ignore, this is just to indicate which group we are redefining,
            --  but this was already taken into account for the enclosing tag
            return;
         end if;

         Handler.Contexts.Attr_Group := Lookup_Attribute_Group
           (Handler.Target_NS, Handler, Get_Value (Atts, Name_Index));
         if Debug then
            Output_Action
              (Ada_Name (Handler.Contexts)
               & " := Lookup_Attribute_Group (Handler.Target_NS, """
               & Get (Get_Value (Atts, Name_Index)).all & """);");
         end if;

      elsif Name_Index /= -1 then
         Handler.Contexts.Attr_Group := Create_Global_Attribute_Group
           (Handler.Target_NS, Handler, Get_Value (Atts, Name_Index));
         if Debug then
            Output_Action
              (Ada_Name (Handler.Contexts)
               & " := Create_Global_Attribute_Group (Handler.Target_NS, """
               & Get (Get_Value (Atts, Name_Index)).all & """);");
         end if;

      elsif Ref_Index /= -1 then
         Lookup_With_NS
           (Handler, Get_Value (Atts, Ref_Index),
            Handler.Contexts.Attr_Group);
         if Debug then
            Output_Action
              (Ada_Name (Handler.Contexts) & " := Attr_Group");
         end if;
      end if;

      if not In_Redefine then
         case Handler.Contexts.Next.Typ is
            when Context_Schema | Context_Redefine =>
               null;

            when Context_Type_Def =>
               Ensure_Type (Handler, Handler.Contexts.Next);
               Add_Attribute_Group
                 (Handler.Contexts.Next.Type_Validator,
                  Handler, Handler.Contexts.Attr_Group);
               if Debug then
                  Output_Action
                    ("Add_Attribute_Group ("
                     & Ada_Name (Handler.Contexts.Next)
                     & ", " & Ada_Name (Handler.Contexts) & ");");
               end if;

            when Context_Extension =>
               if Handler.Contexts.Next.Extension = null then
                  Handler.Contexts.Next.Extension := Extension_Of
                    (Handler.Target_NS,
                     Handler.Contexts.Next.Extension_Base, null);
                  if Debug then
                     Output_Action
                       (Ada_Name (Handler.Contexts.Next)
                        & " := Extension_Of ("
                        & Ada_Name (Handler.Contexts.Next.Extension_Base)
                        & ", null);");
                  end if;
                  Handler.Contexts.Next.Extension_Base := No_Type;
               end if;

               Add_Attribute_Group
                 (Handler.Contexts.Next.Extension, Handler,
                  Handler.Contexts.Attr_Group);
               if Debug then
                  Output_Action
                    ("Add_Attribute_Group ("
                     & Ada_Name (Handler.Contexts.Next)
                     & ", " & Ada_Name (Handler.Contexts) & ");");
               end if;

            when Context_Attribute_Group =>
               Add_Attribute_Group
                 (Handler.Contexts.Next.Attr_Group, Handler,
                  Handler.Contexts.Attr_Group);
               if Debug then
                  Output_Action ("Add_Attribute_Group ("
                          & Ada_Name (Handler.Contexts.Next)
                          & ", " & Ada_Name (Handler.Contexts) & ");");
               end if;

            when others =>
               if Debug then
                  Output_Action
                    ("Context is " & Handler.Contexts.Next.Typ'Img);
               end if;
               Raise_Exception
                 (XML_Not_Implemented'Identity,
                  "Unsupported: ""attributeGroup"" in this context");
         end case;
      end if;
   end Create_Attribute_Group;

   --------------------
   -- Create_Include --
   --------------------

   procedure Create_Include
     (Handler : access Schema_Reader'Class;
      Atts    : Sax_Attribute_List)
   is
      Schema_Location_Index : constant Integer :=
        Get_Index (Atts, Empty_String, Handler.Schema_Location);
   begin
      Parse_Grammar
        (Handler,
         URI      => Get_Namespace_URI (Handler.Target_NS),
         Xsd_File => Get_Value (Atts, Schema_Location_Index),
         Do_Global_Check => False);  --  Will be performed later
   end Create_Include;

   ---------------------
   -- Create_Redefine --
   ---------------------

   procedure Create_Redefine
     (Handler : access Schema_Reader'Class;
      Atts    : Sax_Attribute_List)
   is
      Location_Index : constant Integer :=
        Get_Index (Atts, Empty_String, Handler.Schema_Location);
   begin
      --  Disable for now.
      --  On the test./testschema -xsd boeingData/ipo4/ipo.xsd
      --    -xsd boeingData/ipo4/address.xsd
      --    -xsd boeingData/ipo4/itematt.xsd
      --    boeingData/ipo4/ipo_1.xml
      --  we redefine an extension whose base type comes from the redefined
      --  grammar, and whose name is the same. As a result, the extension and
      --  its base type end up being the same XML_Type, and thus we get
      --  infinite loops. We should really merge the models when the grammar is
      --  parsed.

      Raise_Exception
        (XML_Not_Implemented'Identity,
         "<redefine> not supported");
      Parse_Grammar
        (Handler,
         URI      => Get_Namespace_URI (Handler.Target_NS),
         Do_Global_Check => True,
         Xsd_File => Get_Value (Atts, Location_Index));

      Handler.Contexts := new Context'
        (Typ            => Context_Redefine,
         Next           => Handler.Contexts);
   end Create_Redefine;

   -------------------
   -- Create_Import --
   -------------------

   procedure Create_Import
     (Handler : access Schema_Reader'Class;
      Atts    : Sax_Attribute_List)
   is
      Location_Index : constant Integer :=
        Get_Index (Atts, Empty_String, Handler.Schema_Location);
      Namespace_Index : constant Integer :=
        Get_Index (Atts, Empty_String, Handler.Namespace);
      NS : XML_Grammar_NS;
   begin
      if Location_Index = -1 then
         if Namespace_Index = -1 then
            Validation_Error (Handler, "#Missing ""namespace"" attribute");
         end if;

         Get_NS
           (Get_Grammar (Handler.all), Get_Value (Atts, Namespace_Index),
            Result => NS, Create_If_Needed => False);
         if NS = null then
            Validation_Error
              (Handler, "#Cannot resolve namespace "
               & Get (Get_Value (Atts, Namespace_Index)).all);
         end if;
      else
         declare
            Location : constant Symbol := Get_Value (Atts, Location_Index);
            Absolute : constant Symbol := To_Absolute_URI
              (Handler.all, Location);
         begin
            if Debug then
               Debug_Output ("Import: " & Get (Absolute).all);
               Debug_Output ("Adding new grammar to Handler.Created_Grammar");
            end if;

            if not URI_Was_Parsed (Get_Grammar (Handler.all), Absolute) then
               --  The namespace attribute indicates that the XSD may contain
               --  qualified references to schema components in that namespace.
               --  (4.2.6.1). It does not give the default targetNamespace

               Parse_Grammar
                 (Handler,
                  URI      => Empty_String,
                  Do_Global_Check => True,
                  Xsd_File => Location);
            elsif Debug then
               Debug_Output ("Already imported");
            end if;
         end;
      end if;
   end Create_Import;

   --------------------------
   -- Create_Any_Attribute --
   --------------------------

   procedure Create_Any_Attribute
     (Handler : access Schema_Reader'Class;
      Atts    : Sax_Attribute_List)
   is
      Namespace_Index : constant Integer :=
        Get_Index (Atts, Empty_String, Handler.Namespace);
      Process_Contents : constant Process_Contents_Type :=
        Process_Contents_From_Atts (Handler, Atts);
      Kind  : Namespace_Kind;

      List  : NS_List (1 .. Max_Namespaces_In_Any_Attribute);
      Last  : Integer := List'First;

      procedure Cb_Item (Str : Byte_Sequence);
      procedure Cb_Item (Str : Byte_Sequence) is
      begin
         List (Last) := Find_Symbol (Handler.all, Str);
         Last := Last + 1;
      end Cb_Item;

      procedure For_Each is new For_Each_Item (Cb_Item);
   begin
      if Namespace_Index = -1 then
         Kind := Namespace_Any;
      else
         declare
            Val : constant Symbol := Get_Value (Atts, Namespace_Index);
         begin
            if Val = Handler.Other_Namespace then
               Kind := Namespace_Other;
            elsif Val = Handler.Any_Namespace then
               Kind := Namespace_Any;
            else
               Kind := Namespace_List;
               For_Each (Get (Val).all);
            end if;
         end;
      end if;

      Insert_Attribute
        (Handler,
         Handler.Contexts,
         Create_Any_Attribute
           (Handler.Target_NS, Process_Contents, Kind, List (1 .. Last - 1)),
         "Create_Any_Attribute (" & Process_Contents'Img
         & ", " & Kind'Img & """);",
         Is_Local => False);
   end Create_Any_Attribute;

   --------------------
   -- Create_Element --
   --------------------

   procedure Create_Element
     (Handler : access Schema_Reader'Class;
      Atts    : Sax_Attribute_List)
   is
      Min_Occurs, Max_Occurs : Integer := 1;
      Element : XML_Element;
      Typ     : XML_Type := No_Type;
      Info    : Element_Descr;
      Local   : Symbol;
      Details : Type_Details_Access;

   begin
      for J in 1 .. Get_Length (Atts) loop
         if Get_URI (Atts, J) = Empty_String then
            Local := Get_Local_Name (Atts, J);
            if Local = Handler.Typ then
               Info.Typ := Resolve_QName (Handler, Get_Value (Atts, J));
            elsif Local = Handler.Name then
               Info.Name := (NS    => Get_Namespace_URI (Handler.Target_NS),
                             Local => Get_Value (Atts, J));
            elsif Local = Handler.Ref then
               Info.Ref := Resolve_QName (Handler, Get_Value (Atts, J));
            elsif Local = Handler.Substitution_Group then
               Info.Substitution_Group := Get_Value (Atts, J);
            elsif Local = Handler.Default then
               Info.Default := Get_Value (Atts, J);
            elsif Local = Handler.Fixed then
               Info.Fixed := Get_Value (Atts, J);
            elsif Local = Handler.S_Abstract then
               Info.Is_Abstract := Get_Value_As_Boolean (Atts, J, False);
            elsif Local = Handler.Nillable then
               Info.Nillable := Get_Value_As_Boolean (Atts, J, False);
            elsif Local = Handler.Form then
               Info.Form := Compute_Form
                 (Atts, Handler, J, Handler.Element_Form_Default);
            elsif Local = Handler.Final then
               Info.Final := Compute_Final (Atts, Handler, J);
            elsif Local = Handler.Block then
               Compute_Blocks (Atts, Handler, Info.Block, Info.Has_Block, J);
            end if;
         end if;
      end loop;

      if Info.Name /= No_Qualified_Name then
         if Info.Typ /= No_Qualified_Name then
            Lookup_With_NS (Handler, Info.Typ, Result => Typ);

            if To_QName (Typ) = "IDREF"
              or else To_QName (Typ) = "IDREFS"
            then
               Raise_Exception
                 (XML_Not_Implemented'Identity,
                  "Unsupported type IDREF and IDREFS");
            end if;
         end if;

         case Handler.Contexts.Typ is
            when Context_Schema | Context_Redefine =>
               Element := Create_Global_Element
                 (Handler.Target_NS, Handler, Info.Name.Local,
                  Form => Info.Form);
               if Debug then
                  Output_Action (" -> " & Ada_Name (Element));
               end if;

               if Typ /= No_Type then
                  if Debug then
                     Output_Action ("Set_Type (" & Ada_Name (Element) & ", "
                             & Ada_Name (Typ) & ");");
                  end if;
                  Set_Type (Element, Handler, Typ);
               end if;
            when others =>
               Element := Create_Local_Element
                 (Info.Name.Local, Handler.Target_NS, Typ, Form => Info.Form);

               if Debug then
                  Output_Action
                    (Ada_Name (Element) & " := Create_Local_Element ("""
                     & To_QName (Info.Name) & """, Handler.Target_NS, "
                     & Ada_Name (Typ) & ", " & Info.Form'Img & ");");
               end if;
         end case;

         if Info.Ref /= No_Qualified_Name
           and then Info.Ref.NS = No_Symbol
           and then Info.Name = Info.Ref
           and then not In_Redefine_Context (Handler.all)
         then
            Validation_Error
              (Handler, "#""ref"" attribute cannot be self-referencing");

         elsif Info.Ref /= No_Qualified_Name then
            Validation_Error
              (Handler, "#Name and Ref cannot be both specified");
         end if;

      elsif Info.Ref = No_Qualified_Name then
         Validation_Error
           (Handler, "#Either ""name"" or ""ref"" attribute must be present");

      else
         Lookup_With_NS (Handler, Info.Ref, Result => Element);

         --  Section 3.3.2, validity constraints 3.3.3
         if Info.Typ /= No_Qualified_Name then
            Validation_Error
              (Handler,
               "#""type"" attribute cannot be specified along with ""ref""");
         end if;
      end if;

      if Info.Default /= No_Symbol and then Info.Fixed /= No_Symbol then
         Validation_Error
           (Handler, "#Default and Fixed cannot be both specified");
      end if;

      Get_Occurs (Handler, Atts, Min_Occurs, Max_Occurs);
      Handler.Contexts := new Context'
        (Typ            => Context_Element,
         Element        => Info,
         Next           => Handler.Contexts);

      if Handler.Contexts.Next.Typ /= Context_Schema then
         Details := new Type_Details'
           (Kind         => Type_Element,
            Min_Occurs   => Min_Occurs,
            Max_Occurs   => Max_Occurs,
            Next         => null,
            Element      => Info);
         Insert_In_Type (Handler, Details);
      end if;
   end Create_Element;

   --------------------
   -- Finish_Element --
   --------------------

   procedure Finish_Element (Handler : access Schema_Reader'Class) is
      Ctx  : constant Context_Access := Handler.Contexts;
      Info : constant Element_Descr := Ctx.Element;
   begin
      case Handler.Contexts.Next.Typ is
         when Context_Schema | Context_Redefine =>
            Set (Handler.Global_Elements, Info.Name, Info);
         when others =>
            null;
            --  Need to insert the element in the type definition
      end case;

--        if Info.Ref = No_Qualified_Name
--          and then Get_Type (Ctx.Element) = No_Type
--        then
--          --  From 3.3.2.1, the type should be that of the substitutionGroup
--           --  attribute if there is any
--
--           if Get_Substitution_Group (Ctx.Element) /= No_Element then
--              if Debug then
--                 Output_Action ("Set_Type (" & Ada_Name (Handler.Contexts)
--                         & " from substitution group");
--              end if;
--
--          if Get_Type (Get_Substitution_Group (Ctx.Element)) = No_Type then
--                 Raise_Exception
--                   (XML_Not_Implemented'Identity,
--                  "Not supported: type computed from substitutionGroup when"
--                    & " the group has not been fully defined yet");
--              end if;
--
--              Set_Type
--                (Ctx.Element, Handler,
--                 Get_Type (Get_Substitution_Group (Ctx.Element)));
--
--           else
--              --  Otherwise the type is anyType
--              if Debug then
--                 Output_Action ("Set_Type (" & Ada_Name (Handler.Contexts)
--                         & ", Lookup (Handler.Schema_NS, ""ur-Type"");");
--              end if;
--              Set_Type (Ctx.Element, Handler,
--                      Lookup (Handler.Schema_NS, Handler, Handler.Ur_Type));
--           end if;
--        end if;
   end Finish_Element;

   ------------------------
   -- Create_Simple_Type --
   ------------------------

   procedure Create_Simple_Type
     (Handler  : access Schema_Reader'Class;
      Atts     : Sax_Attribute_List)
   is
--      Info : Type_Descr;
   begin
      if Handler.Contexts.Typ = Context_Restriction
        and then Handler.Contexts.Restriction_Base = No_Type
      then
         --  Info.Simple_Content := True;
         Handler.Contexts := new Context'
           (Typ               => Context_Type_Def,
            Type_Info         => No_Type_Index,
            Type_Validator    => null,
            Redefined_Type    => No_Type,
            Next              => Handler.Contexts);

      else
         Create_Complex_Type (Handler, Atts);
      end if;
   end Create_Simple_Type;

   ------------------------
   -- Finish_Simple_Type --
   ------------------------

   procedure Finish_Simple_Type (Handler : access Schema_Reader'Class) is
      C   : constant Context_Access := Handler.Contexts;
      Info : Type_Descr renames Handler.Types.Table (C.Type_Info);
      Typ : XML_Type;
   begin
      if C.Next.Typ = Context_Restriction
        and then C.Next.Restriction_Base = No_Type
      then
         Ensure_Type (Handler, C);

         Typ := Create_Local_Type (Handler.Target_NS, C.Type_Validator);
         if Debug then
            Output_Action
              (Ada_Name (C) & " := Create_Local_Type (Validator);");
         end if;

         Set_Final (Typ, Info.Final);
         if Debug then
            Output_Action ("Set_Final ("
                           & Ada_Name (Typ) & ", "
                           & To_String (Info.Final) & ");");
            Output_Action ("Setting base type for restriction");
         end if;

         C.Next.Restriction_Base := Typ;
      else
         Info.Mixed := True;
         Finish_Complex_Type (Handler);
      end if;
   end Finish_Simple_Type;

   --------------------
   -- Compute_Blocks --
   --------------------

   procedure Compute_Blocks
     (Atts    : Sax_Attribute_List;
      Handler : access Schema_Reader'Class;
      Blocks  : out Block_Status;
      Is_Set  : out Boolean;
      Index   : Integer)
   is
      procedure On_Item (Str : Byte_Sequence);
      procedure On_Item (Str : Byte_Sequence) is
      begin
         if Str = "restriction" then
            Blocks (Block_Restriction) := True;
         elsif Str = "extension" then
            Blocks (Block_Extension) := True;
         elsif Str = "substitution" then
            Blocks (Block_Substitution) := True;
         elsif Str = "#all" then
            Blocks := (others => True);
         else
            Validation_Error
              (Handler, "#Invalid value for block: """ & Str & """");
         end if;
      end On_Item;

      procedure For_Each
        is new Schema.Validators.Lists.For_Each_Item (On_Item);
   begin
      Is_Set := Index /= -1;
      Blocks := No_Block;

      if Index /= -1 then
         For_Each (Get (Get_Value (Atts, Index)).all);

         if Debug then
            Output_Action ("Set_Block (" & To_String (Blocks) & ")");
         end if;
      end if;
   end Compute_Blocks;

   ------------------
   -- Compute_Form --
   ------------------

   function Compute_Form
     (Atts      : Sax_Attribute_List;
      Handler   : access Schema_Reader'Class;
      Index     : Integer;
      Default   : Form_Type) return Form_Type is
   begin
      if Index = -1 then
         return Default;
      elsif Get_Value (Atts, Index) = Handler.Qualified then
         return Qualified;
      else
         return Unqualified;
      end if;
   end Compute_Form;

   -------------------
   -- Compute_Final --
   -------------------

   function Compute_Final
     (Atts    : Sax_Attribute_List;
      Handler : access Schema_Reader'Class;
      Index   : Integer) return Final_Status
   is
      Final : Final_Status;

      procedure On_Item (Str : Byte_Sequence);
      procedure On_Item (Str : Byte_Sequence) is
      begin
         if Str = "restriction" then
            Final (Final_Restriction) := True;
         elsif Str = "extension" then
            Final (Final_Extension)   := True;
         elsif Str = "#all" then
            Final := (others => True);
         elsif Str = "union" then
            Final (Final_Union)       := True;
         elsif Str = "list" then
            Final (Final_List)        := True;
         else
            Validation_Error
              (Handler, "#Invalid value for final: """ & Str & """");
         end if;
      end On_Item;

      procedure For_Each
         is new Schema.Validators.Lists.For_Each_Item (On_Item);
   begin
      Final := (others => False);

      if Index /= -1 then
         For_Each (Get (Get_Value (Atts, Index)).all);

         if Debug then
            Output_Action ("Set_Final (" & To_String (Final) & ")");
         end if;
      end if;

      return Final;
   end Compute_Final;

   -------------------------
   -- Create_Complex_Type --
   -------------------------

   procedure Create_Complex_Type
     (Handler  : access Schema_Reader'Class;
      Atts     : Sax_Attribute_List)
   is
      Is_Set : Boolean;
      Info   : Type_Descr;
      Local  : Symbol;

   begin
      Info.Block := Get_Block_Default (Handler.Target_NS);

      for J in 1 .. Get_Length (Atts) loop
         if Get_URI (Atts, J) = Empty_String then
            Local := Get_Local_Name (Atts, J);
            if Local = Handler.Mixed then
               Info.Mixed := Get_Value_As_Boolean (Atts, J, False);
            elsif Local = Handler.Name then
               Info.Name := Get_Value (Atts, J);
            elsif Local = Handler.Block then
               Compute_Blocks (Atts, Handler, Info.Block, Is_Set, J);
            elsif Local = Handler.Final then
               Info.Final := Compute_Final (Atts, Handler, J);
            elsif Local = Handler.S_Abstract then
               --  Not supported yet anyway
               Raise_Exception
                 (XML_Not_Implemented'Identity,
                  "Unsupported ""abstract"" attribute for complexType");
            end if;
         end if;
      end loop;

      Info.Simple_Content := False;

      Append (Handler.Types, Info);
      case Handler.Contexts.Typ is
         when Context_Schema | Context_Redefine =>
            Set
              (Handler.Global_Types,
               (NS    => Get_Namespace_URI (Handler.Target_NS),
                Local => Info.Name),
               Last (Handler.Types));
         when others =>
            null;
      end case;

      Handler.Contexts := new Context'
        (Typ               => Context_Type_Def,
         Type_Info         => Last (Handler.Types),
         Type_Validator    => null,
         Redefined_Type    => No_Type,
         Next              => Handler.Contexts);

      --  Do not use In_Redefine_Context, since this only applies for types
      --  that are redefined
      if Handler.Contexts.Next.Typ = Context_Redefine then
         Handler.Contexts.Redefined_Type := Redefine_Type
           (Handler.Target_NS, Info.Name);
         if Debug then
            Output_Action ("Validator := Redefine_Type (Handler.Target_NS, """
                    & Get (Info.Name).all & """);");
         end if;
      end if;
   end Create_Complex_Type;

   -----------------
   -- Ensure_Type --
   -----------------

   procedure Ensure_Type
     (Handler : access Schema_Reader'Class; C : Context_Access)
   is
      Base  : XML_Type;
   begin
      if C.Type_Validator = null then
         --  Create a restriction, instead of a simple ur-Type, so that we can
         --  add attributes to it without impacting ur-Type itself

         if Handler.Types.Table (C.Type_Info).Simple_Content then
            if Debug then
               Output_Action
                 ("Validator := Restriction_Of (Lookup "
                  & "(G, ""anySimpleType""));");
            end if;
            Base := Lookup
              (Handler.Schema_NS, Handler, Handler.Any_Simple_Type);

         else
            if Debug then
               Output_Action
                 ("Validator := Restriction_Of (Lookup (G, ""anyType""));");
            end if;
            Base := Lookup (Handler.Schema_NS, Handler, Handler.Anytype);
         end if;

         C.Type_Validator := Restriction_Of
           (Handler.Schema_NS, Handler, Base);
      end if;

      case Handler.Contexts.Next.Typ is
         when Context_Element =>
            Handler.Contexts.Next.Element.Local_Type :=
              Handler.Contexts.Type_Info;

         when others =>
            null;
      end case;
   end Ensure_Type;

   -------------------------
   -- Finish_Complex_Type --
   -------------------------

   procedure Finish_Complex_Type (Handler  : access Schema_Reader'Class) is
      C   : constant Context_Access := Handler.Contexts;
      Typ : XML_Type;
      Info : Type_Descr renames Handler.Types.Table (C.Type_Info);
   begin
      Ensure_Type (Handler, C);
      if Info.Name = No_Symbol then
         Typ := Create_Local_Type (Handler.Target_NS, C.Type_Validator);
         if Debug then
            Output_Action
              (Ada_Name (C) & " := Create_Local_Type (Validator);");
         end if;
      else
         Typ := Create_Global_Type
           (Handler.Target_NS, Handler, Info.Name, C.Type_Validator);
         if Debug then
            Output_Action (Ada_Name (C)
                    & " := Create_Global_Type (Target_NS, """
                    & Get (Info.Name).all & """, Validator);");
         end if;
      end if;

      Set_Block (Typ, Info.Block);
      if Debug then
         Output_Action ("Set_Block (" & Ada_Name (Typ) & ", "
                 & To_String (Info.Block) & ")");
         Output_Action ("Set_Final ("
                        & Ada_Name (Typ) & ", "
                        & To_String (Info.Final) & ");");
         Output_Action ("Set_Mixed_Content ("
           & Ada_Name (C) & ", "
           & Boolean'Image (Info.Mixed or Info.Simple_Content) & ");");
      end if;

      Set_Final (Typ, Info.Final);
      Set_Mixed_Content
        (Get_Validator (Typ), Info.Mixed or Info.Simple_Content);

      case C.Next.Typ is
         when Context_Schema | Context_Redefine =>
            null;
         when Context_Element =>
            null;
--              if Debug then
--                 Output_Action ("Set_Type (" & Ada_Name (C.Next)
--                         & ", " & Ada_Name (C) & ");");
--              end if;
            --  Set_Type (C.Next.Element, Handler, Typ);
         when Context_Attribute =>
            Set_Type (C.Next.Attribute, Typ);
            if Debug then
               Output_Action ("Set_Type (" & Ada_Name (C.Next)
                       & ", " & Ada_Name (Typ) & ");");
            end if;
         when Context_List =>
            C.Next.List_Items := Typ;
            if Debug then
               Output_Action ("Validator := " & Ada_Name (C) & ";");
            end if;
         when others =>
            if Debug then
               Output_Action ("Can't handle nested type decl");
            end if;
            Raise_Exception
              (XML_Not_Implemented'Identity,
               "Unsupported: ""complexType"" in this context");
      end case;
   end Finish_Complex_Type;

   ------------------------
   -- Create_Restriction --
   ------------------------

   procedure Create_Restriction
     (Handler  : access Schema_Reader'Class;
      Atts     : Sax_Attribute_List)
   is
      Base_Index : constant Integer :=
        Get_Index (Atts, Empty_String, Handler.Base);
      Base_Name  : Qualified_Name;
      Base : XML_Type;
      Info : Type_Descr
         renames Handler.Types.Table (Handler.Contexts.Type_Info);
   begin
      if Info.Name /= No_Symbol
        and then Base_Index /= -1
        and then Get_Value (Atts, Base_Index) = Info.Name
      then
         if In_Redefine_Context (Handler.all) then
            Base := Handler.Contexts.Redefined_Type;
         else
            Validation_Error
              (Handler, "#Self-referencing restriction not allowed");
         end if;

      elsif Base_Index /= -1 then
         Base_Name := Resolve_QName (Handler, Get_Value (Atts, Base_Index));
         Lookup_With_NS (Handler, Base_Name, Result => Base);

         if To_QName (Base) = "IDREF"
           or else To_QName (Base) = "IDREFS"
         then
            Raise_Exception
              (XML_Not_Implemented'Identity,
               "Unsupported type IDREF and IDREFS");
         end if;

      else
         Base := No_Type;
      end if;

      if Info.Simple_Content then
         Check_Content_Type (Base, Handler, Should_Be_Simple => True);
      end if;

      Handler.Contexts := new Context'
        (Typ              => Context_Restriction,
         Restriction_Base => Base,
         Restricted       => null,
         Restriction      => null,
         Next             => Handler.Contexts);
   end Create_Restriction;

   -----------------------
   -- Create_Restricted --
   -----------------------

   procedure Create_Restricted
     (Handler : access Schema_Reader'Class;
      Ctx     : Context_Access) is
   begin
      if Ctx.Restricted = null then
         if Ctx.Restriction_Base = No_Type then
            Ctx.Restriction_Base :=
              Lookup (Handler.Schema_NS, Handler, Handler.Ur_Type);
            if Debug then
               Output_Action ("Restriction has no base type set");
            end if;
         end if;

         Ctx.Restricted := Restriction_Of
           (Handler.Target_NS, Handler,
            Ctx.Restriction_Base, Ctx.Restriction);
         if Debug then
            Output_Action (Ada_Name (Ctx)
                    & " := Restriction_Of ("
                    & Ada_Name (Ctx.Restriction_Base) & ", "
                    & "Validator" & ");");
         end if;
      end if;
   end Create_Restricted;

   ------------------------
   -- Finish_Restriction --
   ------------------------

   procedure Finish_Restriction (Handler : access Schema_Reader'Class) is
   begin
      Create_Restricted (Handler, Handler.Contexts);

      case Handler.Contexts.Next.Typ is
         when Context_Type_Def =>
            Handler.Contexts.Next.Type_Validator :=
              Handler.Contexts.Restricted;
            if Debug then
               Output_Action
                 ("Validator := " & Ada_Name (Handler.Contexts) & ";");
            end if;
         when others =>
            if Debug then
               Output_Action ("Can't handler nested restrictions");
            end if;
            Raise_Exception
              (XML_Not_Implemented'Identity,
               "Unsupported: ""restriction"" in this context");
      end case;
   end Finish_Restriction;

   ------------------
   -- Create_Union --
   ------------------

   procedure Create_Union
     (Handler  : access Schema_Reader'Class;
      Atts     : Sax_Attribute_List)
   is
      Member_Index : constant Integer :=
        Get_Index (Atts, Empty_String, Handler.Member_Types);
   begin
      Handler.Contexts := new Context'
        (Typ        => Context_Union,
         Union      => Create_Union (Handler.Target_NS),
         Next       => Handler.Contexts);
      if Debug then
         Output_Action (Ada_Name (Handler.Contexts) & " := Create_Union;");
      end if;

      if Member_Index /= -1 then
         declare
            procedure Cb_Item (Str : Byte_Sequence);

            procedure Cb_Item (Str : Byte_Sequence) is
               S : constant Qualified_Name := Resolve_QName
                 (Handler, Find_Symbol (Handler.all, Str));
               Typ : XML_Type;
            begin
               Lookup_With_NS (Handler, S, Typ);
               Add_Union (Handler.Contexts.Union, Handler, Typ);
               if Debug then
                  Output_Action
                    ("Add_Union ("
                     & Ada_Name (Handler.Contexts) & ", """ & Str & """)");
               end if;
            end Cb_Item;

            procedure For_Each is new For_Each_Item (Cb_Item);
         begin
            For_Each (Get (Get_Value (Atts, Member_Index)).all);
         end;
      end if;
   end Create_Union;

   ------------------
   -- Finish_Union --
   ------------------

   procedure Finish_Union (Handler : access Schema_Reader'Class) is
   begin
      case Handler.Contexts.Next.Typ is
         when Context_Type_Def =>
            Handler.Contexts.Next.Type_Validator := Handler.Contexts.Union;
            if Debug then
               Output_Action
                 ("Validator := " & Ada_Name (Handler.Contexts) & ";");
            end if;

         when others =>
            if Debug then
               Output_Action ("Can't handle nested unions");
            end if;
            Raise_Exception
              (XML_Not_Implemented'Identity,
               "Unsupported: ""union"" in this context");
      end case;
   end Finish_Union;

   ----------------------
   -- Create_Extension --
   ----------------------

   procedure Create_Extension
     (Handler  : access Schema_Reader'Class;
      Atts     : Sax_Attribute_List)
   is
      Base_Index : constant Integer :=
        Get_Index (Atts, Empty_String, Handler.Base);
      Base_Name : Qualified_Name;
      Base : XML_Type;
      Info : Type_Descr
        renames Handler.Types.Table (Handler.Contexts.Type_Info);
   begin
      if Base_Index = -1 then
         Validation_Error
           (Handler, "#Attribute ""base"" required for <extensionType>");
      end if;

      if Get_Value (Atts, Base_Index) = Info.Name then
         if In_Redefine_Context (Handler.all) then
            Base := Handler.Contexts.Redefined_Type;
         else
            Validation_Error
              (Handler, "#Self-referencing extension not allowed");
         end if;
      else
         Base_Name := Resolve_QName (Handler, Get_Value (Atts, Base_Index));
         Lookup_With_NS (Handler, Base_Name, Result => Base);
      end if;

      Handler.Contexts := new Context'
        (Typ            => Context_Extension,
         Extension_Base => Base,
         Extension      => null,
         Next           => Handler.Contexts);
   end Create_Extension;

   ----------------------
   -- Finish_Extension --
   ----------------------

   procedure Finish_Extension (Handler : access Schema_Reader'Class) is
   begin
      case Handler.Contexts.Next.Typ is
         when Context_Type_Def =>
            if Handler.Contexts.Extension_Base /= No_Type then
               Handler.Contexts.Next.Type_Validator := Extension_Of
                 (Handler.Target_NS,
                  Handler.Contexts.Extension_Base,
                  Handler.Contexts.Extension);
               if Debug then
                  Output_Action
                    (Ada_Name (Handler.Contexts) & " := Extension_Of ("
                     & Ada_Name (Handler.Contexts.Extension_Base)
                     & ", Validator);");
               end if;
            else
               Handler.Contexts.Next.Type_Validator :=
                 Handler.Contexts.Extension;
               if Debug then
                  Output_Action
                    (Ada_Name (Handler.Contexts) & " := Validator;");
               end if;
            end if;

         when others =>
            if Debug then
               Output_Action ("Can't handle nested extensions");
            end if;
            Raise_Exception
              (XML_Not_Implemented'Identity,
               "Unsupported: ""extension"" in this context");
      end case;
   end Finish_Extension;

   -----------------
   -- Create_List --
   -----------------

   procedure Create_List
     (Handler : access Schema_Reader'Class;
      Atts    : Sax_Attribute_List)
   is
      Item_Type_Index : constant Integer :=
        Get_Index (Atts, Empty_String, Handler.Item_Type);
      Name : Qualified_Name;
      Items : XML_Type;
   begin
      if Item_Type_Index /= -1 then
         Name := Resolve_QName (Handler, Get_Value (Atts, Item_Type_Index));
         Lookup_With_NS (Handler, Name, Result => Items);
      end if;

      Handler.Contexts := new Context'
        (Typ            => Context_List,
         List_Items     => Items,
         Next           => Handler.Contexts);
   end Create_List;

   -----------------
   -- Finish_List --
   -----------------

   procedure Finish_List (Handler : access Schema_Reader'Class) is
   begin
      case Handler.Contexts.Next.Typ is
         when Context_Type_Def =>
            Handler.Contexts.Next.Type_Validator :=
              Get_Validator
                (List_Of (Handler.Target_NS, Handler.Contexts.List_Items));
            if Debug then
               Output_Action ("Validator := List_Of (Validator);");
            end if;
         when others =>
            if Debug then
               Output_Action ("Can't handle nested list");
            end if;
            Raise_Exception
              (XML_Not_Implemented'Identity,
               "Unsupported: ""list"" in this context");
      end case;
   end Finish_List;

   --------------------
   -- Insert_In_Type --
   --------------------

   procedure Insert_In_Type
     (Handler : access Schema_Reader'Class;
      Element : Type_Details_Access)
   is
      procedure Append (List : in out Type_Details_Access;
                        Elem : Type_Details_Access);
      procedure Append (List : in out Type_Details_Access;
                        Elem : Type_Details_Access)
      is
         Tmp  : Type_Details_Access;
      begin
         if List = null then
            List := Elem;
         else
            Tmp := List;
            while Tmp.Next /= null loop
               Tmp := Tmp.Next;
            end loop;

            Tmp.Next := Elem;
         end if;
      end Append;

      Ctx  : constant Context_Access := Handler.Contexts;
      Next : constant Context_Access := Ctx.Next;
   begin
      case Next.Typ is
         when Context_Type_Def =>
            Handler.Types.Table (Next.Type_Info).Details := Element;

         when Context_Sequence =>
            Append (Next.Seq.First_In_Seq, Element);

         when Context_Choice =>
            Append (Next.Choice.First_In_Choice, Element);

         when Context_Extension | Context_Restriction | Context_Group =>
            Raise_Exception
              (XML_Not_Implemented'Identity,
               "Support for """ & Element.Kind'Img
               & """ here will be implemented soon");

         when Context_Schema | Context_Attribute | Context_Element
            | Context_All | Context_Union
            | Context_List | Context_Redefine | Context_Attribute_Group =>
            null;  --  Should raise exception when validating grammar
--              Raise_Exception
--                (XML_Not_Implemented'Identity,
--                 "Unsupported: ""sequence"" in this context");
      end case;
   end Insert_In_Type;

   -------------------
   -- Create_Choice --
   -------------------

   procedure Create_Choice
     (Handler : access Schema_Reader'Class;
      Atts : Sax_Attribute_List)
   is
      Min_Occurs, Max_Occurs : Integer := 1;
      Choice  : Type_Details_Access;
   begin
      Get_Occurs (Handler, Atts, Min_Occurs, Max_Occurs);
      Choice := new Type_Details'
        (Kind            => Type_Choice,
         Min_Occurs      => Min_Occurs,
         Max_Occurs      => Max_Occurs,
         Next            => null,
         First_In_Choice => null);
      Handler.Contexts := new Context'
        (Typ         => Context_Choice,
         Choice      => Choice,
         Next        => Handler.Contexts);
      Insert_In_Type (Handler, Choice);
   end Create_Choice;

   ---------------------
   -- Create_Sequence --
   ---------------------

   procedure Create_Sequence
     (Handler  : access Schema_Reader'Class;
      Atts     : Sax_Attribute_List)
   is
      Min_Occurs, Max_Occurs : Integer := 1;
      Seq  : Type_Details_Access;
   begin
      Get_Occurs (Handler, Atts, Min_Occurs, Max_Occurs);
      Seq := new Type_Details'
        (Kind         => Type_Sequence,
         Min_Occurs   => Min_Occurs,
         Max_Occurs   => Max_Occurs,
         Next         => null,
         First_In_Seq => null);
      Handler.Contexts := new Context'
        (Typ  => Context_Sequence,
         Seq  => Seq,
         Next => Handler.Contexts);
      Insert_In_Type (Handler, Seq);
   end Create_Sequence;

   ----------------------
   -- Create_Attribute --
   ----------------------

   procedure Create_Attribute
     (Handler  : access Schema_Reader'Class;
      Atts     : Sax_Attribute_List)
   is
      Name_Index : constant Integer :=
        Get_Index (Atts, Empty_String, Handler.Name);
      Type_Index : constant Integer :=
        Get_Index (Atts, Empty_String, Handler.Typ);
      Use_Index : constant Integer :=
        Get_Index (Atts, Empty_String, Handler.S_Use);
      Fixed_Index : constant Integer :=
        Get_Index (Atts, Empty_String, Handler.Fixed);
      Ref_Index : constant Integer :=
        Get_Index (Atts, Empty_String, Handler.Ref);
      Form_Index : constant Integer :=
        Get_Index (Atts, Empty_String, Handler.Form);
      Default_Index : constant Integer :=
        Get_Index (Atts, Empty_String, Handler.Default);
      Target_NS_Index : constant Integer :=
        Get_Index (Atts, Empty_String, Handler.Namespace_Target);

      Att : Attribute_Validator;
      Typ : XML_Type := No_Type;
      Use_Type : Attribute_Use_Type := Optional;
      Form : Form_Type;
      Type_Name : Qualified_Name;

      function Get_Fixed return Symbol;
      --  Return the "fixed" value for the attribute

      function Get_Fixed return Symbol is
      begin
         if Fixed_Index /= -1 then
            if Typ /= No_Type then
               Normalize_Whitespace  --  Depending on the type of the attribute
                 (Typ    => Typ,
                  Reader => Handler,
                  Atts   => Atts,
                  Index  => Fixed_Index);
            end if;
            return Get_Value (Atts, Fixed_Index);
         else
            return No_Symbol;
         end if;
      end Get_Fixed;

   begin
      --  See section 3.2.3 for valid attributes combination

      if Target_NS_Index /= -1 then
         if Name_Index = -1 then
            Validation_Error
              (Handler,
               "#name must be specified when targetNamespace is specified");
         end if;

         if Form_Index /= -1 then
            Validation_Error
              (Handler,
               "#Cannot specify ""form"" when targetNamespace is given");
         end if;

         Raise_Exception
           (XML_Not_Implemented'Identity,
            "targetNamespace not supported in attribute declaration");
      end if;

      if Form_Index /= -1 then
         Form := Form_Type'Value (Get (Get_Value (Atts, Form_Index)).all);

         if Ref_Index /= -1 then
            Validation_Error
              (Handler,
               "#Attributes ""form"" and ""ref"" cannot be both specified");
         end if;

      else
         Form := Handler.Attribute_Form_Default;
      end if;

      if Type_Index /= -1 then
         if Ref_Index /= -1 then
            Validation_Error
              (Handler,
               "#Attributes ""type"" and ""ref"" cannot be both specified");
         end if;

         Type_Name := Resolve_QName (Handler, Get_Value (Atts, Type_Index));
         Lookup_With_NS (Handler, Type_Name, Result => Typ);

         if To_QName (Typ) = "IDREF"
           or else To_QName (Typ) = "IDREFS"
         then
            Raise_Exception
              (XML_Not_Implemented'Identity,
               "Unsupported type IDREF and IDREFS");
         end if;
      end if;

      if Fixed_Index /= -1 and then Default_Index /= -1 then
         Validation_Error
           (Handler,
            "#Attributes ""fixed"" and ""default"" cannot be both specified");
      end if;

      if Use_Index = -1 then
         Use_Type := Optional;
      else
         declare
            Val : constant Symbol := Get_Value (Atts, Use_Index);
         begin
            if Val = Handler.Required then
               Use_Type := Required;
            elsif Val = Handler.Prohibited then
               Use_Type := Prohibited;
            else
               Use_Type := Optional;
            end if;
         end;

         if Default_Index /= -1
           and then Use_Type /= Optional
         then
            Validation_Error
              (Handler,
               "#Use must be ""optional"" when a default value is specified");
         end if;

         if Fixed_Index /= -1
           and then Use_Type = Prohibited
         then
            Validation_Error
              (Handler,
               "#""prohibited"" is forbidden when"
               & " a fixed value is specified");
         end if;
      end if;

      Handler.Contexts := new Context'
        (Typ              => Context_Attribute,
         Attribute        => null,
         Attribute_Is_Ref => Ref_Index /= -1,
         Next             => Handler.Contexts);

      if Name_Index /= -1 then
         case Handler.Contexts.Next.Typ is
            when Context_Attribute_Group | Context_Type_Def =>
               null;

            when others =>
               if Get_Namespace_URI (Handler.Target_NS) =
                 Handler.XML_Instance_URI
               then
                  Validation_Error
                    (Handler,
                     "Invalid target namespace for attribute declaration: """
                     & Get (Get_Namespace_URI (Handler.Target_NS)).all & """");
               end if;
         end case;

         case Handler.Contexts.Next.Typ is
            when Context_Schema | Context_Redefine =>
               Att := Create_Global_Attribute
                 (Local_Name     => Get_Value (Atts, Name_Index),
                  Reader         => Handler,
                  NS             => Handler.Target_NS,
                  Attribute_Type => Typ,
                  Attribute_Use  => Use_Type,
                  Attribute_Form => Form,
                  Fixed          => Get_Fixed);

               if Debug then
                  Output_Action (Ada_Name (Handler.Contexts)
                          & " := Create_Global_Attribute ("""
                          & Get (Get_Value (Atts, Name_Index)).all
                          & """, Handler.Target_NS, "
                          & Ada_Name (Typ)
                          & ", " & Use_Type'Img & ", Qualified, Has_Fixed="
                          & Boolean'Image (Fixed_Index /= -1)
                          & ", " & Form'Img);
               end if;

            when others =>
               Att := Create_Local_Attribute
                 (Local_Name     => Get_Value (Atts, Name_Index),
                  NS             => Handler.Target_NS,
                  Attribute_Type => Typ,
                  Attribute_Use  => Use_Type,
                  Attribute_Form => Form,
                  Fixed          => Get_Fixed);

               if Debug then
                  Output_Action (Ada_Name (Handler.Contexts)
                          & " := Create_Local_Attribute ("""
                          & Get (Get_Value (Atts, Name_Index)).all
                          & """, Handler.Target_NS, "
                          & Ada_Name (Typ)
                          & ", " & Use_Type'Img & ", Qualified, Has_Fixed="
                          & Boolean'Image (Fixed_Index /= -1)
                          & ", " & Form'Img);
               end if;
         end case;
      else
         declare
            Name : constant Qualified_Name :=
              Resolve_QName (Handler, Get_Value (Atts, Ref_Index));
            G    : XML_Grammar_NS;
         begin
            Get_Grammar_For_Namespace (Handler, Name.NS, G);
            Att := Lookup_Attribute (G, Handler, Name.Local);

            --  ??? We haven't normalized the value for fixed here
            Att := Create_Local_Attribute
              (Based_On       => Att,
               Attribute_Use  => Use_Type,
               Attribute_Form => Form,
               Fixed          => Get_Fixed);

            if Debug then
               Output_Action
                 ("Attr := Lookup_Attribute_NS (G, """
                  & Get (Name.Local).all & """);");
               Output_Action
                 (Ada_Name (Handler.Contexts)
                  & " := Create_Local_Attribute (Attr, Handler.Target_NS, "
                  & Use_Type'Img & ", Qualified, Has_Fixed="
                  & Boolean'Image (Fixed_Index /= -1)
                  & ", " & Form'Img);
            end if;
         end;
      end if;

      Handler.Contexts.Attribute := Att;
   end Create_Attribute;

   ----------------------
   -- Insert_Attribute --
   ----------------------

   procedure Insert_Attribute
     (Handler        : access Schema_Reader'Class;
      In_Context     : Context_Access;
      Attribute      : Attribute_Validator;
      Attribute_Name : Byte_Sequence;
      Is_Local       : Boolean) is
   begin
      case In_Context.Typ is
         when Context_Type_Def =>
            Add_Attribute
              (Handler.Types.Table (In_Context.Type_Info).Attributes,
               Attribute);

         when Context_Schema | Context_Redefine =>
            null;

         when Context_Extension =>
            --  If there is no extension at this point, there won't be any as
            --  per the XML schema, since the attributes come last
            if In_Context.Extension = null then
               In_Context.Extension := Extension_Of
                 (Handler.Target_NS, In_Context.Extension_Base, null);
               if Debug then
                  Output_Action (Ada_Name (In_Context) & " := Extension_Of ("
                          & Ada_Name (In_Context.Extension_Base)
                          & ", null);");
               end if;
               In_Context.Extension_Base := No_Type;
            end if;

            Add_Attribute (In_Context.Extension, Attribute,
                           Is_Local => Is_Local);
            if Debug then
               Output_Action ("Add_Attribute (" & Ada_Name (In_Context) & ", "
                       & Attribute_Name & ");");
            end if;

         when Context_Restriction =>
            Create_Restricted (Handler, In_Context);
            Add_Attribute (In_Context.Restricted, Attribute,
                           Is_Local => Is_Local);
            if Debug then
               Output_Action ("Add_Attribute (" & Ada_Name (In_Context) & ", "
                       & Attribute_Name & ");");
            end if;

         when Context_Attribute_Group =>
            Add_Attribute (In_Context.Attr_Group, Attribute,
                           Is_Local => Is_Local);
            if Debug then
               Output_Action ("Add_Attribute (" & Ada_Name (In_Context) & ", "
                       & Attribute_Name & ");");
            end if;

         when Context_Element | Context_Sequence | Context_Choice
            | Context_Attribute | Context_All
            | Context_Union | Context_List | Context_Group =>
            Raise_Exception
              (XML_Not_Implemented'Identity,
               "Unsupported: ""attribute"" in this context");
      end case;
   end Insert_Attribute;

   ----------------------
   -- Finish_Attribute --
   ----------------------

   procedure Finish_Attribute (Handler : access Schema_Reader'Class) is
   begin
      if not Handler.Contexts.Attribute_Is_Ref
        and then Get_Type (Handler.Contexts.Attribute.all) = No_Type
      then
         Set_Type (Handler.Contexts.Attribute,
                   Lookup (Handler.Schema_NS, Handler, Handler.Ur_Type));
         if Debug then
            Output_Action ("Set_Type (" & Ada_Name (Handler.Contexts)
                    & ", Lookup (Handler.Schema_NS, ""ur-Type"");");
         end if;
      end if;

      Insert_Attribute
        (Handler, Handler.Contexts.Next, Handler.Contexts.Attribute,
         Ada_Name (Handler.Contexts),
         Is_Local => not Handler.Contexts.Attribute_Is_Ref);
   end Finish_Attribute;

   -------------------
   -- Create_Schema --
   -------------------

   procedure Create_Schema
     (Handler  : access Schema_Reader'Class;
      Atts     : Sax_Attribute_List)
   is
      Info   : Schema_Descr;
      Is_Set : Boolean := False;
      Local  : Symbol;
   begin
      for J in 1 .. Get_Length (Atts) loop
         if Get_URI (Atts, J) = Empty_String then
            Local := Get_Local_Name (Atts, J);
            if Local = Handler.S_Element_Form_Default then
               Info.Element_Form_Default :=
                 Compute_Form (Atts, Handler, J, Unqualified);
            elsif Local = Handler.S_Attribute_Form_Default then
               Info.Attribute_Form_Default :=
                 Compute_Form (Atts, Handler, J, Unqualified);
            elsif Local = Handler.Block_Default then
               Compute_Blocks (Atts, Handler, Info.Block, Is_Set, J);
            elsif Local = Handler.Namespace_Target then
               Info.Target_NS := Get_Value (Atts, J);
            end if;
         end if;
      end loop;

      if Info.Target_NS /= No_Symbol then
         if Debug then
            Output_Action ("Get_NS (Handler.Created_Grammar, """
                    & Get (Info.Target_NS).all & """, Handler.Target_NS)");
         end if;

         Get_NS (Get_Grammar (Handler.all), Info.Target_NS, Handler.Target_NS);
         Set_Target_NS (Get_Grammar (Handler.all), Handler.Target_NS);
      end if;

      Handler.Element_Form_Default   := Info.Element_Form_Default;
      Handler.Attribute_Form_Default := Info.Attribute_Form_Default;

      if Is_Set then
         Set_Block_Default (Handler.Target_NS, Info.Block);
      end if;

      Handler.Contexts := new Context'
        (Typ         => Context_Schema,
         Next        => null);
   end Create_Schema;

   --------------------------------
   -- Process_Contents_From_Atts --
   --------------------------------

   function Process_Contents_From_Atts
     (Handler : access Schema_Reader'Class;
      Atts : Sax_Attribute_List) return Process_Contents_Type
   is
      Process_Contents_Index : constant Integer := Get_Index
        (Atts, Empty_String, Handler.Process_Contents);
   begin
      if Process_Contents_Index = -1 then
         return Process_Strict;
      elsif Get_Value (Atts, Process_Contents_Index) = Handler.Lax then
         return Process_Lax;
      elsif Get_Value (Atts, Process_Contents_Index) = Handler.Strict then
         return Process_Strict;
      else
         return Process_Skip;
      end if;
   end Process_Contents_From_Atts;

   ----------------
   -- Create_Any --
   ----------------

   procedure Create_Any
     (Handler : access Schema_Reader'Class;
      Atts    : Sax_Attribute_List)
   is
--        Namespace_Index        : constant Integer := Get_Index
--          (Atts, Empty_String, Handler.Namespace);
      Min_Occurs, Max_Occurs : Integer := 1;
--        Process_Contents       : Process_Contents_Type;
--        Any                    : XML_Any;
   begin
      Get_Occurs (Handler, Atts, Min_Occurs, Max_Occurs);
--        Process_Contents := Process_Contents_From_Atts (Handler, Atts);

--        if Namespace_Index /= -1 then
--           Any := Create_Any
--             (Process_Contents => Process_Contents,
--              Namespace        => Get_Value (Atts, Namespace_Index),
--              Target_NS        => Handler.Target_NS);
--           if Debug then
--              Output_Action
--                ("Validator := Create_Any (" & Process_Contents'Img & ", "
--                 & Get (Get_Value (Atts, Namespace_Index)).all
--                 & ", Handler.Target_NS);");
--           end if;
--        else
--           Any := Create_Any
--             (Process_Contents => Process_Contents,
--              Namespace        => Handler.Any_Namespace,
--              Target_NS        => Handler.Target_NS);
--           if Debug then
--              Output_Action
--                ("Validator := Create_Any (" & Process_Contents'Img
--                 & ", ""##any"", Handler.Target_NS);");
--           end if;
--        end if;

--        case Handler.Contexts.Typ is
--           when Context_Sequence =>
--              Add_Particle
--                (Handler.Contexts.Seq, Handler, Any, Min_Occurs, Max_Occurs);
--              if Debug then
--                 Output_Action ("Add_Particle ("
--                         & Ada_Name (Handler.Contexts)
--                         & ", Validator," & Min_Occurs'Img & ","
--                         & Max_Occurs'Img & ");");
--              end if;
--
--           when Context_Choice =>
--              Add_Particle
--                (Handler.Contexts.C, Handler, Any, Min_Occurs, Max_Occurs);
--              if Debug then
--                 Output_Action ("Add_Particle ("
--                         & Ada_Name (Handler.Contexts)
--                         & ", Validator," & Min_Occurs'Img & ","
--                         & Max_Occurs'Img & ");");
--              end if;
--
--           when others =>
--              if Debug then
--                 Output_Action ("Can't handled nested <any>");
--              end if;
--              Raise_Exception
--                (XML_Not_Implemented'Identity,
--                 "Unsupported: ""any"" in this context");
--        end case;
   end Create_Any;

   ----------------
   -- Create_All --
   ----------------

   procedure Create_All
     (Handler  : access Schema_Reader'Class;
      Atts     : Sax_Attribute_List)
   is
      Min_Occurs, Max_Occurs : Integer := 1;
   begin
      Get_Occurs (Handler, Atts, Min_Occurs, Max_Occurs);
      Handler.Contexts := new Context'
        (Typ           => Context_All,
         All_Validator =>
           Create_All (Handler.Target_NS, Min_Occurs, Max_Occurs),
         Next          => Handler.Contexts);
      if Debug then
         Output_Action (Ada_Name (Handler.Contexts) & " := Create_All ("
                 & Min_Occurs'Img & "," & Max_Occurs'Img & ");");
      end if;
   end Create_All;

   ----------------
   -- Finish_All --
   ----------------

   procedure Finish_All (Handler : access Schema_Reader'Class) is
   begin
      case Handler.Contexts.Next.Typ is
         when Context_Type_Def =>
            Handler.Contexts.Next.Type_Validator :=
              XML_Validator (Handler.Contexts.All_Validator);
            if Debug then
               Output_Action ("Validator := XML_Validator ("
                       & Ada_Name (Handler.Contexts) & ");");
            end if;

         when Context_Group =>
            null;
--              Add_Particle
--                (Handler.Contexts.Next.Group, Handler,
--                 Handler.Contexts.All_Validator);
--              if Debug then
--                 Output_Action
--                   ("Add_Particle (" & Ada_Name (Handler.Contexts.Next)
--                    & ", " & Ada_Name (Handler.Contexts) & ");");
--              end if;

         when others =>
            if Debug then
               Output_Action ("Can't handled nested all");
            end if;
            Raise_Exception
              (XML_Not_Implemented'Identity,
               "Unsupported: ""all"" in this context");
      end case;
   end Finish_All;

   --------------
   -- Ada_Name --
   --------------

   function Ada_Name (C : Context_Access) return String is
      L : constant String := Integer'Image (1);
   begin
      case C.Typ is
         when Context_Schema | Context_Redefine =>
            return "";
         when Context_Choice =>
            return "Choice" & L (L'First + 1 .. L'Last);
         when Context_Sequence =>
            return "Seq" & L (L'First + 1 .. L'Last);
         when Context_All =>
            return "All" & L (L'First + 1 .. L'Last);
         when Context_Element =>
            return "foo";
--              return Ada_Name (C.Element);
         when Context_Type_Def =>
            return "???Type???";
--              if C.Type_Info.Name = No_Symbol then
--                 return "T_" & L (L'First + 1 .. L'Last);
--              else
--                 return "T_" & XML_To_Ada (Get (C.Type_Info.Name).all);
--              end if;
         when Context_Attribute =>
            return "A_" & L (L'First + 1 .. L'Last);
         when Context_Restriction =>
            return "R_" & L (L'First + 1 .. L'Last);
         when Context_Extension =>
            return "E_" & L (L'First + 1 .. L'Last);
         when Context_Union =>
            return "U_" & L (L'First + 1 .. L'Last);
         when Context_List =>
            return "L_" & L (L'First + 1 .. L'Last);
         when Context_Group =>
            return "G_" & L (L'First + 1 .. L'Last);
         when Context_Attribute_Group =>
            return "AG_" & L (L'First + 1 .. L'Last);
      end case;
   end Ada_Name;

   -------------------
   -- Start_Element --
   -------------------

   overriding procedure Start_Element
     (Handler       : in out Schema_Reader;
      NS            : Sax.Utils.XML_NS;
      Local_Name    : Sax.Symbols.Symbol;
      Atts          : Sax.Readers.Sax_Attribute_List)
   is
      H : constant Schema_Reader_Access := Handler'Unchecked_Access;
      Val : Integer;
   begin
      if False and Debug then
         Debug_Dump_Contexts (Handler, "Start");
      end if;

      if Debug then
         Output_Seen ("Seen " & Get (Local_Name).all
                      & " at "
                      & Sax.Locators.To_String (Get_Location (Handler)));
      end if;

      --  Check the grammar
      Start_Element (Validating_Reader (Handler), NS, Local_Name, Atts);

      --  Process the element

      if Handler.Contexts = null then
         if Local_Name /= Handler.S_Schema then
            Validation_Error (H, "#Root element must be <schema>");
         end if;

         Create_Schema (H, Atts);

      elsif Local_Name = Handler.Annotation then
         Handler.In_Annotation := True;

      elsif Local_Name = Handler.Element then
         Create_Element (H, Atts);

      elsif Local_Name = Handler.Complex_Type then
         Create_Complex_Type (H, Atts);

      elsif Local_Name = Handler.Simple_Type then
         Create_Simple_Type (H, Atts);

      elsif Local_Name = Handler.Restriction then
         Create_Restriction (H, Atts);

      elsif Local_Name = Handler.Extension then
         Create_Extension (H, Atts);

      elsif Local_Name = Handler.Any_Attribute then
         Create_Any_Attribute (H, Atts);

      elsif Local_Name = Handler.Pattern then
         Val := Get_Index (Atts, Empty_String, Handler.Value);

         declare
            Val2 : constant Cst_Byte_Sequence_Access :=
              Get (Get_Non_Normalized_Value (Atts, Val));
         begin
            Create_Restricted (H, Handler.Contexts);
            Add_Facet (Handler.Contexts.Restricted, H, Local_Name, Val2.all);
            if Debug then
               Output_Action ("Add_Facet ("
                       & Ada_Name (Handler.Contexts) & ", """
                       & Get (Local_Name).all
                       & """, unnormalized=""" & Val2.all & """);");
            end if;
         end;

      elsif Local_Name = Handler.Maxlength
        or else Local_Name = Handler.Minlength
        or else Local_Name = Handler.Length
        or else Local_Name = Handler.Enumeration
        or else Local_Name = Handler.Whitespace
        or else Local_Name = Handler.Total_Digits
        or else Local_Name = Handler.Fraction_Digits
        or else Local_Name = Handler.MaxInclusive
        or else Local_Name = Handler.MaxExclusive
        or else Local_Name = Handler.MinInclusive
        or else Local_Name = Handler.MinExclusive
      then
         case Handler.Contexts.Typ is
            when Context_Restriction =>
               Create_Restricted (H, Handler.Contexts);
               Val := Get_Index (Atts, Empty_String, Handler.Value);
               Add_Facet
                 (Handler.Contexts.Restricted,
                  H, Local_Name,
                  Trim (Get (Get_Value (Atts, Val)).all, Ada.Strings.Both));

            when Context_Extension =>
               Validation_Error
                 (H,
                  "#Invalid restriction in an extension: """
                  & Get (Local_Name).all & """");

            when others =>
               Raise_Exception
                 (XML_Not_Implemented'Identity,
                  '"' & Get (Local_Name).all
                  & """ not supported outside of restriction or extension");
         end case;

         if Debug then
            Output_Action
              ("Add_Facet ("
               & Ada_Name (Handler.Contexts) & ", """ & Get (Local_Name).all
               & """);");
         end if;

      elsif Local_Name = Handler.S_All then
         Create_All (H, Atts);

      elsif Local_Name = Handler.Sequence then
         Create_Sequence (H, Atts);

      elsif Local_Name = Handler.Choice then
         Create_Choice (H, Atts);

      elsif Local_Name = Handler.List then
         Create_List (H, Atts);

      elsif Local_Name = Handler.Union then
         Create_Union (H, Atts);

      elsif Local_Name = Handler.Attribute then
         Create_Attribute (H, Atts);

      elsif Local_Name = Handler.Group then
         Create_Group (H, Atts);

      elsif Local_Name = Handler.Simple_Content then
         Handler.Types.Table (Handler.Contexts.Type_Info).Simple_Content :=
           True;

      elsif Local_Name = Handler.Complex_Content then
         Handler.Types.Table (Handler.Contexts.Type_Info).Simple_Content :=
           False;

      elsif Local_Name = Handler.Attribute_Group then
         Create_Attribute_Group (H, Atts);

      elsif Local_Name = Handler.Any then
         Create_Any (H, Atts);

      elsif Local_Name = Handler.Redefine then
         Create_Redefine (H, Atts);

      elsif Local_Name = Handler.Include then
         Create_Include (H, Atts);

      elsif Local_Name = Handler.Import then
         Create_Import (H, Atts);

      elsif Handler.In_Annotation then
         null;   --  ignore all tags

      else
         Raise_Exception
           (XML_Not_Implemented'Identity,
            "Unsupported element in the schema: " & Get (Local_Name).all);
      end if;
   end Start_Element;

   -----------------
   -- End_Element --
   -----------------

   overriding procedure End_Element
     (Handler       : in out Schema_Reader;
      NS            : Sax.Utils.XML_NS;
      Local_Name    : Sax.Symbols.Symbol)
   is
      H : constant Schema_Reader_Access := Handler'Unchecked_Access;
      C : Context_Access := Handler.Contexts;
      Handled : Boolean := True;
   begin
      if False and Debug then
         Debug_Dump_Contexts (Handler, "End");
      end if;

      --  Check the grammar
      End_Element (Validating_Reader (Handler), NS, Local_Name);

      --  Process the tag
      if Local_Name = Handler.Element then
         Finish_Element (H);

      elsif Local_Name = Handler.S_Schema then
         null;

      elsif Local_Name = Handler.Complex_Type then
         Finish_Complex_Type (H);

      elsif Local_Name = Handler.Simple_Type then
         Finish_Simple_Type (H);

      elsif Local_Name = Handler.S_All then
         Finish_All (H);

      elsif Local_Name = Handler.Sequence then
         null;

      elsif Local_Name = Handler.Any_Attribute then
         Handled := False;

      elsif Local_Name = Handler.Choice then
         null;

      elsif Local_Name = Handler.Restriction then
         Finish_Restriction (H);

      elsif Local_Name = Handler.Extension then
         Finish_Extension (H);

      elsif Local_Name = Handler.Attribute then
         Finish_Attribute (H);

      elsif Local_Name = Handler.Union then
         Finish_Union (H);

      elsif Local_Name = Handler.List then
         Finish_List (H);

      elsif Local_Name = Handler.Maxlength
        or else Local_Name = Handler.Pattern
        or else Local_Name = Handler.Minlength
        or else Local_Name = Handler.Enumeration
        or else Local_Name = Handler.Whitespace
        or else Local_Name = Handler.Total_Digits
        or else Local_Name = Handler.Fraction_Digits
        or else Local_Name = Handler.MaxInclusive
        or else Local_Name = Handler.MaxExclusive
        or else Local_Name = Handler.MinInclusive
        or else Local_Name = Handler.MinExclusive
      then
         Handled := False;

      elsif Local_Name = Handler.Redefine
        or else Local_Name = Handler.Attribute_Group
      then
         null;

      elsif Local_Name = Handler.Group then
         Finish_Group (H);

      elsif Local_Name = Handler.Any
        or else Local_Name = Handler.Include
        or else Local_Name = Handler.Import
        or else Local_Name = Handler.Simple_Content
        or else Local_Name = Handler.Complex_Content
      then
         Handled := False;

      elsif Local_Name = Handler.Annotation then
         Handler.In_Annotation := False;
         Handled := False;

      else
         if Debug then
            Output_Action
              ("Close tag not handled yet: " & Get (Local_Name).all);
         end if;
         Handled := False;
      end if;

      --  Free the context
      if Handled then
         Handler.Contexts := Handler.Contexts.Next;
         Free (C, Recurse => False);
      end if;
   end End_Element;

   ----------
   -- Free --
   ----------

   procedure Free (C : in out Context_Access; Recurse : Boolean) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Context, Context_Access);
      Tmp : Context_Access;
   begin
      while C /= null loop
         Tmp := C.Next;
         Unchecked_Free (C);

         exit when not Recurse;
         C := Tmp;
      end loop;
   end Free;

   ----------------
   -- Characters --
   ----------------

   procedure Characters
     (Handler : in out Schema_Reader; Ch : Unicode.CES.Byte_Sequence)
   is
   begin
      Characters (Validating_Reader (Handler), Ch);
   end Characters;

   -------------------------------
   -- Get_Grammar_For_Namespace --
   -------------------------------

   procedure Get_Grammar_For_Namespace
     (Handler : access Schema_Reader'Class;
      NS      : Sax.Symbols.Symbol;
      Grammar : out XML_Grammar_NS;
      Create_If_Needed : Boolean := True) is
   begin
      if NS = No_Symbol then
         if Debug then
            Output_Action ("G := Handler.Target_NS;");
         end if;
         Grammar := Handler.Target_NS;

      else
         if Debug then
            Output_Action
              ("Get_NS (Handler.Created_Grammar, """
               & Get (NS).all & """, G);");
         end if;

         Get_NS (Get_Grammar (Handler.all),
                 NS, Grammar,
                 Create_If_Needed or else NS = Handler.XML_Schema_URI);
         if Grammar = null then
            Validation_Error
              (Handler,
               "#No location declared for namespace " & Get (NS).all);
         end if;
      end if;
   end Get_Grammar_For_Namespace;

   ----------
   -- Free --
   ----------

   procedure Free (Self : in out Type_Details_Access) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Type_Details, Type_Details_Access);
      Next : Type_Details_Access;
   begin
      while Self /= null loop
         Next := Self.Next;

         case Self.Kind is
            when Type_Empty | Type_Element =>
               null;
            when Type_Sequence =>
               Free (Self.First_In_Seq);
            when Type_Choice =>
               Free (Self.First_In_Choice);
         end case;

         Unchecked_Free (Self);
         Self := Next;
      end loop;
   end Free;

end Schema.Schema_Readers;
