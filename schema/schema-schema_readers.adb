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
with Unicode;           use Unicode;
with Unicode.CES;       use Unicode.CES;
with Sax.Encodings;     use Sax.Encodings;
with Sax.Exceptions;    use Sax.Exceptions;
with Sax.Locators;      use Sax.Locators;
with Sax.Readers;       use Sax.Readers;
with Sax.Symbols;       use Sax.Symbols;
with Sax.Utils;         use Sax.Utils;
with Schema.Validators.Lists; use Schema.Validators.Lists;
with Schema.Readers;    use Schema.Readers;
with Ada.Unchecked_Deallocation;
with Ada.IO_Exceptions;
with Input_Sources.File;    use Input_Sources.File;

package body Schema.Schema_Readers is
   use Schema_State_Machines, Schema_State_Machines_PP;
   use Type_Tables, Element_HTables, Group_HTables;
   use AttrGroup_HTables, Reference_HTables;

   Default_Contexts : constant := 30;
   --  Default number of nested levels in a schema.
   --  If the actual schema uses more, we will simply reallocate some memory.

--     Max_Namespaces_In_Any_Attribute : constant := 50;
   --  Maximum number of namespaces for a <anyAttribute>
   --  This only impacts the parsing of the grammar, so can easily be raised if
   --  need be.

   procedure Push_Context
     (Handler : access Schema_Reader'Class; Ctx : Context);
   --  Add a new context to the list

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Attr_Array, Attr_Array_Access);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Context_Array, Context_Array_Access);

   procedure Free (Shared : in out XSD_Data_Access);
   --  Free [Shared] and its htables

   procedure Free (Self : in out Type_Details_Access);
   --  Free [Self], [Self.Next] and so on

   procedure Create_NFA (Parser : access Schema_Reader);
   --  Create the state machine from the registered elements and types

   function To_String (Blocks : Block_Status) return String;
   function To_String (Final  : Final_Status) return String;

   function In_Redefine_Context
     (Handler : Schema_Reader'Class) return Boolean;
   --  Whether we are currently processing a <redefine> tag

   function Resolve_QName
     (Handler : access Schema_Reader'Class;
      QName   : Sax.Symbols.Symbol) return Qualified_Name;
   --  Resolve namespaces for QName

   procedure Internal_Parse
     (Parser            : in out Schema_Reader;
      Input             : in out Input_Sources.Input_Source'Class;
      Default_Namespace : Symbol;
      Do_Create_NFA     : Boolean;
      Do_Initialize_Shared : Boolean);
   --  Internal version of [Parse], which allows reuse of the shared data.
   --  This is useful while parsing a <include> XSD

   procedure Insert_In_Type
     (Handler : access Schema_Reader'Class;
      Element : Type_Details_Access);
   --  Insert Element in the type definition in [Handler.Contexts]

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

   procedure Append (List : in out Attr_Array_Access; Attr : Attr_Descr);
   --  Add an attribute to the list

   procedure Insert_Attribute
     (Handler        : access Schema_Reader'Class;
      In_Context     : in out Context;
      Attribute      : Internal_Attribute_Descr);
   --  Insert attribute at the right location in In_Context.

   function Process_Contents_From_Atts
     (Handler : access Schema_Reader'Class;
      Atts    : Sax_Attribute_List;
      Index   : Integer) return Process_Contents_Type;
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
   procedure Finish_Attribute    (Handler : access Schema_Reader'Class);
   procedure Finish_Union        (Handler : access Schema_Reader'Class);
   procedure Finish_List         (Handler : access Schema_Reader'Class);
   procedure Finish_Group        (Handler : access Schema_Reader'Class);
   procedure Finish_Attribute_Group (Handler : access Schema_Reader'Class);
   --  Finish the handling of various tags:
   --  resp. <element>, <complexType>, <restriction>, <all>, <sequence>,
   --  <extension>, <union>, <list>, <choice>, <group>

   procedure Get_Occurs
     (Handler : access Schema_Reader'Class;
      Atts    : Sax_Attribute_List;
      Min_Occurs, Max_Occurs : out Integer);
   --  Get the "minOccurs" and "maxOccurs" attributes

   function Max_Occurs_From_Value
     (Reader : access Abstract_Validation_Reader'Class;
      Atts   : Sax_Attribute_List;
      Index  : Integer) return Integer;
   --  Return the value of maxOccurs from the attributes'value. This properly
   --  takes into account the "unbounded" case

--     procedure Create_Restricted
--       (Handler : access Schema_Reader'Class;
--        Ctx     : Context_Access);
   --  Applies to a Context_Restriction, ensures that the restriction has been
   --  created appropriately.

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
     (Handler : Schema_Reader'Class) return Boolean is
   begin
      for J in 1 .. Handler.Contexts_Last loop
         if Handler.Contexts (J).Typ = Context_Redefine then
            return True;
         end if;
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
   -- Create_NFA --
   ----------------

   procedure Create_NFA (Parser : access Schema_Reader) is
      NFA : constant Schema.Validators.Schema_State_Machines.NFA_Access :=
        Get_NFA (Get_Grammar (Parser.all));
      Ref : constant access Reference_HTables.Instance :=
        Get_References (Get_Grammar (Parser.all));

      Shared : XSD_Data_Access renames Parser.Shared;

      End_Schema_Choice : State;

      package Type_HTables is new GNAT.Dynamic_HTables.Simple_HTable
        (Header_Num => Header_Num,
         Element    => Type_Index,
         No_Element => No_Type_Index,
         Key        => Qualified_Name,
         Hash       => Hash,
         Equal      => "=");
      use Type_HTables;
      Types : Type_HTables.Instance;

      procedure Process_Global_Element (Info : Element_Descr);
      procedure Process_Type (Info : in out Internal_Type_Descr);
      procedure Process_Details
        (Details    : Type_Details_Access;
         Start      : State;
         Nested_End : out State);

      function Create_Element_State
        (Info : Element_Descr; Start : State; Global : Boolean) return State;
      --  Create (and decorate) the node corresponding to an <element>

      procedure Create_Nested_For_Type (J : Type_Index);
      --  Create a a nested machine (with only the start state) for [Info]

      procedure Add_Attributes
        (List  : in out Attribute_Validator_List_Access;
         Attrs : Attr_Array_Access);
      --  Create List from the list of attributes or attribute groups in
      --  [Attrs].

      --------------------------
      -- Create_Element_State --
      --------------------------

      function Create_Element_State
        (Info : Element_Descr; Start : State; Global : Boolean) return State
      is
         S1        : State;
         Typ       : Type_Index;
         Real      : Element_Descr;
         Trans     : Transition_Event;
         TRef      : Global_Reference := No_Global_Reference;
         S         : State := No_State;

      begin
         if Info.Is_Abstract then
            --  ??? Should use the substitutionGroup elements, instead
            return No_State;
         end if;

         S1 := NFA.Add_State (Default_User_Data);

         --  Resolve element references

         if Info.Name = No_Qualified_Name then
            Real := Get (Shared.Global_Elements, Info.Ref);
            if Real = No_Element_Descr then
               TRef := Get (Ref.all, (Info.Ref, Ref_Element));
               if TRef = No_Global_Reference then
                  Validation_Error
                    (Parser, "Unknown refed element " & To_QName (Info.Ref),
                     Info.Loc);
               else
                  S := TRef.Element;
                  NFA.Get_Data (S1).Descr := NFA.Get_Data (S).Descr;
                  NFA.Set_Nested (S1, NFA.Get_Nested (S));
                  Trans := (Transition_Symbol, Info.Ref);
               end if;
            else
               Trans := (Transition_Symbol, Real.Name);
            end if;
         else
            Real := Info;
            Trans := (Transition_Symbol, Real.Name);
         end if;

         --  Create nested NFA for the type, if needed

         if S = No_State then
            if Real.Typ = No_Qualified_Name then
               Typ := Real.Local_Type;

            else
               --  Is the type declared globally in the current XSD ?

               Typ := Get (Types, Real.Typ);
               if Typ /= No_Type_Index then
                  null;

               else
                  --  Is the type declared globally in another namespace ?

                  TRef := Get (Ref.all, (Real.Typ, Ref_Type));
                  if TRef /= No_Global_Reference then
                     null;

                  else
                     Validation_Error
                       (Parser, "Unknown type " & To_QName (Real.Typ),
                        Info.Loc);
                  end if;
               end if;
            end if;

            if TRef = No_Global_Reference
              and then Typ = No_Type_Index
            then
               if Info.Substitution_Group /= No_Qualified_Name then
                  --  ??? Handling of substitutionGroup: the type of the
                  --  element is the same as the head unless overridden.
                  null;
               else
                  Validation_Error
                    (Parser, "Unknown type for element "
                     & To_QName (Real.Name) & To_QName (Info.Ref), Info.Loc);
               end if;
            end if;

            if TRef /= No_Global_Reference then
               if NFA.Get_Data (TRef.Typ).Descr.Simple_Content then
                  NFA.Get_Data (S1).Descr := NFA.Get_Data (TRef.Typ).Descr;
               else
                  NFA.Set_Nested (S1, NFA.Create_Nested (TRef.Typ));
               end if;

            elsif Typ /= No_Type_Index then
               if Shared.Types.Table (Typ).Descr.Simple_Content then
                  NFA.Get_Data (S1).Descr := Shared.Types.Table (Typ).Descr;
               else
                  NFA.Set_Nested
                    (S1, NFA.Create_Nested (Shared.Types.Table (Typ).S));
               end if;
            end if;
         end if;

         --  Link with previous element

         NFA.Add_Transition (Start, S1, Trans);

         --  Save this transition for later reuse in other namespaces

         if Global then
            if Debug then
               Debug_Output ("Global elem: " & To_QName (Real.Name));
            end if;
            Set (Ref.all, (Real.Name, Ref_Element),
                 (Kind => Ref_Element, Element => S1)); --  Tr => Trans));
         end if;

         return S1;
      end Create_Element_State;

      ----------------------------
      -- Process_Global_Element --
      ----------------------------

      procedure Process_Global_Element (Info : Element_Descr) is
         S1  : constant State :=
           Create_Element_State (Info, Start_State, True);
      begin
         if S1 /= No_State then
            NFA.Add_Empty_Transition (S1, End_Schema_Choice);
         end if;
      end Process_Global_Element;

      ---------------------
      -- Process_Details --
      ---------------------

      procedure Process_Details
        (Details    : Type_Details_Access;
         Start      : State;
         Nested_End : out State)
      is
         type Details_Array is array (Natural range <>) of Type_Details_Access;
         type Natural_Array is array (Natural range <>) of Natural;

         procedure All_Permutations
           (Details : Details_Array;
            Permut  : in out Natural_Array;
            Index   : Natural);
         --  Generate a choice for all possible permutations of Details

         procedure All_Permutations
           (Details : Details_Array;
            Permut  : in out Natural_Array;
            Index   : Natural)
         is
            Exists : Boolean;
            S1, S2 : State;
            D : Type_Details_Access;
         begin
            for Choice in Details'Range loop
               Exists := False;

               for Prev in 1 .. Index - 1 loop
                  if Permut (Prev) = Choice then
                     Exists := True;
                     exit;
                  end if;
               end loop;

               if not Exists then
                  Permut (Index) := Choice;

                  if Index = Permut'Last then
                     S1 := Start;
                     for J in Permut'Range loop
                        D := Details (Permut (J));
                        Process_Details (D, S1, S2);
                        NFA.Repeat (S1, S2, D.Min_Occurs, D.Max_Occurs);
                        S1 := S2;
                     end loop;
                     NFA.Add_Empty_Transition (S2, Nested_End);
                  else
                     All_Permutations (Details, Permut, Index + 1);
                  end if;
               end if;
            end loop;
         end All_Permutations;

         S  : State;
         T  : Type_Details_Access;
         Gr : Group_Descr;
         Ty : Type_Index;
         Count : Natural := 0;
      begin
         if Details = null then
            Nested_End := Start;
            return;
         end if;

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

            when Type_All =>
               if Details.First_In_All = null then
                  Nested_End := Start;
               else
                  T := Details.First_In_All;
                  while T /= null loop
                     Count := Count + 1;
                     T := T.Next;
                  end loop;

                  declare
                     A      : Details_Array (1 .. Count);
                     Permut : Natural_Array (1 .. Count);
                  begin
                     Count := A'First;
                     T := Details.First_In_All;
                     while T /= null loop
                        A (Count) := T;
                        Count := Count + 1;
                        T := T.Next;
                     end loop;

                     Nested_End := NFA.Add_State (Default_User_Data);
                     All_Permutations (A, Permut, Permut'First);
                  end;
               end if;

            when Type_Element =>
               Nested_End :=
                 Create_Element_State (Details.Element, Start, False);

            when Type_Group =>
               Gr := Get (Parser.Shared.Global_Groups, Details.Group.Ref);
               if Gr = No_Group_Descr then
                  Validation_Error
                    (Parser, "No group """ & To_QName (Details.Group.Ref)
                     & """");
               end if;
               Process_Details (Gr.Details, Start, Nested_End);
               NFA.Repeat
                 (Start, Nested_End, Details.Min_Occurs, Details.Max_Occurs);

            when Type_Extension =>
               Ty := Get (Types, Details.Extension.Base);
               pragma Assert (Ty /= No_Type_Index);
               --  Checked in [Create_Nested_For_Type]

               Process_Details (Shared.Types.Table (Ty).Details, Start, S);
               Process_Details (Details.Extension.Details, S, Nested_End);

            when Type_Restriction =>
               Ty := Get (Types, Details.Restriction.Base);
               pragma Assert (Ty /= No_Type_Index);
               --  Checked in [Create_Nested_For_Type]

               Process_Details
                 (Details.Restriction.Details, Start, Nested_End);

            when Type_Any =>
               Nested_End := NFA.Add_State (Default_User_Data);
               NFA.Add_Transition
                 (Start, Nested_End, (Transition_Any, Details.Any));
               NFA.Repeat
                 (Start, Nested_End, Details.Min_Occurs, Details.Max_Occurs);
         end case;
      end Process_Details;

      --------------------
      -- Add_Attributes --
      --------------------

      procedure Add_Attributes
        (List  : in out Attribute_Validator_List_Access;
         Attrs : Attr_Array_Access)
      is
         Gr   : AttrGroup_Descr;
         TRef : Global_Reference;
         S    : State;
      begin
         if Attrs /= null then
            for A in Attrs'Range loop
               case Attrs (A).Kind is
                  when Kind_Unset =>
                     null;
                  when Kind_Group =>
                     Gr := Get (Shared.Global_AttrGroups, Attrs (A).Group_Ref);
                     if Gr = No_AttrGroup_Descr then
                        Validation_Error
                          (Parser,
                           "Reference to undefined attributeGroup: "
                           & To_QName (Attrs (A).Group_Ref));
                     else
                        Add_Attributes (List, Gr.Attributes);
                     end if;

                  when Kind_Attribute =>
                     if Attrs (A).Attr.Ref /= No_Qualified_Name then
                        TRef := Get
                          (Ref.all, (Attrs (A).Attr.Ref, Ref_Attribute));
                        if TRef = No_Global_Reference then
                           Validation_Error
                             (Parser,
                              "Unknown referenced attribute: "
                              & To_QName (Attrs (A).Attr.Ref),
                              Attrs (A).Attr.Loc);
                        end if;

                     elsif Attrs (A).Attr.Local_Type /= No_Type_Index then
                        S := Shared.Types.Table (Attrs (A).Attr.Local_Type).S;
                     else
                        TRef := Get (Ref.all, (Attrs (A).Attr.Typ, Ref_Type));
                        if TRef = No_Global_Reference then
                           --  ??? Type should be ur-type (3.2.2)
                           null;
                           S := No_State;
--                             Validation_Error
--                               (Parser,
--                                "Unknown type for attribute """
--                                & To_QName (Attrs (A).Attr.Descr.Name)
--                                & To_QName (Attrs (A).Attr.Ref)
--                                & """: " & To_QName (Attrs (A).Attr.Typ),
--                                Attrs (A).Attr.Loc);
                        else
                           S := TRef.Typ;
                        end if;
                     end if;

                     if S /= No_State then
                        Attrs (A).Attr.Descr.Simple_Type :=
                          NFA.Get_Data (S).Descr.Simple_Type;
                     end if;

                     Add_Attribute (List, Attrs (A).Attr.Descr);
               end case;
            end loop;
         end if;
      end Add_Attributes;

      ----------------------------
      -- Create_Nested_For_Type --
      ----------------------------

      procedure Create_Nested_For_Type (J : Type_Index) is
         Info : Internal_Type_Descr renames Shared.Types.Table (J);
      begin
         Info.S := NFA.Add_State ((Descr => Info.Descr));

         --  Do we have a global element ?
         if Info.Descr.Name /= No_Qualified_Name then
            Set
              (Ref.all,
               (Kind => Ref_Type, Name => Info.Descr.Name),
               (Kind => Ref_Type, Typ  => Info.S));
            Set (Types, Info.Descr.Name, J);
         end if;
      end Create_Nested_For_Type;

      ------------------
      -- Process_Type --
      ------------------

      procedure Process_Type (Info : in out Internal_Type_Descr) is
         S1    : State;
         List  : Attribute_Validator_List_Access;

         procedure Recursive_Add_Attributes (Info : Internal_Type_Descr);
         procedure Recursive_Add_Attributes (Info : Internal_Type_Descr) is
            Ty    : Global_Reference;
            Index : Type_Index;
         begin
            if Info.Details.Kind = Type_Extension then
               Ty := Get (Ref.all, (Info.Details.Extension.Base, Ref_Type));
               if Ty = No_Global_Reference then
                  Validation_Error
                    (Parser, "No type """
                     & To_QName (Info.Details.Extension.Base) & """");
               end if;

               --  If the base type is in the current package, we might not
               --  have computed all its attributes. Otherwise, get the list of
               --  attributes already computed in the grammar, since it is
               --  complete.

               Index := Get (Types, Info.Details.Extension.Base);
               if Index /= No_Type_Index then
                  Recursive_Add_Attributes (Shared.Types.Table (Index));
               else
                  Add_Attributes
                    (List, NFA.Get_Data (Ty.Typ).Descr.Attributes);
               end if;

               Add_Attributes (List, Info.Details.Extension.Attributes);

            elsif Info.Details.Kind = Type_Restriction then
               --  ??? Should check Final and Block, too
               Ty := Get (Ref.all, (Info.Details.Restriction.Base, Ref_Type));
               if Ty = No_Global_Reference then
                  Validation_Error
                    (Parser, "No type """
                     & To_QName (Info.Details.Restriction.Base) & """");
               end if;

               Index := Get (Types, Info.Details.Restriction.Base);
               if Index /= No_Type_Index then
                  Recursive_Add_Attributes (Shared.Types.Table (Index));
               else
                  Add_Attributes
                    (List, NFA.Get_Data (Ty.Typ).Descr.Attributes);
               end if;

               Add_Attributes (List, Info.Details.Restriction.Attributes);

            else
               Add_Attributes (List, Info.Attributes);
            end if;
         end Recursive_Add_Attributes;

      begin
         if Info.Details /= null
           and then not Info.Descr.Simple_Content
         then
            if Debug then
               Debug_Output ("Process type " & To_QName (Info.Descr.Name));
            end if;

            Recursive_Add_Attributes (Info);
            NFA.Get_Data (Info.S).Descr.Attributes := List;
            Process_Details
              (Details    => Info.Details,
               Start      => Info.S,
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

      --  Prepare the nested machines for the global types. These are empty,
      --  but we need to know the global elements and groups to be able to
      --  complete them.

      for J in Type_Tables.First .. Last (Shared.Types) loop
         Create_Nested_For_Type (J);
      end loop;

      --  Prepare schema for global elements

      if Debug then
         Debug_Output ("Create_NFA: adding global elements");
      end if;

      End_Schema_Choice := NFA.Add_State (Default_User_Data);
      NFA.Add_Transition
        (End_Schema_Choice, Final_State, (Kind => Transition_Close_Nested));

      Element_Info := Get_First (Shared.Global_Elements);
      while Element_Info /= No_Element_Descr loop
         Process_Global_Element (Element_Info);
         Element_Info := Get_Next (Shared.Global_Elements);
      end loop;

      if Debug then
         Debug_Output ("Create_NFA: complete type definition");
      end if;

      --  Finally, complete the definition of complexTypes

      for J in Type_Tables.First .. Last (Shared.Types) loop
         Process_Type (Shared.Types.Table (J));
      end loop;

      if Debug then
         Output_Action
           ("NFA: " & Dump
              (NFA,
               Mode                => Dump_Dot_Compact,
               Show_Details        => True,
               Show_Isolated_Nodes => False));
      end if;

      Reset (Types);
   end Create_NFA;

   ----------
   -- Free --
   ----------

   procedure Free (Shared : in out XSD_Data_Access) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (XSD_Data, XSD_Data_Access);
      Attr         : AttrGroup_Descr;
      Gr           : Group_Descr;
   begin
      if Shared /= null then

         --  Free all data structures, no longer needed

         Reset (Shared.Global_Elements);

         Gr := Get_First (Shared.Global_Groups);
         while Gr /= No_Group_Descr loop
            Free (Gr.Details);
            Gr := Get_Next (Shared.Global_Groups);
         end loop;
         Reset (Shared.Global_Groups);

         Attr := Get_First (Shared.Global_AttrGroups);
         while Attr /= No_AttrGroup_Descr loop
            Unchecked_Free (Attr.Attributes);
            Attr := Get_Next (Shared.Global_AttrGroups);
         end loop;
         Reset (Shared.Global_AttrGroups);

         for T in Type_Tables.First .. Last (Shared.Types) loop
            Unchecked_Free (Shared.Types.Table (T).Attributes);
            Free (Shared.Types.Table (T).Details);
         end loop;
         Free (Shared.Types);

         Unchecked_Free (Shared);
      end if;
   end Free;

   --------------------------
   -- Set_Document_Locator --
   --------------------------

   overriding procedure Set_Document_Locator
     (Handler : in out Schema_Reader; Loc : in out Sax.Locators.Locator) is
   begin
      Handler.Locator := Loc;
   end Set_Document_Locator;

   -------------------
   -- Parse_Grammar --
   -------------------

   procedure Parse_Grammar
     (Handler  : access Validating_Reader'Class;
      URI      : Symbol;
      Xsd_File : Symbol;
      Do_Create_NFA : Boolean)
   is
      File     : File_Input;
      Schema   : Schema_Reader;
      S_File_Full : constant Symbol := To_Absolute_URI (Handler.all, Xsd_File);
      Need_To_Initialize : Boolean := True;
   begin
      if Debug then
         Debug_Output ("Parse_Grammar NS={" & Get (URI).all
                       & "} XSD={" & Get (Xsd_File).all & "}");
      end if;

      if Get_XSD_Version (Handler.Grammar) = XSD_1_0 then
         --  Must check that no element of the same namespace was seen
         --  already (as per 4.3.2 (4) in the XSD 1.0 norm, which was
         --  changed in XSD 1.1).

         declare
            NS : XML_NS;
            Local_Grammar : XML_Grammar_NS;
         begin
            Get_NS
              (Handler.Grammar,
               Namespace_URI    => URI,
               Result           => Local_Grammar,
               Create_If_Needed => False);

            if Local_Grammar /= null then
               Find_NS_From_URI
                 (Handler.all,
                  URI     => URI,
                  NS      => NS);

               if NS /= No_XML_NS
                 and then Element_Count (NS) > 0
                 and then S_File_Full /= Get_System_Id (Local_Grammar)
               then
                  Validation_Error
                    (Handler,
                     "#schemaLocation for """
                     & Get (URI).all
                     & """ cannot occur after the first"
                     & " element of that namespace in XSD 1.0");
               end if;
            end if;
         end;
      end if;

      if Debug then
         Output_Seen ("Parsing grammar: " & Get (S_File_Full).all);
      end if;

      Open (Get (S_File_Full).all, File);
      Set_Public_Id (File, Get (S_File_Full).all);
      Set_System_Id (File, Get (S_File_Full).all);

      --  Add_To will likely already contain the grammar for the
      --  schema-for-schema, and we won't have to recreate it in most cases.

      Set_Symbol_Table (Schema, Get_Symbol_Table (Handler.all));
      Set_Grammar (Schema, Handler.Grammar);
      Use_Basename_In_Error_Messages
        (Schema, Use_Basename_In_Error_Messages (Handler.all));

--        begin
         if Handler.all in Schema_Reader'Class then
            Schema.Shared := Schema_Reader (Handler.all).Shared;
            Need_To_Initialize := False;
         end if;

         Internal_Parse
           (Schema, File,
            Default_Namespace    => URI,
            Do_Initialize_Shared => Need_To_Initialize,
            Do_Create_NFA        => Need_To_Initialize and Do_Create_NFA);
--        exception
--           when XML_Validation_Error =>
--              --  Have to resolve locations and context now through
--              --  Get_Error_Message, since the error was in another parser
--              Free (Handler.Error_Msg);
--              Handler.Error_Msg :=
--                new Byte_Sequence'(Get_Error_Message (Schema));
--              raise;
--        end;

      Close (File);

      if Debug then
         Output_Seen ("Done parsing new grammar: " & Get (Xsd_File).all);
      end if;

   exception
      when Ada.IO_Exceptions.Name_Error =>
         Close (File);

         if Debug then
            Debug_Output
              (ASCII.LF
               & "!!!! Could not open file " & Get (S_File_Full).all
               & ASCII.LF);
         end if;

         --  According to XML Schema Primer 0, section 5.6, this is not an
         --  error when we do not find the schema, since this attribute is only
         --  a hint.
         Warning
           (Handler.all,
            Create (Message => "Could not open file " & Get (S_File_Full).all,
                    Loc     => Get_Locator (Handler.all)));
      when others =>
         Close (File);
         raise;
   end Parse_Grammar;

   --------------------
   -- Internal_Parse --
   --------------------

   procedure Internal_Parse
     (Parser            : in out Schema_Reader;
      Input             : in out Input_Sources.Input_Source'Class;
      Default_Namespace : Symbol;
      Do_Create_NFA     : Boolean;
      Do_Initialize_Shared : Boolean)
   is
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

         if Do_Initialize_Shared then
            Parser.Shared := new XSD_Data;
            Init (Parser.Shared.Types);
         end if;

         Initialize_Grammar (Parser'Access);

         Get_NS (Grammar, Default_Namespace,     Parser.Target_NS);
         Get_NS (Grammar, Parser.XML_Schema_URI, Parser.Schema_NS);

         Set_Grammar (Parser, Grammar); --  In case it was not initialized yet
         Set_Feature (Parser, Sax.Readers.Schema_Validation_Feature, False);
         Set_Parsed_URI (Parser'Access, Grammar, URI);
         Set_System_Id (Parser.Target_NS, URI);

         Schema.Readers.Parse (Validating_Reader (Parser), Input);

         if Do_Create_NFA then
            Create_NFA (Parser'Access);
         end if;

         if Do_Initialize_Shared then
            Free (Parser.Shared);
         end if;

         Unchecked_Free (Parser.Contexts);
      end if;

   exception
      when others =>
         Unchecked_Free (Parser.Contexts);

         if Do_Initialize_Shared then
            Free (Parser.Shared);
         end if;

         raise;
   end Internal_Parse;

   -----------
   -- Parse --
   -----------

   procedure Parse
     (Parser : in out Schema_Reader;
      Input  : in out Input_Sources.Input_Source'Class) is
   begin
      Internal_Parse
        (Parser,
         Input,
         Default_Namespace => Empty_String,
         Do_Create_NFA     => True,
         Do_Initialize_Shared => True);
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
   -- Push_Context --
   ------------------

   procedure Push_Context
     (Handler : access Schema_Reader'Class; Ctx : Context)
   is
      Tmp : Context_Array_Access;
   begin
      if Handler.Contexts_Last = 0 then
         Handler.Contexts := new Context_Array (1 .. Default_Contexts);
      elsif Handler.Contexts_Last = Handler.Contexts'Last then
         Tmp := new Context_Array (1 .. Handler.Contexts'Last + 30);
         Tmp (Handler.Contexts'Range) := Handler.Contexts.all;
         Unchecked_Free (Handler.Contexts);
         Handler.Contexts := Tmp;
      end if;

      Handler.Contexts_Last := Handler.Contexts_Last + 1;
      Handler.Contexts (Handler.Contexts_Last) := Ctx;
   end Push_Context;

   ------------------
   -- Create_Group --
   ------------------

   procedure Create_Group
     (Handler : access Schema_Reader'Class;
      Atts    : Sax_Attribute_List)
   is
      Min_Occurs, Max_Occurs : Integer := 1;
      Group   : Group_Descr;
      Local   : Symbol;
      Details : Type_Details_Access;
   begin
      for J in 1 .. Get_Length (Atts) loop
         if Get_URI (Atts, J) = Empty_String then
            Local := Get_Local_Name (Atts, J);
            if Local = Handler.Name then
               Group.Name := (NS    => Get_Namespace_URI (Handler.Target_NS),
                              Local => Get_Value (Atts, J));
            elsif Local = Handler.Ref then
               Group.Ref := Resolve_QName (Handler, Get_Value (Atts, J));
            end if;
         end if;
      end loop;

      case Handler.Contexts (Handler.Contexts_Last).Typ is
         when Context_Schema | Context_Redefine =>
            null;

         when Context_Sequence | Context_Choice | Context_Extension
            | Context_Restriction =>
            Get_Occurs (Handler, Atts, Min_Occurs, Max_Occurs);
            Details := new Type_Details'
              (Kind       => Type_Group,
               Min_Occurs => Min_Occurs,
               Max_Occurs => Max_Occurs,
               Next       => null,
               Group      => Group);
            Insert_In_Type (Handler, Details);

         when others =>
            Validation_Error
              (Handler,
               "Unsupported ""group"" in this context",
              Except => XML_Not_Implemented'Identity);
      end case;

      Push_Context (Handler,
                    (Typ   => Context_Group,
                     Group => Group));

      --  Do not use In_Redefine_Context, since this only applies for types
      --  that are redefined

--        if Handler.Contexts.Next.Typ = Context_Redefine then
--           Handler.Contexts.Redefined_Group := Redefine_Group
--             (Handler.Target_NS, Get_Value (Atts, Name_Index));
--           if Debug then
--              Output_Action (Ada_Name (Handler.Contexts)
--                             & " := Redefine_Group (Handler.Target_NS, """
--                          & Get (Get_Value (Atts, Name_Index)).all & """);");
--           end if;
--        end if;

--        if Name_Index /= -1 then
--           Handler.Contexts.Group := Create_Global_Group
--             (Handler.Target_NS, Handler, Get_Value (Atts, Name_Index));
--           if Debug then
--              Output_Action
--                (Ada_Name (Handler.Contexts)
--                 & " := Create_Global_Group (Handler.Target_NS, """
--                 & Get (Get_Value (Atts, Name_Index)).all & """);");
--           end if;
--
--        elsif Ref_Index /= -1 then
--           if In_Redefine_Context (Handler.all) then
--              Tmp := Handler.Contexts;
--              while Tmp /= null loop
--                 if Tmp.Typ = Context_Group
--                   and then Tmp.Next.Typ = Context_Redefine
--                   and then Get_Local_Name (Tmp.Group) =
--                   Get_Value (Atts, Ref_Index)
--                 then
--                    Handler.Contexts.Group := Tmp.Redefined_Group;
--                    Output_Action
--                      (Ada_Name (Handler.Contexts)
--                       & " := <old definition of group>;");
--                    exit;
--                 end if;
--                 Tmp := Tmp.Next;
--              end loop;
--           end if;
--
--           if Handler.Contexts.Group = No_XML_Group then
--              Lookup_With_NS
--                (Handler, Get_Value (Atts, Ref_Index),
--                 Handler.Contexts.Group);
--              if Debug then
--                 Output_Action
--                   (Ada_Name (Handler.Contexts) & " := Group;");
--              end if;
--           end if;
--        end if;
   end Create_Group;

   ------------------
   -- Finish_Group --
   ------------------

   procedure Finish_Group (Handler : access Schema_Reader'Class) is
      Ctx : constant Context_Access :=
        Handler.Contexts (Handler.Contexts_Last)'Access;
      Next : constant Context_Access :=
        Handler.Contexts (Handler.Contexts_Last - 1)'Access;
   begin
      case Next.Typ is
         when Context_Schema | Context_Redefine =>
            Set (Handler.Shared.Global_Groups, Ctx.Group.Name, Ctx.Group);
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
--        In_Redefine : constant Boolean := In_Redefine_Context (Handler.all);
      Group : AttrGroup_Descr;
      Local : Symbol;
   begin
      for J in 1 .. Get_Length (Atts) loop
         if Get_URI (Atts, J) = Empty_String then
            Local := Get_Local_Name (Atts, J);
            if Local = Handler.Name then
               Group.Name := (NS    => Get_Namespace_URI (Handler.Target_NS),
                              Local => Get_Value (Atts, J));
            elsif Local = Handler.Ref then
               Group.Ref := Resolve_QName (Handler, Get_Value (Atts, J));
            end if;
         end if;
      end loop;

      Push_Context
        (Handler, (Typ        => Context_Attribute_Group,
                   Attr_Group => Group));

--        if In_Redefine then
--           --  <redefine><attributeGroup>
--           --     <attributeGroup ref="foo" />
--           --     <attribute name="bar" />
--           --  </attributeGroup></redefine>    <!--  xsd003b.xsd test -->
--
--           if Handler.Contexts.Next.Typ = Context_Attribute_Group then
--          --  Ignore, this is just to indicate which group we are redefining,
--           --  but this was already taken into account for the enclosing tag
--              return;
--           end if;
--
--           Handler.Contexts.Attr_Group := Lookup_Attribute_Group
--             (Handler.Target_NS, Handler, Get_Value (Atts, Name_Index));
--
--        elsif Name_Index /= -1 then
--           Handler.Contexts.Attr_Group := Create_Global_Attribute_Group
--             (Handler.Target_NS, Handler, Get_Value (Atts, Name_Index));
--
--        elsif Ref_Index /= -1 then
--           Lookup_With_NS
--             (Handler, Get_Value (Atts, Ref_Index),
--              Handler.Contexts.Attr_Group);
--        end if;
--
--        if not In_Redefine then
--           case Handler.Contexts.Next.Typ is
--              when Context_Schema | Context_Redefine =>
--                 null;
--
--              when Context_Type_Def =>
--                 Ensure_Type (Handler, Handler.Contexts.Next);
--                 Add_Attribute_Group
--                   (Handler.Contexts.Next.Type_Validator,
--                    Handler, Handler.Contexts.Attr_Group);
--
--              when Context_Extension =>
--                 if Handler.Contexts.Next.Extension = null then
--                    Handler.Contexts.Next.Extension := Extension_Of
--                      (Handler.Target_NS,
--                       Handler.Contexts.Next.Extension_Base, null);
--                    Handler.Contexts.Next.Extension_Base := No_Type;
--                 end if;
--
--                 Add_Attribute_Group
--                   (Handler.Contexts.Next.Extension, Handler,
--                    Handler.Contexts.Attr_Group);
--
--              when Context_Attribute_Group =>
--                 Add_Attribute_Group
--                   (Handler.Contexts.Next.Attr_Group, Handler,
--                    Handler.Contexts.Attr_Group);
--
--              when others =>
--                 Raise_Exception
--                   (XML_Not_Implemented'Identity,
--                    "Unsupported: ""attributeGroup"" in this context");
--           end case;
--        end if;
   end Create_Attribute_Group;

   ----------------------------
   -- Finish_Attribute_Group --
   ----------------------------

   procedure Finish_Attribute_Group (Handler : access Schema_Reader'Class) is
      Ctx : constant Context_Access :=
        Handler.Contexts (Handler.Contexts_Last)'Access;
      Next : constant Context_Access :=
        Handler.Contexts (Handler.Contexts_Last - 1)'Access;
   begin
      case Next.Typ is
         when Context_Schema | Context_Redefine =>
            Set
              (Handler.Shared.Global_AttrGroups,
               Ctx.Attr_Group.Name, Ctx.Attr_Group);
         when Context_Type_Def =>
            Append
              (Handler.Shared.Types.Table (Next.Type_Info).Attributes,
               (Kind      => Kind_Group,
                Group_Ref => Ctx.Attr_Group.Ref));
         when Context_Extension =>
            Append
              (Next.Extension.Extension.Attributes,
               (Kind      => Kind_Group,
                Group_Ref => Ctx.Attr_Group.Ref));
         when others =>
            null;
      end case;
   end Finish_Attribute_Group;

   ------------
   -- Append --
   ------------

   procedure Append (List : in out Attr_Array_Access; Attr : Attr_Descr) is
      Tmp : Attr_Array_Access;
   begin
      if List = null then
         List := new Attr_Array'(1 .. 10 => (Kind => Kind_Unset));
      elsif List (List'Last).Kind = Kind_Unset then
         Tmp := new Attr_Array (1 .. List'Last + 10);
         Tmp (List'Range) := List.all;
         Tmp (List'Last + 1 .. Tmp'Last) :=
           (others => Attr_Descr'(Kind => Kind_Unset));
         Unchecked_Free (List);
         List := Tmp;
      end if;

      for L in List'Range loop
         if List (L).Kind = Kind_Unset then
            List (L) := Attr;
            return;
         end if;
      end loop;
   end Append;

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
         Do_Create_NFA => False);  --  Will be performed later
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
         Do_Create_NFA => True,
         Xsd_File => Get_Value (Atts, Location_Index));

      Push_Context (Handler, (Typ => Context_Redefine));
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
                  Do_Create_NFA => True,
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
--        Namespace_Index : constant Integer :=
--          Get_Index (Atts, Empty_String, Handler.Namespace);
--        Process_Index : constant Integer :=
--          Get_Index (Atts, Empty_String, Handler.Process_Contents);
--        Process_Contents : constant Process_Contents_Type :=
--          Process_Contents_From_Atts (Handler, Atts, Process_Index);
--        Kind  : Namespace_Kind;

--        List  : NS_List (1 .. Max_Namespaces_In_Any_Attribute);
--        Last  : Integer := List'First;
--
--        procedure Cb_Item (Str : Byte_Sequence);
--        procedure Cb_Item (Str : Byte_Sequence) is
--        begin
--           List (Last) := Find_Symbol (Handler.all, Str);
--           Last := Last + 1;
--        end Cb_Item;
--
--        procedure For_Each is new For_Each_Item (Cb_Item);
   begin
      null;
--        if Namespace_Index = -1 then
--           Kind := Namespace_Any;
--        else
--           declare
--              Val : constant Symbol := Get_Value (Atts, Namespace_Index);
--           begin
--              if Val = Handler.Other_Namespace then
--                 Kind := Namespace_Other;
--              elsif Val = Handler.Any_Namespace then
--                 Kind := Namespace_Any;
--              else
--                 Kind := Namespace_List;
--                 For_Each (Get (Val).all);
--              end if;
--           end;
--        end if;

--        Insert_Attribute
--          (Handler,
--           Handler.Contexts (Handler.Contexts_Last),
--           Create_Any_Attribute
--           (Handler.Target_NS, Process_Contents, Kind, List (1 .. Last - 1))
--           Is_Local => False);
   end Create_Any_Attribute;

   --------------------
   -- Create_Element --
   --------------------

   procedure Create_Element
     (Handler : access Schema_Reader'Class;
      Atts    : Sax_Attribute_List)
   is
      Min_Occurs, Max_Occurs : Integer := 1;
      Info    : Element_Descr;
      Local   : Symbol;
      Details : Type_Details_Access;

   begin
      Info.Loc := Get_Location (Get_Locator (Handler.all));

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
               Info.Substitution_Group :=
                 Resolve_QName (Handler, Get_Value (Atts, J));
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
--           if Info.Typ /= No_Qualified_Name then
--              Lookup_With_NS (Handler, Info.Typ, Result => Typ);
--
--              if To_QName (Typ) = "IDREF"
--                or else To_QName (Typ) = "IDREFS"
--              then
--                 Raise_Exception
--                   (XML_Not_Implemented'Identity,
--                    "Unsupported type IDREF and IDREFS");
--              end if;
--           end if;

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

      if Handler.Contexts (Handler.Contexts_Last).Typ /= Context_Schema then
         Get_Occurs (Handler, Atts, Min_Occurs, Max_Occurs);
         Details := new Type_Details'
           (Kind         => Type_Element,
            Min_Occurs   => Min_Occurs,
            Max_Occurs   => Max_Occurs,
            Next         => null,
            Element      => Info);
         Insert_In_Type (Handler, Details);
      end if;

      Push_Context
        (Handler,
         (Typ          => Context_Element,
          Elem_Details => Details,
          Element      => Info));
   end Create_Element;

   --------------------
   -- Finish_Element --
   --------------------

   procedure Finish_Element (Handler : access Schema_Reader'Class) is
      Ctx : constant Context_Access :=
        Handler.Contexts (Handler.Contexts_Last)'Access;
      Next : constant Context_Access :=
        Handler.Contexts (Handler.Contexts_Last - 1)'Access;
      Info : constant Element_Descr := Ctx.Element;
   begin
      case Next.Typ is
         when Context_Schema | Context_Redefine =>
            Set (Handler.Shared.Global_Elements, Info.Name, Info);
         when others =>
            --  We might have added the type definition
            Ctx.Elem_Details.Element := Ctx.Element;
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
     (Handler : access Schema_Reader'Class;
      Atts    : Sax_Attribute_List)
   is
      Ctx : Context_Access := Handler.Contexts (Handler.Contexts_Last)'Access;
   begin
      if Ctx.Typ = Context_Simple_Restriction then
         --  We are in a <simpleType><restriction><simpleType> context
         Push_Context
           (Handler,
            (Typ       => Context_Type_Def,
             Type_Info => No_Type_Index));
      else
         Create_Complex_Type (Handler, Atts);
         Ctx := Handler.Contexts (Handler.Contexts_Last)'Access;
         Handler.Shared.Types.Table (Ctx.Type_Info).Descr.Simple_Content :=
           True;
      end if;
   end Create_Simple_Type;

   ------------------------
   -- Finish_Simple_Type --
   ------------------------

   procedure Finish_Simple_Type (Handler : access Schema_Reader'Class) is
      Ctx : constant Context_Access :=
        Handler.Contexts (Handler.Contexts_Last)'Access;
      Next : constant Context_Access :=
        Handler.Contexts (Handler.Contexts_Last - 1)'Access;
   begin
      case Next.Typ is
         when Context_Schema | Context_Redefine => null;
         when Context_Element   => Next.Element.Local_Type   := Ctx.Type_Info;
         when Context_Attribute => Next.Attribute.Local_Type := Ctx.Type_Info;
         when Context_List    => Next.List.List_Local_Type   := Ctx.Type_Info;
         when Context_Union   => Next.Union.Union_Local_Type := Ctx.Type_Info;
         when Context_Restriction =>
            Next.Restriction.Restriction.Details :=
              Handler.Shared.Types.Table (Ctx.Type_Info).Details;
         when Context_Simple_Restriction =>
            null;
         when others          =>
            Raise_Exception
              (XML_Not_Implemented'Identity,
               "Unsupported: ""simpleType"" in this context");
      end case;
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
      Info   : Internal_Type_Descr;
      Local  : Symbol;
--        Ctx : constant Context_Access :=
--          Handler.Contexts (Handler.Contexts_Last)'Access;
--        Redefined : XML_Type := No_Type;

   begin
      Info.Descr.Block := Get_Block_Default (Handler.Target_NS);

      for J in 1 .. Get_Length (Atts) loop
         if Get_URI (Atts, J) = Empty_String then
            Local := Get_Local_Name (Atts, J);
            if Local = Handler.Mixed then
               Info.Descr.Mixed := Get_Value_As_Boolean (Atts, J, False);
            elsif Local = Handler.Name then
               Info.Descr.Name :=
                 (NS    => Get_Namespace_URI (Handler.Target_NS),
                  Local => Get_Value (Atts, J));
            elsif Local = Handler.Block then
               Compute_Blocks (Atts, Handler, Info.Descr.Block, Is_Set, J);
            elsif Local = Handler.Final then
               Info.Descr.Final := Compute_Final (Atts, Handler, J);
            elsif Local = Handler.S_Abstract then
               Info.Descr.Is_Abstract := Get_Value_As_Boolean (Atts, J, False);
            end if;
         end if;
      end loop;

      Info.Descr.Simple_Content := False;

      Append (Handler.Shared.Types, Info);

      --  Do not use In_Redefine_Context, since this only applies for types
      --  that are redefined
--        if Ctx.Typ = Context_Redefine then
--      Redefined := Redefine_Type (Handler.Target_NS, Info.Descr.Name.Local);
--        end if;

      Push_Context
        (Handler,
         (Typ       => Context_Type_Def,
          Type_Info => Last (Handler.Shared.Types)));
   end Create_Complex_Type;

   -------------------------
   -- Finish_Complex_Type --
   -------------------------

   procedure Finish_Complex_Type (Handler  : access Schema_Reader'Class) is
      Ctx : constant Context_Access :=
        Handler.Contexts (Handler.Contexts_Last)'Access;
      Next : constant Context_Access :=
        Handler.Contexts (Handler.Contexts_Last - 1)'Access;
   begin
      case Next.Typ is
         when Context_Element => Next.Element.Local_Type := Ctx.Type_Info;
         when others          => null;
      end case;
   end Finish_Complex_Type;

   ------------------------
   -- Create_Restriction --
   ------------------------

   procedure Create_Restriction
     (Handler  : access Schema_Reader'Class;
      Atts     : Sax_Attribute_List)
   is
      Ctx : constant Context_Access :=
        Handler.Contexts (Handler.Contexts_Last)'Access;
      Restr      : Restriction_Descr;
--        Base       : XML_Type;
      Details    : Type_Details_Access;
      Local      : Symbol;
      In_Type    : constant Type_Index := Ctx.Type_Info;
   begin
      for J in 1 .. Get_Length (Atts) loop
         if Get_URI (Atts, J) = Empty_String then
            Local := Get_Local_Name (Atts, J);
            if Local = Handler.Base then
               Restr.Base := Resolve_QName (Handler, Get_Value (Atts, J));
            end if;
         end if;
      end loop;

      if In_Type /= No_Type_Index then
         if Handler.Shared.Types.Table (In_Type).Descr.Name = Restr.Base then
            --  if In_Redefine_Context (Handler.all) then
            --    Base := Handler.Contexts.Redefined_Type;
            --  else
            Validation_Error
              (Handler, "#Self-referencing restriction not allowed");
            --  end if;
         end if;
      end if;

      if Handler.Shared.Types.Table (In_Type).Descr.Simple_Content then
         Handler.Shared.Types.Table (In_Type).Simple :=
           (Kind             => Simple_Type_Restriction,
            Restriction_Base => Restr.Base);

         if To_QName (Restr.Base) = "IDREF"
           or else To_QName (Restr.Base) = "IDREFS"
         then
            Raise_Exception
              (XML_Not_Implemented'Identity,
               "Unsupported type IDREF and IDREFS");
         end if;

         --  Check_Content_Type (Base, Handler, Should_Be_Simple => True);

         Push_Context
           (Handler,
            (Typ         => Context_Simple_Restriction,
             Simple      => (Kind => Simple_Type)));

      else
         Details := new Type_Details'
           (Kind        => Type_Restriction,
            Min_Occurs  => 1,
            Max_Occurs  => 1,
            Next        => null,
            Restriction => Restr);
         Insert_In_Type (Handler, Details);
         Push_Context
           (Handler,
            (Typ         => Context_Restriction,
             Restriction => Details));
      end if;
   end Create_Restriction;

   -----------------------
   -- Create_Restricted --
   -----------------------

--     procedure Create_Restricted
--       (Handler : access Schema_Reader'Class;
--        Ctx     : Context_Access) is
--     begin
--        if Ctx.Restricted = null then
--           if Ctx.Restriction_Base = No_Type then
--              Ctx.Restriction_Base :=
--                Lookup (Handler.Schema_NS, Handler, Handler.Ur_Type);
--              if Debug then
--                 Output_Action ("Restriction has no base type set");
--              end if;
--           end if;
--
--           Ctx.Restricted := Restriction_Of
--             (Handler.Target_NS, Handler,
--              Ctx.Restriction_Base, Ctx.Restriction_Validator);
--           if Debug then
--              Output_Action (Ada_Name (Ctx)
--                      & " := Restriction_Of ("
--                      & Ada_Name (Ctx.Restriction_Base) & ", "
--                      & "Validator" & ");");
--           end if;
--        end if;
--     end Create_Restricted;

   ------------------------
   -- Finish_Restriction --
   ------------------------

   procedure Finish_Restriction (Handler : access Schema_Reader'Class) is
      Ctx : constant Context_Access :=
        Handler.Contexts (Handler.Contexts_Last)'Access;
      Next : constant Context_Access :=
        Handler.Contexts (Handler.Contexts_Last - 1)'Access;
--        In_Type    : constant Type_Index := Next.Type_Info;
   begin
      if Ctx.Typ = Context_Simple_Restriction then
         pragma Assert (Next.Typ = Context_Type_Def);  --  a simple type
         Handler.Shared.Types.Table (Next.Type_Info).Simple := Ctx.Simple;
      end if;

--        if Handler.Types.Table (In_Type).Descr.Simple_Content then
--           case Next.Typ is
--              when Context_Type_Def =>
--                 Next.Type_Validator := Ctx.Restricted;
--              when others =>
--                 Raise_Exception
--                   (XML_Not_Implemented'Identity,
--                    "Unsupported: ""restriction"" in this context");
--           end case;
--        end if;
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
      Push_Context
        (Handler,
         (Typ   => Context_Union,
          Union => (Kind             => Simple_Type_Union,
                    Union_Items      => Get_Value (Atts, Member_Index),
                    Union_Local_Type => No_Type_Index)));

--        if Member_Index /= -1 then
--           declare
--              procedure Cb_Item (Str : Byte_Sequence);
--
--              procedure Cb_Item (Str : Byte_Sequence) is
--                 S : constant Qualified_Name := Resolve_QName
--                   (Handler, Find_Symbol (Handler.all, Str));
--                 Typ : Type_Descr;
--              begin
--                 Lookup_With_NS (Handler, S, Typ);
--                 Add_Union (Union, Handler, Typ);
--              end Cb_Item;
--
--              procedure For_Each is new For_Each_Item (Cb_Item);
--           begin
--              For_Each (Get (Get_Value (Atts, Member_Index)).all);
--           end;
--        end if;
   end Create_Union;

   ------------------
   -- Finish_Union --
   ------------------

   procedure Finish_Union (Handler : access Schema_Reader'Class) is
      Ctx : constant Context_Access :=
        Handler.Contexts (Handler.Contexts_Last)'Access;
      Next : constant Context_Access :=
        Handler.Contexts (Handler.Contexts_Last - 1)'Access;
   begin
      case Next.Typ is
         when Context_Type_Def =>
            Handler.Shared.Types.Table (Next.Type_Info).Simple := Ctx.Union;
         when others =>
            --  ??? Should be checked in [Create_Union]
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
      Ctx : constant Context_Access :=
        Handler.Contexts (Handler.Contexts_Last)'Access;
      Info : Internal_Type_Descr
        renames Handler.Shared.Types.Table (Ctx.Type_Info);
      Ext   : Extension_Descr;
      Local : Symbol;
      Details : Type_Details_Access;
   begin
      for J in 1 .. Get_Length (Atts) loop
         if Get_URI (Atts, J) = Empty_String then
            Local := Get_Local_Name (Atts, J);
            if Local = Handler.Base then
               Ext.Base := Resolve_QName (Handler, Get_Value (Atts, J));
            end if;
         end if;
      end loop;

      if Ext.Base = No_Qualified_Name then
         Validation_Error
           (Handler, "#Attribute ""base"" required for <extensionType>");
      end if;

      if Ext.Base = Info.Descr.Name then
--           if In_Redefine_Context (Handler.all) then
--              Base := Handler.Contexts.Redefined_Type;
--           else
            Validation_Error
              (Handler, "#Self-referencing extension not allowed");
--           end if;
      end if;

      Details := new Type_Details'
        (Kind       => Type_Extension,
         Min_Occurs => 1,
         Max_Occurs => 1,
         Next       => null,
         Extension  => Ext);
      Insert_In_Type (Handler, Details);
      Push_Context
        (Handler,
         (Typ       => Context_Extension,
          Extension => Details));
   end Create_Extension;

   -----------------
   -- Create_List --
   -----------------

   procedure Create_List
     (Handler : access Schema_Reader'Class;
      Atts    : Sax_Attribute_List)
   is
      Item_Type_Index : constant Integer :=
        Get_Index (Atts, Empty_String, Handler.Item_Type);
      Name  : Qualified_Name := No_Qualified_Name;
   begin
      if Item_Type_Index /= -1 then
         Name := Resolve_QName (Handler, Get_Value (Atts, Item_Type_Index));
      end if;

      Push_Context
        (Handler,
         (Typ             => Context_List,
          List => (Kind            => Simple_Type_List,
                   List_Local_Type => No_Type_Index,
                   List_Items      => Name)));
   end Create_List;

   -----------------
   -- Finish_List --
   -----------------

   procedure Finish_List (Handler : access Schema_Reader'Class) is
      Ctx : constant Context_Access :=
        Handler.Contexts (Handler.Contexts_Last)'Access;
      Next : constant Context_Access :=
        Handler.Contexts (Handler.Contexts_Last - 1)'Access;
      Next_Next : constant Context_Access :=
        Handler.Contexts (Handler.Contexts_Last - 2)'Access;
   begin
      case Next.Typ is
         when Context_Type_Def =>
            if Next.Type_Info = No_Type_Index then
               --  within a <simpleType><restriction><simpleType><list>
               pragma Assert (Next_Next.Typ = Context_Simple_Restriction);
               Next_Next.Simple := Ctx.List;

            else
               --  within a <simpleType><list>
               Handler.Shared.Types.Table (Next.Type_Info).Simple := Ctx.List;
            end if;
         when others =>
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

      Ctx : constant Context_Access :=
        Handler.Contexts (Handler.Contexts_Last)'Access;

   begin
      case Ctx.Typ is
         when Context_Type_Def =>
            pragma Assert
              (Handler.Shared.Types.Table (Ctx.Type_Info).Details = null);
            Handler.Shared.Types.Table (Ctx.Type_Info).Details := Element;

         when Context_Sequence =>
            Append (Ctx.Seq.First_In_Seq, Element);

         when Context_Choice =>
            Append (Ctx.Choice.First_In_Choice, Element);

         when Context_All =>
            Append (Ctx.All_Detail.First_In_All, Element);

         when Context_Group =>
            pragma Assert (Ctx.Group.Details = null);
            Ctx.Group.Details := Element;

         when Context_Extension =>
            pragma Assert (Ctx.Extension.Extension.Details = null);
            Ctx.Extension.Extension.Details := Element;

         when Context_Restriction =>
            pragma Assert (Ctx.Restriction.Restriction.Details = null);
            Ctx.Restriction.Restriction.Details := Element;

         when Context_Simple_Restriction =>
            null;

         when Context_Schema | Context_Attribute | Context_Element
            | Context_Union
            | Context_List | Context_Redefine | Context_Attribute_Group =>
            null;  --  Should have raised exception when validating grammar
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
      Insert_In_Type (Handler, Choice);
      Push_Context
        (Handler,
         (Typ    => Context_Choice,
          Choice => Choice));
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
      Insert_In_Type (Handler, Seq);
      Push_Context
        (Handler,
         (Typ  => Context_Sequence,
          Seq  => Seq));
   end Create_Sequence;

   ----------------------
   -- Create_Attribute --
   ----------------------

   procedure Create_Attribute
     (Handler  : access Schema_Reader'Class;
      Atts     : Sax_Attribute_List)
   is
      Local : Symbol;
      Att   : Internal_Attribute_Descr;
      Ctx : constant Context_Access :=
        Handler.Contexts (Handler.Contexts_Last)'Access;
      Has_Form : Boolean := False;

   begin
      Att.Descr.Form := Handler.Attribute_Form_Default;
      Att.Loc := Get_Location (Handler.Locator);

      for J in 1 .. Get_Length (Atts) loop
         if Get_URI (Atts, J) = Empty_String then
            Local := Get_Local_Name (Atts, J);
            if Local = Handler.Name then
               Att.Descr.Name := (NS => Get_Namespace_URI (Handler.Target_NS),
                                  Local => Get_Value (Atts, J));
            elsif Local = Handler.Typ then
               Att.Typ  := Resolve_QName (Handler, Get_Value (Atts, J));
            elsif Local = Handler.S_Use then
               if Get_Value (Atts, J) = Handler.Required then
                  Att.Descr.Use_Type := Required;
               elsif Get_Value (Atts, J) = Handler.Prohibited then
                  Att.Descr.Use_Type := Prohibited;
               else
                  Att.Descr.Use_Type := Optional;
               end if;
            elsif Local = Handler.Fixed then
--            Normalize_Whitespace  --  Depending on the type of the attribute
--                   (Typ    => Typ,
--                    Reader => Handler,
--                    Atts   => Atts,
--                    Index  => Fixed_Index);
--              end if;
               Att.Descr.Fixed := Get_Value (Atts, J);
            elsif Local = Handler.Ref then
               Att.Ref := Resolve_QName (Handler, Get_Value (Atts, J));
            elsif Local = Handler.Form then
               Att.Descr.Form :=
                 Form_Type'Value (Get (Get_Value (Atts, J)).all);
               Has_Form := True;
            elsif Local = Handler.Default then
               Att.Descr.Default := Get_Value (Atts, J);
            elsif Local = Handler.Namespace_Target then
               Att.Descr.Target_NS := Get_Value (Atts, J);
            end if;
         end if;
      end loop;

      --  See section 3.2.3 for valid attributes combination

      if Att.Descr.Target_NS /= No_Symbol then
         if Att.Descr.Name /= No_Qualified_Name then
            Validation_Error
              (Handler,
               "#name must be specified when targetNamespace is specified");
         end if;

         if Has_Form then
            Validation_Error
              (Handler,
               "#Cannot specify ""form"" when targetNamespace is given");
         end if;

         Raise_Exception
           (XML_Not_Implemented'Identity,
            "targetNamespace not supported in attribute declaration");
      end if;

      if Has_Form and then Att.Ref /= No_Qualified_Name then
         Validation_Error
           (Handler,
            "#Attributes ""form"" and ""ref"" cannot be both specified");
      end if;

      if Att.Typ /= No_Qualified_Name then
         if Att.Ref /= No_Qualified_Name then
            Validation_Error
              (Handler,
               "#Attributes ""type"" and ""ref"" cannot be both specified");
         end if;

         if To_QName (Att.Typ) = "IDREF"
           or else To_QName (Att.Typ) = "IDREFS"
         then
            Raise_Exception
              (XML_Not_Implemented'Identity,
               "Unsupported type IDREF and IDREFS");
         end if;
      end if;

      if Att.Descr.Fixed /= No_Symbol
        and then Att.Descr.Default /= No_Symbol
      then
         Validation_Error
           (Handler,
            "#Attributes ""fixed"" and ""default"" cannot be both specified");
      end if;

      if Att.Descr.Default /= No_Symbol
        and then Att.Descr.Use_Type /= Optional
      then
         Validation_Error
           (Handler,
            "#Use must be ""optional"" when a default value is specified");
      end if;

      if Att.Descr.Fixed /= No_Symbol
        and then Att.Descr.Use_Type = Prohibited
      then
         Validation_Error
           (Handler,
            "#""prohibited"" is forbidden when"
            & " a fixed value is specified");
      end if;

      if Att.Descr.Name /= No_Qualified_Name then
         case Ctx.Typ is
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
      end if;

      Push_Context
        (Handler,
         (Typ       => Context_Attribute,
          Attribute => Att));
   end Create_Attribute;

   ----------------------
   -- Insert_Attribute --
   ----------------------

   procedure Insert_Attribute
     (Handler        : access Schema_Reader'Class;
      In_Context     : in out Context;
      Attribute      : Internal_Attribute_Descr) is
   begin
      case In_Context.Typ is
         when Context_Type_Def =>
            Append
              (Handler.Shared.Types.Table (In_Context.Type_Info).Attributes,
               (Kind     => Kind_Attribute,
                Attr     => Attribute));

         when Context_Schema | Context_Redefine =>
            null;

         when Context_Extension =>
            Append
              (In_Context.Extension.Extension.Attributes,
               (Kind     => Kind_Attribute,
                Attr     => Attribute));

         when Context_Restriction =>
            null;
--              Create_Restricted (Handler, In_Context);
--            Append (In_Context.Restricted, Attribute, Is_Local => Is_Local);
--              if Debug then
--             Output_Action ("Add_Attribute (" & Ada_Name (In_Context) & ", "
--                         & Attribute_Name & ");");
--              end if;

         when Context_Simple_Restriction =>
            null;

         when Context_Attribute_Group =>
            Append (In_Context.Attr_Group.Attributes,
                    (Kind => Kind_Attribute,
                     Attr => Attribute));

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
      Ctx : constant Context_Access :=
        Handler.Contexts (Handler.Contexts_Last)'Access;
      Next : constant Context_Access :=
        Handler.Contexts (Handler.Contexts_Last - 1)'Access;
   begin
      Insert_Attribute (Handler, Next.all, Ctx.Attribute);
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
         Local := Get_Local_Name (Atts, J);
         if Get_URI (Atts, J) = Empty_String then
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

         elsif Get_URI (Atts, J) = Handler.XML_Instance_URI then
            if Local = Handler.No_Namespace_Schema_Location then
               --  Already handled through Hook_Start_Element when validating
               --  the grammar itself, but needed if we do not validate the
               --  grammar
               Parse_Grammar
                 (Handler,
                  URI           => Empty_String,
                  Xsd_File      => Get_Value (Atts, J),
                  Do_Create_NFA => False);

            elsif Local = Handler.Schema_Location then
               --  Already handled through Hook_Start_Element when validating
               --  the grammar itself
               Parse_Grammars
                 (Handler, Get_Value (Atts, J), Do_Create_NFA => False);
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

      Push_Context (Handler, (Typ => Context_Schema));
   end Create_Schema;

   --------------------------------
   -- Process_Contents_From_Atts --
   --------------------------------

   function Process_Contents_From_Atts
     (Handler : access Schema_Reader'Class;
      Atts    : Sax_Attribute_List;
      Index   : Integer) return Process_Contents_Type is
   begin
      if Get_Value (Atts, Index) = Handler.Lax then
         return Process_Lax;
      elsif Get_Value (Atts, Index) = Handler.Strict then
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
      Details : Type_Details_Access;
      Any     : Any_Descr;
      Local   : Symbol;
      Min_Occurs, Max_Occurs : Integer := 1;

   begin
      Any.Target_NS := Get_Namespace_URI (Handler.Target_NS);
      Any.Namespace := Handler.Any_Namespace;

      for J in 1 .. Get_Length (Atts) loop
         if Get_URI (Atts, J) = Empty_String then
            Local := Get_Local_Name (Atts, J);
            if Local = Handler.Namespace then
               Any.Namespace := Get_Value (Atts, J);
            elsif Local = Handler.Process_Contents then
               Any.Process_Contents :=
                 Process_Contents_From_Atts (Handler, Atts, J);
            end if;
         end if;
      end loop;

      Get_Occurs (Handler, Atts, Min_Occurs, Max_Occurs);
      Details := new Type_Details'
        (Kind       => Type_Any,
         Min_Occurs => Min_Occurs,
         Max_Occurs => Max_Occurs,
         Next       => null,
         Any        => Any);
      Insert_In_Type (Handler, Details);
   end Create_Any;

   ----------------
   -- Create_All --
   ----------------

   procedure Create_All
     (Handler  : access Schema_Reader'Class;
      Atts     : Sax_Attribute_List)
   is
      Min_Occurs, Max_Occurs : Integer := 1;
      Details : Type_Details_Access;
   begin
      Get_Occurs (Handler, Atts, Min_Occurs, Max_Occurs);
      Details := new Type_Details'
        (Kind         => Type_All,
         Min_Occurs   => Min_Occurs,
         Max_Occurs   => Max_Occurs,
         Next         => null,
         First_In_All => null);
      Insert_In_Type (Handler, Details);
      Push_Context
        (Handler,
         (Typ        => Context_All,
          All_Detail => Details));
   end Create_All;

   -------------------
   -- Start_Element --
   -------------------

   overriding procedure Start_Element
     (Handler       : in out Schema_Reader;
      NS            : Sax.Utils.XML_NS;
      Local_Name    : Sax.Symbols.Symbol;
      Atts          : Sax.Readers.Sax_Attribute_List)
   is
      H   : constant Schema_Reader_Access := Handler'Unchecked_Access;
      Ctx : Context_Access;
--        Val : Integer;
   begin
      if Debug then
         Output_Seen ("Start " & Get (Local_Name).all
                      & " at "
                      & Sax.Locators.To_String (Handler.Locator));
      end if;

      --  Check the grammar
      Start_Element (Validating_Reader (Handler), NS, Local_Name, Atts);

      --  Process the element

      if Handler.Contexts = null then
         if Local_Name /= Handler.S_Schema then
            Validation_Error (H, "#Root element must be <schema>");
         end if;

         Create_Schema (H, Atts);

      elsif Local_Name = Handler.Annotation
        or else Local_Name = Handler.Notation
      then
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
         null;

--           Val := Get_Index (Atts, Empty_String, Handler.Value);
--           declare
--              Val2 : constant Cst_Byte_Sequence_Access :=
--                Get (Get_Non_Normalized_Value (Atts, Val));
--           begin
--              Ctx := Handler.Contexts (Handler.Contexts_Last)'Access;
--              Create_Restricted (H, Ctx);
--              Add_Facet (Ctx.Restricted, H, Local_Name, Val2.all);
--           end;

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
         null;
--
--           Ctx := Handler.Contexts (Handler.Contexts_Last)'Access;
--           case Ctx.Typ is
--              when Context_Restriction =>
--                 Create_Restricted (H, Ctx);
--                 Val := Get_Index (Atts, Empty_String, Handler.Value);
--                 Add_Facet
--                   (Ctx.Restricted,
--                    H, Local_Name,
--                  Trim (Get (Get_Value (Atts, Val)).all, Ada.Strings.Both));
--
--              when Context_Extension =>
--                 Validation_Error
--                   (H,
--                    "#Invalid restriction in an extension: """
--                    & Get (Local_Name).all & """");
--
--              when others =>
--                 Raise_Exception
--                   (XML_Not_Implemented'Identity,
--                    '"' & Get (Local_Name).all
--                  & """ not supported outside of restriction or extension");
--           end case;

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
         Ctx := Handler.Contexts (Handler.Contexts_Last)'Access;
         Handler.Shared.Types.Table (Ctx.Type_Info).Descr.Simple_Content :=
           True;

      elsif Local_Name = Handler.Complex_Content then
         Ctx := Handler.Contexts (Handler.Contexts_Last)'Access;
         Handler.Shared.Types.Table (Ctx.Type_Info).Descr.Simple_Content :=
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
      H       : constant Schema_Reader_Access := Handler'Unchecked_Access;
      Handled : Boolean := True;
   begin
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
         null;

      elsif Local_Name = Handler.Sequence then
         null;

      elsif Local_Name = Handler.Any_Attribute then
         Handled := False;

      elsif Local_Name = Handler.Choice then
         null;

      elsif Local_Name = Handler.Restriction then
         Finish_Restriction (H);

      elsif Local_Name = Handler.Extension then
         null;

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

      elsif Local_Name = Handler.Attribute_Group then
         Finish_Attribute_Group (H);

      elsif Local_Name = Handler.Redefine then
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

      --  Release the context
      if Handled then
         Handler.Contexts_Last := Handler.Contexts_Last - 1;
      end if;
   end End_Element;

   ----------------
   -- Characters --
   ----------------

   procedure Characters
     (Handler : in out Schema_Reader; Ch : Unicode.CES.Byte_Sequence)
   is
   begin
      Characters (Validating_Reader (Handler), Ch);
   end Characters;

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
            when Type_Empty | Type_Element | Type_Any => null;
            when Type_Sequence    => Free (Self.First_In_Seq);
            when Type_Choice      => Free (Self.First_In_Choice);
            when Type_All         => Free (Self.First_In_All);
            when Type_Group       => Free (Self.Group.Details);
            when Type_Extension   =>
               Unchecked_Free (Self.Extension.Attributes);
               Free (Self.Extension.Details);
            when Type_Restriction =>
               Unchecked_Free (Self.Restriction.Attributes);
               Free (Self.Restriction.Details);
         end case;

         Unchecked_Free (Self);
         Self := Next;
      end loop;
   end Free;

   -----------------
   -- Get_Locator --
   -----------------

   overriding function Get_Locator
     (Reader : Schema_Reader) return Sax.Locators.Locator is
   begin
      return Reader.Locator;
   end Get_Locator;

end Schema.Schema_Readers;
