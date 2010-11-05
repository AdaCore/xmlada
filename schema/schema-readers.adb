-----------------------------------------------------------------------
--                XML/Ada - An XML suite for Ada95                   --
--                                                                   --
--                Copyright (C) 2003-2010, AdaCore                   --
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

with Unicode;           use Unicode;
with Unicode.CES;       use Unicode.CES;
with Sax.Exceptions;    use Sax.Exceptions;
with Sax.Locators;      use Sax.Locators;
with Sax.Utils;         use Sax.Utils;
with Sax.Readers;       use Sax.Readers;
with Sax.Symbols;       use Sax.Symbols;
with Schema.Simple_Types; use Schema.Simple_Types;
with Schema.Validators;   use Schema.Validators;
with Ada.Strings.Fixed;     use Ada.Strings.Fixed;
with Ada.Unchecked_Deallocation;
with Schema.Schema_Readers; use Schema.Schema_Readers;
with Schema.Validators.Lists; use Schema.Validators.Lists;

with GNAT.Directory_Operations; use GNAT.Directory_Operations;

package body Schema.Readers is
   use Schema_State_Machines, Schema_State_Machines_PP;

   procedure Internal_Characters
     (Handler : access Validating_Reader'Class;
      Ch      : Unicode.CES.Byte_Sequence);
   --  Store Ch in the current sequence of characters. This is needed to
   --  collapse multiple calls to Characters and Ignorable_Whitespace into a
   --  single string, for validation purposes.

   procedure Validate_Current_Characters
     (Handler : access Validating_Reader'Class;
      Loc     : Location);
   --  Validate the current set of characters

   procedure Reset (Parser : in out Validating_Reader);
   --  Reset the state of the parser so that we can parse other documents.
   --  This doesn't reset the grammar

   procedure Hook_Start_Element
     (Handler : access Sax_Reader'Class;
      Elem    : Element_Access;
      Atts    : in out Sax_Attribute_List);
   procedure Hook_End_Element
     (Handler : access Sax_Reader'Class;
      Elem    : Element_Access);
   procedure Hook_Characters
     (Handler : access Sax_Reader'Class;
      Ch      : Unicode.CES.Byte_Sequence);
   procedure Hook_Ignorable_Whitespace
     (Handler : access Sax_Reader'Class;
      Ch      : Unicode.CES.Byte_Sequence);
   procedure Hook_Set_Document_Locator
     (Handler : in out Sax_Reader'Class;
      Loc     : in out Sax.Locators.Locator);
   --  See for the corresponding primitive operations. These provide the
   --  necessary validation hooks.

   -----------------
   -- Set_Grammar --
   -----------------

   procedure Set_Grammar
     (Reader  : in out Validating_Reader;
      Grammar : Schema.Validators.XML_Grammar)
   is
      use Symbol_Table_Pointers;
   begin
      if Debug then
         Debug_Output
           ("Set_Grammar, should we set the parser's symbol table ?");
      end if;

      if Grammar /= No_Grammar then
         if Get (Get_Symbol_Table (Reader)) = null then
            if Debug then
               Debug_Output ("Set parser's symbol table");
            end if;

            Set_Symbol_Table (Reader, Get_Symbol_Table (Grammar));

         elsif Get_Symbol_Table (Grammar) =
           Symbol_Table_Pointers.Null_Pointer
         then
            Set_Symbol_Table (Grammar, Get_Symbol_Table (Reader));

         elsif Get_Symbol_Table (Reader) /= Get_Symbol_Table (Grammar) then
            raise XML_Fatal_Error with
              "The grammar and the reader must use the same symbol table";
         end if;
      end if;

      Reader.Grammar := Grammar;
   end Set_Grammar;

   ----------------------
   -- Set_Symbol_Table --
   ----------------------

   overriding procedure Set_Symbol_Table
     (Parser  : in out Validating_Reader;
      Symbols : Sax.Utils.Symbol_Table)
   is
      use Symbol_Table_Pointers;
   begin
      if Parser.Grammar /= No_Grammar
        and then Get_Symbol_Table (Parser.Grammar) /= Symbols
      then
         raise XML_Fatal_Error with
           "The grammar and the reader must use the same symbol table";
      end if;

      if Symbols /= Get_Symbol_Table (Parser) then
         Parser.Xmlns := No_Symbol;  --  Will force another lookup next time
         Set_Symbol_Table (Sax_Reader (Parser), Symbols);
      end if;
   end Set_Symbol_Table;

   -----------------
   -- Get_Grammar --
   -----------------

   function Get_Grammar
     (Reader  : Validating_Reader) return Schema.Validators.XML_Grammar is
   begin
      return Reader.Grammar;
   end Get_Grammar;

   ---------------------
   -- To_Absolute_URI --
   ---------------------

   function To_Absolute_URI
     (Handler : Validating_Reader;
      URI     : Symbol) return Symbol
   is
      U : constant Cst_Byte_Sequence_Access := Get (URI);
   begin
      if URI = Empty_String then
         return URI;
      elsif U (U'First) /= '/'
        and then U (U'First) /= '\'
      then
         Debug_Output ("MANU Normalizing "
                       & Get (Get_System_Id (Handler.Locator)).all
                       & "   " & U.all);
         return Find_Symbol
           (Handler,
            Dir_Name (Get (Get_System_Id (Handler.Locator)).all) & U.all);
      else
         return URI;
      end if;
   end To_Absolute_URI;

   --------------------
   -- Parse_Grammars --
   --------------------

   procedure Parse_Grammars
     (Handler         : access Validating_Reader'Class;
      Schema_Location : Symbol;
      Do_Create_NFA : Boolean)
   is
      URI : Symbol := No_Symbol;

      procedure Callback (Ch : Byte_Sequence);
      procedure Callback (Ch : Byte_Sequence) is
      begin
         if URI = No_Symbol then
            URI := Find_Symbol (Handler.all, Ch);
         else
            Parse_Grammar
              (Handler,
               URI           => URI,
               Xsd_File      => Find_Symbol (Handler.all, Ch),
               Do_Create_NFA => Do_Create_NFA);
            URI := No_Symbol;
         end if;
      end Callback;

      procedure For_Each is new For_Each_Item (Callback);

   begin
      For_Each (Get (Schema_Location).all);
   end Parse_Grammars;

   ---------------------------------
   -- Validate_Current_Characters --
   ---------------------------------

   procedure Validate_Current_Characters
     (Handler : access Validating_Reader'Class;
      Loc     : Location)
   is
      Is_Empty   : Boolean;
      Whitespace : Whitespace_Restriction := Preserve;

      procedure Callback (Self : access NFA'Class; S : State);
      procedure Find_Whitespace (Self : access NFA'Class; S : State);

      procedure Callback (Self : access NFA'Class; S : State) is
--        Typ, Typ_For_Mixed : XML_Type;
--        Val2               : Cst_Byte_Sequence_Access;
--        S1, S2             : Symbol;
         Data : constant State_Data_Access := Get_Data (Self, S);
      begin
         if Data.Descr.Simple_Content /= No_Simple_Type_Index then
            if Debug then
               Debug_Output
                 ("Validate characters ("
                  & To_QName (Data.Descr.Name) & "): "
                  & Handler.Characters (1 .. Handler.Characters_Count) & "--");
            end if;

            Validate_Simple_Type
              (Handler, Data.Descr.Simple_Content,
               Handler.Characters (1 .. Handler.Characters_Count),
               Empty_Element => Is_Empty,
               Loc           => Loc);

         elsif not Data.Descr.Mixed then
            if Debug then
               Debug_Output ("No character data for "
                             & To_QName (Data.Descr.Name) & S'Img);
               Debug_Output
                 ("Got "
                  & Handler.Characters
                    (1 .. Integer'Min (20, Handler.Characters_Count)) & "--");
            end if;
            Validation_Error
              (Handler,
               "No character data allowed by content model",
               Loc);
         end if;

         --  If no explicit character data: we might need to simulate some, so
         --  that we can check facets like "minLength".

--           if Is_Empty and then not Handler.Validators.Is_Nil then
--              if Handler.Validators.Element /= No_Element
--                and then Has_Default (Handler.Validators.Element)
--              then
--                 Val2 := Get (Get_Default (Handler.Validators.Element));
--                 Characters (Handler.all, Val2.all);
--              else
--                 Val2 := Get (Empty_String);
--              end if;
--           else
--           Val2 := Cst_Byte_Sequence_Access (Handler.Validators.Characters);
--           end if;

--           if Val2 /= null then
--              if Handler.Validators.Is_Nil then
--                 if not Is_Empty then
--                    Free (Handler.Validators.Characters);
--                    Validation_Error
--                      (Handler,
--                     "#Element has character data, but is declared as nil");
--                 end if;
--
--              elsif Has_Fixed (Handler) then
--                 if Is_Empty then
--                 --  If a xsi:type was specified, the fixed value must match
--                    --  it too
--
--                    if Handler.Validators.Typ /= No_Type then
--                       if Debug then
--                          Output_Seen
--                            ("characters: " & Get (Get_Fixed (Handler)).all);
--                       end if;
--
--                       Mask := (others => True);
--                       Validate_Characters
--                         (Get_Validator (Handler.Validators.Typ), Handler,
--                          Get (Get_Fixed (Handler)).all,
--                          Empty_Element => False,
--                          Mask          => Mask);
--                    end if;
--
--                    --  in 3.3.1: if the element is empty, the "fixed" value
--                    --  should be used for it, just as for "default"
--                    Characters (Handler.all, Get (Get_Fixed (Handler)).all);
--
--                 else
--                    Typ := Handler.Validators.Typ;
--                    if Typ = No_Type then
--                       Typ := Get_Type (Handler.Validators.Element);
--                    end if;
--
--                    if not Equal
--                      (Get_Validator (Typ), Handler,
--                       Val2.all, Get (Get_Fixed (Handler)).all)
--                    then
--                       Free (Handler.Validators.Characters);
--                       Validation_Error
--                         (Handler, "#Element's value must be """
--                          & Get (Get_Fixed (Handler.Validators.Element)).all
--                          & """");
--                    end if;
--                 end if;
--
--              else
--                 Typ := Handler.Validators.Typ;
--                 if Typ /= No_Type then
--                    Typ_For_Mixed := Typ;
--                 else
--                    Typ_For_Mixed := Get_Type (Handler.Validators.Element);
--                 end if;
--
--                 if not Is_Empty
--               and then not Get_Mixed_Content (Get_Validator (Typ_For_Mixed))
--                 then
--                    Validation_Error
--                      (Handler,
--                       "#No character data allowed by content model");
--                 end if;
--
--               --  If we had a <simpleType> we need to normalize whitespaces
--
--                 if Is_Simple_Type (Handler, Typ_For_Mixed) then
--                    --  ??? Not efficient
--                    S1 := Find_Symbol (Handler.all, Val2.all);
--                S2 := Do_Normalize_Whitespaces (Typ_For_Mixed, Handler, S1);
--
--                    --  Nothing to do if replacement was done in place
--                    if S1 /= S2 then
--                       Free (Handler.Validators.Characters);
--
--                       Val2 := Get (S2);
--                       Handler.Validators.Characters :=
--                         new Byte_Sequence'(Val2.all);
--                    end if;
--                 end if;
--
--                 if Typ /= No_Type then
--                    --  If the element specified a xsi:attribute, we need to
--                 --  validate with this type (which might be more restrictive
--                    --  than the parent type)
--
--                    if Debug then
--                       Output_Seen ("characters: " & Val2.all);
--                    end if;
--
--                    Mask := (others => True);
--                    Validate_Characters
--                      (Get_Validator (Typ), Handler,
--                       Val2.all,
--                       Empty_Element => Is_Empty,
--                       Mask          => Mask);
--                 end if;
--
--              --  We still need to check the "fixed" restriction of the base
--              --  type, so ask the base type. So whether there was a xsi:type
--              --  or not, we still need to validate the attributes with that
--                 --  type from XSD
--
--                 if Handler.Validators.Element /= No_Element then
--                    if Debug then
--                       Output_Seen ("characters: " & Val2.all);
--                    end if;
--
--                    Typ := Get_Type (Handler.Validators.Element);
--
--                    Mask := (others => True);
--                    Validate_Characters
--                      (Get_Validator (Typ), Handler,
--                       Val2.all,
--                       Empty_Element => Is_Empty,
--                       Mask          => Mask);
--                 end if;
--              end if;
--           end if;
      end Callback;

      procedure Find_Whitespace (Self : access NFA'Class; S : State) is
      begin
         if Self.Get_Data (S).Descr.Simple_Content /= No_Simple_Type_Index then
            Whitespace := Get_Simple_Type
              (Handler.Grammar,
               Self.Get_Data (S).Descr.Simple_Content).Whitespace;
         end if;
      end Find_Whitespace;

      procedure Find_Whitespace_Type
         is new For_Each_Active_State (Find_Whitespace);
      procedure For_Each_Active is new For_Each_Active_State (Callback);

   begin
      if Debug then
         Debug_Print
           (Handler.Matcher, Dump_Compact, "Validate_Current_Char: ");
      end if;

      --  If we were in the middle of a series of Characters callback, we need
      --  to process them now

      if Handler.Characters_Count = 0 then
         return;
      end if;

      Find_Whitespace_Type (Handler.Matcher,
                            Ignore_If_Nested => True,
                            Ignore_If_Default => True);
      Normalize_Whitespace
        (Whitespace, Handler.Characters.all, Handler.Characters_Count);

      Is_Empty := Handler.Characters_Count = 0;
      For_Each_Active (Handler.Matcher,
                       Ignore_If_Nested => True,
                       Ignore_If_Default => True);

      Handler.Characters_Count := 0;
   end Validate_Current_Characters;

   ------------------------
   -- Hook_Start_Element --
   ------------------------

   procedure Hook_Start_Element
     (Handler : access Sax_Reader'Class;
      Elem    : Element_Access;
      Atts    : in out Sax_Attribute_List)
   is
      H : constant Validating_Reader_Access :=
        Validating_Reader_Access (Handler);
      Type_Index     : constant Integer := Get_Index
        (Atts, H.XML_Instance_URI, H.Typ);
      No_Index       : constant Integer := Get_Index
        (Atts, H.XML_Instance_URI, H.No_Namespace_Schema_Location);
      Location_Index : constant Integer := Get_Index
        (Atts, H.XML_Instance_URI, H.Schema_Location);

--        Element       : XML_Element := No_Element;
      --  Data          : Validator_Data;
--        Typ           : XML_Type;
      Xsi_Type      : Type_Descr;
      pragma Unreferenced (Xsi_Type);
--      Parent_Type   : XML_Type;
--        Is_Nil        : Boolean;

      procedure Validate_Attributes_Cb
        (Self : access NFA'Class; S : State);
      --  Validate the attributes for state [S]. If they are not valid, [S]
      --  is marked as invalid.

      function Compute_Type_From_Attribute return Type_Descr;
      --  Compute the type to use, depending on whether the xsi:type attribute
      --  was specified

      ------------------
      -- Compute_Type --
      ------------------

      function Compute_Type_From_Attribute return Type_Descr is
--           G : XML_Grammar_NS;
--           Had_Restriction, Had_Extension : Boolean := False;
--           Valid : Boolean;
--           Typ : XML_Type := No_Type;
      begin
         if Type_Index /= -1 then
            declare
               --  We need to trim whitespaces, because Validate_Attributes,
               --  which does the validation, is only done after calling this
               --  procedure
               Qname : constant Byte_Sequence :=
                 Ada.Strings.Fixed.Trim
                   (Get (Get_Value (Atts, Type_Index)).all,
                    Ada.Strings.Both);
--                 Local_Name : Symbol;
               Separator : constant Integer := Split_Qname (Qname);
               NS        : XML_NS;
            begin
               if Debug then
                  Debug_Output
                    ("Getting element definition from type attribute: "
                     & Qname);
               end if;

               Get_Namespace_From_Prefix
                 (Validating_Reader (Handler.all),
                  Find_Symbol
                    (Handler.all, Qname (Qname'First .. Separator - 1)),
                  NS);
--                 Get_NS
--                 (Validating_Reader (Handler.all).Grammar, Get_URI (NS), G);
--                 Local_Name := Find_Symbol
--                   (Handler.all, Qname (Separator + 1 .. Qname'Last));

--                 Typ := Lookup (G, H, Local_Name, Create_If_Needed => False);
--
--                 if Typ = No_Type then
--                    Validation_Error
--                      (H,
--                       "#Unknown type """
--                       & Get (Get_Value (Atts, Type_Index)).all & '"');
--                 end if;

--                 if Element /= No_Element
--                   and then Get_Validator (Typ) /=
--                   Get_Validator (Get_Type (Element))
--                 then
--                    Check_Replacement_For_Type
--                      (Get_Validator (Typ), Element,
--                       Valid           => Valid,
--                       Had_Restriction => Had_Restriction,
--                       Had_Extension   => Had_Extension);
--
--                    if not Valid then
--                       Validation_Error
--                         (H, '#' & Qname & " is not a valid replacement for "
--                          & To_QName (Get_Type (Element)));
--                    end if;
--
--                    if Had_Restriction
--                      and then Get_Block (Element) (Block_Restriction)
--                    then
--                       Validation_Error
--                         (H, "#Element """ & To_QName (Element)
--                          & """ blocks the use of restrictions of the type");
--                    end if;
--
--                    if Had_Extension
--                      and then Get_Block (Element) (Block_Extension)
--                    then
--                       Validation_Error
--                         (H, "#Element """ & To_QName (Element)
--                          & """ blocks the use of extensions of the type");
--                    end if;
--                 end if;
            end;
         end if;
         return No_Type_Descr;
      end Compute_Type_From_Attribute;

      procedure Validate_Attributes_Cb
        (Self : access NFA'Class; S : State)
      is
         Is_Nil : Boolean;
         Data2 : State_Data_Access;
      begin
         --  The list of valid attributes is attached to the type, that is to
         --  the nested NFA.

         Data2 := Self.Get_Data (S);
         if Data2.all = Default_User_Data then
            return;  --  This is a dummy node, ignore
         end if;

         if Debug then
            Debug_Output ("Checking attributes for state" & S'Img);
         end if;

         Validate_Attributes
           (H.Grammar,
            Data2.Descr.Attributes, H, Atts,
            Nillable  => False,  --  Is_Nillable (Element),
            Is_Nil    => Is_Nil);
      end Validate_Attributes_Cb;

      procedure Validate_All_Attributes is new For_Each_Active_State
        (Validate_Attributes_Cb);

--        G : XML_Grammar_NS;
      Success : Boolean;
   begin
      if Debug then
         Output_Seen ("Start_Element: " & To_QName (Elem)
                      & " " & To_String (H.Locator));
      end if;

      --  We should get the location of the enclosing element

      Validate_Current_Characters (H, Loc => Start_Tag_Location (Elem));

      --  Get the name of the grammar to use from the element's attributes

      if No_Index /= -1 then
         Parse_Grammar
           (H,
            URI      => Empty_String,
            Xsd_File => Get_Value (Atts, No_Index),
            Do_Create_NFA => True);
      end if;

      if Location_Index /= -1 then
         Parse_Grammars
           (H, Get_Value (Atts, Location_Index), Do_Create_NFA => True);
      end if;

      if H.Grammar = No_Grammar then
         return;  --  Always valid, since we have no grammar anyway
      end if;

      Process
        (H.Matcher,
         Input   => (Kind => Transition_Symbol,
                     Name => (NS    => Get_URI (Get_NS (Elem)),
                              Local => Get_Local_Name (Elem))),
         Success => Success);

      if Debug then
         Debug_Print (H.Matcher, Dump_Compact, "After: ");
      end if;

      if not Success then
         Validation_Error
           (H, "#Unexpected element """
            & To_QName (Elem) & """: expecting """
            & Expected (H.Matcher) & '"');
      end if;

      Validate_All_Attributes (H.Matcher, Ignore_If_Nested => True);

      --  Whether this element is valid in the current context

--        if H.Validators /= null then
--           Parent_Type := H.Validators.Typ;
--           if Parent_Type = No_Type then
--              Parent_Type := Get_Type (H.Validators.Element);
--           end if;
--
--           if Has_Fixed (Handler) then
--              Validation_Error
--                (H, "#No child allowed because """
--                 & To_QName (H.Validators.Element)
--                 & """ has a fixed value");
--           end if;

--           Validate_Start_Element
--             (Get_Validator (Parent_Type), H, Get_Local_Name (Elem),
--              G, H.Validators.Data, Element);
--        else
--           Element := Lookup_Element (G, H, Get_Local_Name (Elem), False);
--        end if;

--        if Element = No_Element and then Type_Index = -1 then
--           if H.Validators /= null then
--              Validation_Error
--                (H, "#Unexpected element """ & To_QName (Elem) & """");
--           else
--              Validation_Error
--                (H, "#Element """ & To_QName (Elem)
--                 & """: No matching declaration available");
--           end if;
--        end if;

--        if Element /= No_Element and then Is_Abstract (Element) then
--           Validation_Error
--             (H, "#Element """ & To_QName (Elem) & """ is abstract");
--        end if;

      Xsi_Type := Compute_Type_From_Attribute;

--        if Xsi_Type = No_Type then
--           if Element = No_Element then
--              Validation_Error
--                (H, "#Type """
--                 & Get (Get_Value (Atts, Type_Index)).all
--                 & """: No matching declaration available");
--           else
--              Typ := Get_Type (Element);
--           end if;
--        else
--           Typ := Xsi_Type;
--        end if;
--
--        Data := Create_Validator_Data (Get_Validator (Typ));
--
--        Validate_Attributes
--          (Get_Validator (Typ), H, Atts,
--           Element /= No_Element and then Is_Nillable (Element),
--           Is_Nil);
--
--        if H.Validators /= null then
--           if H.Validators.Is_Nil then
--              Validation_Error
--                (H,
--                 "#Element is set as nil,"
--                 & " and doesn't accept any child element");
--           end if;
--        else
--        --  For root element, we need to check nillable here, otherwise this
--           --  has been done in Validate_Attributes
--
--           declare
--              Nil_Index : constant Integer :=
--                Get_Index (Atts,
--                           URI        => H.XML_Instance_URI,
--                           Local_Name => H.Nil);
--           begin
--              if Nil_Index /= -1 then
--                 if not Is_Nillable (Element) then
--                    Validation_Error
--                      (H, "#Element """
--                       & To_QName (Elem) & """ cannot be nil");
--                 end if;
--
--                 Is_Nil := Get_Value_As_Boolean (Atts, Nil_Index);
--              else
--                 Is_Nil := False;
--              end if;
--           end;
--        end if;
--
--        if Is_Nil
--          and then Element /= No_Element
--          and then Has_Fixed (Element)
--        then
--           Validation_Error
--             (H, "#Element cannot be nilled because"
--              & " a fixed value is defined for it");
--        end if;
--
--        Push (H.Validators, Element,
--              Xsi_Type, G, Data, Is_Nil);
   end Hook_Start_Element;

   ----------------------
   -- Hook_End_Element --
   ----------------------

   procedure Hook_End_Element
     (Handler : access Sax_Reader'Class;
      Elem    : Element_Access)
   is
      H : constant Validating_Reader_Access :=
        Validating_Reader_Access (Handler);
      Success : Boolean;
   begin
      if Debug then
         Output_Seen
           ("End_Element: " & To_QName (Elem) & " " & To_String (H.Locator));
      end if;

      Validate_Current_Characters (H, Loc => Start_Tag_End_Location (Elem));

      Process (H.Matcher, (Kind => Transition_Close), Success);
      if Debug then
         Debug_Print (H.Matcher, Dump_Compact, "After end element:");
      end if;

      if not Success then
         Validation_Error
           (H,
            "Unexpected end of sequence, expecting """
            & Expected (H.Matcher) & '"');
      end if;
   end Hook_End_Element;

   -------------------------
   -- Internal_Characters --
   -------------------------

   procedure Internal_Characters
     (Handler : access Validating_Reader'Class;
      Ch      : Unicode.CES.Byte_Sequence)
   is
      Tmp : Byte_Sequence_Access;
      Max : constant Natural := Handler.Characters_Count + Ch'Length;
   begin
      --  Preserve the characters, but avoid allocating every time. We
      --  therefore reuse the buffer as much as possible, and only extend it
      --  when needed.

      if Handler.Characters = null then
         Handler.Characters := new String'(Ch);
         Handler.Characters_Count := Ch'Length;

      elsif Max <= Handler.Characters'Last then
         Handler.Characters (Handler.Characters_Count + 1 .. Max) := Ch;
         Handler.Characters_Count := Max;

      else
         Tmp := new String (1 .. Max);
         Tmp (1 .. Handler.Characters_Count) :=
           Handler.Characters (1 .. Handler.Characters_Count);
         Tmp (Handler.Characters_Count + 1 .. Max) := Ch;
         Handler.Characters_Count := Max;
         Free (Handler.Characters);
         Handler.Characters := Tmp;
      end if;
   end Internal_Characters;

   ---------------------
   -- Hook_Characters --
   ---------------------

   procedure Hook_Characters
     (Handler : access Sax_Reader'Class;
      Ch      : Unicode.CES.Byte_Sequence) is
   begin
      Internal_Characters (Validating_Reader_Access (Handler), Ch);
   end Hook_Characters;

   -------------------------------
   -- Hook_Ignorable_Whitespace --
   -------------------------------

   procedure Hook_Ignorable_Whitespace
     (Handler : access Sax_Reader'Class;
      Ch      : Unicode.CES.Byte_Sequence)
   is
      H : constant Validating_Reader_Access :=
        Validating_Reader_Access (Handler);

      Store : Boolean := False;

      procedure Callback (Self : access NFA'Class; S : State);
      procedure Callback (Self : access NFA'Class; S : State) is
      begin
         if Self.Get_Data (S).Descr.Simple_Content /= No_Simple_Type_Index then
            Store := True;
         elsif Self.Get_Data (S).Descr.Mixed then
            Store := True;
         end if;
      end Callback;

      procedure For_Each_Active is new For_Each_Active_State (Callback);

   begin
      For_Each_Active (H.Matcher,
                       Ignore_If_Nested => True,
                       Ignore_If_Default => True);

      if Store then
         Internal_Characters (H, Ch);
      end if;
   end Hook_Ignorable_Whitespace;

   -----------
   -- Reset --
   -----------

   procedure Reset (Parser : in out Validating_Reader) is
   begin
      --  Save current location, for retrieval by Get_Error_Message
      Free (Parser.Id_Table);
      Free (Parser.Matcher);
      Free (Parser.Characters);
      Parser.Characters_Count := 0;
   end Reset;

   -----------
   -- Parse --
   -----------

   overriding procedure Parse
     (Parser : in out Validating_Reader;
      Input  : in out Input_Sources.Input_Source'Class) is
   begin
      if Debug then
         Output_Action
           ("Parsing XML file " & Input_Sources.Get_System_Id (Input));
      end if;

      Initialize_Symbols (Parser);
      Initialize_Grammar (Parser);

      if Get_Feature (Parser, Schema_Validation_Feature) then
         Set_Hooks (Parser,
                    Start_Element => Hook_Start_Element'Access,
                    End_Element   => Hook_End_Element'Access,
                    Characters    => Hook_Characters'Access,
                    Whitespace    => Hook_Ignorable_Whitespace'Access,
                    Doc_Locator   => Hook_Set_Document_Locator'Access);

         Parser.Matcher := Get_NFA (Parser.Grammar).Start_Match;
      else
         Set_Hooks (Parser,
                    Start_Element => null,
                    End_Element   => null,
                    Characters    => null,
                    Whitespace    => null,
                    Doc_Locator   => null);
      end if;

      --  Not a dispatching call
      Parse (Schema.Validators.Abstract_Validation_Reader (Parser), Input);

      if not In_Final (Parser.Matcher) then
         Validation_Error
           (Parser'Access,
            "Unexpected end of file: expecting "
            & Expected (Parser.Matcher));
      end if;

      Reset (Parser);

   exception
      when others =>
         Reset (Parser);
         raise;
   end Parse;

   ----------------------
   -- Validation_Error --
   ----------------------

   procedure Validation_Error
     (Reader : in out Validating_Reader;
      Except : Sax.Exceptions.Sax_Parse_Exception'Class)
   is
      pragma Unreferenced (Reader, Except);
   begin
      null;
   end Validation_Error;

   -----------------
   -- Get_Locator --
   -----------------

   function Get_Locator
     (Reader : Validating_Reader) return Sax.Locators.Locator is
   begin
      return Reader.Locator;
   end Get_Locator;

   -------------------------------
   -- Hook_Set_Document_Locator --
   -------------------------------

   procedure Hook_Set_Document_Locator
     (Handler : in out Sax_Reader'Class;
      Loc     : in out Sax.Locators.Locator) is
   begin
      Set_Locator (Validating_Reader (Handler), Loc);
   end Hook_Set_Document_Locator;

   -------------------------------
   -- Get_Namespace_From_Prefix --
   -------------------------------

   procedure Get_Namespace_From_Prefix
     (Handler  : in out Validating_Reader;
      Prefix   : Symbol;
      NS       : out Sax.Utils.XML_NS) is
   begin
      Find_NS
        (Parser  => Handler,
         Prefix  => Prefix,
         NS      => NS);
      if Get_URI (NS) = Empty_String then
         NS := No_XML_NS;
      end if;
   end Get_Namespace_From_Prefix;

   ----------
   -- Free --
   ----------

   procedure Free (Reader : in out Validating_Reader_Access) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Validating_Reader'Class, Validating_Reader_Access);
   begin
      if Reader /= null then
         Unchecked_Free (Reader);
      end if;
   end Free;

   -------------
   -- Locator --
   -------------

   function Locator (Parser : Validating_Reader) return Sax.Locators.Locator is
   begin
      return Parser.Locator;
   end Locator;

   -----------------
   -- Set_Locator --
   -----------------

   procedure Set_Locator
     (Parser : in out Validating_Reader; Loc : Sax.Locators.Locator) is
   begin
      Parser.Locator := Loc;
   end Set_Locator;

end Schema.Readers;
