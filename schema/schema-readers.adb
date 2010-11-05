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

   function Match
     (Self       : access NFA'Class;
      From_State : State;
      Trans      : Transition_Descr;
      Sym        : Transition_Event) return Boolean;
   --  Whether [Sym] matches [Trans]

   procedure Process is new Schema_State_Machines.Process (Match);

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
         Debug_Output ("Set_Grammar");
      end if;

      if Grammar /= No_Grammar then
         if Get (Get_Symbol_Table (Reader)) = null then
            if Debug then
               Debug_Output ("Set reader's symbol table from grammar");
            end if;

            Set_Symbol_Table (Reader, Get_Symbol_Table (Grammar));

         elsif Get_Symbol_Table (Grammar) =
           Symbol_Table_Pointers.Null_Pointer
         then
            if Debug then
               Debug_Output ("Set grammar's symbol table from reader");
            end if;
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
         return Find_Symbol
           (Handler,
            Dir_Name
              (Get (Handler.Current_Location.System_Id).all) & U.all);
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
      NFA  : constant Schema_NFA_Access := Get_NFA (Handler.Grammar);
      Iter : Active_State_Iterator;
      S    : State;
      Descr : access Type_Descr;
      Fixed : Symbol := No_Symbol;
      Data  : State_Data;
      Ty    : Type_Index;

   begin
      if Debug then
         Debug_Print
           (Handler.Matcher, Dump_Compact, "Validate_Current_Char: ");
      end if;

      --  Check all active states to find our whitespace normalization rules,
      --  and whether elements have fixed values.

      Iter := For_Each_Active_State (Handler.Matcher,
                                     Ignore_If_Nested  => True,
                                     Ignore_If_Default => True);
      loop
         S := Current (Handler.Matcher, Iter);
         exit when S = No_State;

         Data := Current_Data (Handler.Matcher, Iter);

         if Fixed = No_Symbol then
            Fixed := Data.Fixed;
         end if;

         --  Unless we have a <any> type
         if Data.Simple /= No_Type_Index then
            Descr := Get_Type_Descr (NFA, Data.Simple);
            if Descr.Simple_Content /= No_Simple_Type_Index then
               Whitespace := Get_Simple_Type
                 (Get_NFA (Handler.Grammar), Descr.Simple_Content)
                 .Whitespace;
            end if;
         end if;

         Next (Handler.Matcher, Iter);
      end loop;

      Is_Empty := Handler.Characters_Count = 0;

      --  in 3.3.1: if the element is empty, the "fixed" value
      --  should be used for it, just as for "default"
      --     Characters (Handler.all, Get (Get_Fixed (Handler)).all);

      if Is_Empty and then Fixed /= No_Symbol then
         Internal_Characters (Handler, Get (Fixed).all);
         Is_Empty := Handler.Characters_Count = 0;
         if Debug then
            Debug_Output
              ("Substitute fixed value for empty characters:"
               & Get (Fixed).all);
         end if;
      end if;

      if not Is_Empty then
         if Debug then
            Debug_Output ("Normalize whitespace: " & Whitespace'Img);
         end if;

         Normalize_Whitespace
           (Whitespace, Handler.Characters.all, Handler.Characters_Count);
      end if;

      if Fixed /= No_Symbol
        and then Get (Fixed).all /=
        Handler.Characters (1 .. Handler.Characters_Count)
      then
         Validation_Error
           (Handler,
            "Invalid character content (fixed to """
            & Get (Fixed).all & """)");
      end if;

      Iter := For_Each_Active_State (Handler.Matcher,
                                     Ignore_If_Nested => True,
                                     Ignore_If_Default => True);
      loop
         S := Current (Handler.Matcher, Iter);
         exit when S = No_State;

         Ty := Current_Data (Handler.Matcher, Iter).Simple;

         if Ty /= No_Type_Index then
            Descr := Get_Type_Descr (NFA, Ty);

            if Descr.Simple_Content /= No_Simple_Type_Index then
               if Debug and not Is_Empty then
                  Debug_Output
                    ("Validate characters ("
                     & To_QName (Descr.Name) & "): "
                     & Handler.Characters (1 .. Handler.Characters_Count)
                     & "--");
               end if;

               if Handler.Characters_Count = 0 then
                  Validate_Simple_Type
                    (Handler, Descr.Simple_Content,
                     "",
                     Empty_Element => Is_Empty,
                     Loc           => Loc);
               else
                  Validate_Simple_Type
                    (Handler, Descr.Simple_Content,
                     Handler.Characters (1 .. Handler.Characters_Count),
                     Empty_Element => Is_Empty,
                     Loc           => Loc);
               end if;

            elsif not Descr.Mixed
              and then not Is_Empty
            then
               if Debug then
                  Debug_Output ("No character data for "
                                & To_QName (Descr.Name) & S'Img);
                  Debug_Output
                    ("Got "
                     & Handler.Characters
                       (1 .. Integer'Min (20, Handler.Characters_Count))
                     & "--");
               end if;
               Validation_Error
                 (Handler,
                  "No character data allowed by content model",
                  Loc);
            end if;
         end if;

            --  If no explicit character data: we might need to simulate some,
            --  so that we can check facets like "minLength".

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

         Next (Handler.Matcher, Iter);
      end loop;

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
      No_Index       : constant Integer := Get_Index
        (Atts, H.XML_Instance_URI, H.No_Namespace_Schema_Location);
      Location_Index : constant Integer := Get_Index
        (Atts, H.XML_Instance_URI, H.Schema_Location);
      NFA            : constant Schema_NFA_Access := Get_NFA (H.Grammar);

      procedure Compute_Type_From_Attribute;
      --  If xsi:type was specified, verify that the given type is a valid
      --  substitution for the original type in the NFA, and replace the
      --  current nested automaton with the one for the type. The replacement
      --  does not affect the NFA itself, but the NFA_Matcher, so is only
      --  temporary and does not affect over running matchers.

      ---------------------------------
      -- Compute_Type_From_Attribute --
      ---------------------------------

      procedure Compute_Type_From_Attribute is
         Xsi_Type_Index     : constant Integer := Get_Index
           (Atts, H.XML_Instance_URI, H.Typ);
         TRef : Global_Reference;
         Data : State_Data;
         Prev_Data : State_Data := No_State_Data;
      begin
         if Xsi_Type_Index /= -1 then
            declare
               Qname : constant Byte_Sequence :=
                 Ada.Strings.Fixed.Trim
                   (Get (Get_Value (Atts, Xsi_Type_Index)).all,
                    Ada.Strings.Both);
               Separator : constant Integer := Split_Qname (Qname);
               Prefix    : Symbol;
               NS        : XML_NS;
               Typ       : Qualified_Name;
               Iter      : Active_State_Iterator;
               S, Prev_S : State := No_State;
               Nested_Start : State;
            begin
               Prefix := Find_Symbol
                 (H.all, Qname (Qname'First .. Separator - 1));
               Get_Namespace_From_Prefix (H.all, Prefix, NS);

               Typ := (NS    => Get_URI (NS),
                       Local => Find_Symbol
                         (H.all, Qname (Separator + 1 .. Qname'Last)));

               if Debug then
                  Debug_Output
                    ("Getting element definition from type attribute: "
                     & To_QName (Typ));
               end if;

               TRef := Reference_HTables.Get
                 (Get_References (H.Grammar).all, (Typ, Ref_Type));

               if TRef = No_Global_Reference then
                  Validation_Error (H, "Unknown type " & To_QName (Typ));
               end if;

               Nested_Start := Get_Type_Descr (NFA, TRef.Typ).Complex_Content;

               Iter := For_Each_Active_State
                 (H.Matcher,
                  Ignore_If_Default => True, Ignore_If_Nested => False);
               loop
                  S := Current (H.Matcher, Iter);
                  exit when S = No_State;

                  if Has_Active_Nested (H.Matcher, Iter) then
                     --  An state with a nested NFA indicates an element (the
                     --  nested is its type). From the element, we might get a
                     --  special list of blocked restrictions or extensions),
                     --  but we do not want to check anything else
                     Prev_Data := Current_Data (H.Matcher, Iter);
                     Prev_S    := S;

                  else
                     Data := Current_Data (H.Matcher, Iter);

                     if Prev_S /= No_State
                       and then Get_Start_State (NFA.Get_Nested (Prev_S)) = S
                     then
                        Check_Substitution_Group_OK
                          (H, TRef.Typ, Data.Simple,
                           Loc           => H.Current_Location,
                           Element_Block => Prev_Data.Block);
                     else
                        Check_Substitution_Group_OK
                          (H, TRef.Typ, Data.Simple,
                           Loc           => H.Current_Location,
                           Element_Block => Data.Block);
                     end if;

                     if Nested_Start /= No_State then
                        if Debug then
                           Debug_Output
                             ("Because of xsi:type, replace state" & S'Img
                              & " with" & Nested_Start'Img);
                        end if;
                        Replace_State (H.Matcher, Iter, Nested_Start);
                        --  Override (temporarily) the current state
                     else
                        if Debug then
                           Debug_Output ("Override state data" & S'Img
                                         & " to type" & TRef.Typ'Img);
                        end if;
                        Override_Data
                          (H.Matcher, Iter,
                           State_Data'
                             (Simple => TRef.Typ,
                              Fixed  => Current_Data (H.Matcher, Iter).Fixed,
                              Block  => Current_Data (H.Matcher, Iter).Block));
                     end if;
                  end if;

                  Next (H.Matcher, Iter);
               end loop;

               if Debug then
                  Debug_Print (H.Matcher, Dump_Compact, "After substitution:");
               end if;
            end;
         end if;
      end Compute_Type_From_Attribute;

      Success : Boolean;
      Iter    : Active_State_Iterator;
      Is_Nil  : Boolean;
      S       : State;
      Ty      : Type_Index;
   begin
      if Debug then
         Output_Seen ("Start_Element: " & To_QName (Elem)
                      & " " & To_String (H.Current_Location));
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

      --  Create the NFA matcher now if not done yet. This has to be done after
      --  we have seen the toplevel element, which might result in parsing
      --  additional grammars, and finding the target NS

      if H.Matcher = No_NFA_Matcher then
         if Debug then
            Debug_Output ("Creating NFA matcher");
         end if;

         H.Matcher := Get_NFA (H.Grammar).Start_Match
           (Start_At => Start_State);
      end if;

      Process
        (H.Matcher,
         Input   => (Closing => False,
                     Name    => (NS    => Get_URI (Get_NS (Elem)),
                                 Local => Get_Local_Name (Elem))),
         Success => Success);

      if Debug then
         Debug_Print (H.Matcher, Dump_Compact, "After: ");
      end if;

      if not Success then
         Validation_Error
           (H, "Unexpected element """
            & To_QName (Elem) & """: expecting """
            & Expected (H.Matcher) & '"');
      end if;

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

      Compute_Type_From_Attribute;

      --  Validate the attributes

      Iter := For_Each_Active_State
        (H.Matcher, Ignore_If_Nested => True, Ignore_If_Default => True);
      loop
         S := Current (H.Matcher, Iter);
         exit when S = No_State;

         --  The list of valid attributes is attached to the type, that is to
         --  the nested NFA.

         if Debug then
            Debug_Output ("Checking attributes for state" & S'Img);
         end if;

         Ty := NFA.Get_Data (S).Simple;
         if Ty /= No_Type_Index then --  otherwise with have a <any> type
            Validate_Attributes
              (Get_NFA (H.Grammar),
               Get_Type_Descr (NFA, Ty),
               H, Atts,
               Nillable  => False,  --  Is_Nillable (Element),
               Is_Nil    => Is_Nil);
         end if;

         Next (H.Matcher, Iter);
      end loop;

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
           ("End_Element: "
            & To_QName (Elem) & " " & To_String (H.Current_Location));
      end if;

      Validate_Current_Characters (H, Loc => Start_Tag_End_Location (Elem));

      Process
        (H.Matcher,
         (Closing => True,
          Name    => (NS    => Get_URI (Get_NS (Elem)),
                      Local => Get_Local_Name (Elem))),
         Success);

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
         Handler.Characters_Count := Ch'Length;
         Handler.Characters := new String (1 .. Ch'Length);
         Handler.Characters.all := Ch;

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
      H     : constant Validating_Reader_Access :=
        Validating_Reader_Access (Handler);
      NFA   : constant Schema_NFA_Access := Get_NFA (H.Grammar);
      S     : State;
      Descr : access Type_Descr;
      Iter  : Active_State_Iterator := For_Each_Active_State
        (H.Matcher, Ignore_If_Nested => True, Ignore_If_Default => True);

   begin
      loop
         S := Current (H.Matcher, Iter);
         exit when S = No_State;

         Descr := Get_Type_Descr (NFA, Current_Data (H.Matcher, Iter).Simple);
         if Descr.Simple_Content /= No_Simple_Type_Index
           or else Descr.Mixed
         then
            Internal_Characters (H, Ch);
            return;
         end if;

         Next (H.Matcher, Iter);
      end loop;
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
                    Whitespace    => Hook_Ignorable_Whitespace'Access);

         Parser.Matcher := No_NFA_Matcher;
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

   -----------
   -- Match --
   -----------

   function Match
     (Self       : access NFA'Class;
      From_State : State;
      Trans      : Transition_Descr;
      Sym        : Transition_Event) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      case Trans.Kind is
         when Transition_Close =>
            return Sym.Closing;

         when Transition_Symbol =>
            if Sym.Closing then
               return False;
            else
               if From_State = Start_State then
                  --  At toplevel, always qualified
                  return Trans.Name = Sym.Name;
               else
                  case Trans.Form is
                  when Unqualified =>
                     return (NS => Empty_String, Local => Trans.Name.Local) =
                       Sym.Name;
                  when Qualified =>
                     return Trans.Name = Sym.Name;
                  end case;
               end if;
            end if;

         when Transition_Any =>
            if Sym.Closing then
               return False;
            else
               return Match_Any (Trans.Any, Sym.Name);
            end if;
      end case;
   end Match;

end Schema.Readers;
