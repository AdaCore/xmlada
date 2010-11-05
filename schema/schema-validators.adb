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

pragma Ada_05;

with Ada.Characters.Handling;        use Ada.Characters.Handling;
with Ada.Exceptions;                 use Ada.Exceptions;
with Ada.Unchecked_Deallocation;
with Interfaces;                     use Interfaces;
with Sax.Attributes;                 use Sax.Attributes;
with Sax.Encodings;                  use Sax.Encodings;
with Sax.Locators;                   use Sax.Locators;
with Sax.Readers;                    use Sax.Readers;
with Sax.Symbols;                    use Sax.Symbols;
with Sax.Utils;                      use Sax.Utils;
with Schema.Validators.XSD_Grammar;  use Schema.Validators.XSD_Grammar;
with Schema.Validators.Extensions;   use Schema.Validators.Extensions;
with Schema.Validators.Facets;       use Schema.Validators.Facets;
with Schema.Validators.Lists;        use Schema.Validators.Lists;
with Schema.Validators.Restrictions; use Schema.Validators.Restrictions;
with Schema.Validators.Simple_Types; use Schema.Validators.Simple_Types;
with System.Address_Image;
with Unicode.CES;                    use Unicode.CES;
with Unicode;                        use Unicode;

package body Schema.Validators is
   use XML_Grammars;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Grammar_NS_Array, Grammar_NS_Array_Access);

   function Create_NS_Grammar
     (Grammar       : XML_Grammar;
      Namespace_URI : Sax.Symbols.Symbol) return XML_Grammar_NS;
   --  Create a new namespace in the grammar

   function Debug_Name (Grammar : XML_Grammar_NS) return String;
   --  Debug output of Grammar

   function Extension_Of
     (G         : XML_Grammar_NS;
      Base      : XML_Type;
      Extension : XML_Validator := null) return XML_Validator
      renames Schema.Validators.Extensions.Create_Extension_Of;

   function Restriction_Of
     (G           : XML_Grammar_NS;
      Reader      : access Abstract_Validation_Reader'Class;
      Base        : XML_Type;
      Restriction : XML_Validator := null) return XML_Validator
      renames Schema.Validators.Restrictions.Create_Restriction_Of;

   function To_Graphic_String (Str : Byte_Sequence) return String;
   --  Convert non-graphic characters in Str to make them visible in a display

   ------------------------------
   -- Attribute_Validator_List --
   ------------------------------

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Attribute_Validator_List, Attribute_Validator_List_Access);

   procedure Free (List : in out Attribute_Validator_List_Access);
   --  Free the contents of List, including contained

   ----------------------
   -- Validation_Error --
   ----------------------

   procedure Validation_Error
     (Reader  : access Abstract_Validation_Reader;
      Message : Byte_Sequence;
      Loc     : Sax.Locators.Location := Sax.Locators.No_Location;
      Except  : Exception_Id := XML_Validation_Error'Identity) is
   begin
      if Debug then
         Debug_Output ("Validation_Error: " & Message);
      end if;

      Free (Reader.Error_Msg);
      Reader.Error_Msg := new Byte_Sequence'(Message);

      if Loc /= No_Location then
         Reader.Error_Location := Loc;
      else
         Reader.Error_Location := Get_Location
           (Get_Locator (Abstract_Validation_Reader'Class (Reader.all)));
      end if;

      Raise_Exception (Except);
   end Validation_Error;

   -----------------------
   -- Get_Error_Message --
   -----------------------

   function Get_Error_Message
     (Reader : Abstract_Validation_Reader) return Unicode.CES.Byte_Sequence
   is
      Loc : Location;
      First : Natural;
   begin
      if Reader.Error_Msg = null then
         return "";

      else
         Loc := Reader.Error_Location;
         if Loc = No_Location then
            Loc := Get_Location
              (Get_Locator (Abstract_Validation_Reader'Class (Reader)));
         end if;

         --  Backward compatibility: '#' used to indicate we want the location
         --  displayed, but that should no longer be necessary now.
         First := Reader.Error_Msg'First;
         if Reader.Error_Msg (First) = '#' then
            First := First + 1;
         end if;

         if Loc /= No_Location then
            return To_String (Loc, Use_Basename_In_Error_Messages (Reader))
              & ": " & Reader.Error_Msg (First .. Reader.Error_Msg'Last);
         else
            return Reader.Error_Msg (First .. Reader.Error_Msg'Last);
         end if;
      end if;
   end Get_Error_Message;

   ----------
   -- Free --
   ----------

   procedure Free (List : in out Attribute_Validator_List_Access) is
   begin
      --  List (..).Attr is already freed through the htable in the grammar
      Unchecked_Free (List);
   end Free;

   ------------------------------
   -- Do_Normalize_Whitespaces --
   ------------------------------

   function Do_Normalize_Whitespaces
     (Typ     : XML_Type;
      Reader  : access Abstract_Validation_Reader'Class;
      Val     : Sax.Symbols.Symbol) return Sax.Symbols.Symbol
   is
      Whitespace : Whitespace_Restriction := Preserve;
      Facets     : constant Facets_Description :=
        Get_Facets (Typ.Validator, Reader);
      C          : Unicode_Char;
      Changed    : Boolean := False;
   begin
      if Facets /= null
        and then Facets.all in Common_Facets_Description'Class
      then
         Whitespace := Common_Facets_Description (Facets.all).Whitespace;
      end if;

      --  Normalize attribute value if necessary
      --  replace: All occurrences of #x9 (tab), #xA (line feed) and #xD
      --     (carriage return) are replaced with #x20 (space).
      --  collapse: Subsequent to the replacements specified above under
      --     replace, contiguous sequences of #x20s are collapsed to a single
      --     #x20, and initial and/or final #x20s are deleted.

      case Whitespace is
         when Preserve =>
            return Val;

         when Replace =>
            declare
               Val2  : Byte_Sequence := Get (Val).all;
               Idx   : Natural := Val2'First;
               First : Natural := Val2'Last + 1;
            begin
               while Idx <= Val2'Last loop
                  First := Idx;
                  Encoding.Read (Val2, Idx, C);

                  if Is_White_Space (C) then
                     --  Assumes all characters we replace are encoded as
                     --  single byte
                     Changed := Changed or C /= 32;
                     Val2 (First) := ' ';
                  end if;
               end loop;

               if Changed then
                  return Find_Symbol (Reader.all, Val2);
               else
                  return Val;
               end if;
            end;

         when Collapse =>
            if Val = Empty_String then
               return Val;  --  nothing to do
            end if;

            declare
               V       : Byte_Sequence := Get (Val).all;
               C       : Unicode_Char;
               Last    : Natural := V'Last + 1;
               Idx, Idx_Output : Natural := V'First;
               First   : Natural := V'Last + 1;
               Tmp     : Natural;
               Prev_Is_Whitespace : Boolean := False;
            begin
               --  Remove leading spaces.
               --  At the end of this loop, First points to the first non
               --  blank character

               loop
                  First := Idx;
                  Encoding.Read (V, Idx, C);
                  exit when not Is_White_Space (C);

                  if Idx > V'Last then
                     return Empty_String;
                  end if;
               end loop;

               Idx_Output := Idx;

               --  Iterate and replace all whitespaces. Mark the spot of the
               --  last whitespace so that we can ignore trailing spaces.
               --  At the same time, we can copy to Idx_Output, since the
               --  output string will always be at least as short as Val.

               while Idx <= V'Last loop
                  Tmp := Idx;
                  Encoding.Read (V, Idx, C);

                  if Is_White_Space (C) then
                     if not Prev_Is_Whitespace then
                        Changed := Changed or C /= 32;
                        Last := Idx_Output;
                        V (Idx_Output) := ' ';
                        Idx_Output := Idx_Output + 1;
                        Prev_Is_Whitespace := True;
                     else
                        Changed := True;
                     end if;
                  else
                     V (Idx_Output .. Idx_Output + Idx - Tmp - 1) :=
                       V (Tmp .. Idx - 1);
                     Idx_Output := Idx_Output + Idx - Tmp;
                     Last := Idx_Output;  --  after this char
                     Prev_Is_Whitespace := False;
                  end if;
               end loop;

               if Changed
                 or else First /= V'First
                 or else Last - 1 /= V'Last
               then
                  return Find_Symbol (Reader.all, V (First .. Last - 1));
               else
                  return Val;
               end if;
            end;
      end case;
   end Do_Normalize_Whitespaces;

   --------------------------
   -- Normalize_Whitespace --
   --------------------------

   procedure Normalize_Whitespace
     (Typ    : XML_Type;
      Reader : access Abstract_Validation_Reader'Class;
      Atts   : Sax.Readers.Sax_Attribute_List;
      Index  : Natural) is
   begin
      Set_Normalized_Value
        (Atts, Index,
         Do_Normalize_Whitespaces (Typ, Reader, Get_Value (Atts, Index)));
   end Normalize_Whitespace;

   --     --------------
   --     -- Is_Equal --
   --     --------------
   --
   --     function Is_Equal
   --       (Attribute : Named_Attribute_Validator_Record;
   --        Attr2     : Attribute_Validator_Record'Class)
   --       return Boolean is
   --     begin
   --        return Attr2 in Named_Attribute_Validator_Record'Class
   --          and then Attribute.NS = Attr2.NS
   --          and then Attribute.Local_Name =
   --            Named_Attribute_Validator_Record (Attr2).Local_Name;
   --     end Is_Equal;
   --
   --     function Is_Equal
   --       (Attribute : Any_Attribute_Validator;
   --        Attr2     : Attribute_Validator_Record'Class)
   --       return Boolean is
   --     begin
   --        return Attr2 in Any_Attribute_Validator'Class
   --          and then Attribute.NS = Attr2.NS
   --          and then Attribute.Kind = Any_Attribute_Validator (Attr2).Kind;
   --     end Is_Equal;

   -----------
   -- Is_ID --
   -----------

   function Is_ID (Attr : Attribute_Descr) return Boolean is
      pragma Unreferenced (Attr);
   begin
      return False;
      --  return Is_ID (Attr.Simple_Type);
   end Is_ID;

   -------------------
   -- Add_Attribute --
   -------------------

   procedure Add_Attribute
     (List      : in out Attribute_Validator_List_Access;
      Attribute : Attribute_Descr)
   is
      L : Attribute_Validator_List_Access;
   begin
      if List /= null then
         for A in List'Range loop
            if List (A) = Attribute then
               return;
            end if;
         end loop;

         L := new Attribute_Validator_List (List'First .. List'Last + 1);
         L (List'Range) := List.all;
         L (L'Last) := Attribute;
         Unchecked_Free (List);
         List := L;
      else
         List := new Attribute_Validator_List'(1 => Attribute);
      end if;
   end Add_Attribute;

   --------------------
   -- Add_Attributes --
   --------------------

   procedure Add_Attributes
     (List       : in out Attribute_Validator_List_Access;
      Attributes : Attribute_Validator_List_Access)
   is
   begin
      if Attributes /= null then
         for A in Attributes'Range loop
            Add_Attribute (List, Attributes (A));
         end loop;
      end if;
   end Add_Attributes;

   --------------
   -- Get_Name --
   --------------

   function Get_Name
     (Validator : access XML_Validator_Record'Class) return String is
   begin
      return Debug_Tag_Name (Validator'Tag);
   end Get_Name;

   -----------------------
   -- Set_Mixed_Content --
   -----------------------

   procedure Set_Mixed_Content
     (Validator : access XML_Validator_Record;
      Mixed     : Boolean) is
   begin
      Validator.Mixed_Content := Mixed;
   end Set_Mixed_Content;

   -----------------------
   -- Get_Mixed_Content --
   -----------------------

   function Get_Mixed_Content
     (Validator : access XML_Validator_Record) return Boolean is
   begin
      return Validator.Mixed_Content;
   end Get_Mixed_Content;

   -------------------------
   -- Validate_Attributes --
   -------------------------

   procedure Validate_Attributes
     (Attributes : Attribute_Validator_List_Access;
      Reader     : access Abstract_Validation_Reader'Class;
      Atts       : in out Sax.Readers.Sax_Attribute_List;
      Nillable   : Boolean;
      Is_Nil     : out Boolean)
   is
      Length   : constant Natural := Get_Length (Atts);

      function Attr_Length return Natural;
      function Attr_Length return Natural is
      begin
         if Attributes = null then
            return 0;
         else
            return Attributes'Length;
         end if;
      end Attr_Length;

      type Attr_Status is record
         Prohibited : Boolean := False;
         --  Prohibited explicitly, but it might be allowed through
         --  <anyAttribute>

         Seen  : Boolean := False;
      end record;
      Seen : array (1 .. Length) of Attr_Status := (others => (False, False));

      Visited : array (1 .. Attr_Length) of Boolean := (others => False);

      type Any_Status is (Any_None, Any_All, Any_Not_All);
      type Any_Status_Array is array (1 .. Length) of Any_Status;
      Seen_Any : constant Any_Status_Array := (others => Any_None);
      pragma Unreferenced (Any_Not_All);

      function Find_Attribute (Index : Integer) return Integer;
      --  Chech whether Named appears in Atts

      --        procedure Check_Any
      --          (List : Attribute_Descr; Must_Match_All_Any : Boolean);
      --  Check recursively the attributes provided by Validator.

      procedure Check_Named_Attribute (Index : Integer);
      --        procedure Check_Any_Attribute
      --          (Any : Any_Attribute_Validator; Index : Integer);
      --  Check a named attribute or a wildcard attribute

      procedure Check_Single_ID;
      --  If using XSD 1.0, check that there is a single ID attribute.
      --  This relies on the Sax.Attributes.Get_Type being set correctly.
      --  XSD 1.0 prevents having two such attributes, for easier conversion
      --  to DTD (see G.1.7 ID, IDREF, and related types)

      ---------------------
      -- Check_Single_ID --
      ---------------------

      procedure Check_Single_ID is
         Seen_ID : Boolean := False;
      begin
         for A in 1 .. Length loop
            if Get_Type (Atts, A) = Sax.Attributes.Id then
               if Seen_ID then
                  Validation_Error
                    (Reader,
                     "#Elements can have a single ID attribute in XSD 1.0");
               end if;

               Seen_ID := True;
            end if;
         end loop;
      end Check_Single_ID;

      ---------------------------
      -- Check_Named_Attribute --
      ---------------------------

      procedure Check_Named_Attribute (Index : Integer) is
         Found  : Integer;
      begin
         if not Visited (Index) then
            Visited (Index) := True;
            Found := Find_Attribute (Index);

            if Found = -1 then
               case Attributes (Index).Use_Type is
                  when Required =>
                     Validation_Error
                       (Reader, "#Attribute """
                        & To_QName (Attributes (Index).Name)
                        & """ is required in this context");
                  when Prohibited | Optional | Default =>
                     null;
               end case;

            else
               Seen (Found).Seen := True;

               case Attributes (Index).Form is
                  when Qualified =>
                     if Attributes (Index).Is_Local
                       and then Get_Prefix (Atts, Found) = Empty_String
                     then
                        Validation_Error
                          (Reader, "#Attribute " & Get_Qname (Atts, Found)
                           & " must have a namespace");
                     end if;

                  when Unqualified =>
                     if Attributes (Index).Is_Local
                       and then Get_Prefix (Atts, Found) /= Empty_String
                       and then Get_URI (Atts, Found) =
                       Get_Namespace_URI
                         (Get (Reader.Grammar).Target_NS)
                     then
                        Validation_Error
                          (Reader, "#Attribute " & Get_Qname (Atts, Found)
                           & " must not have a namespace");
                     end if;
               end case;

               case Attributes (Index).Use_Type is
                  when Prohibited =>
                     Seen (Found) := (Seen       => False,
                                      Prohibited => True);

                  when Optional | Required | Default =>
                     --  We do not need to check id here, since that is
                     --  automatically checked from Validate_Characters for the
                     --  attribute
                     --     Check_Id
                     --       (Id_Table, Get_Type (Named.all).Validator,
                     --        Get_Value (Atts, Found));

                     --                       Normalize_Whitespace
                     --           (Get_Type (Named.all), Reader, Atts, Found);

                     begin
                        Validate_Attribute
                          (Attributes (Index), Reader, Atts, Found);
                     exception
                        when XML_Validation_Error =>
                           --  Avoid duplicate locations in error messages
                           --  ??? This is just a hack for now

                           declare
                              Str : constant Byte_Sequence :=
                                Reader.Error_Msg.all;
                           begin
                              if Str (Str'First) = '#' then
                                 Free (Reader.Error_Msg);
                                 Reader.Error_Msg := new Byte_Sequence'
                                   ("#Attribute """ & Get_Qname (Atts, Found)
                                    & """: "
                                    & Str (Str'First + 1 .. Str'Last));
                              else
                                 Free (Reader.Error_Msg);
                                 Reader.Error_Msg := new Byte_Sequence'
                                   ("#Attribute """ & Get_Qname (Atts, Found)
                                    & """: " & Str);
                              end if;
                              raise;
                           end;
                     end;
               end case;
            end if;
         end if;
      end Check_Named_Attribute;

      -------------------------
      -- Check_Any_Attribute --
      -------------------------

      --        procedure Check_Any_Attribute
      --          (Any   : Any_Attribute_Validator;
      --           Index : Integer) is
      --        begin
      --           if Debug then
      --              Debug_Push_Prefix
      --                ("Checking any attribute index="
      --                 & Index'Img & " name="
      --                 & To_QName (Get_URI (Atts, Index),
      --                             Get_Local_Name (Atts, Index)));
      --           end if;
      --
      --           Validate_Attribute (Any, Reader, Atts, Index);
      --
      --           Debug_Pop_Prefix;
      --
      --        exception
      --           when XML_Validation_Error =>
      --              Debug_Pop_Prefix;
      --
      --              --  Avoid duplicate locations in error messages
      --              --  ??? This is just a hack for now
      --
      --              declare
      --                 Str : constant Byte_Sequence := Reader.Error_Msg.all;
      --              begin
      --                 if Str (Str'First) = '#' then
      --                    Free (Reader.Error_Msg);
      --                    Reader.Error_Msg := new Byte_Sequence'
      --                      ("#Attribute """ & Get_Qname (Atts, Index)
      --                       & """: " & Str (Str'First + 1 .. Str'Last));
      --                 else
      --                    Free (Reader.Error_Msg);
      --                    Reader.Error_Msg := new Byte_Sequence'
      --                      ("#Attribute """ & Get_Qname (Atts, Index)
      --                       & """: " & Str);
      --                 end if;
      --
      --                 raise;
      --              end;
      --        end Check_Any_Attribute;

      --------------------
      -- Find_Attribute --
      --------------------

      function Find_Attribute (Index : Integer) return Integer is
         Is_Local : constant Boolean := Attributes (Index).Is_Local;
      begin
         for A in 1 .. Length loop
            if not Seen (A).Seen
              and then Get_Local_Name (Atts, A) = Attributes (Index).Name.Local
              and then ((Is_Local
                         and Get_Prefix (Atts, A) = Empty_String)
                        or else Get_URI (Atts, A) = Attributes (Index).Name.NS)
            then
               if Debug then
                  Debug_Output
                    ("Found attribute: " & To_QName (Attributes (Index).Name));
               end if;
               return A;
            end if;
         end loop;
         return -1;
      end Find_Attribute;

      ---------------
      -- Check_Any --
      ---------------

      --        procedure Check_Any
      --          (List : Attribute_Descr; Must_Match_All_Any : Boolean) is
      --        begin
      --           if List.Validator.all in Any_Attribute_Validator'Class then
      --      --  From 3.4.2 (intersection of anyAttribute), an attribute must
      --    --  match *all* the anyAttribute, so we do not modify Seen yet, so
      --              --  that the attribute is tested multiple times
      --
      --              for A in 1 .. Length loop
      --                 if not Seen (A).Seen
      --                   and then (Must_Match_All_Any
      --                             or else Seen_Any (A) /= Any_All)
      --                 then
      --                    begin
      --                       Check_Any_Attribute
      --                   (Any_Attribute_Validator (List.Validator.all), A);
      --
      --            --  If there was an exception, don't mark the attribute as
      --            --  seen, it is invalid. Maybe another <anyAttribute> will
      --                       --  match
      --
      --                       case Seen_Any (A) is
      --                   when Any_None | Any_All => Seen_Any (A) := Any_All;
      --                          when Any_Not_All        => null;
      --                       end case;
      --
      --                    exception
      --                       when XML_Validation_Error =>
      --                          if Must_Match_All_Any then
      --                             Seen_Any (A) := Any_Not_All;
      --                          end if;
      --                    end;
      --                 end if;
      --              end loop;
      --           end if;
      --        end Check_Any;

   begin
      if Attributes /= null then
         for L in Attributes'Range loop
            Check_Named_Attribute (L);
         end loop;
      end if;

      --        if Attributes /= null and then not Ignore_Wildcard then
      --     --  If the policy for <anyAttribute> has changed, we restart from
      --      --  scratch: we need to ensure that within the current validator
      --    --  (and its dependencies), all <anyAttribute> matches for a given
      --   --  attribute (or not). This computation should not be influence by
      --           --  validators seen previously.
      --
      --           if Must_Match_All_Any2 /= Must_Match_All_Any then
      --              declare
      --               Saved_Seen_Any : constant Any_Status_Array := Seen_Any;
      --              begin
      --                 for S in Seen_Any'Range loop
      --                    if Seen_Any (S) = Any_Not_All then
      --                       Seen_Any (S) := Any_None;
      --                    end if;
      --                 end loop;
      --
      --                 for L in List'Range loop
      --                    Check_Any (List (L), Must_Match_All_Any2);
      --                 end loop;
      --
      --                 for S in Seen_Any'Range loop
      --                    if Seen_Any (S) /= Any_All then
      --                       Seen_Any (S) := Saved_Seen_Any (S);
      --                    end if;
      --                 end loop;
      --              end;
      --           else
      --              for L in List'Range loop
      --                 Check_Any (List (L), Must_Match_All_Any2);
      --              end loop;
      --           end if;
      --        end if;

      Is_Nil := False;

      for S in Seen'Range loop
         if not Seen (S).Seen and then Seen_Any (S) /= Any_All then
            if Get_URI (Atts, S) = Reader.XML_Instance_URI then
               if Get_Local_Name (Atts, S) = Reader.Nil then
                  if not Nillable then
                     Validation_Error (Reader, "#Element cannot be nil");
                  end if;

                  Is_Nil := Get_Value_As_Boolean (Atts, S);

                  --  Following attributes are always valid
                  --  See "Element Locally Valid (Complex Type)" 3.4.4.2
               elsif Get_Local_Name (Atts, S) = Reader.Typ
                 or else Get_Local_Name (Atts, S) = Reader.Schema_Location
                 or else Get_Local_Name (Atts, S) =
                 Reader.No_Namespace_Schema_Location
               then
                  null;

               else
                  Validation_Error
                    (Reader, "#Attribute """ & Get_Qname (Atts, S)
                     & """ invalid for this element");
               end if;

            elsif Seen (S).Prohibited then
               Validation_Error
                 (Reader, "#Attribute """ & Get_Qname (Atts, S)
                  & """ is prohibited in this context");

            else
               Validation_Error
                 (Reader, "#Attribute """ & Get_Qname (Atts, S)
                  & """ invalid for this element");
            end if;
         end if;
      end loop;

      Check_Single_ID;
   end Validate_Attributes;

   -----------
   -- Is_ID --
   -----------

   function Is_ID (Typ : XML_Type) return Boolean is
   begin
      return Typ.Validator /= null and then Is_ID (Typ.Validator.all);
   end Is_ID;

   -----------
   -- Is_ID --
   -----------

   function Is_ID (Validator : XML_Validator_Record) return Boolean is
      pragma Unreferenced (Validator);
   begin
      return False;
   end Is_ID;

   -------------------------
   -- Validate_Characters --
   -------------------------

   procedure Validate_Characters
     (Validator      : access XML_Validator_Record;
      Reader         : access Abstract_Validation_Reader'Class;
      Ch             : Unicode.CES.Byte_Sequence;
      Empty_Element  : Boolean;
      Mask           : in out Facets_Mask)
   is
      pragma Unreferenced (Ch, Empty_Element, Mask, Reader);
   begin
      if Debug then
         Debug_Output ("Validate_Chars (unknown) " & Get_Name (Validator));
      end if;
   end Validate_Characters;

   -----------
   -- Equal --
   -----------

   function Equal
     (Validator      : access XML_Validator_Record;
      Reader         : access Abstract_Validation_Reader'Class;
      Value1, Value2 : Unicode.CES.Byte_Sequence) return Boolean
   is
      pragma Unreferenced (Validator, Reader);
   begin
      return Value1 = Value2;
   end Equal;

   -------------
   -- List_Of --
   -------------

   --     function List_Of
   --       (Grammar : XML_Grammar_NS; Typ : XML_Type) return XML_Type is
   --     begin
   --        return Create_Local_Type (Grammar, List_Of (Typ));
   --     end List_Of;

   ---------------
   -- Add_Facet --
   ---------------

   procedure Add_Facet
     (Validator   : access XML_Validator_Record;
      Reader      : access Abstract_Validation_Reader'Class;
      Facet_Name  : Symbol;
      Facet_Value : Unicode.CES.Byte_Sequence)
   is
      pragma Unreferenced (Validator);
   begin
      if Facet_Name = Reader.Whitespace then
         if Facet_Value /= "collapse" then
            Validation_Error
              (Reader,
               "#Invalid value for restriction whiteSpace: " & Facet_Value);
         end if;
      else
         Validation_Error
           (Reader, "#Invalid restriction: " & Get (Facet_Name).all);
      end if;
   end Add_Facet;

   -----------------------
   -- To_Graphic_String --
   -----------------------

   function To_Graphic_String (Str : Byte_Sequence) return String is
      To_Hex : constant array (0 .. 15) of Character :=
        ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C',
        'D', 'E', 'F');
      Result : String (1 .. 4 * Str'Length);
      Index : Integer := Result'First;
   begin
      for S in Str'Range loop
         if Character'Pos (Str (S)) >= 32
           and then Character'Pos (Str (S)) <= 128
           and then Is_Graphic (Str (S))
         then
            Result (Index) := Str (S);
            Index := Index + 1;
         else
            Result (Index) := '[';
            Result (Index + 1) := To_Hex (Character'Pos (Str (S)) / 16);
            Result (Index + 2) := To_Hex (Character'Pos (Str (S)) mod 16);
            Result (Index + 3) := ']';
            Index := Index + 4;
         end if;
      end loop;
      return Result (1 .. Index - 1);
   end To_Graphic_String;

   ------------------------
   -- Validate_Attribute --
   ------------------------

   procedure Validate_Attribute
     (Attr      : Attribute_Descr;
      Reader    : access Abstract_Validation_Reader'Class;
      Atts      : in out Sax_Attribute_List;
      Index     : Natural)
   is
      Val   : constant Cst_Byte_Sequence_Access :=
        Get (Get_Value (Atts, Index));
      Mask  : Facets_Mask := (others => True);
   begin
      if Debug then
         Debug_Output ("Checking attribute " & To_QName (Attr.Name)
                       & "=" & Val.all & "--");
      end if;

      if Attr.Simple_Type = null then
         return;
      end if;

      Validate_Characters
        (Attr.Simple_Type, Reader, Val.all,
         Empty_Element => False, Mask => Mask);

      if Is_ID (Attr) then
         Set_Type (Atts, Index, Sax.Attributes.Id);
      end if;

      if Debug and then Attr.Fixed /= No_Symbol then
         Debug_Output ("Attribute value must be equal to """
                       & Get (Attr.Fixed).all & """");
      end if;

      --  3.2.4 [Attribute Declaration Value] indicates we should check Fixed
      --  with the "actual value" of the attribute, not the "normalized value".
      --  However, we need to match depending on the type of the attribute: if
      --  it is an integer, the whitespaces are irrelevant; likewise for a list

      if Attr.Fixed /= No_Symbol
        and then
          not Equal (Attr.Simple_Type, Reader, Get (Attr.Fixed).all, Val.all)
      then
         Validation_Error
           (Reader, "#value must be """
            & To_Graphic_String (Get (Attr.Fixed).all)
            & """ (found """ & To_Graphic_String (Val.all) & """)");
      end if;
   end Validate_Attribute;

   ----------
   -- Free --
   ----------

   procedure Free (Validator : in out XML_Validator_Record) is
   begin
      Free (Validator.Attributes);
   end Free;

   --------------
   -- To_QName --
   --------------

   function To_QName
     (Name : Qualified_Name) return Unicode.CES.Byte_Sequence is
   begin
      if Name = No_Qualified_Name then
         return "";
      else
         return Sax.Readers.To_QName (Name.NS, Name.Local);
      end if;
   end To_QName;

   function To_QName
     (NS     : XML_Grammar_NS; Local : Sax.Symbols.Symbol)
      return Byte_Sequence is
   begin
      return Sax.Readers.To_QName (NS.Namespace_URI, Local);
   end To_QName;

   ----------------------
   -- Get_Symbol_Table --
   ----------------------

   function Get_Symbol_Table
     (Grammar : XML_Grammar) return Sax.Utils.Symbol_Table is
   begin
      if Grammar = No_Grammar then
         return Symbol_Table_Pointers.Null_Pointer;
      else
         return Get (Grammar).Symbols;
      end if;
   end Get_Symbol_Table;

   ----------------------
   -- Set_Symbol_Table --
   ----------------------

   procedure Set_Symbol_Table
     (Grammar : XML_Grammar; Symbols : Sax.Utils.Symbol_Table) is
   begin
      if Grammar /= No_Grammar then
         Get (Grammar).Symbols := Symbols;
      end if;
   end Set_Symbol_Table;

   ------------
   -- Get_NS --
   ------------

   procedure Get_NS
     (Grammar       : XML_Grammar;
      Namespace_URI : Symbol;
      Result        : out XML_Grammar_NS;
      Create_If_Needed : Boolean := True)
   is
   begin
      if Grammar /= No_Grammar and then Get (Grammar).Grammars /= null then
         for G in Get (Grammar).Grammars'Range loop
            if Get (Grammar).Grammars (G).Namespace_URI = Namespace_URI then
               Result := Get (Grammar).Grammars (G);
               if Debug then
                  Debug_Output ("  Get_NS -> " & Debug_Name (Result));
               end if;
               return;
            end if;
         end loop;
      end if;

      if Create_If_Needed then
         Result := Create_NS_Grammar (Grammar, Namespace_URI);
         if Debug then
            Debug_Output ("  Get_NS new -> " & Debug_Name (Result));
         end if;
      else
         Result := null;
      end if;
   end Get_NS;

   ----------
   -- Free --
   ----------

   procedure Free (List : in out String_List) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (String_List_Record, String_List);
      L : String_List;
   begin
      while List /= null loop
         L := List.Next;
         Unchecked_Free (List);
         List := L;
      end loop;
   end Free;

   ---------------------
   -- Set_XSD_Version --
   ---------------------

   procedure Set_XSD_Version
     (Grammar : in out XML_Grammar;
      XSD_Version : XSD_Versions)
   is
      G   : XML_Grammars.Encapsulated_Access;
   begin
      if Grammar = No_Grammar then
         G := new XML_Grammar_Record;
         G.NFA := new Schema_State_Machines.NFA;
         G.NFA.Initialize (Nested_Must_Be_Final => True,
                           States_Are_Statefull => True);
         Grammar  := Allocate (G);
      end if;

      G := Get (Grammar);
      G.XSD_Version := XSD_Version;
   end Set_XSD_Version;

   ---------------------
   -- Get_XSD_Version --
   ---------------------

   function Get_XSD_Version (Grammar : XML_Grammar) return XSD_Versions is
      G   : XML_Grammars.Encapsulated_Access;
   begin
      G := Get (Grammar);
      if G = null then
         return XSD_1_1;
      else
         return G.XSD_Version;
      end if;
   end Get_XSD_Version;

   -----------------------
   -- Create_NS_Grammar --
   -----------------------

   function Create_NS_Grammar
     (Grammar       : XML_Grammar;
      Namespace_URI : Sax.Symbols.Symbol) return XML_Grammar_NS
   is
      G   : constant XML_Grammars.Encapsulated_Access := Get (Grammar);
      Tmp : Grammar_NS_Array_Access;
   begin
      if G.Grammars = null then
         G.Grammars := new Grammar_NS_Array (1 .. 1);
      else
         Tmp := G.Grammars;
         G.Grammars := new Grammar_NS_Array (1 .. Tmp'Length + 1);
         G.Grammars (Tmp'Range) := Tmp.all;
         Unchecked_Free (Tmp);
      end if;

      if Debug then
         Debug_Output ("Create_NS_Grammar: " & Debug_Print (Namespace_URI));
      end if;

      G.Grammars (G.Grammars'Last) := new XML_Grammar_NS_Record'
        (Namespace_URI      => Namespace_URI,
         System_ID          => No_Symbol,
         Blocks             => No_Block);

      return G.Grammars (G.Grammars'Last);
   end Create_NS_Grammar;

   ------------------------
   -- Initialize_Grammar --
   ------------------------

   procedure Initialize_Grammar
     (Reader : access Abstract_Validation_Reader'Class)
   is
      Actual_G : XML_Grammars.Encapsulated_Access;
   begin
      if Reader.Grammar = No_Grammar then
         if Debug then
            Debug_Output ("Initialize_Grammar, settings its symbol table");
         end if;
         Actual_G         := new XML_Grammar_Record;
         Actual_G.NFA := new Schema_State_Machines.NFA;
         Actual_G.NFA.Initialize (Nested_Must_Be_Final => True,
                                  States_Are_Statefull => True);
         Reader.Grammar   := Allocate (Actual_G);
         Actual_G.Symbols := Get_Symbol_Table (Reader.all);
      end if;

      if Get (Reader.Grammar).Grammars = null then
         Add_Schema_For_Schema (Reader);
      end if;
   end Initialize_Grammar;

   ----------------
   -- Debug_Dump --
   ----------------

   procedure Debug_Dump (Grammar : XML_Grammar) is
      Str : String_List;
      G    : constant XML_Grammars.Encapsulated_Access := Get (Grammar);
   begin
      if Debug then
         Str := G.Parsed_Locations;
         while Str /= null loop
            Debug_Output ("   Parsed location: " & Get (Str.Str).all);
            Str := Str.Next;
         end loop;
      end if;
   end Debug_Dump;

   ----------
   -- Free --
   ----------

   procedure Free (Facets : in out Facets_Description) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Facets_Description_Record'Class, Facets_Description);
   begin
      if Facets /= null then
         Free (Facets.all);
         Unchecked_Free (Facets);
      end if;
   end Free;

   ----------------
   -- Get_Facets --
   ----------------

   function Get_Facets
     (Validator : access XML_Validator_Record;
      Reader    : access Abstract_Validation_Reader'Class)
      return Facets_Description
   is
      pragma Unreferenced (Validator, Reader);
   begin
      return null;
   end Get_Facets;

   ----------------
   -- Debug_Name --
   ----------------

   function Debug_Name (Grammar : XML_Grammar_NS) return String is
      S : Symbol;
   begin
      if Grammar = null then
         return "<Grammar: null>";
      else
         S := Grammar.Namespace_URI;
         if S = No_Symbol then
            return "<Grammar: "
              & System.Address_Image (Grammar.all'Address) & " no ns>";
         else
            return "<Grammar: "
              & System.Address_Image (Grammar.all'Address)
              & " {" & Get (S).all & "}>";
         end if;
      end if;
   end Debug_Name;

   ------------------------
   -- Create_Global_Type --
   ------------------------

   procedure Create_Global_Type
     (Grammar    : XML_Grammar;
      Name       : Qualified_Name;
      Validator  : access XML_Validator_Record'Class)
   is
      use Reference_HTables;
      NFA  : constant Schema.Validators.Schema_State_Machines.NFA_Access :=
        Get_NFA (Grammar);
      Ref  : constant access Reference_HTables.Instance :=
        Get_References (Grammar);
      S    : State;
      Info : Type_Descr;
   begin
      Info := (Name           => Name,
               Simple_Content => True,
               Simple_Type    => XML_Validator (Validator),
               others         => <>);
      S := NFA.Add_State ((Descr => Info));
      Set (Ref.all, (Name, Ref_Type), (Ref_Type, S));
   end Create_Global_Type;

   ----------
   -- Free --
   ----------

   procedure Free (Grammar : in out XML_Grammar_Record) is
   begin
      if Debug then
         Debug_Output ("Freeing grammar");
      end if;
      if Grammar.Grammars /= null then
         for NS in Grammar.Grammars'Range loop
            Free (Grammar.Grammars (NS));
         end loop;
         Unchecked_Free (Grammar.Grammars);
      end if;

      Reference_HTables.Reset (Grammar.References);
      Free (Grammar.NFA);
      Free (Grammar.Parsed_Locations);
   end Free;

   -----------
   -- Reset --
   -----------

   procedure Reset (Grammar : in out XML_Grammar) is
      G     : constant XML_Grammars.Encapsulated_Access := Get (Grammar);
      Count : Natural := 0;
      Tmp   : Grammar_NS_Array_Access;
   begin
      if Debug then
         Debug_Output ("Partial reset of the grammar");
      end if;

      if G = null then
         return;
      end if;

      Tmp := G.Grammars;

      if Tmp /= null then
         for NS in Tmp'Range loop
            if Get (Tmp (NS).Namespace_URI).all /= XML_Schema_URI
              and then Get (Tmp (NS).Namespace_URI).all /= XML_URI
            then
               Free (Tmp (NS));
            else
               Count := Count + 1;
            end if;
         end loop;

         if Count /= 0 then
            G.Grammars := new Grammar_NS_Array (1 .. Count);
            Count := G.Grammars'First;
            for NS in Tmp'Range loop
               if Tmp (NS) /= null then
                  G.Grammars (Count) := Tmp (NS);
                  Count := Count + 1;
               end if;
            end loop;
         else
            Unchecked_Free (G.Grammars);
         end if;

         Unchecked_Free (Tmp);
      end if;

      Free (G.Parsed_Locations);
      G.Target_NS := null;
   end Reset;

   --------------------
   -- URI_Was_Parsed --
   --------------------

   function URI_Was_Parsed
     (Grammar : XML_Grammar; URI : Symbol) return Boolean
   is
      L : String_List;
   begin
      if Grammar /= No_Grammar then
         L := Get (Grammar).Parsed_Locations;
         while L /= null loop
            if Debug then
               Debug_Output ("URI_Was_Parsed ("
                             & Get (URI).all & ") ? Compare with "
                             & Get (L.Str).all);
            end if;
            if L.Str = URI then
               if Debug then
                  Debug_Output ("    => Yes, already parsed");
               end if;
               return True;
            end if;
            L := L.Next;
         end loop;
      end if;
      return False;
   end URI_Was_Parsed;

   --------------------
   -- Set_Parsed_URI --
   --------------------

   procedure Set_Parsed_URI
     (Reader  : access Abstract_Validation_Reader'Class;
      Grammar : in out XML_Grammar;
      URI     : Symbol) is
   begin
      Initialize_Grammar (Reader);

      if Debug then
         Debug_Output ("Set_Parsed_UI: " & Get (URI).all);
      end if;
      Get (Grammar).Parsed_Locations := new String_List_Record'
        (Str  => URI,
         Next => Get (Grammar).Parsed_Locations);
   end Set_Parsed_URI;

   ----------
   -- Free --
   ----------

   procedure Free (Grammar : in out XML_Grammar_NS) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (XML_Grammar_NS_Record, XML_Grammar_NS);
   begin
      if Grammar /= null then
         Unchecked_Free (Grammar);
      end if;
   end Free;

   -------------
   -- Get_NFA --
   -------------

   function Get_NFA
     (Grammar : XML_Grammar) return Schema_State_Machines.NFA_Access is
   begin
      return Get (Grammar).NFA;
   end Get_NFA;

   --------------------
   -- Get_References --
   --------------------

   function Get_References
     (Grammar : XML_Grammar) return access Reference_HTables.Instance is
   begin
      return Get (Grammar).References'Access;
   end Get_References;

   -----------------
   -- Set_Default --
   -----------------

--     procedure Set_Default
--       (Element  : XML_Element;
--        Reader   : access Abstract_Validation_Reader'Class;
--        Default  : Symbol)
--     is
--        Mask : Facets_Mask;
--     begin
--        --  3.3 Element Declaration details: Can't have both
--        --  "default" and "fixed"
--
--        if Element.Elem.Fixed /= No_Symbol then
--           Validation_Error
--             (Reader,
--           "#Attributes ""fixed"" and ""default"" conflict with each other");
--        end if;
--
--        --  3.3 Element Declaration details:  Validation Rule 3.1
--        --  The "default" attribute of element must match the validation rule
--        --  for that element.
--     --  Test whether we have a forward reference to the type, in which case
--        --  default will be checked when we know the actual type
--
--        if Element.Elem.Of_Type /= No_Type
--          and then Get_Validator (Element.Elem.Of_Type) /= null
--        then
--           Mask := (others => True);
--           Validate_Characters
--             (Get_Validator (Element.Elem.Of_Type), Reader,
--              Get (Default).all, Empty_Element => False, Mask => Mask);
--        end if;
--
--        Element.Elem.Default := Default;
--     end Set_Default;
   ---------------
   -- Set_Fixed --
   ---------------

--     procedure Set_Fixed
--       (Element  : XML_Element;
--        Reader   : access Abstract_Validation_Reader'Class;
--        Fixed    : Symbol)
--     is
--        Mask : Facets_Mask;
--     begin
--        --  3.3 Element Declaration details: Can't have both
--        --  "default" and "fixed"
--
--        if Element.Elem.Default /= No_Symbol then
--           Validation_Error
--             (Reader,
--          "#Attributes ""fixed"" and ""default"" conflict with each other");
--        end if;
--
--        --  3.3 Element Declaration details:  Validation Rule 3.1
--        --  The "fixed" attribute of element must match the validation rule
--        --  for that element
--     --  Test whether we have a forward reference to the type, in which case
--        --  default will be checked when we know the actual type
--
--        if Element.Elem.Of_Type /= No_Type
--          and then Get_Validator (Element.Elem.Of_Type) /= null
--        then
--           Mask := (others => True);
--           Validate_Characters
--             (Get_Validator (Element.Elem.Of_Type), Reader, Get (Fixed).all,
--              Empty_Element => False, Mask => Mask);
--        end if;
--
--        Element.Elem.Fixed := Fixed;
--     end Set_Fixed;

   ----------------------------
   -- Set_Substitution_Group --
   ----------------------------

--     procedure Set_Substitution_Group
--       (Element : XML_Element;
--        Reader  : access Abstract_Validation_Reader'Class;
--        Head    : XML_Element)
--     is
--        Had_Restriction, Had_Extension : Boolean := False;
--        HeadPtr : constant XML_Element_Access := Head.Elem;
--        ElemPtr : constant XML_Element_Access := Element.Elem;
--        Valid_Replacement : Boolean;
--     begin
--    --  ??? Should Head be fully defined here, so that we can check we are a
--        --  possible replacement for it ?
--        if Get_Validator (Get_Type (Element)) /= null
--          and then Get_Validator (Get_Type (Head)) /= null
--          and then Get_Validator (Get_Type (Element)) /=
--          Get_Validator (Get_Type (Head))
--        then
--           Check_Replacement
--             (Get_Validator (Get_Type (Element)),
--              Element         => Head,
--              Typ             => Get_Type (Head),
--              Valid           => Valid_Replacement,
--              Had_Restriction => Had_Restriction,
--              Had_Extension   => Had_Extension);
--
--           if not Valid_Replacement then
--              Validation_Error
--                (Reader, '#' & To_QName (Get_Type (Element))
--                 & " is not a valid replacement for "
--                 & To_QName (Get_Type (Head)));
--           end if;
--
--           if HeadPtr.Final (Final_Restriction) and then Had_Restriction then
--              Validation_Error
--                (Reader, "#""" & Get (HeadPtr.Local_Name).all
--              & """ is final for restrictions, and cannot be substituted by"
--                 & """" & Get (ElemPtr.Local_Name).all & """");
--           end if;
--
--           if HeadPtr.Final (Final_Extension) and then Had_Extension then
--              Validation_Error
--                (Reader, "#""" & Get (HeadPtr.Local_Name).all
--                 & """ is final for extensions, and cannot be substituted by"
--                 & """" & Get (ElemPtr.Local_Name).all & """");
--           end if;
--        end if;
--
--        if ElemPtr.Substitution_Group /= No_Element then
--           Validation_Error
--             (Reader, "#""" & Get (ElemPtr.Local_Name).all
--              & """ already belongs to another substitution group");
--        end if;
--
--        ElemPtr.Substitution_Group := Head;
--     end Set_Substitution_Group;

   -------------------
   -- Get_Validator --
   -------------------

   function Get_Validator (Typ : XML_Type) return XML_Validator is
   begin
      if Typ = null then
         return null;
      else
         return Typ.Validator;
      end if;
   end Get_Validator;

   ----------
   -- Free --
   ----------

   procedure Free (Id : in out Id_Ref) is
      pragma Unreferenced (Id);
   begin
      null;
   end Free;

   -------------
   -- Get_Key --
   -------------

   function Get_Key (Id : Id_Ref) return Symbol is
   begin
      return Id.Key;
   end Get_Key;

   ----------
   -- Free --
   ----------

   procedure Free (Id_Table : in out Id_Htable_Access) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Id_Htable.HTable, Id_Htable_Access);
   begin
      if Id_Table /= null then
         Id_Htable.Reset (Id_Table.all);
         Unchecked_Free (Id_Table);
      end if;
   end Free;

   --------------------------
   -- Create_Any_Attribute --
   --------------------------

   function Create_Any_Attribute
     (In_NS  : XML_Grammar_NS;
      Process_Contents : Process_Contents_Type := Process_Strict;
      Kind   : Namespace_Kind;
      List   : NS_List := Empty_NS_List) return Attribute_Descr
   is
      pragma Unreferenced (In_NS, Process_Contents, Kind, List);
   begin
      --  ??? Incorrect
      return Attribute_Descr'(others => <>);
   end Create_Any_Attribute;

   ------------------------
   -- Validate_Attribute --
   ------------------------

   --     procedure Validate_Attribute
   --       (Validator : Any_Attribute_Validator;
   --        Reader    : access Abstract_Validation_Reader'Class;
   --        Atts      : in out Sax_Attribute_List;
   --        Index     : Natural)
   --     is
   --        URI  : constant Symbol := Get_URI (Atts, Index);
   --        Attr : Attribute_Validator;
   --        G    : XML_Grammar_NS;
   --        NS_Matches : Boolean := False;
   --     begin
   --        if Debug then
   --           Debug_Output ("Validate_Attribute, anyAttribute: "
   --                         & Validator.Kind'Img & " "
   --                         & Validator.Process_Contents'Img & " "
   --                         & Get (Get_Local_Name (Atts, Index)).all);
   --        end if;
   --
   --        --  See 3.10.1 for interpretation of processContent.
   --        --  See also 3.4.2 for the intersection of <anyAttribute> elements
   --
   --        case Validator.Kind is
   --           when Namespace_Other =>
   --              NS_Matches := URI /= Empty_String
   --                and then URI /= Validator.NS.Namespace_URI;
   --           when Namespace_Any =>
   --              NS_Matches := True;
   --           when Namespace_List =>
   --              for N in Validator.List'Range loop
   --                 if Validator.List (N) = Reader.Local then
   --                    NS_Matches := NS_Matches or else URI = Empty_String;
   --                 elsif Validator.List (N) = Reader.Target_Namespace then
   --                    NS_Matches := NS_Matches
   --                      or else URI = Validator.NS.Namespace_URI;
   --                 else
   --               NS_Matches := NS_Matches or else URI = Validator.List (N);
   --                 end if;
   --              end loop;
   --        end case;
   --
   --        if not NS_Matches then
   --           Validation_Error (Reader, "#Invalid namespace for "
   --                             & To_QName
   --                               (Get_URI (Atts, Index),
   --                                Get_Local_Name (Atts, Index)));
   --        end if;
   --
   --        case Validator.Process_Contents is
   --           when Process_Strict =>
   --              Get_NS (Reader.Grammar, URI, G);
   --              Attr := Lookup_Attribute
   --                (G, Reader,
   --                 Get_Local_Name (Atts, Index), Create_If_Needed => False);
   --              if Attr = null then
   --                 Validation_Error (Reader, "#No definition provided");
   --              else
   --                 Validate_Attribute (Attr.all, Reader, Atts, Index);
   --
   --                 if Is_ID (Attr.all) then
   --                    Set_Type (Atts, Index, Sax.Attributes.Id);
   --                 end if;
   --              end if;
   --
   --           when Process_Lax =>
   --              Get_NS (Reader.Grammar, URI, G);
   --              Attr := Lookup_Attribute
   --                (G, Reader,
   --                 Get_Local_Name (Atts, Index), Create_If_Needed => False);
   --              if Attr = null then
   --                 if Debug then
   --                    Debug_Output
   --                      ("Definition not found for attribute "
   --                       & To_QName (Get_URI (Atts, Index),
   --                                   Get_Local_Name (Atts, Index)));
   --                 end if;
   --              else
   --                 Validate_Attribute (Attr.all, Reader, Atts, Index);
   --                 if Is_ID (Attr.all) then
   --                    Set_Type (Atts, Index, Sax.Attributes.Id);
   --                 end if;
   --              end if;
   --
   --           when Process_Skip =>
   --              null;
   --        end case;
   --     end Validate_Attribute;

   ------------------------
   -- Check_Content_Type --
   ------------------------

--     procedure Check_Content_Type
--       (Validator        : access XML_Validator_Record;
--        Reader           : access Abstract_Validation_Reader'Class;
--        Should_Be_Simple : Boolean)
--     is
--        pragma Unreferenced (Validator);
--     begin
--        if Should_Be_Simple then
--           Validation_Error
--             (Reader,
--              "#Type specified in a simpleContent context must not have a "
--              & "complexContent");
--        end if;
--     end Check_Content_Type;

   -------------------------
   -- Check_Qualification --
   -------------------------

--     procedure Check_Qualification
--       (Reader        : access Abstract_Validation_Reader'Class;
--        Element       : XML_Element;
--        NS            : XML_Grammar_NS) is
--     begin
--        if not Is_Global (Element)
--          and then Element.Elem.Form = Unqualified
--          and then NS.Namespace_URI /= Empty_String
--        then
--           Validation_Error
--             (Reader,
--              "#Namespace specification not authorized in this context");
--
--        elsif Element.Elem.Form = Qualified
--          and then NS.Namespace_URI = Empty_String
--          and then Get (Reader.Grammar).Target_NS /= null
--        then
--           Validation_Error
--           (Reader, "#Namespace specification is required in this context");
--        end if;
--     end Check_Qualification;

   ------------------------
   -- Check_Content_Type --
   ------------------------

--     procedure Check_Content_Type
--       (Typ              : XML_Type;
--        Reader           : access Abstract_Validation_Reader'Class;
--        Should_Be_Simple : Boolean) is
--     begin
--        if Typ.Simple_Type = Unknown_Content then
--           if Typ.Validator /= null then
--              Check_Content_Type (Typ.Validator, Reader, Should_Be_Simple);
--           end if;
--
--           --  If we matched, we now know the content type
--           if Should_Be_Simple then
--              Typ.Simple_Type := Simple_Content;
--           else
--              Typ.Simple_Type := Complex_Content;
--           end if;
--
--        elsif Should_Be_Simple and Typ.Simple_Type = Complex_Content then
--           if Typ.Local_Name /= No_Symbol then
--              Validation_Error
--                (Reader,
--              '#' & To_QName (Typ) & " specified in a simpleContent context"
--                 & " must not have a complexContext");
--           else
--              Validation_Error
--                (Reader, "#Expecting simple type, got complex type");
--           end if;
--        elsif not Should_Be_Simple and Typ.Simple_Type = Simple_Content then
--           Validation_Error
--             (Reader, "#Expecting complex type, got simple type");
--        end if;
--     end Check_Content_Type;

   --------------------
   -- Is_Simple_Type --
   --------------------

--     function Is_Simple_Type
--       (Reader : access Abstract_Validation_Reader'Class;
--        Typ    : XML_Type) return Boolean is
--     begin
--        if Typ.Simple_Type = Unknown_Content then
--           begin
--              Check_Content_Type
--                (Typ.Validator, Reader, Should_Be_Simple => True);
--              Typ.Simple_Type := Simple_Content;
--           exception
--              when XML_Validation_Error =>
--                 Typ.Simple_Type := Complex_Content;
--           end;
--        end if;
--
--        return Typ.Simple_Type = Simple_Content;
--     end Is_Simple_Type;

   -----------------------
   -- Get_Namespace_URI --
   -----------------------

   function Get_Namespace_URI (Grammar : XML_Grammar_NS) return Symbol is
   begin
      if Grammar = null then
         return Empty_String;
      else
         return Grammar.Namespace_URI;
      end if;
   end Get_Namespace_URI;

   ---------------
   -- Set_Block --
   ---------------

   procedure Set_Block
     (Typ    : XML_Type;
      Blocks : Block_Status) is
   begin
      Typ.Blocks := Blocks;
   end Set_Block;

   ---------------
   -- Get_Block --
   ---------------

   function Get_Block (Typ : XML_Type) return Block_Status is
   begin
      return Typ.Blocks;
   end Get_Block;

   ---------------
   -- Get_Final --
   ---------------

   function Get_Final (Typ : XML_Type) return Final_Status is
   begin
      return Typ.Final;
   end Get_Final;

   -----------------------
   -- Set_Block_Default --
   -----------------------

   procedure Set_Block_Default
     (Grammar : XML_Grammar_NS;
      Blocks  : Block_Status) is
   begin
      Grammar.Blocks := Blocks;
   end Set_Block_Default;

   -----------------------
   -- Get_Block_Default --
   -----------------------

   function Get_Block_Default (Grammar : XML_Grammar_NS) return Block_Status is
   begin
      return Grammar.Blocks;
   end Get_Block_Default;

   -------------------
   -- Set_Target_NS --
   -------------------

   procedure Set_Target_NS
     (Grammar : XML_Grammar;
      NS      : XML_Grammar_NS) is
   begin
      if Grammar /= No_Grammar then
         Get (Grammar).Target_NS := NS;
      end if;
   end Set_Target_NS;

   -------------------
   -- Get_Target_NS --
   -------------------

   function Get_Target_NS (Grammar : XML_Grammar) return XML_Grammar_NS is
   begin
      if Grammar = No_Grammar then
         return null;
      else
         return Get (Grammar).Target_NS;
      end if;
   end Get_Target_NS;

   ------------------
   -- Create_Union --
   ------------------

   function Create_Union (G : XML_Grammar_NS) return XML_Validator is
      pragma Unreferenced (G);
      Result : constant XML_Validator := new XML_Union_Record;
   begin
      return Result;
   end Create_Union;

   ---------------
   -- Add_Union --
   ---------------

   procedure Add_Union
     (Validator : access XML_Validator_Record'Class;
      Reader    : access Abstract_Validation_Reader'Class;
      Part      : Type_Descr) is
   begin
      Schema.Validators.Simple_Types.Add_Union
        (XML_Union (Validator), Reader, Part);
   end Add_Union;

   --------------
   -- Check_Id --
   --------------

   procedure Check_Id
     (Reader    : access Abstract_Validation_Reader'Class;
      Validator : access XML_Validator_Record'Class;
      Value     : Unicode.CES.Byte_Sequence)
   is
      Val : Symbol;
   begin
      if Is_ID (Validator.all) then
         Val := Find_Symbol (Reader.all, Value);

         if Reader.Id_Table = null then
            Reader.Id_Table := new Id_Htable.HTable (101);
         else
            if Id_Htable.Get (Reader.Id_Table.all, Val) /= No_Id then
               Validation_Error
                 (Reader, "#ID """ & Value & """ already defined");
            end if;
         end if;

         Id_Htable.Set (Reader.Id_Table.all, Id_Ref'(Key => Val));
      end if;
   end Check_Id;

   -------------------
   -- Set_System_Id --
   -------------------

   procedure Set_System_Id
     (Grammar   : XML_Grammar_NS;
      System_Id : Symbol) is
   begin
      Grammar.System_ID := System_Id;
   end Set_System_Id;

   -------------------
   -- Get_System_Id --
   -------------------

   function Get_System_Id (Grammar : XML_Grammar_NS) return Symbol is
   begin
      return Grammar.System_ID;
   end Get_System_Id;

   ---------------------
   -- Is_Extension_Of --
   ---------------------

   function Is_Extension_Of
     (Validator : XML_Validator_Record;
      Base      : access XML_Validator_Record'Class) return Boolean
   is
      pragma Unreferenced (Validator, Base);
   begin
      return False;
   end Is_Extension_Of;

   ---------------------
   -- Is_Extension_Of --
   ---------------------

--     function Is_Extension_Of
--       (Element : XML_Element; Base : XML_Element) return Boolean is
--     begin
--        return Is_Extension_Of
--          (Element.Elem.Of_Type.Validator.all,
--           Base.Elem.Of_Type.Validator);
--     end Is_Extension_Of;

   -----------------------
   -- Is_Restriction_Of --
   -----------------------

   function Is_Restriction_Of
     (Validator : XML_Validator_Record;
      Base      : access XML_Validator_Record'Class) return Boolean
   is
      pragma Unreferenced (Validator, Base);
   begin
      return False;
   end Is_Restriction_Of;

   -----------------------
   -- Is_Restriction_Of --
   -----------------------

--     function Is_Restriction_Of
--       (Element : XML_Element; Base : XML_Element) return Boolean is
--     begin
--        return Is_Restriction_Of
--          (Element.Elem.Of_Type.Validator.all,
--           Base.Elem.Of_Type.Validator);
--     end Is_Restriction_Of;

   -----------------
   -- Is_Wildcard --
   -----------------

   function Is_Wildcard
     (Validator : access XML_Validator_Record) return Boolean
   is
      pragma Unreferenced (Validator);
   begin
      return False;
   end Is_Wildcard;

   ----------
   -- Free --
   ----------

   procedure Free (Reader : in out Abstract_Validation_Reader) is
   begin
      Free (Reader.Error_Msg);
   end Free;

   ------------------------
   -- Initialize_Symbols --
   ------------------------

   overriding procedure Initialize_Symbols
     (Parser : in out Abstract_Validation_Reader)
   is
      use Symbol_Table_Pointers;
   begin
      Initialize_Symbols (Sax_Reader (Parser));

      if Parser.Grammar /= No_Grammar then
         if Debug then
            Debug_Output ("Initialize_Symbols, grammar is not null,"
                          & " setting its symbol table if needed");
         end if;

         if Get (Parser.Grammar).Symbols =
           Symbol_Table_Pointers.Null_Pointer
         then
            Get (Parser.Grammar).Symbols := Get_Symbol_Table (Parser);
         end if;
      end if;

      if Parser.Xmlns /= No_Symbol then
         return;
      end if;

      Parser.All_NNI                := Find_Symbol (Parser, "allNNI");
      Parser.Annotated              := Find_Symbol (Parser, "annotated");
      Parser.Annotation             := Find_Symbol (Parser, "annotation");
      Parser.Any                    := Find_Symbol (Parser, "any");
      Parser.Any_Attribute          := Find_Symbol (Parser, "anyAttribute");
      Parser.Any_Namespace     := Find_Symbol (Parser, "##any");
      Parser.Any_Simple_Type        := Find_Symbol (Parser, "anySimpleType");
      Parser.Anytype           := Find_Symbol (Parser, "anyType");
      Parser.Appinfo                := Find_Symbol (Parser, "appinfo");
      Parser.Attr_Decls             := Find_Symbol (Parser, "attrDecls");
      Parser.Attribute              := Find_Symbol (Parser, "attribute");
      Parser.Attribute_Group        := Find_Symbol (Parser, "attributeGroup");
      Parser.Attribute_Group_Ref  := Find_Symbol (Parser, "attributeGroupRef");
      Parser.Base                   := Find_Symbol (Parser, "base");
      Parser.Block                  := Find_Symbol (Parser, "block");
      Parser.Block_Default          := Find_Symbol (Parser, "blockDefault");
      Parser.Block_Set              := Find_Symbol (Parser, "blockSet");
      Parser.Choice                 := Find_Symbol (Parser, "choice");
      Parser.Complex_Content        := Find_Symbol (Parser, "complexContent");
      Parser.Complex_Extension_Type :=
        Find_Symbol (Parser, "complexExtensionType");
      Parser.Complex_Restriction_Type :=
        Find_Symbol (Parser, "complexRestrictionType");
      Parser.Complex_Type           := Find_Symbol (Parser, "complexType");
      Parser.Complex_Type_Model := Find_Symbol (Parser, "complexTypeModel");
      Parser.Def_Ref                := Find_Symbol (Parser, "defRef");
      Parser.Default                := Find_Symbol (Parser, "default");
      Parser.Derivation_Control   := Find_Symbol (Parser, "derivationControl");
      Parser.Derivation_Set         := Find_Symbol (Parser, "derivationSet");
      Parser.Documentation          := Find_Symbol (Parser, "documentation");
      Parser.Element                := Find_Symbol (Parser, "element");
      Parser.Enumeration       := Find_Symbol (Parser, "enumeration");
      Parser.Explicit_Group         := Find_Symbol (Parser, "explicitGroup");
      Parser.Extension              := Find_Symbol (Parser, "extension");
      Parser.Extension_Type         := Find_Symbol (Parser, "extensionType");
      Parser.Facet                  := Find_Symbol (Parser, "facet");
      Parser.Field                  := Find_Symbol (Parser, "field");
      Parser.Final                  := Find_Symbol (Parser, "final");
      Parser.Final_Default          := Find_Symbol (Parser, "finalDefault");
      Parser.Fixed                  := Find_Symbol (Parser, "fixed");
      Parser.Form                   := Find_Symbol (Parser, "form");
      Parser.Form_Choice            := Find_Symbol (Parser, "formChoice");
      Parser.Fraction_Digits   := Find_Symbol (Parser, "fractionDigits");
      Parser.Group                  := Find_Symbol (Parser, "group");
      Parser.Group_Def_Particle := Find_Symbol (Parser, "groupDefParticle");
      Parser.Group_Ref              := Find_Symbol (Parser, "groupRef");
      Parser.Id                     := Find_Symbol (Parser, "id");
      Parser.Identity_Constraint := Find_Symbol (Parser, "identityConstraint");
      Parser.Import                 := Find_Symbol (Parser, "import");
      Parser.Include                := Find_Symbol (Parser, "include");
      Parser.Item_Type              := Find_Symbol (Parser, "itemType");
      Parser.Key                    := Find_Symbol (Parser, "key");
      Parser.Keybase                := Find_Symbol (Parser, "keybase");
      Parser.Keyref                 := Find_Symbol (Parser, "keyref");
      Parser.Lang                   := Find_Symbol (Parser, "lang");
      Parser.Lax                    := Find_Symbol (Parser, "lax");
      Parser.Length            := Find_Symbol (Parser, "length");
      Parser.List                   := Find_Symbol (Parser, "list");
      Parser.Local             := Find_Symbol (Parser, "##local");
      Parser.Local_Complex_Type    := Find_Symbol (Parser, "localComplexType");
      Parser.Local_Element          := Find_Symbol (Parser, "localElement");
      Parser.Local_Simple_Type      := Find_Symbol (Parser, "localSimpleType");
      Parser.MaxExclusive      := Find_Symbol (Parser, "maxExclusive");
      Parser.MaxInclusive      := Find_Symbol (Parser, "maxInclusive");
      Parser.MaxOccurs         := Find_Symbol (Parser, "maxOccurs");
      Parser.Max_Bound              := Find_Symbol (Parser, "maxBound");
      Parser.Maxlength         := Find_Symbol (Parser, "maxLength");
      Parser.Member_Types           := Find_Symbol (Parser, "memberTypes");
      Parser.MinExclusive      := Find_Symbol (Parser, "minExclusive");
      Parser.MinInclusive      := Find_Symbol (Parser, "minInclusive");
      Parser.MinOccurs         := Find_Symbol (Parser, "minOccurs");
      Parser.Min_Bound              := Find_Symbol (Parser, "minBound");
      Parser.Minlength         := Find_Symbol (Parser, "minLength");
      Parser.Mixed                  := Find_Symbol (Parser, "mixed");
      Parser.NCName                 := Find_Symbol (Parser, "NCName");
      Parser.NMTOKEN                := Find_Symbol (Parser, "NMTOKEN");
      Parser.Name              := Find_Symbol (Parser, "name");
      Parser.Named_Attribute_Group  :=
        Find_Symbol (Parser, "namedAttributeGroup");
      Parser.Named_Group            := Find_Symbol (Parser, "namedGroup");
      Parser.Namespace         := Find_Symbol (Parser, "namespace");
      Parser.Namespace_List         := Find_Symbol (Parser, "namespaceList");
      Parser.Nested_Particle        := Find_Symbol (Parser, "nestedParticle");
      Parser.Nil               := Find_Symbol (Parser, "nil");
      Parser.Nillable               := Find_Symbol (Parser, "nillable");
      Parser.No_Namespace_Schema_Location :=
        Find_Symbol (Parser, "noNamespaceSchemaLocation");
      Parser.Non_Negative_Integer   :=
        Find_Symbol (Parser, "nonNegativeInteger");
      Parser.Notation               := Find_Symbol (Parser, "notation");
      Parser.Num_Facet              := Find_Symbol (Parser, "numFacet");
      Parser.Occurs                 := Find_Symbol (Parser, "occurs");
      Parser.Open_Attrs             := Find_Symbol (Parser, "openAttrs");
      Parser.Optional               := Find_Symbol (Parser, "optional");
      Parser.Other_Namespace   := Find_Symbol (Parser, "##other");
      Parser.Particle               := Find_Symbol (Parser, "particle");
      Parser.Pattern           := Find_Symbol (Parser, "pattern");
      Parser.Positive_Integer       := Find_Symbol (Parser, "positiveInteger");
      Parser.Precision_Decimal := Find_Symbol (Parser, "precisionDecimal");
      Parser.Process_Contents   := Find_Symbol (Parser, "processContents");
      Parser.Prohibited         := Find_Symbol (Parser, "prohibited");
      Parser.Public                 := Find_Symbol (Parser, "public");
      Parser.QName                  := Find_Symbol (Parser, "QName");
      Parser.Qualified          := Find_Symbol (Parser, "qualified");
      Parser.Real_Group             := Find_Symbol (Parser, "realGroup");
      Parser.Redefinable            := Find_Symbol (Parser, "redefinable");
      Parser.Redefine           := Find_Symbol (Parser, "redefine");
      Parser.Reduced_Derivation_Control :=
        Find_Symbol (Parser, "reducedDerivationControl");
      Parser.Ref               := Find_Symbol (Parser, "ref");
      Parser.Refer                  := Find_Symbol (Parser, "refer");
      Parser.Required           := Find_Symbol (Parser, "required");
      Parser.Restriction        := Find_Symbol (Parser, "restriction");
      Parser.Restriction_Type       := Find_Symbol (Parser, "restrictionType");
      Parser.S_1                    := Find_Symbol (Parser, "1");
      Parser.S_Abstract         := Find_Symbol (Parser, "abstract");
      Parser.S_All              := Find_Symbol (Parser, "all");
      Parser.S_Attribute_Form_Default :=
        Find_Symbol (Parser, "attributeFormDefault");
      Parser.S_Boolean              := Find_Symbol (Parser, "boolean");
      Parser.S_Element_Form_Default   :=
        Find_Symbol (Parser, "elementFormDefault");
      Parser.S_False                := Find_Symbol (Parser, "false");
      Parser.S_Schema           := Find_Symbol (Parser, "schema");
      Parser.S_String               := Find_Symbol (Parser, "string");
      Parser.S_Use              := Find_Symbol (Parser, "use");
      Parser.Schema_Location   := Find_Symbol (Parser, "schemaLocation");
      Parser.Schema_Top             := Find_Symbol (Parser, "schemaTop");
      Parser.Selector               := Find_Symbol (Parser, "selector");
      Parser.Sequence           := Find_Symbol (Parser, "sequence");
      Parser.Simple_Content     := Find_Symbol (Parser, "simpleContent");
      Parser.Simple_Derivation     := Find_Symbol (Parser, "simpleDerivation");
      Parser.Simple_Derivation_Set  :=
        Find_Symbol (Parser, "simpleDerivationSet");
      Parser.Simple_Extension_Type  :=
        Find_Symbol (Parser, "simpleExtensionType");
      Parser.Simple_Restriction_Model :=
        Find_Symbol (Parser, "simpleRestrictionModel");
      Parser.Simple_Restriction_Type  :=
        Find_Symbol (Parser, "simpleRestrictionType");
      Parser.Simple_Type        := Find_Symbol (Parser, "simpleType");
      Parser.Source                 := Find_Symbol (Parser, "source");
      Parser.Strict             := Find_Symbol (Parser, "strict");
      Parser.Substitution_Group := Find_Symbol (Parser, "substitutionGroup");
      Parser.System                 := Find_Symbol (Parser, "system");
      Parser.Target_Namespace  := Find_Symbol (Parser, "##targetNamespace");
      Parser.Namespace_Target  := Find_Symbol (Parser, "targetNamespace");
      Parser.Token                  := Find_Symbol (Parser, "token");
      Parser.Top_Level_Attribute  := Find_Symbol (Parser, "topLevelAttribute");
      Parser.Top_Level_Complex_Type :=
        Find_Symbol (Parser, "topLevelComplexType");
      Parser.Top_Level_Element      :=
        Find_Symbol (Parser, "topLevelElement");
      Parser.Top_Level_Simple_Type  :=
        Find_Symbol (Parser, "topLevelSimpleType");
      Parser.Total_Digits      := Find_Symbol (Parser, "totalDigits");
      Parser.Typ   := Find_Symbol (Parser, "type");
      Parser.Type_Def_Particle      := Find_Symbol (Parser, "typeDefParticle");
      Parser.UC_ID                  := Find_Symbol (Parser, "ID");
      Parser.URI_Reference          := Find_Symbol (Parser, "uriReference");
      Parser.Unbounded         := Find_Symbol (Parser, "unbounded");
      Parser.Union              := Find_Symbol (Parser, "union");
      Parser.Unique                 := Find_Symbol (Parser, "unique");
      Parser.Unqualified            := Find_Symbol (Parser, "unqualified");
      Parser.Ur_Type            := Find_Symbol (Parser, "ur-Type");
      Parser.Value              := Find_Symbol (Parser, "value");
      Parser.Version                := Find_Symbol (Parser, "version");
      Parser.Whitespace        := Find_Symbol (Parser, "whiteSpace");
      Parser.Wildcard               := Find_Symbol (Parser, "wildcard");
      Parser.XML_Instance_URI := Find_Symbol (Parser, XML_Instance_URI);

      Parser.XML_Schema_URI    := Find_Symbol (Parser, XML_Schema_URI);
      Parser.XML_URI                := Find_Symbol (Parser, XML_URI);
      Parser.XPath                  := Find_Symbol (Parser, "xpath");
      Parser.XPath_Expr_Approx      := Find_Symbol (Parser, "XPathExprApprox");
      Parser.XPath_Spec             := Find_Symbol (Parser, "XPathSpec");
      Parser.Xmlns := Find_Symbol (Parser, "xmlns");
   end Initialize_Symbols;

   -----------
   -- Match --
   -----------

   function Match (Trans, Sym : Transition_Event) return Boolean is
   begin
      case Trans.Kind is
         when Transition_Symbol | Transition_Close_Nested =>
            return Trans = Sym;

         when Transition_Any =>
            if Sym.Kind = Transition_Close_Nested then
               return False;
            elsif Get (Trans.Any.Namespace).all = "##any" then
               return True;
            elsif Get (Trans.Any.Namespace).all = "##other" then
               return Sym.Name.NS /= Trans.Any.Target_NS;
            else
               declare
                  Matches : Boolean := True;

                  procedure Callback (Str : Byte_Sequence);
                  procedure Callback (Str : Byte_Sequence) is
                  begin
                     if Matches then
                        null;
                     elsif Str = "##targetNamespace" then
                        Matches := Sym.Name.NS = Trans.Any.Target_NS;
                     elsif Str = "##local" then
                        Matches := Sym.Name.NS = Empty_String;
                     else
                        Matches := Get (Sym.Name.NS).all = Str;
                     end if;
                  end Callback;

                  procedure All_Items is new For_Each_Item (Callback);
               begin
                  All_Items (Get (Trans.Any.Namespace).all);
                  return Matches;
               end;
            end if;
      end case;
   end Match;

   -----------
   -- Image --
   -----------

   function Image (Trans : Transition_Event) return String is
   begin
      case Trans.Kind is
         when Transition_Symbol       =>
            --  return To_QName (Trans.Name);
            if Trans.Name.Local = No_Symbol then
               return "";
            else
               return Get (Trans.Name.Local).all;
            end if;
         when Transition_Close_Nested => return "end of parent element";
         when Transition_Any          => return "<any>";
      end case;
   end Image;

   -----------
   -- Image --
   -----------

   function Image (S : State; Data : State_User_Data) return String is
      pragma Unreferenced (S);
   begin
      if Data.Descr.Name.Local = No_Symbol then
         return "";
      else
         return Get (Data.Descr.Name.Local).all;
         --  return To_QName (Data.Descr.Name);
      end if;
   end Image;

   ----------
   -- Hash --
   ----------

   function Hash (Name : Reference_Name) return Header_Num is
   begin
      return (Hash (Name.Name) + Reference_Kind'Pos (Name.Kind))
      mod Header_Num'Last;
   end Hash;

   ----------
   -- Hash --
   ----------

   function Hash (Name : Qualified_Name) return Header_Num is
   begin
      return (Hash (Name.NS) + Hash (Name.Local)) / 2;
   end Hash;

   ----------
   -- Hash --
   ----------

   function Hash (Name : Sax.Symbols.Symbol) return Header_Num is
   begin
      if Name = No_Symbol then
         return 0;
      else
         return Header_Num
           (Sax.Symbols.Hash (Name)
            mod Interfaces.Unsigned_32 (Header_Num'Last));
      end if;
   end Hash;

end Schema.Validators;
