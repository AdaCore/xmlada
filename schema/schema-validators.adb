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

with Ada.Characters.Handling;        use Ada.Characters.Handling;
with Ada.Exceptions;                 use Ada.Exceptions;
with Ada.Strings.Unbounded;          use Ada.Strings.Unbounded;
with Ada.Tags;                       use Ada.Tags;
with Ada.Unchecked_Deallocation;
with GNAT.IO;                        use GNAT.IO;
with Sax.Attributes;                 use Sax.Attributes;
with Sax.Encodings;                  use Sax.Encodings;
with Sax.Utils;                      use Sax.Utils;
with Schema.Validators.XSD_Grammar;  use Schema.Validators.XSD_Grammar;
with Schema.Validators.Extensions;   use Schema.Validators.Extensions;
with Schema.Validators.Facets;       use Schema.Validators.Facets;
with Schema.Validators.Lists;        use Schema.Validators.Lists;
with Schema.Validators.Restrictions; use Schema.Validators.Restrictions;
with Schema.Validators.Simple_Types; use Schema.Validators.Simple_Types;
with Schema.Validators.UR_Type;      use Schema.Validators.UR_Type;
with Unicode.CES;                    use Unicode.CES;
with Unicode;                        use Unicode;

package body Schema.Validators is
   use XML_Grammars;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Element_List, Element_List_Access);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Particle_Iterator_Record, Particle_Iterator);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (XML_Particle, XML_Particle_Access);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Particle_List_Record, Particle_List);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (XML_Attribute_Group_Record, XML_Attribute_Group);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Grammar_NS_Array, Grammar_NS_Array_Access);

   procedure Create_NS_Grammar
     (Grammar       : XML_Grammar;
      Namespace_URI : Unicode.CES.Byte_Sequence);
   --  Create a new namespace in the grammar

   procedure Free (Element : in out XML_Element_Record);
   --  Free Element

   function To_QName (Self : Named_Attribute_Validator) return Byte_Sequence;
   function To_QName (Particle : XML_Particle) return Byte_Sequence;
   --  Return the QName for the element described by particle

   package QName_Attributes_Htable is new Sax.HTable
     (Element       => Named_Attribute_Validator,
      Empty_Element => null,
      Free          => Do_Nothing,
      Key           => Unicode.CES.Byte_Sequence,
      Get_Key       => To_QName,
      Hash          => Sax.Utils.Hash,
      Equal         => "=");
   --  Same as Attributes_Htable, but with QName as key

   function Move_To_Next_Particle
     (Seq   : access Sequence_Record'Class;
      Data  : Sequence_Data_Access;
      Force : Boolean := False;
      Increase_Count : Boolean := True) return Boolean;
   --  Move to the next particle to match in the sequence, or stay on the
   --  current one if it still can match (its maxOccurs hasn't been reached
   --  for instance).
   --  If Increase_Count is true, the current particle will be considered as
   --  matched

   procedure Check_Nested
     (Nested            : access Group_Model_Record'Class;
      Data              : access Group_Model_Data_Record'Class;
      Reader            : access Abstract_Validation_Reader'Class;
      Local_Name        : Byte_Sequence;
      Namespace_URI     : Unicode.CES.Byte_Sequence;
      NS                : XML_Grammar_NS;
      Grammar           : XML_Grammar;
      Element_Validator : out XML_Element;
      Skip_Current      : out Boolean);
   --  Check whether Nested matches Local_Name.
   --  If Nested should match but there is an error, XML_Validator_Record is
   --  raised.
   --  If Nested cannot match Local_Name, Element_Validator is set to null on
   --  exit.
   --  Data should be the parent's data.
   --  Skip_Current is set to True if Nested didn't match Local_Name, but
   --  if it should be considered as terminated successfully (and thus we
   --  should hand out Local_Name to the next validator in the list)

   procedure Run_Nested
     (Validator         : access Group_Model_Record'Class;
      Data              : access Group_Model_Data_Record'Class;
      Reader            : access Abstract_Validation_Reader'Class;
      Local_Name        : Byte_Sequence;
      Namespace_URI     : Unicode.CES.Byte_Sequence;
      NS                : XML_Grammar_NS;
      Grammar           : XML_Grammar;
      Element_Validator : out XML_Element);
   --  Run the nested group of Validator, if there is any.
   --  On exit, Element_Validator is set to No_Element if either the nested
   --  group didn't match, or there was no nested group.

   function Check_Substitution_Groups
     (Element            : XML_Element_Access;
      Reader             : access Abstract_Validation_Reader'Class;
      Local_Name         : Unicode.CES.Byte_Sequence;
      Namespace_URI      : Unicode.CES.Byte_Sequence;
      Parent_NS          : XML_Grammar_NS;
      Grammar            : XML_Grammar;
      Is_Local_Element   : Boolean;
      Check_All_In_Group : Boolean := True) return XML_Element;
   --  Check whether any element in the substitution group of Validator can
   --  be used to match Local_Name. This also check whether Element itself
   --  matches.
   --  This also raises an XML_Validator_Record if the matching element is
   --  in fact abstract.
   --  If Check_All_In_Group is False, then only Element itself is tested, this
   --  is used to avoid infinite recursion while checking the group.
   --  This also raises an XML_Validator_Record if the matching element is
   --  in fact abstract

   function Extension_Of
     (G         : XML_Grammar_NS;
      Base      : XML_Type;
      Extension : XML_Validator := null) return XML_Validator
     renames Schema.Validators.Extensions.Create_Extension_Of;

   function Extension_Of
     (G          : XML_Grammar_NS;
      Reader     : access Abstract_Validation_Reader'Class;
      Base       : XML_Type;
      Group      : XML_Group;
      Min_Occurs : Natural := 1;
      Max_Occurs : Integer := 1) return XML_Validator
      renames Schema.Validators.Extensions.Create_Extension_Of;

   function Restriction_Of
     (G           : XML_Grammar_NS;
      Reader      : access Abstract_Validation_Reader'Class;
      Base        : XML_Type;
      Restriction : XML_Validator := null) return XML_Validator
      renames Schema.Validators.Restrictions.Create_Restriction_Of;

   function Internal_Type_Model
     (Validator  : access Group_Model_Record'Class;
      Separator  : Byte_Sequence;
      First_Only : Boolean;
      All_In_List : Boolean) return Byte_Sequence;
   --  Internal version of Type_Model (shared implementation)
   --  All_In_List indicates whether all the elements from the list should be
   --  included, or only the first one.

   function Type_Model
     (Iter       : Particle_Iterator;
      First_Only : Boolean) return Byte_Sequence;
   --  Return the content model describes by Particle.

   function Is_Optional (Iterator : Particle_Iterator) return Boolean;
   pragma Inline (Is_Optional);
   --  Whether the current optional can be omitted

   function To_Graphic_String (Str : Byte_Sequence) return String;
   --  Convert non-graphic characters in Str to make them visible in a display

   Debug_Prefixes_Level : Natural := 0;
   --  Provide Debug_Output body early to allow it to be inlined

   ------------------
   -- Debug_Output --
   ------------------

   procedure Debug_Output (Str : String) is
   begin
      Put ((1 .. Debug_Prefixes_Level * 2 => ' '));
      Put_Line (Str);
   end Debug_Output;

   ------------------------------
   -- Attribute_Validator_List --
   ------------------------------

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Attribute_Validator_List, Attribute_Validator_List_Access);

   procedure Free (List : in out Attribute_Validator_List_Access);
   --  Free the contents of List, including contained

   procedure Append
     (List        : in out Attribute_Validator_List_Access;
      Validator   : access Attribute_Validator_Record'Class;
      Is_Local    : Boolean;
      Override    : Boolean);
   procedure Append
     (Reader    : access Abstract_Validation_Reader'Class;
      List      : in out Attribute_Validator_List_Access;
      Group     : XML_Attribute_Group);
   --  Append a new value to List.
   --  If a similar attribute already exists in the list, Validator will either
   --  be ignored (Override is False), or replace the existing definition
   --  (Override is True).

   -------------------------
   -- Get_Attribute_Lists --
   -------------------------

   procedure Get_Attribute_Lists
     (Validator   : access XML_Validator_Record;
      List        : out Attribute_Validator_List_Access;
      Dependency1 : out XML_Validator;
      Ignore_Wildcard_In_Dep1 : out Boolean;
      Dependency2 : out XML_Validator;
      Must_Match_All_Any_In_Dep2 : out Boolean) is
   begin
      List := Validator.Attributes;
      Dependency1 := null;
      Ignore_Wildcard_In_Dep1 := False;
      Dependency2 := null;

      --  All <anyAttribute> in a <complexType> must match. See 3.6.2.2
      Must_Match_All_Any_In_Dep2 := True;
   end Get_Attribute_Lists;

   ----------------------
   -- Set_Debug_Output --
   ----------------------

   procedure Set_Debug_Output (Output : Boolean) is
   begin
      Debug := Output;
   end Set_Debug_Output;

   ----------------------
   -- Validation_Error --
   ----------------------

   procedure Validation_Error
     (Reader  : access Abstract_Validation_Reader;
      Message : Byte_Sequence)
   is
   begin
      if Debug then
         Debug_Output ("Validation_Error: " & Message);
      end if;

      Free (Reader.Error_Msg);
      Reader.Error_Msg := new Byte_Sequence'(Message);

      Raise_Exception (XML_Validation_Error'Identity, Message);
   end Validation_Error;

   ----------
   -- Free --
   ----------

   procedure Free (List : in out Attribute_Validator_List_Access) is
   begin
      if List /= null then
         for L in List'Range loop
            if not List (L).Is_Group then
               --  ??? Already freed through the htable in the grammar
               null;
--               Free (List (L).Attr);
            end if;
         end loop;
         Unchecked_Free (List);
      end if;
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (List : in out Particle_List) is
      L : XML_Particle_Access;
   begin
      if List /= null then
         while List.First /= null loop
            L := List.First.Next;
            Unchecked_Free (List.First);
            List.First := L;
         end loop;
         Unchecked_Free (List);
      end if;
   end Free;

   ------------------------------
   -- Do_Normalize_Whitespaces --
   ------------------------------

   function Do_Normalize_Whitespaces
     (Typ    : XML_Type;
      Reader : access Abstract_Validation_Reader'Class;
      Val    : Byte_Sequence_Access) return Byte_Sequence_Access
   is
      Whitespace : Whitespace_Restriction := Preserve;
      Facets     : constant Facets_Description :=
        Get_Facets (Typ.Validator, Reader);
      C          : Unicode_Char;
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
            return null;

         when Replace =>
            declare
               Idx   : Natural := Val'First;
               First : Natural := Val'Last + 1;
            begin
               while Idx <= Val'Last loop
                  First := Idx;
                  Encoding.Read (Val.all, Idx, C);

                  if Is_White_Space (C) then
                     --  Assumes all characters we replace are encoded as
                     --  single byte
                     Val (First) := ' ';
                  end if;
               end loop;

               return Val;  --  replacement was done in place
            end;

         when Collapse =>
            declare
               C          : Unicode_Char;
               Last       : Natural := Val'Last + 1;
               Idx, Idx_Output : Natural := Val'First;
               First      : Natural := Val'Last + 1;
               Tmp        : Natural;
               Prev_Is_Whitespace : Boolean := False;
            begin
               if Val.all = "" then
                  return null;  --  nothing to do
               end if;

               --  Remove leading spaces.
               --  At the end of this loop, First points to the first non
               --  blank character

               loop
                  First := Idx;
                  Encoding.Read (Val.all, Idx, C);
                  exit when not Is_White_Space (C);

                  if Idx > Val'Last then
                     return new Byte_Sequence'("");
                  end if;
               end loop;

               Idx_Output := Idx;

               --  Iterate and replace all whitespaces. Mark the spot of the
               --  last whitespace so that we can ignore trailing spaces.
               --  At the same time, we can copy to Idx_Output, since the
               --  output string will always be at least as short as Val.

               while Idx <= Val'Last loop
                  Tmp := Idx;
                  Encoding.Read (Val.all, Idx, C);

                  if Is_White_Space (C) then
                     if not Prev_Is_Whitespace then
                        Last := Idx_Output;
                        Val (Idx_Output) := ' ';
                        Idx_Output := Idx_Output + 1;
                        Prev_Is_Whitespace := True;
                     end if;
                  else
                     Val (Idx_Output .. Idx_Output + Idx - Tmp - 1) :=
                       Val (Tmp .. Idx - 1);
                     Idx_Output := Idx_Output + Idx - Tmp;
                     Last := Idx_Output;  --  after this char
                     Prev_Is_Whitespace := False;
                  end if;
               end loop;

               return new Byte_Sequence'(Val (First .. Last - 1));
            end;
      end case;
   end Do_Normalize_Whitespaces;

   --------------------------
   -- Normalize_Whitespace --
   --------------------------

   procedure Normalize_Whitespace
     (Typ    : XML_Type;
      Reader : access Abstract_Validation_Reader'Class;
      Atts   : Sax.Attributes.Attributes'Class;
      Index  : Natural)
   is
      Val   : aliased Byte_Sequence := Get_Value (Atts, Index);
      Val_A : constant Byte_Sequence_Access := Val'Unchecked_Access;
      Val_B : Byte_Sequence_Access;
   begin
      Val_B := Do_Normalize_Whitespaces (Typ, Reader, Val_A);

      if Val_B /= null then
         Set_Value (Atts, Index, Val_B.all);
         if Val_A /= Val_B then
            Free (Val_B);
         end if;
      end if;
   end Normalize_Whitespace;

   --------------
   -- Is_Equal --
   --------------

   function Is_Equal
     (Attribute : Named_Attribute_Validator_Record;
      Attr2     : Attribute_Validator_Record'Class)
     return Boolean is
   begin
      return Attr2 in Named_Attribute_Validator_Record'Class
        and then Attribute.NS = Attr2.NS
        and then Attribute.Local_Name.all =
          Named_Attribute_Validator_Record (Attr2).Local_Name.all;
   end Is_Equal;

   function Is_Equal
     (Attribute : Any_Attribute_Validator;
      Attr2     : Attribute_Validator_Record'Class)
     return Boolean is
   begin
      return Attr2 in Any_Attribute_Validator'Class
        and then Attribute.NS = Attr2.NS
        and then Attribute.Kind = Any_Attribute_Validator (Attr2).Kind;
   end Is_Equal;

   -----------
   -- Is_ID --
   -----------

   function Is_ID (Attr : Attribute_Validator_Record) return Boolean is
      pragma Unreferenced (Attr);
   begin
      return False;
   end Is_ID;

   function Is_ID (Attr : Named_Attribute_Validator_Record) return Boolean is
   begin
      return Is_ID (Get_Type (Attr));
   end Is_ID;

   ------------
   -- Append --
   ------------

   procedure Append
     (List        : in out Attribute_Validator_List_Access;
      Validator   : access Attribute_Validator_Record'Class;
      Is_Local    : Boolean;
      Override    : Boolean)
   is
      L : Attribute_Validator_List_Access;
   begin
      if List /= null then
         for A in List'Range loop
            if not List (A).Is_Group then
               if Is_Equal (List (A).Attr.all, Validator.all) then
                  if Override then
                     --  ??? Should we free the previous value => We are
                     --  sharing the attribute definition through
                     --  Restriction_Of...
                     List (A) :=
                       (Is_Group => False,
                        Is_Local => Is_Local,
                        Attr     => Attribute_Validator (Validator));
                  end if;
                  return;
               end if;
            end if;
         end loop;

         L := new Attribute_Validator_List (List'First .. List'Last + 1);
         L (List'Range) := List.all;
         L (L'Last) := Attribute_Or_Group'
           (Is_Group => False,
            Is_Local => Is_Local,
            Attr     => Attribute_Validator (Validator));
         Unchecked_Free (List);
         List := L;
      else
         List := new Attribute_Validator_List'
           (1 => Attribute_Or_Group'
              (Is_Group => False,
               Is_Local => Is_Local,
               Attr     => Attribute_Validator (Validator)));
      end if;
   end Append;

   ------------
   -- Append --
   ------------

   procedure Append
     (Reader : access Abstract_Validation_Reader'Class;
      List   : in out Attribute_Validator_List_Access;
      Group  : XML_Attribute_Group)
   is
      L : Attribute_Validator_List_Access;
   begin
      if Group = null then
         Validation_Error (Reader, "Cannot add null attribute group");
      end if;

      if List /= null then
         for A in List'Range loop
            if List (A).Is_Group and then List (A).Group = Group then
               return;
            end if;
         end loop;

         L := new Attribute_Validator_List'
           (List.all & Attribute_Or_Group'(Is_Group => True, Group => Group));
         Unchecked_Free (List);
         List := L;
      else
         List := new Attribute_Validator_List'
           (1 => Attribute_Or_Group'(Is_Group => True, Group => Group));
      end if;
   end Append;

   ------------
   -- Append --
   ------------

   procedure Append
     (List    : in out Element_List_Access;
      Element : XML_Element)
   is
      L : Element_List_Access;
   begin
      if List /= null then
         L := new Element_List'(List.all & Element.Elem);
         Unchecked_Free (List);
         List := L;
      else
         List := new Element_List'(1 => Element.Elem);
      end if;
   end Append;

   ----------
   -- Free --
   ----------

   procedure Free (Validator : in out Named_Attribute_Validator_Record) is
   begin
      Free (Validator.Local_Name);
      Free (Validator.Value);
   end Free;

   -------------------
   -- Has_Attribute --
   -------------------

   function Has_Attribute
     (Validator  : access XML_Validator_Record;
      NS         : XML_Grammar_NS;
      Local_Name : Unicode.CES.Byte_Sequence) return Boolean
   is
   begin
      if Validator.Attributes = null then
         return False;
      end if;

      for A in Validator.Attributes'Range loop
         if not Validator.Attributes (A).Is_Group
           and then Validator.Attributes (A).Attr.NS = NS
           and then Validator.Attributes (A).Attr.all in
              Named_Attribute_Validator_Record'Class
         then
            if Named_Attribute_Validator_Record
              (Validator.Attributes (A).Attr.all).Local_Name.all = Local_Name
            then
               return True;
            end if;
         end if;
      end loop;
      return False;
   end Has_Attribute;

   -------------------
   -- Add_Attribute --
   -------------------

   procedure Add_Attribute
     (Validator  : access XML_Validator_Record;
      Attribute  : access Attribute_Validator_Record'Class;
      Is_Local   : Boolean := True) is
   begin
      Append (Validator.Attributes, Attribute,
              Is_Local => Is_Local, Override => True);
   end Add_Attribute;

   -------------------
   -- Add_Attribute --
   -------------------

   procedure Add_Attribute
     (Group    : in out XML_Attribute_Group;
      Attr     : access Attribute_Validator_Record'Class;
      Is_Local : Boolean := True) is
   begin
      Append (Group.Attributes, Attribute_Validator (Attr),
              Is_Local => Is_Local, Override => True);
   end Add_Attribute;

   -------------------------
   -- Add_Attribute_Group --
   -------------------------

   procedure Add_Attribute_Group
     (Validator : access XML_Validator_Record;
      Reader    : access Abstract_Validation_Reader'Class;
      Group     : XML_Attribute_Group) is
   begin
      Append (Reader, Validator.Attributes, Group);
   end Add_Attribute_Group;

   -------------------------
   -- Add_Attribute_Group --
   -------------------------

   procedure Add_Attribute_Group
     (Group  : in out XML_Attribute_Group;
      Reader : access Abstract_Validation_Reader'Class;
      Attr   : XML_Attribute_Group) is
   begin
      if Attr = null then
         if Debug then
            Debug_Output ("Add_Attribute_Group: adding empty attribute group");
         end if;
      end if;
      Append (Reader, Group.Attributes, Attr);
   end Add_Attribute_Group;

   --------------
   -- Get_Name --
   --------------

   function Get_Name
     (Validator : access XML_Validator_Record'Class) return String is
   begin
      if Validator.Debug_Name = null then
         return External_Tag (Validator'Tag);
      else
         return Validator.Debug_Name.all;
      end if;
   end Get_Name;

   ----------------------------
   -- Validate_Start_Element --
   ----------------------------

   procedure Validate_Start_Element
     (Validator         : access XML_Validator_Record;
      Reader            : access Abstract_Validation_Reader'Class;
      Local_Name        : Unicode.CES.Byte_Sequence;
      Namespace_URI     : Unicode.CES.Byte_Sequence;
      NS                : XML_Grammar_NS;
      Data              : Validator_Data;
      Grammar           : XML_Grammar;
      Element_Validator : out XML_Element)
   is
      pragma Unreferenced (Validator, Data, Namespace_URI, NS, Grammar);
   begin
      Validation_Error
        (Reader, "No definition found for """ & Local_Name & """");
      Element_Validator := No_Element;
   end Validate_Start_Element;

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
     (Validator : access XML_Validator_Record;
      Reader    : access Abstract_Validation_Reader'Class;
      Atts      : in out Sax.Attributes.Attributes'Class;
      Nillable  : Boolean;
      Is_Nil    : out Boolean)
   is
      Length   : constant Natural := Get_Length (Atts);

      type Attr_Status is record
         Prohibited : Boolean := False;
         --  Prohibited explicitly, but it might be allowed through
         --  <anyAttribute>

         Seen  : Boolean := False;
      end record;
      Seen : array (0 .. Length - 1) of Attr_Status :=
        (others => (False, False));

      type Any_Status is (Any_None, Any_All, Any_Not_All);
      type Any_Status_Array is array (0 .. Length - 1) of Any_Status;
      Seen_Any : Any_Status_Array := (others => Any_None);

      use QName_Attributes_Htable;
      Visited : QName_Attributes_Htable.HTable (101);

      function Find_Attribute
        (Named : Named_Attribute_Validator;
         Is_Local_In_XSD : Boolean) return Integer;
      --  Chech whether Named appears in Atts

      procedure Recursive_Check
        (Validator       : XML_Validator;
         Ignore_Wildcard : Boolean;
         Must_Match_All_Any : Boolean);
      procedure Recursive_Check_Named (List : Attribute_Or_Group);
      procedure Check_Any
        (List : Attribute_Or_Group; Must_Match_All_Any : Boolean);
      --  Check recursively the attributes provided by Validator.

      procedure Check_Named_Attribute
        (Named : Named_Attribute_Validator;
         Is_Local_In_XSD : Boolean);
      procedure Check_Any_Attribute
        (Any : Any_Attribute_Validator; Index : Integer);
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
         for A in 0 .. Length - 1 loop
            if Get_Type (Atts, A) = Sax.Attributes.Id then
               if Seen_ID then
                  Validation_Error
                    (Reader,
                     "Elements can have a single ID attribute in XSD 1.0");
               end if;

               Seen_ID := True;
            end if;
         end loop;
      end Check_Single_ID;

      ---------------------------
      -- Check_Named_Attribute --
      ---------------------------

      procedure Check_Named_Attribute
        (Named : Named_Attribute_Validator;
         Is_Local_In_XSD : Boolean)
      is
         Found  : Integer;
      begin
         if Get (Visited, To_QName (Named)) = null then
            Set (Visited, Named);
            Found := Find_Attribute (Named, Is_Local_In_XSD);

            if Found = -1 then
               case Named.Attribute_Use is
                  when Required =>
                     Validation_Error
                       (Reader, "Attribute """ & Named.Local_Name.all
                        & """ is required in this context");
                  when Prohibited | Optional | Default =>
                     null;
               end case;

            else
               Seen (Found).Seen := True;

               case Named.Attribute_Form is
                  when Qualified =>
                     if Is_Local_In_XSD
                       and then Get_Prefix (Atts, Found) = ""
                     then
                        Validation_Error
                          (Reader, "Attribute " & Get_Qname (Atts, Found)
                           & " must have a namespace");
                     end if;

                  when Unqualified =>
                     if Is_Local_In_XSD
                       and then Get_Prefix (Atts, Found) /= ""
                       and then Get_URI (Atts, Found) =
                       Get_Namespace_URI
                         (Get (Reader.Grammar).Target_NS)
                     then
                        Validation_Error
                          (Reader, "Attribute " & Get_Qname (Atts, Found)
                           & " must not have a namespace");
                     end if;
               end case;

               case Named.Attribute_Use is
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

                     Normalize_Whitespace
                       (Get_Type (Named.all), Reader, Atts, Found);

                     begin
                        Validate_Attribute (Named.all, Reader, Atts, Found);
                     exception
                        when E : XML_Validation_Error =>
                           Validation_Error
                             (Reader, "Attribute """ & Get_Qname (Atts, Found)
                              & """: " & Exception_Message (E));
                     end;
               end case;
            end if;
         end if;
      end Check_Named_Attribute;

      -------------------------
      -- Check_Any_Attribute --
      -------------------------

      procedure Check_Any_Attribute
        (Any   : Any_Attribute_Validator;
         Index : Integer) is
      begin
         if Debug then
            Debug_Push_Prefix ("Checking any attribute index="
                               & Index'Img
                               & " name={" & Get_URI (Atts, Index)
                               & "}" & Get_Local_Name (Atts, Index));
         end if;

         Validate_Attribute (Any, Reader, Atts, Index);

         Debug_Pop_Prefix;

      exception
         when E : XML_Validation_Error =>
            Debug_Pop_Prefix;
            Validation_Error
              (Reader, "Attribute """ & Get_Qname (Atts, Index)
               & """: " & Exception_Message (E));
      end Check_Any_Attribute;

      --------------------
      -- Find_Attribute --
      --------------------

      function Find_Attribute
        (Named : Named_Attribute_Validator;
         Is_Local_In_XSD : Boolean) return Integer is
      begin
         for A in 0 .. Length - 1 loop
            if not Seen (A).Seen
              and then Get_Local_Name (Atts, A) = Named.Local_Name.all
              and then ((Is_Local_In_XSD and Get_Prefix (Atts, A) = "")
                        or else Get_URI (Atts, A) = Named.NS.Namespace_URI.all)
            then
               if Debug then
                  Debug_Output ("Found attribute: "
                                & Named.NS.Namespace_URI.all
                                & ':' & Named.Local_Name.all);
               end if;
               return A;
            end if;
         end loop;
         return -1;
      end Find_Attribute;

      ---------------------------
      -- Recursive_Check_Named --
      ---------------------------

      procedure Recursive_Check_Named (List : Attribute_Or_Group) is
      begin
         if List.Is_Group then
            if List.Group.Attributes /= null then
               for L in List.Group.Attributes'Range loop
                  Recursive_Check_Named (List.Group.Attributes (L));
               end loop;
            end if;

         elsif List.Attr.all in Named_Attribute_Validator_Record'Class then
            Check_Named_Attribute
              (Named_Attribute_Validator (List.Attr),
               Is_Local_In_XSD => List.Is_Local);
         end if;
      end Recursive_Check_Named;

      ---------------
      -- Check_Any --
      ---------------

      procedure Check_Any
        (List : Attribute_Or_Group; Must_Match_All_Any : Boolean) is
      begin
         if List.Is_Group then
            if List.Group.Attributes /= null then
               for A in List.Group.Attributes'Range loop
                  Check_Any (List.Group.Attributes (A), Must_Match_All_Any);
               end loop;
            end if;

         elsif List.Attr.all in Any_Attribute_Validator'Class then
            --  From 3.4.2 (intersection of anyAttribute), an attribute must
            --  match *all* the anyAttribute, so we do not modify Seen yet, so
            --  that the attribute is tested multiple times

            for A in 0 .. Length - 1 loop
               if not Seen (A).Seen
                 and then (Must_Match_All_Any
                           or else Seen_Any (A) /= Any_All)
               then
                  begin
                     Check_Any_Attribute
                       (Any_Attribute_Validator (List.Attr.all), A);

                     --  If there was an exception, don't mark the attribute as
                     --  seen, it is invalid. Maybe another <anyAttribute> will
                     --  match

                     case Seen_Any (A) is
                        when Any_None | Any_All => Seen_Any (A) := Any_All;
                        when Any_Not_All        => null;
                     end case;

                  exception
                     when XML_Validation_Error =>
                        if Must_Match_All_Any then
                           Seen_Any (A) := Any_Not_All;
                        end if;
                  end;
               end if;
            end loop;
         end if;
      end Check_Any;

      ---------------------
      -- Recursive_Check --
      ---------------------

      procedure Recursive_Check
        (Validator       : XML_Validator;
         Ignore_Wildcard : Boolean;
         Must_Match_All_Any : Boolean)
      is
         List   : Attribute_Validator_List_Access;
         Dep1, Dep2 : XML_Validator;
         Ignore_Dep1_Wildcard : Boolean;
         Must_Match_All_Any2 : Boolean;
      begin
         Get_Attribute_Lists
           (Validator, List,
            Dep1, Ignore_Dep1_Wildcard, Dep2, Must_Match_All_Any2);

         if Debug then
            Debug_Push_Prefix
              ("Checking attributes from " & Get_Name (Validator)
               & " ignore_wildcards=" & Ignore_Wildcard'Img
               & " all_any=" & Must_Match_All_Any2'Img);
         end if;

         if List /= null then
            for L in List'Range loop
               Recursive_Check_Named (List (L));
            end loop;
         end if;

         if Dep1 /= null then
            Recursive_Check (Dep1, Ignore_Wildcard or Ignore_Dep1_Wildcard,
                             Must_Match_All_Any2);
         end if;

         if Dep2 /= null then
            Recursive_Check (Dep2, Ignore_Wildcard, Must_Match_All_Any2);
         end if;

         if List /= null and then not Ignore_Wildcard then
            --  If the policy for <anyAttribute> has changed, we restart from
            --  scratch: we need to ensure that within the current validator
            --  (and its dependencies), all <anyAttribute> matches for a given
            --  attribute (or not). This computation should not be influence by
            --  validators seen previously.

            if Must_Match_All_Any2 /= Must_Match_All_Any then
               declare
                  Saved_Seen_Any : constant Any_Status_Array := Seen_Any;
               begin
                  for S in Seen_Any'Range loop
                     if Seen_Any (S) = Any_Not_All then
                        Seen_Any (S) := Any_None;
                     end if;
                  end loop;

                  for L in List'Range loop
                     Check_Any (List (L), Must_Match_All_Any2);
                  end loop;

                  for S in Seen_Any'Range loop
                     if Seen_Any (S) /= Any_All then
                        Seen_Any (S) := Saved_Seen_Any (S);
                     end if;
                  end loop;
               end;
            else
               for L in List'Range loop
                  Check_Any (List (L), Must_Match_All_Any2);
               end loop;
            end if;
         end if;

         Debug_Pop_Prefix;

      exception
         when others =>
            Debug_Pop_Prefix;
            raise;
      end Recursive_Check;

   begin
      Debug_Push_Prefix ("Validate_Attributes " & Get_Name (Validator));

      Recursive_Check (XML_Validator (Validator), Ignore_Wildcard => False,
                       Must_Match_All_Any => False);

      Is_Nil := False;

      for S in Seen'Range loop
         if not Seen (S).Seen and then Seen_Any (S) /= Any_All then
            if Get_URI (Atts, S) = XML_Instance_URI then
               if Get_Local_Name (Atts, S) = "nil" then
                  if not Nillable then
                     Validation_Error (Reader, "Element cannot be nil");
                  end if;

                  Is_Nil := Get_Value_As_Boolean (Atts, S);

                  --  Following attributes are always valid
                  --  See "Element Locally Valid (Complex Type)" 3.4.4.2
               elsif Get_Local_Name (Atts, S) = "type"
                 or else Get_Local_Name (Atts, S) = "schemaLocation"
                 or else Get_Local_Name (Atts, S) = "noNamespaceSchemaLocation"
               then
                  null;

               else
                  Validation_Error
                    (Reader, "Attribute """ & Get_Qname (Atts, S)
                     & """ invalid for this element");
               end if;

            elsif Seen (S).Prohibited then
               Validation_Error
                 (Reader, "Attribute """ & Get_Qname (Atts, S)
                  & """ is prohibited in this context");

            else
               Validation_Error
                 (Reader, "Attribute """ & Get_Qname (Atts, S)
                  & """ invalid for this element");
            end if;
         end if;
      end loop;

      Check_Single_ID;

      Reset (Visited);

      Debug_Pop_Prefix;

   exception
      when others =>
         Debug_Pop_Prefix;
         Reset (Visited);
         raise;
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

   --------------------------
   -- Validate_End_Element --
   --------------------------

   procedure Validate_End_Element
     (Validator      : access XML_Validator_Record;
      Reader         : access Abstract_Validation_Reader'Class;
      Local_Name     : Unicode.CES.Byte_Sequence;
      Data           : Validator_Data)
   is
      pragma Unreferenced (Validator, Local_Name, Data, Reader);
   begin
      null;
   end Validate_End_Element;

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

   function List_Of
     (Grammar : XML_Grammar_NS; Typ : XML_Type) return XML_Type is
   begin
      return Create_Local_Type (Grammar, List_Of (Typ));
   end List_Of;

   ---------------
   -- Add_Facet --
   ---------------

   procedure Add_Facet
     (Validator   : access XML_Validator_Record;
      Reader      : access Abstract_Validation_Reader'Class;
      Facet_Name  : Unicode.CES.Byte_Sequence;
      Facet_Value : Unicode.CES.Byte_Sequence)
   is
      pragma Unreferenced (Validator);
   begin
      if Facet_Name = "whiteSpace" then
         if Facet_Value /= "collapse" then
            Validation_Error
              (Reader,
               "Invalid value for restriction whiteSpace: " & Facet_Value);
         end if;
      else
         Validation_Error (Reader, "Invalid restriction: " & Facet_Name);
      end if;
   end Add_Facet;

   ----------------------------
   -- Create_Local_Attribute --
   ----------------------------

   function Create_Local_Attribute
     (Local_Name     : Unicode.CES.Byte_Sequence;
      NS             : XML_Grammar_NS;
      Attribute_Type : XML_Type                  := No_Type;
      Attribute_Form : Form_Type                 := Unqualified;
      Attribute_Use  : Attribute_Use_Type        := Optional;
      Fixed          : Unicode.CES.Byte_Sequence := "";
      Has_Fixed      : Boolean := False;
      Value          : Unicode.CES.Byte_Sequence := "")
      return Attribute_Validator
   is
      Result : constant Attribute_Validator :=
        new Named_Attribute_Validator_Record'
          (Local_Name     => new Unicode.CES.Byte_Sequence'(Local_Name),
           NS             => NS,
           Ref_Attr       => null,
           Attribute_Type => Attribute_Type,
           Attribute_Form => Attribute_Form,
           Attribute_Use  => Attribute_Use,
           Fixed          => null,
           Value          => new Unicode.CES.Byte_Sequence'(Value),
           Next           => null);

   begin
      if Has_Fixed then
         Named_Attribute_Validator_Record (Result.all).Fixed :=
           new Unicode.CES.Byte_Sequence'(Fixed);
      end if;

      Register (NS, Result);
      return Result;
   end Create_Local_Attribute;

   ----------------------------
   -- Create_Local_Attribute --
   ----------------------------

   function Create_Local_Attribute
     (Based_On       : Attribute_Validator;
      Attribute_Form : Form_Type                 := Unqualified;
      Attribute_Use  : Attribute_Use_Type        := Optional;
      Fixed          : Unicode.CES.Byte_Sequence := "";
      Has_Fixed      : Boolean := False;
      Value          : Unicode.CES.Byte_Sequence := "")
      return Attribute_Validator
   is
      --  We cannot extract the type from Based_On yet, because it might not
      --  have been defined yet in the grammar.

      Result : constant Attribute_Validator :=
        new Named_Attribute_Validator_Record'
          (Local_Name     => new Unicode.CES.Byte_Sequence'
               (Named_Attribute_Validator (Based_On).Local_Name.all),
           NS             => Based_On.NS,
           Ref_Attr       => Named_Attribute_Validator (Based_On),
           Attribute_Type => No_Type,
           Attribute_Form => Attribute_Form,
           Attribute_Use  => Attribute_Use,
           Fixed          => null,
           Value          => new Unicode.CES.Byte_Sequence'(Value),
           Next           => null);

   begin
      if Has_Fixed then
         Named_Attribute_Validator_Record (Result.all).Fixed :=
           new Unicode.CES.Byte_Sequence'(Fixed);
      end if;

      Register (Based_On.NS, Result);
      return Result;
   end Create_Local_Attribute;

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

   -----------
   -- Equal --
   -----------

   function Equal
     (Validator      : Named_Attribute_Validator_Record;
      Reader         : access Abstract_Validation_Reader'Class;
      Value1, Value2 : Unicode.CES.Byte_Sequence) return Boolean is
   begin
      if Validator.Attribute_Type = No_Type then
         return Value1 = Value2;
      else
         return Equal
           (Get_Validator (Validator.Attribute_Type), Reader, Value1, Value2);
      end if;
   end Equal;

   function Equal
     (Validator      : Any_Attribute_Validator;
      Reader         : access Abstract_Validation_Reader'Class;
      Value1, Value2 : Unicode.CES.Byte_Sequence) return Boolean
   is
      pragma Unreferenced (Validator, Reader);
   begin
      return Value1 = Value2;
   end Equal;

   ------------------------
   -- Validate_Attribute --
   ------------------------

   procedure Validate_Attribute
     (Validator : Named_Attribute_Validator_Record;
      Reader    : access Abstract_Validation_Reader'Class;
      Atts      : in out Sax.Attributes.Attributes'Class;
      Index     : Natural)
   is
      Val   : constant Byte_Sequence := Get_Value (Atts, Index);
      Fixed : Byte_Sequence_Access;
      Mask  : Facets_Mask := (others => True);
   begin
      if Debug then
         Debug_Output ("Checking attribute "
                       & Validator.Local_Name.all
                       & "=" & Val & "--");
      end if;

      if Get_Type (Validator) /= No_Type then
         Validate_Characters
           (Get_Validator (Get_Type (Validator)), Reader, Val,
            Empty_Element => False, Mask => Mask);

         if Is_ID (Get_Type (Validator)) then
            Set_Type (Atts, Index, Sax.Attributes.Id);
         end if;
      end if;

      Fixed := Validator.Fixed;
      if Fixed = null and then Validator.Ref_Attr /= null then
         Fixed := Validator.Ref_Attr.Fixed;
      end if;

      if Debug and then Fixed /= null then
         Debug_Output ("Attribute value must be equal to """
                       & Fixed.all & """");
      end if;

      --  3.2.4 [Attribute Declaration Value] indicates we should check Fixed
      --  with the "actual value" of the attribute, not the "normalized value".
      --  However, we need to match depending on the type of the attribute: if
      --  it is an integer, the whitespaces are irrelevant; likewise for a list

      if Fixed /= null
        and then not Equal (Validator, Reader, Fixed.all, Val)
      then
         Validation_Error
           (Reader, "value must be """
            & To_Graphic_String (Fixed.all)
            & """ (found """ & To_Graphic_String (Val) & """)");
      end if;
   end Validate_Attribute;

   ----------
   -- Free --
   ----------

   procedure Free (Validator : in out XML_Validator_Record) is
   begin
      Free (Validator.Debug_Name);
      Free (Validator.Attributes);
   end Free;

   ------------
   -- Lookup --
   ------------

   function Lookup
     (Grammar    : XML_Grammar_NS;
      Reader     : access Abstract_Validation_Reader'Class;
      Local_Name : Unicode.CES.Byte_Sequence;
      Create_If_Needed : Boolean := True) return XML_Type
   is
      Typ : XML_Type := Types_Htable.Get (Grammar.Types.all, Local_Name);
   begin
      if Typ = No_Type and then Create_If_Needed then
         if Local_Name = "precisionDecimal"
           and then Get_Namespace_URI (Grammar) = XML_Schema_URI
         then
            Raise_Exception
              (XML_Not_Implemented'Identity,
               "Unsupported type: precisionDecimal");
         end if;

         if Grammar.Checked then
            Validation_Error
              (Reader, "Declaration not found for "
               & To_QName (Get_Namespace_URI (Grammar), Local_Name));
         end if;

         Typ := new XML_Type_Record'
           (Local_Name        => new Byte_Sequence'(Local_Name),
            Validator         => null,
            Simple_Type       => Unknown_Content,
            Blocks            => Get_Block_Default (Grammar),
            Final_Extension   => False,
            Final_Restriction => False,
            Next              => null);
         Types_Htable.Set (Grammar.Types.all, Typ);
         Register (Grammar, Typ);
         if Debug then
            Debug_Output
              ("Forward type decl: "
               & To_QName (Get_Namespace_URI (Grammar), Local_Name));
         end if;
      elsif Typ = No_Type then
         if Debug then
            Debug_Output ("Type not found: " & Local_Name);
         end if;
      end if;

      return Typ;
   end Lookup;

   --------------------
   -- Lookup_Element --
   --------------------

   function Lookup_Element
     (Grammar    : XML_Grammar_NS;
      Reader     : access Abstract_Validation_Reader'Class;
      Local_Name : Unicode.CES.Byte_Sequence;
      Create_If_Needed : Boolean := True) return XML_Element
   is
      Result : constant XML_Element_Access := Elements_Htable.Get
        (Grammar.Elements.all, Local_Name);
   begin
      if Result = null then
         if Create_If_Needed then
            if Grammar.Checked then
               Validation_Error
                 (Reader, "Declaration not found for "
                  & To_QName (Get_Namespace_URI (Grammar), Local_Name));
            end if;

            if Debug then
               Debug_Output ("Lookup_Element: creating forward "
                             & Grammar.Namespace_URI.all & " : "
                             & Local_Name);
            end if;
            return Create_Global_Element
              (Grammar, Reader, Local_Name, Form => Unqualified);
         else
            return No_Element;
         end if;
      end if;
      return (Elem => Result, Is_Ref => True);
   end Lookup_Element;

   ------------------
   -- Lookup_Group --
   ------------------

   function Lookup_Group
     (Grammar    : XML_Grammar_NS;
      Reader     : access Abstract_Validation_Reader'Class;
      Local_Name : Unicode.CES.Byte_Sequence) return XML_Group
   is
      Result : XML_Group := Groups_Htable.Get (Grammar.Groups.all, Local_Name);
   begin
      if Result = No_XML_Group then
         if Grammar.Checked then
            Validation_Error
              (Reader, "Declaration not found for "
               & To_QName (Get_Namespace_URI (Grammar), Local_Name));
         end if;

         Result := Create_Global_Group (Grammar, Reader, Local_Name);
         Result.Is_Forward_Decl := True;
      end if;
      return Result;
   end Lookup_Group;

   ----------------------------
   -- Lookup_Attribute_Group --
   ----------------------------

   function Lookup_Attribute_Group
     (Grammar    : XML_Grammar_NS;
      Reader     : access Abstract_Validation_Reader'Class;
      Local_Name : Unicode.CES.Byte_Sequence) return XML_Attribute_Group
   is
      Result : XML_Attribute_Group :=
        Attribute_Groups_Htable.Get (Grammar.Attribute_Groups.all, Local_Name);
   begin
      if Result = Empty_Attribute_Group then
         if Grammar.Checked then
            Validation_Error
              (Reader, "Declaration not found for "
               & To_QName (Get_Namespace_URI (Grammar), Local_Name));
         end if;

         if Debug then
            Debug_Output ("Lookup_Attribute_Group: Create forward decl");
         end if;
         Result := Create_Global_Attribute_Group (Grammar, Reader, Local_Name);
         Result.Is_Forward_Decl := True;
      end if;
      return Result;
   end Lookup_Attribute_Group;

   ----------------------
   -- Lookup_Attribute --
   ----------------------

   function Lookup_Attribute
     (Grammar       : XML_Grammar_NS;
      Reader        : access Abstract_Validation_Reader'Class;
      Local_Name    : Unicode.CES.Byte_Sequence;
      Create_If_Needed : Boolean := True) return Attribute_Validator
   is
      Result : constant Named_Attribute_Validator :=
        Attributes_Htable.Get (Grammar.Attributes.all, Local_Name);
   begin
      if Result = null and then Create_If_Needed then
         if Grammar.Checked then
            Validation_Error
              (Reader, "Declaration not found for "
               & To_QName (Get_Namespace_URI (Grammar), Local_Name));
         end if;

         return Create_Global_Attribute (Grammar, Reader, Local_Name, No_Type);
      end if;
      return Attribute_Validator (Result);
   end Lookup_Attribute;

   --------------
   -- To_QName --
   --------------

   function To_QName (Self : Named_Attribute_Validator) return Byte_Sequence is
   begin
      return To_QName (Get_Namespace_URI (Self.NS), Self.Local_Name.all);
   end To_QName;

   --------------
   -- To_QName --
   --------------

   function To_QName
     (Element : XML_Element) return Unicode.CES.Byte_Sequence
   is
   begin
      return To_QName
        (Get_Namespace_URI (Element.Elem.NS), Element.Elem.Local_Name.all);
   end To_QName;

   --------------
   -- To_QName --
   --------------

   function To_QName (Typ : XML_Type) return Unicode.CES.Byte_Sequence is
   begin
      if Typ = No_Type then
         return "???";
      elsif Typ.Local_Name = null then
         if Typ.Validator /= null then
            return "anonymous/" & Get_Name (Typ.Validator);
         else
            return "anonymous";
         end if;
      else
         return Typ.Local_Name.all;
      end if;
   end To_QName;

   --------------
   -- Get_Type --
   --------------

   function Get_Type (Element : XML_Element) return XML_Type is
   begin
      if Element.Elem = null then
         return null;
      else
         return Element.Elem.Of_Type;
      end if;
   end Get_Type;

   --------------
   -- Set_Type --
   --------------

   procedure Set_Type
     (Attr      : access Attribute_Validator_Record;
      Attr_Type : XML_Type)
   is
      pragma Unreferenced (Attr, Attr_Type);
   begin
      null;
   end Set_Type;

   --------------
   -- Get_Type --
   --------------

   function Get_Type
     (Attr : Attribute_Validator_Record) return XML_Type
   is
      pragma Unreferenced (Attr);
   begin
      return No_Type;
   end Get_Type;

   --------------
   -- Set_Type --
   --------------

   procedure Set_Type
     (Attr      : access Named_Attribute_Validator_Record;
      Attr_Type : XML_Type) is
   begin
      Attr.Attribute_Type := Attr_Type;
   end Set_Type;

   --------------
   -- Get_Type --
   --------------

   function Get_Type
     (Attr : Named_Attribute_Validator_Record) return XML_Type is
   begin
      if Attr.Attribute_Type = null
        and then Attr.Ref_Attr /= null
      then
         return Attr.Ref_Attr.Attribute_Type;
      end if;

      return Attr.Attribute_Type;
   end Get_Type;

   --------------
   -- Set_Type --
   --------------

   procedure Set_Type
     (Element      : XML_Element;
      Reader       : access Abstract_Validation_Reader'Class;
      Element_Type : XML_Type)
   is
      Mask : Facets_Mask;
   begin
      if Element /= No_Element then
         if Element.Is_Ref then
            Validation_Error
              (Reader,
               "Cannot mix complexType definition and ""ref"" attribute");
         end if;

         Element.Elem.Of_Type := Element_Type;

         --  3.3 Element Declaration details:  Validation Rule 3.1
         --  The "default" attribute of element must match the validation rule
         --  for that element

         if Element.Elem.Default /= null then
            Mask := (others => True);
            Validate_Characters
              (Get_Validator (Element_Type), Reader, Element.Elem.Default.all,
               Empty_Element => False, Mask => Mask);
         end if;

         --  3.3 Element Declaration details:  Validation Rule 3.1
         --  The "fixed" attribute of element must match the validation rule
         --  for that element

         if Element.Elem.Fixed /= null then
            Mask := (others => True);
            Validate_Characters
              (Get_Validator (Element_Type), Reader, Element.Elem.Fixed.all,
               Empty_Element => False, Mask => Mask);
         end if;
      end if;
   end Set_Type;

   ------------
   -- Get_NS --
   ------------

   procedure Get_NS
     (Grammar       : XML_Grammar;
      Namespace_URI : Unicode.CES.Byte_Sequence;
      Result        : out XML_Grammar_NS;
      Create_If_Needed : Boolean := True)
   is
   begin
      if Grammar /= No_Grammar and then Get (Grammar).Grammars /= null then
         for G in Get (Grammar).Grammars'Range loop
            if Get (Grammar).Grammars (G).Namespace_URI.all =
              Namespace_URI
            then
               Result := Get (Grammar).Grammars (G);
               return;
            end if;
         end loop;
      end if;

      if Create_If_Needed then
         Create_NS_Grammar (Grammar, Namespace_URI);
         Result := Get (Grammar).Grammars (Get (Grammar).Grammars'Last);
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
         Free (List.Str);
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

   procedure Create_NS_Grammar
     (Grammar       : XML_Grammar;
      Namespace_URI : Unicode.CES.Byte_Sequence)
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
         Debug_Output ("Create_NS_Grammar: " & Namespace_URI);
      end if;

      G.Grammars (G.Grammars'Last) := new XML_Grammar_NS_Record'
        (Namespace_URI      => new Byte_Sequence'(Namespace_URI),
         System_ID          => null,
         Types              => new Types_Htable.HTable (101),
         Elements           => new Elements_Htable.HTable (101),
         Groups             => new Groups_Htable.HTable (101),
         Attributes         => new Attributes_Htable.HTable (101),
         Attribute_Groups   => new Attribute_Groups_Htable.HTable (101),
         Checked            => False,
         Validators_For_Mem => null,
         Types_For_Mem      => null,
         Atts_For_Mem       => null,
         Elems_For_Mem      => null,
         Blocks             => (others => False));
   end Create_NS_Grammar;

   --------------
   -- Register --
   --------------

   procedure Register
     (NS        : XML_Grammar_NS;
      Validator : access XML_Validator_Record'Class) is
   begin
      --  We make the last element of the list point to itself, so that
      --  we can cheaply detect whether an element is already chained.

      if Validator.Next = null then
         Validator.Next         := NS.Validators_For_Mem;
         NS.Validators_For_Mem  := XML_Validator (Validator);
         if Validator.Next = null then
            Validator.Next := XML_Validator (Validator);
         end if;
      end if;
   end Register;

   procedure Register
     (NS  : XML_Grammar_NS;
      Typ : access XML_Type_Record) is
   begin
      if Typ.Next = null then
         Typ.Next         := NS.Types_For_Mem;
         NS.Types_For_Mem := XML_Type (Typ);
         if Typ.Next = null then
            Typ.Next := XML_Type (Typ);
         end if;
      end if;
   end Register;

   procedure Register
     (NS   : XML_Grammar_NS;
      Attr : access Attribute_Validator_Record'Class) is
   begin
      if Attr.Next = null then
         Attr.Next := NS.Atts_For_Mem;
         NS.Atts_For_Mem := Attribute_Validator (Attr);
         if Attr.Next = null then
            Attr.Next := Attribute_Validator (Attr);
         end if;
      end if;
   end Register;

   procedure Register
     (NS      : XML_Grammar_NS;
      Element : access XML_Element_Record) is
   begin
      if Element.Next = null then
         Element.Next := NS.Elems_For_Mem;
         NS.Elems_For_Mem := XML_Element_Access (Element);
         if Element.Next = null then
            Element.Next := XML_Element_Access (Element);
         end if;
      end if;
   end Register;

   ------------------------
   -- Initialize_Grammar --
   ------------------------

   procedure Initialize_Grammar
     (Reader  : access Abstract_Validation_Reader'Class)
   is
      Actual_G : XML_Grammars.Encapsulated_Access;
   begin
      if Reader.Grammar = No_Grammar then
         Actual_G := new XML_Grammar_Record;
         Reader.Grammar  := Allocate (Actual_G);
      end if;

      if Get (Reader.Grammar).Grammars = null then
         Add_Schema_For_Schema (Reader);
      end if;
   end Initialize_Grammar;

   ----------------
   -- Debug_Dump --
   ----------------

   procedure Debug_Dump (Grammar : XML_Grammar) is
      use Elements_Htable;
      Str : String_List;
      Elem : Elements_Htable.Iterator;
      G    : constant XML_Grammars.Encapsulated_Access := Get (Grammar);
   begin
      if Debug then
         Str := G.Parsed_Locations;
         while Str /= null loop
            Put_Line ("   Parsed location: " & Str.Str.all);
            Str := Str.Next;
         end loop;

         if G.Grammars /= null then
            for NS in G.Grammars'Range  loop
               Put_Line ("   NS=" & G.Grammars (NS).Namespace_URI.all);

               Put ("      Elements:");
               Elem := First (G.Grammars (NS).Elements.all);
               while Elem /= Elements_Htable.No_Iterator loop
                  Put (' ' & Elements_Htable.Current (Elem).Local_Name.all);
                  Next (G.Grammars (NS).Elements.all, Elem);
               end loop;
               New_Line;

            end loop;
         end if;

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

   --------------------------
   -- Create_Local_Element --
   --------------------------

   function Create_Local_Element
     (Local_Name : Unicode.CES.Byte_Sequence;
      NS         : XML_Grammar_NS;
      Of_Type    : XML_Type;
      Form       : Form_Type) return XML_Element
   is
      Ptr : constant XML_Element_Access := new XML_Element_Record'
        (Local_Name          => new Unicode.CES.Byte_Sequence'(Local_Name),
         NS                  => NS,
         Substitution_Group  => No_Element,
         Of_Type             => Of_Type,
         Default             => null,
         Is_Abstract         => False,
         Nillable            => True,
         Final_Restriction   => False,
         Final_Extension     => False,
         Blocks_Is_Set       => False,
         Blocks              => (others => False),
         Is_Global           => False,
         Form                => Form,
         Fixed               => null,
         Next                => null);
   begin
      --  ??? Should be stored in a list in the grammar, so that we can free
      --  them all later on.
      Register (NS, Ptr);
      return
        (Elem   => Ptr,
         Is_Ref => False);
   end Create_Local_Element;

   -------------------
   -- Redefine_Type --
   -------------------

   function Redefine_Type
     (Grammar : XML_Grammar_NS; Local_Name : Byte_Sequence) return XML_Type
   is
      Old : constant XML_Type := Types_Htable.Get
        (Grammar.Types.all, Local_Name);
      Result : XML_Type;
   begin
      if Old /= No_Type then
         Result := Create_Local_Type (Grammar, Get_Validator (Old));
         Old.Validator := null;
         return Result;
      end if;
      return No_Type;
   end Redefine_Type;

   --------------------
   -- Redefine_Group --
   --------------------

   function Redefine_Group
     (Grammar : XML_Grammar_NS; Local_Name : Byte_Sequence) return XML_Group
   is
      Old : constant XML_Group := Groups_Htable.Get
        (Grammar.Groups.all, Local_Name);
--        Result : XML_Group;
   begin
      if Old /= No_XML_Group then
         Old.Is_Forward_Decl := True;
         return Old;
--           Result := new XML_Group_Record'(Old.all);
--           Old.all := (Particles       => Empty_Particle_List,
--                       Local_Name      => new Byte_Sequence'(Local_Name),
--                       Is_Forward_Decl => True);
--           Groups_Htable.Set (Grammar.Groups.all, Result);
--           return Result;
      end if;
      return No_XML_Group;
   end Redefine_Group;

   ---------------------------
   -- Create_Global_Element --
   ---------------------------

   function Create_Global_Element
     (Grammar    : XML_Grammar_NS;
      Reader     : access Abstract_Validation_Reader'Class;
      Local_Name : Unicode.CES.Byte_Sequence;
      Form       : Form_Type) return XML_Element
   is
      Old : XML_Element_Access := Elements_Htable.Get
        (Grammar.Elements.all, Local_Name);
   begin
      if Old /= null then
         if Old.Of_Type /= No_Type then
            Validation_Error
              (Reader,
               "Element """ & Local_Name & """ has already been declared");
         end if;

         Old.Form := Form;
      else
         Old := new XML_Element_Record'
           (Local_Name          => new Unicode.CES.Byte_Sequence'(Local_Name),
            NS                  => Grammar,
            Substitution_Group  => No_Element,
            Of_Type             => No_Type,
            Default             => null,
            Is_Abstract         => False,
            Nillable            => True,
            Final_Restriction   => False,
            Final_Extension     => False,
            Blocks_Is_Set       => False,
            Blocks              => Get_Block_Default (Grammar),
            Is_Global           => True,
            Form                => Form,
            Fixed               => null,
            Next                => null);
         Register (Grammar, Old);
         Elements_Htable.Set (Grammar.Elements.all, Old);
      end if;

      return (Elem => Old, Is_Ref => False);
   end Create_Global_Element;

   ------------------------
   -- Create_Global_Type --
   ------------------------

   function Create_Global_Type
     (Grammar    : XML_Grammar_NS;
      Reader     : access Abstract_Validation_Reader'Class;
      Local_Name : Unicode.CES.Byte_Sequence;
      Validator  : access XML_Validator_Record'Class) return XML_Type
   is
      Typ : XML_Type := Types_Htable.Get (Grammar.Types.all, Local_Name);
   begin
      if Typ /= No_Type then
         if Typ.Validator /= null then
            Validation_Error
              (Reader, "Type has already been declared: " & Local_Name);
         end if;

         if Debug then
            Debug_Output ("Overriding forward type " & Local_Name);
         end if;
         Register (Grammar, Validator);
         Typ.Validator := XML_Validator (Validator);

         if Typ.Simple_Type /= Unknown_Content then
            Check_Content_Type
              (Validator, Reader, Typ.Simple_Type = Simple_Content);
         end if;

      else
         Register (Grammar, Validator);
         Typ := new XML_Type_Record'
           (Local_Name        => new Byte_Sequence'(Local_Name),
            Validator         => XML_Validator (Validator),
            Simple_Type       => Unknown_Content,
            Blocks            => Get_Block_Default (Grammar),
            Final_Extension   => False,
            Final_Restriction => False,
            Next              => null);
         Types_Htable.Set (Grammar.Types.all, Typ);
         if Debug then
            Debug_Output ("Creating global type {"
                          & Get_Namespace_URI (Grammar) & '}'
                          & Local_Name);
         end if;
         Register (Grammar, Typ);

         if Debug and then Typ.Validator.Debug_Name = null then
            Set_Debug_Name (Typ.Validator, Local_Name);
         end if;
      end if;

      return Typ;
   end Create_Global_Type;

   ------------------------
   -- Create_Global_Type --
   ------------------------

   procedure Create_Global_Type
     (Grammar    : XML_Grammar_NS;
      Reader     : access Abstract_Validation_Reader'Class;
      Local_Name : Unicode.CES.Byte_Sequence;
      Validator  : access XML_Validator_Record'Class)
   is
      Typ : constant XML_Type :=
              Create_Global_Type (Grammar, Reader, Local_Name, Validator);
      pragma Unreferenced (Typ);
   begin
      null;
   end Create_Global_Type;

   -----------------------------
   -- Create_Global_Attribute --
   -----------------------------

   procedure Create_Global_Attribute
     (NS             : XML_Grammar_NS;
      Reader         : access Abstract_Validation_Reader'Class;
      Local_Name     : Unicode.CES.Byte_Sequence;
      Attribute_Type : XML_Type)
   is
      Att : constant Attribute_Validator :=
              Create_Global_Attribute (NS, Reader, Local_Name, Attribute_Type);
      pragma Unreferenced (Att);
   begin
      null;
   end Create_Global_Attribute;

   -----------------------------
   -- Create_Global_Attribute --
   -----------------------------

   function Create_Global_Attribute
     (NS             : XML_Grammar_NS;
      Reader         : access Abstract_Validation_Reader'Class;
      Local_Name     : Unicode.CES.Byte_Sequence;
      Attribute_Type : XML_Type;
      Attribute_Form : Form_Type                 := Qualified;
      Attribute_Use  : Attribute_Use_Type        := Optional;
      Fixed          : Unicode.CES.Byte_Sequence := "";
      Has_Fixed      : Boolean := False) return Attribute_Validator
   is
      Old : Named_Attribute_Validator :=
        Attributes_Htable.Get (NS.Attributes.all, Local_Name);
   begin
      if Old /= null then
         if Get_Type (Old.all) /= No_Type then
            Validation_Error
              (Reader, "Attribute has already been declared: " & Local_Name);
         end if;

         Old.Attribute_Type := Attribute_Type;
      else
         Old := new Named_Attribute_Validator_Record'
           (NS             => NS,
            Local_Name     => new Byte_Sequence'(Local_Name),
            Ref_Attr       => null,
            Attribute_Type => Attribute_Type,
            Attribute_Form => Attribute_Form,
            Attribute_Use  => Attribute_Use,
            Fixed          => null,
            Value          => null,
            Next           => null);

         Register (NS, Old);
         Attributes_Htable.Set (NS.Attributes.all, Old);
      end if;

      Old.Attribute_Form := Attribute_Form;
      Old.Attribute_Use  := Attribute_Use;

      if Has_Fixed then
         Old.Fixed := new Byte_Sequence'(Fixed);
      end if;
      return Attribute_Validator (Old);
   end Create_Global_Attribute;

   -------------------------
   -- Create_Global_Group --
   -------------------------

   function Create_Global_Group
     (Grammar    : XML_Grammar_NS;
      Reader     : access Abstract_Validation_Reader'Class;
      Local_Name : Unicode.CES.Byte_Sequence) return XML_Group
   is
      Group : XML_Group := Groups_Htable.Get (Grammar.Groups.all, Local_Name);
   begin
      if Group /= No_XML_Group then
         if not Group.Is_Forward_Decl then
            Validation_Error
              (Reader, "Group has already been declared: " & Local_Name);
         end if;

         Group.Is_Forward_Decl := False;
      else
         Group := new XML_Group_Record'
           (Local_Name      => new Byte_Sequence'(Local_Name),
            Particles       => Empty_Particle_List,
            Is_Forward_Decl => False);
         Groups_Htable.Set (Grammar.Groups.all, Group);
      end if;
      return Group;
   end Create_Global_Group;

   -----------------------------------
   -- Create_Global_Attribute_Group --
   -----------------------------------

   function Create_Global_Attribute_Group
     (NS         : XML_Grammar_NS;
      Reader     : access Abstract_Validation_Reader'Class;
      Local_Name : Unicode.CES.Byte_Sequence) return XML_Attribute_Group
   is
      Group : XML_Attribute_Group := Attribute_Groups_Htable.Get
        (NS.Attribute_Groups.all, Local_Name);
   begin
      if Group /= Empty_Attribute_Group then
         if not Group.Is_Forward_Decl then
            Validation_Error
              (Reader,
               "Attribute group has already been declared: " & Local_Name);
         end if;

         Group.Is_Forward_Decl := False;
      else
         Group := new XML_Attribute_Group_Record'
           (Local_Name => new Unicode.CES.Byte_Sequence'(Local_Name),
            Attributes => null,
            Is_Forward_Decl => False);
         Attribute_Groups_Htable.Set (NS.Attribute_Groups.all, Group);
      end if;

      return Group;
   end Create_Global_Attribute_Group;

   ----------
   -- Free --
   ----------

   procedure Free (Grammar : in out XML_Grammar_Record) is
   begin
      if Debug then
         Put_Line ("Freeing grammar");
      end if;
      if Grammar.Grammars /= null then
         for NS in Grammar.Grammars'Range loop
            Free (Grammar.Grammars (NS));
         end loop;
         Unchecked_Free (Grammar.Grammars);
      end if;

      Free (Grammar.Parsed_Locations);
   end Free;

   --------------------
   -- URI_Was_Parsed --
   --------------------

   function URI_Was_Parsed
     (Grammar : XML_Grammar; URI : Byte_Sequence) return Boolean
   is
      L : String_List;
   begin
      if Grammar /= No_Grammar then
         L := Get (Grammar).Parsed_Locations;
         while L /= null loop
            if Debug then
               Put_Line ("URI_Was_Parsed (" & URI & ") ? Compare with "
                         & L.Str.all);
            end if;
            if L.Str.all = URI then
               if Debug then
                  Put_Line ("    => Yes, already parsed");
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
      Grammar : in out XML_Grammar; URI : Byte_Sequence) is
   begin
      Initialize_Grammar (Reader);

      if Debug then
         Put_Line ("Set_Parsed_UI: " & URI);
      end if;
      Get (Grammar).Parsed_Locations := new String_List_Record'
        (Str  => new Byte_Sequence'(URI),
         Next => Get (Grammar).Parsed_Locations);
   end Set_Parsed_URI;

   ----------
   -- Free --
   ----------

   procedure Free (Grammar : in out XML_Grammar_NS) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Types_Htable.HTable, Types_Htable_Access);
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Elements_Htable.HTable, Elements_Htable_Access);
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Groups_Htable.HTable, Groups_Htable_Access);
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (XML_Grammar_NS_Record, XML_Grammar_NS);
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Attributes_Htable.HTable, Attributes_Htable_Access);
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Attribute_Groups_Htable.HTable, Attribute_Groups_Htable_Access);
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (XML_Type_Record, XML_Type);
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Attribute_Validator_Record'Class, Attribute_Validator);
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (XML_Element_Record, XML_Element_Access);

      procedure Free (Typ : in out XML_Type);
      procedure Free (Validator : in out XML_Validator);
      procedure Free (Validator : in out Attribute_Validator);
      procedure Free (Element : in out XML_Element_Access);
      --  Free the memory used by the parameter

      ----------
      -- Free --
      ----------

      procedure Free (Validator : in out XML_Validator) is
         procedure Unchecked_Free is new Ada.Unchecked_Deallocation
           (XML_Validator_Record'Class, XML_Validator);
      begin
         Free (Validator.all);
         Unchecked_Free (Validator);
      end Free;

      procedure Free (Typ : in out XML_Type) is
      begin
         Free (Typ.Local_Name);
         Unchecked_Free (Typ);
      end Free;

      procedure Free (Validator : in out Attribute_Validator) is
      begin
         Free (Validator.all);
         Unchecked_Free (Validator);
      end Free;

      procedure Free (Element : in out XML_Element_Access) is
      begin
         Free (Element.all);
         Unchecked_Free (Element);
      end Free;

      Val      : XML_Validator;
      Typ      : XML_Type;
      Atts     : Attribute_Validator;
      Elem     : XML_Element_Access;
   begin
      if Grammar /= null then
         Elements_Htable.Reset (Grammar.Elements.all);
         Unchecked_Free (Grammar.Elements);
         Types_Htable.Reset (Grammar.Types.all);
         Unchecked_Free (Grammar.Types);
         Groups_Htable.Reset (Grammar.Groups.all);
         Unchecked_Free (Grammar.Groups);
         Attribute_Groups_Htable.Reset (Grammar.Attribute_Groups.all);
         Unchecked_Free (Grammar.Attribute_Groups);
         Attributes_Htable.Reset (Grammar.Attributes.all);
         Unchecked_Free (Grammar.Attributes);

         --  Free types

         while Grammar.Types_For_Mem /= null loop
            Typ := Grammar.Types_For_Mem.Next;
            if Typ = Grammar.Types_For_Mem then
               Typ := null;
            end if;
            Free (Grammar.Types_For_Mem);
            Grammar.Types_For_Mem := Typ;
         end loop;

         --  Free elements

         while Grammar.Elems_For_Mem /= null loop
            Elem := Grammar.Elems_For_Mem.Next;
            if Elem = Grammar.Elems_For_Mem then
               Elem := null;
            end if;
            Free (Grammar.Elems_For_Mem);
            Grammar.Elems_For_Mem := Elem;
         end loop;

         --  Free attribute validators

         while Grammar.Atts_For_Mem /= null loop
            Atts := Grammar.Atts_For_Mem.Next;
            if Atts = Grammar.Atts_For_Mem then
               Atts := null;
            end if;
            Free (Grammar.Atts_For_Mem);
            Grammar.Atts_For_Mem := Atts;
         end loop;

         --  Free validators

         while Grammar.Validators_For_Mem /= null loop
            Val := Grammar.Validators_For_Mem.Next;
            if Val = Grammar.Validators_For_Mem then
               Val := null;
            end if;
            Free (Grammar.Validators_For_Mem);
            Grammar.Validators_For_Mem := Val;
         end loop;

         Free (Grammar.Namespace_URI);
         Free (Grammar.System_ID);
         Unchecked_Free (Grammar);
      end if;
   end Free;

   ----------------
   -- Do_Nothing --
   ----------------

   procedure Do_Nothing (Att : in out Named_Attribute_Validator) is
      pragma Unreferenced (Att);
   begin
      null;
   end Do_Nothing;

   procedure Do_Nothing (Typ : in out XML_Type) is
      pragma Unreferenced (Typ);
   begin
      null;
   end Do_Nothing;

   procedure Do_Nothing (Element : in out XML_Element_Access) is
      pragma Unreferenced (Element);
   begin
      null;
   end Do_Nothing;

   -----------------
   -- Set_Default --
   -----------------

   procedure Set_Default
     (Element  : XML_Element;
      Reader   : access Abstract_Validation_Reader'Class;
      Default  : Unicode.CES.Byte_Sequence)
   is
      Mask : Facets_Mask;
   begin
      --  3.3 Element Declaration details: Can't have both
      --  "default" and "fixed"

      if Element.Elem.Fixed /= null then
         Validation_Error
           (Reader,
            "Attributes ""fixed"" and ""default"" conflict with each other");
      end if;

      --  3.3 Element Declaration details:  Validation Rule 3.1
      --  The "default" attribute of element must match the validation rule
      --  for that element.
      --  Test whether we have a forward reference to the type, in which case
      --  default will be checked when we know the actual type

      if Element.Elem.Of_Type /= No_Type
        and then Get_Validator (Element.Elem.Of_Type) /= null
      then
         Mask := (others => True);
         Validate_Characters
           (Get_Validator (Element.Elem.Of_Type), Reader, Default,
            Empty_Element => False, Mask => Mask);
      end if;

      Free (Element.Elem.Default);
      Element.Elem.Default := new Byte_Sequence'(Default);
   end Set_Default;

   -----------------
   -- Has_Default --
   -----------------

   function Has_Default (Element : XML_Element) return Boolean is
   begin
      return Element.Elem.Default /= null;
   end Has_Default;

   -----------------
   -- Get_Default --
   -----------------

   function Get_Default
     (Element : XML_Element) return Byte_Sequence_Access is
   begin
      return Element.Elem.Default;
   end Get_Default;

   ---------------
   -- Set_Fixed --
   ---------------

   procedure Set_Fixed
     (Element  : XML_Element;
      Reader   : access Abstract_Validation_Reader'Class;
      Fixed    : Unicode.CES.Byte_Sequence)
   is
      Mask : Facets_Mask;
   begin
      --  3.3 Element Declaration details: Can't have both
      --  "default" and "fixed"

      if Element.Elem.Default /= null then
         Validation_Error
           (Reader,
            "Attributes ""fixed"" and ""default"" conflict with each other");
      end if;

      --  3.3 Element Declaration details:  Validation Rule 3.1
      --  The "fixed" attribute of element must match the validation rule
      --  for that element
      --  Test whether we have a forward reference to the type, in which case
      --  default will be checked when we know the actual type

      if Element.Elem.Of_Type /= No_Type
        and then Get_Validator (Element.Elem.Of_Type) /= null
      then
         Mask := (others => True);
         Validate_Characters
           (Get_Validator (Element.Elem.Of_Type), Reader, Fixed,
            Empty_Element => False, Mask => Mask);
      end if;

      Free (Element.Elem.Fixed);
      Element.Elem.Fixed := new Byte_Sequence'(Fixed);
   end Set_Fixed;

   ---------------
   -- Has_Fixed --
   ---------------

   function Has_Fixed (Element : XML_Element) return Boolean is
   begin
      return Element.Elem.Fixed /= null;
   end Has_Fixed;

   ---------------
   -- Get_Fixed --
   ---------------

   function Get_Fixed
     (Element : XML_Element) return Byte_Sequence_Access is
   begin
      return Element.Elem.Fixed;
   end Get_Fixed;

   ----------
   -- Free --
   ----------

   procedure Free (Element : in out XML_Element_Record) is
   begin
      Free (Element.Local_Name);
      Free (Element.Default);
      Free (Element.Fixed);
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Group : in out XML_Group) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (XML_Group_Record, XML_Group);
   begin
      Free (Group.Local_Name);
      Free (Group.Particles);
      Unchecked_Free (Group);
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Att : in out XML_Attribute_Group) is
   begin
      Free (Att.Local_Name);
      Free (Att.Attributes);
      Unchecked_Free (Att);
   end Free;

   -------------
   -- Get_Key --
   -------------

   function Get_Key
     (Att : XML_Attribute_Group) return Unicode.CES.Byte_Sequence is
   begin
      return Att.Local_Name.all;
   end Get_Key;

   -------------
   -- Get_Key --
   -------------

   function Get_Key
     (Element : XML_Element_Access) return Unicode.CES.Byte_Sequence is
   begin
      return Element.Local_Name.all;
   end Get_Key;

   -------------
   -- Get_Key --
   -------------

   function Get_Key
     (Typ : XML_Type) return Unicode.CES.Byte_Sequence is
   begin
      return Typ.Local_Name.all;
   end Get_Key;

   -------------
   -- Get_Key --
   -------------

   function Get_Key
     (Group : XML_Group) return Unicode.CES.Byte_Sequence is
   begin
      return Group.Local_Name.all;
   end Get_Key;

   -------------
   -- Get_Key --
   -------------

   function Get_Key
     (Att : Named_Attribute_Validator) return Unicode.CES.Byte_Sequence is
   begin
      return Att.Local_Name.all;
   end Get_Key;

   -------------------------------
   -- Check_Substitution_Groups --
   -------------------------------

   function Check_Substitution_Groups
     (Element            : XML_Element_Access;
      Reader             : access Abstract_Validation_Reader'Class;
      Local_Name         : Unicode.CES.Byte_Sequence;
      Namespace_URI      : Unicode.CES.Byte_Sequence;
      Parent_NS          : XML_Grammar_NS;
      Grammar            : XML_Grammar;
      Is_Local_Element   : Boolean;
      Check_All_In_Group : Boolean := True) return XML_Element
   is
      Result : XML_Element := No_Element;
      Local_Matches : constant Boolean :=
        Element.Local_Name.all = Local_Name;
   begin
      if Debug then
         Debug_Push_Prefix ("SubstitutionGroup, can " & Namespace_URI
                            & ":" & Local_Name & " replace "
                            & Get_Namespace_URI (Parent_NS)
                            & ":" & Element.Local_Name.all
                            & " element.form="
                            & Element.Form'Img);
      end if;

      --  Shortcut if Element itself is the validator for (namespace, local)

      if Local_Matches
        and then Get_Namespace_URI (Element.NS) = Namespace_URI
      then
         Result := (Elem => Element, Is_Ref => True);
         Check_Qualification (Grammar, Reader, Result, Namespace_URI);

      elsif Is_Local_Element
        and then Element.Form = Unqualified
        and then Namespace_URI = ""
        and then Local_Matches
      then
         Result := (Elem => Element, Is_Ref => True);
         Check_Qualification (Grammar, Reader, Result, Namespace_URI);

      elsif Local_Matches and then Namespace_URI = "" then
         Check_Qualification (Grammar, Reader, (Element, True), Namespace_URI);
      end if;

      --  Search for the global element (namespace_uri, local_name), and check
      --  that it can indeed be a substitute for Element

      if Check_All_In_Group and then Result = No_Element then
         declare
            R : XML_Element;
            G : XML_Element;
         begin
            R := Lookup_Element (Parent_NS, Reader, Local_Name, False);
            if R /= No_Element then
               G := R.Elem.Substitution_Group;
               while G /= No_Element loop
                  if Debug then
                     Debug_Output ("Is this a substitution of: "
                                   & G.Elem.Local_Name.all);
                  end if;

                  --  We compare directly the XML_Element, since namespace and
                  --  local_name might correspond to a local element inside the
                  --  parent, but the substitution itself only applies for
                  --  global elements. This is also more efficient

                  if G.Elem = Element then
                     Result := R;

                     if G.Elem.Blocks (Block_Substitution) then
                        Validation_Error
                          (Reader, "Unexpected element """
                           & To_QName (Namespace_URI, Local_Name)
                           & """ (substitutions are blocked)");

                     elsif (G.Elem.Blocks (Block_Extension)
                            or else G.Elem.Of_Type.Blocks (Block_Extension))
                       and then Is_Extension_Of (R, Base => G)
                     then
                        Validation_Error
                          (Reader, "Invalid substitution, because """
                           & G.Elem.Local_Name.all
                           & """ blocks extensions");

                     elsif (G.Elem.Blocks (Block_Restriction)
                            or else G.Elem.Of_Type.Blocks (Block_Restriction))
                       and then Is_Restriction_Of (R, Base => G)
                     then
                        Validation_Error
                          (Reader, "Invalid substitution, because """
                           & G.Elem.Local_Name.all
                           & """ blocks restrictions");
                     end if;

                     exit;
                  end if;

                  G := G.Elem.Substitution_Group;
               end loop;
            end if;
         end;
      end if;

      if Result /= No_Element and then Is_Abstract (Result) then
         Validation_Error
           (Reader, '"' & Result.Elem.Local_Name.all & """ is abstract");
      end if;

      Debug_Pop_Prefix;
      return Result;

   exception
      when others =>
         Debug_Pop_Prefix;
         raise;
   end Check_Substitution_Groups;

   ----------
   -- Free --
   ----------

   procedure Free (Data : in out Sequence_Data) is
   begin
      Free (Data.Current);
      Free (Group_Model_Data_Record (Data));
   end Free;

   procedure Free (Data : in out Choice_Data) is
   begin
      Free (Data.Current);
      Free (Group_Model_Data_Record (Data));
   end Free;

   ---------------------------
   -- Create_Validator_Data --
   ---------------------------

   function Create_Validator_Data
     (Validator : access Sequence_Record) return Validator_Data
   is
   begin
      return new Sequence_Data'
        (Group_Model_Data_Record with
         Previous              => null,
         Current               => Start (Validator.Particles),
         Num_Occurs_Of_Current => 0);
   end Create_Validator_Data;

   ---------------------------
   -- Move_To_Next_Particle --
   ---------------------------

   function Move_To_Next_Particle
     (Seq            : access Sequence_Record'Class;
      Data           : Sequence_Data_Access;
      Force          : Boolean := False;
      Increase_Count : Boolean := True) return Boolean is
   begin
      if Increase_Count then
         Data.Num_Occurs_Of_Current := Data.Num_Occurs_Of_Current + 1;
         if Debug then
            Debug_Output ("Current particle, now occured="
                          & Data.Num_Occurs_Of_Current'Img);
         end if;
      end if;

      Data.Previous := Get (Data.Current);

      if Get (Data.Current) /= null then
         if Force
           or else
             (Get_Max_Occurs (Data.Current) /= Unbounded
              and then Data.Num_Occurs_Of_Current >=
                Get_Max_Occurs (Data.Current))
         then
            if Debug then
               Debug_Output ("Goto next particle in " & Get_Name (Seq));
            end if;
            Next (Data.Current);
            Data.Num_Occurs_Of_Current := 0;
            return Get (Data.Current) /= null;
         end if;
      end if;

      return True;
   end Move_To_Next_Particle;

   ------------------
   -- Check_Nested --
   ------------------

   procedure Check_Nested
     (Nested            : access Group_Model_Record'Class;
      Data              : access Group_Model_Data_Record'Class;
      Reader            : access Abstract_Validation_Reader'Class;
      Local_Name        : Byte_Sequence;
      Namespace_URI     : Unicode.CES.Byte_Sequence;
      NS                : XML_Grammar_NS;
      Grammar           : XML_Grammar;
      Element_Validator : out XML_Element;
      Skip_Current      : out Boolean)
   is
      Applies : Boolean;
   begin
      Debug_Push_Prefix ("Check_Nested " & Get_Name (Nested));

      Applies_To_Tag
        (Nested, Reader, Local_Name, Namespace_URI, NS,
         Grammar, Applies, Skip_Current);

      if Applies then
         Data.Nested      := Group_Model (Nested);
         Data.Nested_Data := Create_Validator_Data (Nested);

         Validate_Start_Element
           (Data.Nested, Reader, Local_Name, Namespace_URI, NS,
            Data.Nested_Data, Grammar, Element_Validator);

         if Element_Validator = No_Element then
            Validate_End_Element
              (Nested, Reader, Local_Name, Data.Nested_Data);
            Free_Nested_Group (Group_Model_Data (Data));
         end if;
      else
         Element_Validator := No_Element;
      end if;

      Debug_Pop_Prefix;

   exception
      when others =>
         Debug_Pop_Prefix;
         raise;
   end Check_Nested;

   ----------------
   -- Run_Nested --
   ----------------

   procedure Run_Nested
     (Validator         : access Group_Model_Record'Class;
      Data              : access Group_Model_Data_Record'Class;
      Reader            : access Abstract_Validation_Reader'Class;
      Local_Name        : Byte_Sequence;
      Namespace_URI     : Unicode.CES.Byte_Sequence;
      NS                : XML_Grammar_NS;
      Grammar           : XML_Grammar;
      Element_Validator : out XML_Element) is
   begin
      Debug_Push_Prefix ("Run_Nested: " & Get_Name (Validator)
                         & " -> " & Get_Name (Data.Nested));

      Validate_Start_Element
        (Data.Nested, Reader, Local_Name, Namespace_URI, NS,
         Data.Nested_Data, Grammar, Element_Validator);

      if Element_Validator = No_Element then
         Free_Nested_Group (Group_Model_Data (Data));
      end if;

      Debug_Pop_Prefix;

   exception
      when others =>
         Debug_Pop_Prefix;
         Element_Validator := No_Element;
   end Run_Nested;

   --------------
   -- To_QName --
   --------------

   function To_QName (Particle : XML_Particle) return Byte_Sequence is
   begin
      case Particle.Typ is
         when Particle_Element =>
            return To_QName
              (Particle.Element.Elem.NS.Namespace_URI.all,
               Particle.Element.Elem.Local_Name.all);
         when Particle_Any =>
            return "<any>";
         when Particle_Nested =>
            return Type_Model (Particle.Validator, First_Only => False);
         when others =>
            return "???";
      end case;
   end To_QName;

   ----------------------------
   -- Validate_Start_Element --
   ----------------------------

   procedure Validate_Start_Element
     (Validator         : access Sequence_Record;
      Reader            : access Abstract_Validation_Reader'Class;
      Local_Name        : Unicode.CES.Byte_Sequence;
      Namespace_URI     : Unicode.CES.Byte_Sequence;
      NS                : XML_Grammar_NS;
      Data              : Validator_Data;
      Grammar           : XML_Grammar;
      Element_Validator : out XML_Element)
   is
      D         : constant Sequence_Data_Access := Sequence_Data_Access (Data);
      Curr       : XML_Particle_Access;
      Tmp        : Boolean;
      pragma Unreferenced (Tmp);
      Skip_Current : Boolean;

      procedure Check_Min_Occurs;
      --  Check that D.Current was indeed seen at least the minimal required
      --  number of times

      procedure Check_Min_Occurs is
      begin
         if D.Num_Occurs_Of_Current < Get_Min_Occurs (D.Current) then
            if D.Num_Occurs_Of_Current = 0
              and then D.Previous /= null
              and then D.Previous.Typ = Particle_Element
              and then Namespace_URI =
                D.Previous.Element.Elem.NS.Namespace_URI.all
              and then Local_Name = D.Previous.Element.Elem.Local_Name.all
            then
               Validation_Error
                 (Reader, "Too many occurrences of """
                  & To_QName (Namespace_URI, Local_Name)
                  & """ (expecting """ & To_QName (Curr.all) & """)");
            else
               Validation_Error
                 (Reader, "Expecting at least"
                  & Integer'Image (Get_Min_Occurs (D.Current))
                  & " occurrences of """ & To_QName (Curr.all) & """");
            end if;
         end if;
      end Check_Min_Occurs;

   begin
      Debug_Push_Prefix
        ("Validate_Start seq " & Get_Name (Validator)
         & " occurs=" & D.Num_Occurs_Of_Current'Img
         & " has_nested=" & Boolean'Image (D.Nested /= null)
         & ' ' & To_QName (Namespace_URI, Local_Name));

      if D.Nested /= null then
         Run_Nested
           (Validator, D, Reader, Local_Name, Namespace_URI, NS,
            Grammar, Element_Validator);
         if Element_Validator /= No_Element then
            Debug_Pop_Prefix;
            return;
         end if;

         --  We might have to try the same element again, in case it only
         --  returned No_Element because its maxOccurs was reached, be could
         --  itself be repeat another time:
         --     <sequence>
         --       <choice maxOccurs="3">
         --          <element name="foo" maxOccurs="2" />
         --          <element name="bar" maxOccurs="1" />
         --  and the following instance
         --       <foo/> <foo/> <bar/>    (the choice is repeat twice here)

         Check_Nested
           (Get (D.Current).Validator, D, Reader, Local_Name,
            Namespace_URI, NS, Grammar,
            Element_Validator, Skip_Current);

         if Element_Validator /= No_Element then
            D.Num_Occurs_Of_Current := D.Num_Occurs_Of_Current + 1;
            if Get_Max_Occurs (D.Current) = Unbounded
              or else D.Num_Occurs_Of_Current <= Get_Max_Occurs (D.Current)
            then
               Debug_Pop_Prefix;
               return;
            end if;

            --  We cannot raise a Validation_Error here, since there might
            --  be other valid occurrences of this element. For instance
            --    <choice maxOccurs="unbounded">
            --      <sequence>
            --          <element ref="shell" minOccurs="1" maxOccurs="1" />
            --  and the following instance
            --     <shell><shell>

            if Debug then
               Debug_Output
                 ("Giving up on sequence, since repeated too often (maxOcc="
                  & Integer'Image (Get_Max_Occurs (D.Current)) & ")");
            end if;
            Element_Validator := No_Element;
            Debug_Pop_Prefix;
            return;
         end if;

         --  The number of calls should only be incremented when we start
         --  the sequence initially, not when encountering the end of the
         --  sequence

         if not Move_To_Next_Particle
           (Validator, D, Force => False, Increase_Count => False)
         then
            Debug_Pop_Prefix;
            return;
         end if;
      end if;

      loop
         Curr := Get (D.Current);
         exit when Curr = null;

         case Curr.Typ is
            when Particle_Element =>
               Element_Validator := Check_Substitution_Groups
                 (Curr.Element.Elem, Reader, Local_Name, Namespace_URI, NS,
                  Grammar, Is_Local_Element => not Curr.Element.Is_Ref);

               if Element_Validator /= No_Element then
                  if Debug then
                     Debug_Output
                       ("Element matched in seq: "
                        & Element_Validator.Elem.Local_Name.all & ' '
                        & To_QName (Element_Validator.Elem.Of_Type));
                  end if;
                  Tmp := Move_To_Next_Particle (Validator, D, Force => False);

               else
                  Check_Min_Occurs;
               end if;

            when Particle_Any =>
               begin
                  Validate_Start_Element
                    (Curr.Any, Reader, Local_Name, Namespace_URI, NS,
                     null, Grammar, Element_Validator);
                  if Element_Validator /= No_Element then
                     Tmp := Move_To_Next_Particle
                       (Validator, D, Force => False);
                  end if;

               exception
                  when XML_Validation_Error =>
                     Check_Min_Occurs;
               end;

            when Particle_Nested =>
               if Debug then
                  Debug_Output ("Testing nested of sequence "
                                & Get_Name (Validator) & ": "
                                & Get_Name (Curr.Validator));
               end if;
               Check_Nested
                 (Curr.Validator, D, Reader, Local_Name,
                  Namespace_URI, NS,
                  Grammar, Element_Validator, Skip_Current);

               if Element_Validator /= No_Element then
                  D.Num_Occurs_Of_Current := D.Num_Occurs_Of_Current + 1;

               elsif not Skip_Current then
                  Check_Min_Occurs;
               end if;

            when Particle_Group | Particle_XML_Type =>
               --  Not possible, since the iterator doesn't return those
               raise Program_Error;
         end case;

         if Element_Validator /= No_Element then
            if Debug then
               Debug_Output ("Found validator: "
                             & Element_Validator.Elem.Local_Name.all
                             & " type="
                             & To_QName (Get_Type (Element_Validator)));
            end if;
            Debug_Pop_Prefix;
            return;
         end if;

         --  The current element was in fact optional at this point

         if Debug then
            Debug_Output ("Skip optional particle in " & Get_Name (Validator)
                          & ':' & Type_Model (D.Current, First_Only => True));
         end if;

         if not Move_To_Next_Particle (Validator, D, Force => True) then
            Element_Validator := No_Element;
            Debug_Pop_Prefix;
            return;
         end if;
      end loop;

      Debug_Pop_Prefix;

   exception
      when others =>
         Debug_Pop_Prefix;
         raise;
   end Validate_Start_Element;

   -----------------
   -- Is_Optional --
   -----------------

   function Is_Optional (Iterator : Particle_Iterator) return Boolean is
   begin
      return Get_Min_Occurs (Iterator) = 0
        or else (Get (Iterator).Typ = Particle_Nested
                 and then Can_Be_Empty (Get (Iterator).Validator));
   end Is_Optional;

   --------------------------
   -- Validate_End_Element --
   --------------------------

   procedure Validate_End_Element
     (Validator         : access Sequence_Record;
      Reader            : access Abstract_Validation_Reader'Class;
      Local_Name        : Unicode.CES.Byte_Sequence;
      Data              : Validator_Data)
   is
      D : constant Sequence_Data_Access := Sequence_Data_Access (Data);
   begin
      Debug_Push_Prefix ("Validate_End_Element " & Get_Name (Validator)
                         & " occurs=" & D.Num_Occurs_Of_Current'Img);

      if D.Nested /= null then
         Validate_End_Element (D.Nested, Reader, Local_Name, D.Nested_Data);
      end if;

      if Get (D.Current) /= null
        and then (D.Num_Occurs_Of_Current >= Get_Min_Occurs (D.Current)
                 or else Is_Optional (D.Current))
      then
         Next (D.Current);

         while Get (D.Current) /= null loop
            if not Is_Optional (D.Current) then
               Validation_Error
                 (Reader,
                  "Expecting " & Type_Model (D.Current, First_Only => True));
            end if;

            Next (D.Current);
         end loop;
      end if;

      if Get (D.Current) /= null then
         if Debug then
            Debug_Output ("Current element occurred "
                          & D.Num_Occurs_Of_Current'Img & " times, minimum is"
                          & Get_Min_Occurs (D.Current)'Img);
         end if;
         Validation_Error (Reader, "Unexpected end of sequence, expecting """
                           & Type_Model (D.Current, True) & """");
      end if;

      Debug_Pop_Prefix;

   exception
      when others =>
         Debug_Pop_Prefix;
         raise;
   end Validate_End_Element;

   ------------
   -- Append --
   ------------

   procedure Append
     (List       : Particle_List;
      Reader     : access Abstract_Validation_Reader'Class;
      Item       : XML_Particle) is
   begin
      pragma Assert (List /= null, "List was never created");

      if Item.Max_Occurs /= Unbounded
        and then Item.Min_Occurs > Item.Max_Occurs
      then
         Validation_Error
           (Reader, "minOccurs > maxOccurs when creating particle");
      end if;

      if Item.Max_Occurs /= 0 then
         if List.First = null then
            List.First := new XML_Particle'(Item);
            List.First.Next := null;
            List.Last  := List.First;
         else
            List.Last.Next := new XML_Particle'(Item);
            List.Last := List.Last.Next;
         end if;
      end if;
   end Append;

   ---------------------
   -- Create_Sequence --
   ---------------------

   function Create_Sequence (G : XML_Grammar_NS) return Sequence is
      Seq : constant Sequence := new Sequence_Record;
   begin
      Register (G, Seq);
      return Seq;
   end Create_Sequence;

   ------------------
   -- Add_Particle --
   ------------------

   procedure Add_Particle
     (Seq        : access Sequence_Record;
      Reader     : access Abstract_Validation_Reader'Class;
      Item       : XML_Element;
      Min_Occurs : Natural := 1;
      Max_Occurs : Integer := 1) is
   begin
      if Item.Elem.Local_Name = null then
         Raise_Exception
           (Program_Error'Identity,
            "Adding empty element to a sequence");
      end if;

      Append
        (Seq.Particles, Reader, XML_Particle'
           (Typ        => Particle_Element,
            Element    => Item,
            Next       => null,
            Min_Occurs => Min_Occurs,
            Max_Occurs => Max_Occurs));
   end Add_Particle;

   ------------------
   -- Add_Particle --
   ------------------

   procedure Add_Particle
     (Seq        : access Sequence_Record;
      Reader     : access Abstract_Validation_Reader'Class;
      Item       : Sequence;
      Min_Occurs : Natural := 1; Max_Occurs : Integer := 1) is
   begin
      Append (Seq.Particles, Reader, XML_Particle'
          (Typ        => Particle_Nested,
           Validator  => Group_Model (Item),
           Next       => null,
           Min_Occurs => Min_Occurs,
           Max_Occurs => Max_Occurs));
   end Add_Particle;

   ------------------
   -- Add_Particle --
   ------------------

   procedure Add_Particle
     (Seq        : access Sequence_Record;
      Reader     : access Abstract_Validation_Reader'Class;
      Item       : Choice;
      Min_Occurs : Natural := 1; Max_Occurs : Integer := 1) is
   begin
      Append (Seq.Particles, Reader, XML_Particle'
          (Typ        => Particle_Nested,
           Validator  => Group_Model (Item),
           Next       => null,
           Min_Occurs => Min_Occurs,
           Max_Occurs => Max_Occurs));
   end Add_Particle;

   ------------------
   -- Add_Particle --
   ------------------

   procedure Add_Particle
     (Seq        : access Sequence_Record;
      Reader     : access Abstract_Validation_Reader'Class;
      Item       : XML_Group;
      Min_Occurs : Natural := 1; Max_Occurs : Integer := 1) is
   begin
      Append (Seq.Particles, Reader, XML_Particle'
          (Typ        => Particle_Group,
           Group      => Item,
           Next       => null,
           Min_Occurs => Min_Occurs,
           Max_Occurs => Max_Occurs));
   end Add_Particle;

   ----------------------------
   -- Validate_Start_Element --
   ----------------------------

   procedure Validate_Start_Element
     (Validator         : access Choice_Record;
      Reader            : access Abstract_Validation_Reader'Class;
      Local_Name        : Unicode.CES.Byte_Sequence;
      Namespace_URI     : Unicode.CES.Byte_Sequence;
      NS                : XML_Grammar_NS;
      Data              : Validator_Data;
      Grammar           : XML_Grammar;
      Element_Validator : out XML_Element)
   is
      D     : constant Choice_Data_Access := Choice_Data_Access (Data);
      Skip_Current : Boolean;
      Has_Single_Choice : Boolean;

      procedure Check_Particle (It : XML_Particle_Access);
      --  Check whether a specific particle matches

      procedure Check_Particle (It : XML_Particle_Access) is
      begin
         case It.Typ is
            when Particle_Element =>
               Element_Validator := Check_Substitution_Groups
                 (It.Element.Elem, Reader, Local_Name, Namespace_URI, NS,
                  Grammar, Is_Local_Element => not It.Element.Is_Ref);

            when Particle_Nested =>
               Check_Nested
                 (It.Validator, D, Reader, Local_Name, Namespace_URI, NS,
                  Grammar, Element_Validator, Skip_Current);

            when Particle_Any =>
               Validate_Start_Element
                 (It.Any, Reader, Local_Name, Namespace_URI, NS,
                  null, Grammar, Element_Validator);

            when Particle_Group | Particle_XML_Type =>
               --  Not possible, since the iterator hides these
               raise Program_Error;
         end case;
      end Check_Particle;

   begin
      Debug_Push_Prefix
        ("Validate_Start choice " & Get_Name (Validator)
         & " occurs=" & D.Num_Occurs_Of_Current'Img
         & " has_nested=" & Boolean'Image (D.Nested /= null)
         & ' ' & To_QName (Namespace_URI, Local_Name));

      if D.Nested /= null then
         Run_Nested
           (Validator, D, Reader, Local_Name, Namespace_URI, NS,
            Grammar, Element_Validator);
         if Element_Validator /= No_Element then
            Debug_Pop_Prefix;
            return;
         end if;
      end if;

      if D.Current /= No_Iter then
         if Debug then
            Debug_Output ("Testing again same element in choice");
         end if;
         Check_Particle (Get (D.Current));
         if Element_Validator = No_Element then
            if D.Num_Occurs_Of_Current < Get_Min_Occurs (D.Current) then
               Validation_Error
                 (Reader,
                  "Not enough occurrences of current element, expecting at"
                  & " least" & Integer'Image (Get_Min_Occurs (D.Current)));
            end if;
            Free (D.Current);

         else
            D.Num_Occurs_Of_Current := D.Num_Occurs_Of_Current + 1;
            if Get_Max_Occurs (D.Current) /= Unbounded
              and then D.Num_Occurs_Of_Current > Get_Max_Occurs (D.Current)
            then
               --  No need to insist on the same element again next time
               Free (D.Current);
               D.Current := No_Iter;
               Element_Validator := No_Element;
               if Debug then
                  Debug_Output
                    ("Giving up choice, since too many occurrences");
               end if;
            end if;

            Debug_Pop_Prefix;
            return;
         end if;
      end if;

      --  Choice has already matched once, by definition it can no longer match
      --  so we give back the control to the parent
      if D.Num_Occurs_Of_Current > 0 then
         Element_Validator := No_Element;
         Debug_Pop_Prefix;
         return;
      end if;

      if Debug then
         Debug_Output ("Testing all elements from start in choice");
      end if;
      D.Num_Occurs_Of_Current := 0;

      D.Current := Start (Validator.Particles);
      Next (D.Current);
      Has_Single_Choice := Get (D.Current) = null;

      Free (D.Current);
      D.Current := Start (Validator.Particles);

      --  Check whether the current item is valid

      while Get (D.Current) /= null loop
         begin
            Check_Particle (Get (D.Current));
            exit when Element_Validator /= No_Element;
            Next (D.Current);

         exception
            when XML_Validation_Error =>
               Element_Validator := No_Element;
               if Has_Single_Choice then
                  --  If we have a single choice, we report its errors
                  --  directly to the caller, instead of using our own error
                  --  messages for <choice>. This is slightly more helpful to
                  --  the user.
                  Free (D.Current);
                  raise;
               end if;

               Next (D.Current);
         end;
      end loop;

      if Get (D.Current) = null then
         Free (D.Current);
         if Debug then
            Debug_Output ("Choice didn't match");
         end if;
         Element_Validator := No_Element;
         Debug_Pop_Prefix;
         return;
      end if;

      D.Num_Occurs_Of_Current := D.Num_Occurs_Of_Current + 1;
      if Get_Max_Occurs (D.Current) /= Unbounded
        and then D.Num_Occurs_Of_Current >= Get_Max_Occurs (D.Current)
      then
         Free (D.Current);
      else
         --  Keep same current element, and we will try again next time
         null;
      end if;

      Debug_Pop_Prefix;

   exception
      when others =>
         Debug_Pop_Prefix;
         raise;
   end Validate_Start_Element;

   --------------------------
   -- Validate_End_Element --
   --------------------------

   procedure Validate_End_Element
     (Validator      : access Choice_Record;
      Reader         : access Abstract_Validation_Reader'Class;
      Local_Name     : Unicode.CES.Byte_Sequence;
      Data           : Validator_Data)
   is
      pragma Unreferenced (Local_Name);
      D     : constant Choice_Data_Access := Choice_Data_Access (Data);
   begin
      if D.Current /= No_Iter then
         case Get (D.Current).Typ is
            when Particle_Nested =>
               Validate_End_Element
                 (Get (D.Current).Validator, Reader,
                  Local_Name => "",
                  Data       => D.Nested_Data);
            when others =>
               null;
         end case;

         if D.Num_Occurs_Of_Current < Get_Min_Occurs (D.Current) then
            if Get (D.Current).Typ = Particle_Element then
               Validation_Error
                 (Reader, "Not enough occurrences of """
                  & To_QName (Get (D.Current).Element) & """");
            else
               Validation_Error
                 (Reader, "Not enough occurrences of current element");
            end if;
         end if;

      --  Never occurred ?
      elsif D.Num_Occurs_Of_Current = 0 then
         Validation_Error
           (Reader, "Expecting one of """
            & Type_Model (Validator, First_Only => True)
            & """");
      end if;
   end Validate_End_Element;

   ---------------------------
   -- Create_Validator_Data --
   ---------------------------

   function Create_Validator_Data
     (Validator : access Choice_Record) return Validator_Data
   is
      pragma Unreferenced (Validator);
   begin
      return new Choice_Data'
        (Group_Model_Data_Record with
         Current               => No_Iter,
         Num_Occurs_Of_Current => 0);
   end Create_Validator_Data;

   ----------
   -- Free --
   ----------

   procedure Free (Data : in out Validator_Data_Record) is
      pragma Unreferenced (Data);
   begin
      null;
   end Free;

   ---------------------------
   -- Create_Validator_Data --
   ---------------------------

   function Create_Validator_Data
     (Validator : access XML_Validator_Record) return Validator_Data
   is
      pragma Unreferenced (Validator);
   begin
      return null;
   end Create_Validator_Data;

   -------------------
   -- Create_Choice --
   -------------------

   function Create_Choice (G : XML_Grammar_NS) return Choice is
      Result : constant Choice := new Choice_Record;
   begin
      Register (G, Result);
      return Result;
   end Create_Choice;

   ------------------
   -- Add_Particle --
   ------------------

   procedure Add_Particle
     (C          : access Choice_Record;
      Reader     : access Abstract_Validation_Reader'Class;
      Item       : XML_Element;
      Min_Occurs : Natural := 1; Max_Occurs : Integer := 1) is
   begin
      if Item.Elem.Local_Name = null then
         Raise_Exception
           (Program_Error'Identity,
            "Adding unnamed element to choice");
      end if;

      Append (C.Particles, Reader, XML_Particle'
                (Typ        => Particle_Element,
                 Element    => Item,
                 Next       => null,
                 Min_Occurs => Min_Occurs,
                 Max_Occurs => Max_Occurs));
   end Add_Particle;

   ------------------
   -- Add_Particle --
   ------------------

   procedure Add_Particle
     (C          : access Choice_Record;
      Reader     : access Abstract_Validation_Reader'Class;
      Item       : XML_Any;
      Min_Occurs : Natural := 1; Max_Occurs : Integer := 1) is
   begin
      Append (C.Particles, Reader, XML_Particle'
          (Typ        => Particle_Any,
           Any        => Item,
           Next       => null,
           Min_Occurs => Min_Occurs,
           Max_Occurs => Max_Occurs));
   end Add_Particle;

   ------------------
   -- Add_Particle --
   ------------------

   procedure Add_Particle
     (Seq    : access Sequence_Record;
      Reader : access Abstract_Validation_Reader'Class;
      Item   : XML_Any;
      Min_Occurs : Natural := 1; Max_Occurs : Integer := 1) is
   begin
      Append (Seq.Particles, Reader, XML_Particle'
          (Typ        => Particle_Any,
           Any        => Item,
           Next       => null,
           Min_Occurs => Min_Occurs,
           Max_Occurs => Max_Occurs));
   end Add_Particle;

   ------------------
   -- Add_Particle --
   ------------------

   procedure Add_Particle
     (C      : access Choice_Record;
      Reader : access Abstract_Validation_Reader'Class;
      Item   : Sequence;
      Min_Occurs : Natural := 1; Max_Occurs : Integer := 1) is
   begin
      Append (C.Particles, Reader, XML_Particle'
          (Typ        => Particle_Nested,
           Validator  => Group_Model (Item),
           Next       => null,
           Min_Occurs => Min_Occurs,
           Max_Occurs => Max_Occurs));
   end Add_Particle;

   ------------------
   -- Add_Particle --
   ------------------

   procedure Add_Particle
     (C      : access Choice_Record;
      Reader : access Abstract_Validation_Reader'Class;
      Item   : Choice;
      Min_Occurs : Natural := 1; Max_Occurs : Integer := 1) is
   begin
      Append (C.Particles, Reader, XML_Particle'
          (Typ        => Particle_Nested,
           Validator  => Group_Model (Item),
           Next       => null,
           Min_Occurs => Min_Occurs,
           Max_Occurs => Max_Occurs));
   end Add_Particle;

   ------------------
   -- Add_Particle --
   ------------------

   procedure Add_Particle
     (C      : access Choice_Record;
      Reader : access Abstract_Validation_Reader'Class;
      Item   : XML_Group;
      Min_Occurs : Natural := 1; Max_Occurs : Integer := 1) is
   begin
      Append (C.Particles, Reader, XML_Particle'
          (Typ        => Particle_Group,
           Group      => Item,
           Next       => null,
           Min_Occurs => Min_Occurs,
           Max_Occurs => Max_Occurs));
   end Add_Particle;

   ----------
   -- Free --
   ----------

   procedure Free (Data : in out Validator_Data) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Validator_Data_Record'Class, Validator_Data);
   begin
      if Data /= null then
         Free (Data.all);
         Unchecked_Free (Data);
      end if;
   end Free;

   --------------------
   -- Applies_To_Tag --
   --------------------

   procedure Applies_To_Tag
     (Group         : access Group_Model_Record;
      Reader        : access Abstract_Validation_Reader'Class;
      Local_Name    : Unicode.CES.Byte_Sequence;
      Namespace_URI : Unicode.CES.Byte_Sequence;
      NS            : XML_Grammar_NS;
      Grammar       : XML_Grammar;
      Applies       : out Boolean;
      Skip_Current  : out Boolean)
   is
      pragma Unreferenced
        (Group, Local_Name, Namespace_URI, NS, Grammar, Reader);
   begin
      Applies := False;
      Skip_Current := False;
   end Applies_To_Tag;

   --------------------
   -- Applies_To_Tag --
   --------------------

   procedure Applies_To_Tag
     (Group         : access Sequence_Record;
      Reader        : access Abstract_Validation_Reader'Class;
      Local_Name    : Unicode.CES.Byte_Sequence;
      Namespace_URI : Unicode.CES.Byte_Sequence;
      NS            : XML_Grammar_NS;
      Grammar       : XML_Grammar;
      Applies       : out Boolean;
      Skip_Current  : out Boolean)
   is
      Iter : Particle_Iterator := Start (Group.Particles);
      Item : XML_Particle_Access;

   begin
      Debug_Push_Prefix ("Applies_To_Tag " & Get_Name (Group));
      Skip_Current := False;

      loop
         Item := Get (Iter);
         exit when Item = null;

         case Item.Typ is
            when Particle_Element =>
               Applies := Check_Substitution_Groups
                 (Item.Element.Elem, Reader, Local_Name, Namespace_URI, NS,
                  Grammar, Is_Local_Element => not Item.Element.Is_Ref)
                 /= No_Element;

            when Particle_Nested =>
               Applies_To_Tag
                 (Item.Validator, Reader, Local_Name, Namespace_URI, NS,
                  Grammar, Applies, Skip_Current);

            when Particle_Any =>
               declare
                  Element_Validator : XML_Element;
               begin
                  Validate_Start_Element
                    (Item.Any, Reader, Local_Name, Namespace_URI, NS, null,
                     Grammar, Element_Validator);
                  Applies := True;
               exception
                  when XML_Validation_Error =>
                     Applies := False;
               end;

            when Particle_Group | Particle_XML_Type =>
               --  Not possible since hidden by the iterator
               raise Program_Error;
         end case;

         if Debug then
            Debug_Output ("Applies= " & Applies'Img
                          & " Item.minOccurs=" & Item.Min_Occurs'Img);
         end if;

         if Applies then
            Debug_Pop_Prefix;
            Free (Iter);
            return;
         elsif Get_Min_Occurs (Iter) > 0 and then not Skip_Current then
            if Debug then
               Debug_Output
                 ("Current element is not optional => doesn't apply");
            end if;
            Skip_Current := False;
            Applies := False;
            Free (Iter);
            Debug_Pop_Prefix;
            return;
         end if;
         Next (Iter);
      end loop;

      Free (Iter);
      Skip_Current := True;
      Applies := False;

      Debug_Pop_Prefix;

   exception
      when others =>
         Debug_Pop_Prefix;
         raise;
   end Applies_To_Tag;

   --------------------
   -- Applies_To_Tag --
   --------------------

   procedure Applies_To_Tag
     (Group         : access Choice_Record;
      Reader        : access Abstract_Validation_Reader'Class;
      Local_Name    : Unicode.CES.Byte_Sequence;
      Namespace_URI : Unicode.CES.Byte_Sequence;
      NS            : XML_Grammar_NS;
      Grammar       : XML_Grammar;
      Applies       : out Boolean;
      Skip_Current  : out Boolean)
   is
      Item : Particle_Iterator := Start (Group.Particles);
      It   : XML_Particle_Access;
   begin
      Debug_Push_Prefix ("Applies_To_Tag " & Get_Name (Group));
      while Get (Item) /= null loop
         It := Get (Item);
         case It.Typ is
            when Particle_Element =>
               Applies := Check_Substitution_Groups
                 (It.Element.Elem, Reader, Local_Name, Namespace_URI, NS,
                  Grammar, Is_Local_Element => not It.Element.Is_Ref)
                 /= No_Element;

            when Particle_Nested =>
               Applies_To_Tag
                 (It.Validator, Reader, Local_Name, Namespace_URI,
                  NS, Grammar, Applies, Skip_Current);
               Applies := Applies
                 or else Can_Be_Empty (It.Validator);

            when Particle_Any =>
               declare
                  Element_Validator : XML_Element;
               begin
                  Validate_Start_Element
                    (It.Any, Reader, Local_Name, Namespace_URI, NS, null,
                     Grammar, Element_Validator);
                  Applies := True;
               exception
                  when XML_Validation_Error =>
                     Applies := False;
               end;

            when Particle_Group | Particle_XML_Type =>
               --  Not possible since hidden by the iterator
               raise Program_Error;
         end case;

         if Applies then
            Free (Item);
            if Debug then
               Debug_Output ("Applies");
            end if;
            Debug_Pop_Prefix;
            return;
         end if;

         Next (Item);
      end loop;

      Free (Item);
      Applies := False;
      Skip_Current := Can_Be_Empty (Group);
      if Debug then
         Debug_Output ("Doesn't apply");
      end if;
      Debug_Pop_Prefix;

   exception
      when others =>
         Free (Item);
         raise;
   end Applies_To_Tag;

   ----------------------------
   -- Set_Substitution_Group --
   ----------------------------

   procedure Set_Substitution_Group
     (Element : XML_Element;
      Reader  : access Abstract_Validation_Reader'Class;
      Head    : XML_Element)
   is
      Had_Restriction, Had_Extension : Boolean := False;
      HeadPtr : constant XML_Element_Access := Head.Elem;
      ElemPtr : constant XML_Element_Access := Element.Elem;
      Valid_Replacement : Boolean;
   begin
      --  ??? Should Head be fully defined here, so that we can check we are a
      --  possible replacement for it ?
      if Get_Validator (Get_Type (Element)) /= null
        and then Get_Validator (Get_Type (Head)) /= null
        and then Get_Validator (Get_Type (Element)) /=
           Get_Validator (Get_Type (Head))
      then
         Check_Replacement
           (Get_Validator (Get_Type (Element)),
            Element         => Head,
            Typ             => Get_Type (Head),
            Valid           => Valid_Replacement,
            Had_Restriction => Had_Restriction,
            Had_Extension   => Had_Extension);

         if not Valid_Replacement then
            Validation_Error
              (Reader, To_QName (Get_Type (Element))
               & " is not a valid replacement for "
               & To_QName (Get_Type (Head)));
         end if;

         if HeadPtr.Final_Restriction and then Had_Restriction then
            Validation_Error
              (Reader, """" & HeadPtr.Local_Name.all
               & """ is final for restrictions, and cannot be substituted by"
               & """" & ElemPtr.Local_Name.all & """");
         end if;

         if HeadPtr.Final_Extension and then Had_Extension then
            Validation_Error
              (Reader, """" & HeadPtr.Local_Name.all
               & """ is final for extensions, and cannot be substituted by"
               & """" & ElemPtr.Local_Name.all & """");
         end if;
      end if;

      if ElemPtr.Substitution_Group /= No_Element then
         Validation_Error
           (Reader, """" & ElemPtr.Local_Name.all
            & """ already belongs to another substitution group");
      end if;

      ElemPtr.Substitution_Group := Head;
   end Set_Substitution_Group;

   ----------------------------
   -- Get_Substitution_Group --
   ----------------------------

   function Get_Substitution_Group
     (Element : XML_Element) return XML_Element is
   begin
      return Element.Elem.Substitution_Group;
   end Get_Substitution_Group;

   -------------------------
   -- Empty_Particle_List --
   -------------------------

   function Empty_Particle_List return Particle_List is
   begin
      return new Particle_List_Record;
   end Empty_Particle_List;

   --------------------
   -- Get_Local_Name --
   --------------------

   function Get_Local_Name
     (Group : XML_Group) return Unicode.CES.Byte_Sequence is
   begin
      return Group.Local_Name.all;
   end Get_Local_Name;

   -----------------------
   -- Create_Local_Type --
   -----------------------

   function Create_Local_Type
     (Grammar    : XML_Grammar_NS;
      Validator  : access XML_Validator_Record'Class) return XML_Type
   is
      Result : XML_Type;
   begin
      Register (Grammar, Validator);
      Result := new XML_Type_Record'
        (Local_Name        => null,
         Validator         => XML_Validator (Validator),
         Simple_Type       => Unknown_Content,
         Blocks            => (others => False),
         Final_Extension   => False,
         Final_Restriction => False,
         Next              => null);
      Register (Grammar, Result);
      return Result;
   end Create_Local_Type;

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

   ------------------
   -- Add_Particle --
   ------------------

   procedure Add_Particle
     (Group      : in out XML_Group;
      Reader     : access Abstract_Validation_Reader'Class;
      Particle   : access Group_Model_Record'Class;
      Min_Occurs : Natural := 1; Max_Occurs : Natural := 1) is
   begin
      Append
        (Group.Particles, Reader, XML_Particle'
           (Typ        => Particle_Nested,
            Validator  => Group_Model (Particle),
            Next       => null,
            Min_Occurs => Min_Occurs,
            Max_Occurs => Max_Occurs));
   end Add_Particle;

   --------------------
   -- Set_Debug_Name --
   --------------------

   procedure Set_Debug_Name
     (Typ : access XML_Validator_Record'Class; Name : String) is
   begin
      Free (Typ.Debug_Name);
      Typ.Debug_Name := new Unicode.CES.Byte_Sequence'(Name);
   end Set_Debug_Name;

   ----------
   -- Free --
   ----------

   procedure Free (Iter : in out Particle_Iterator) is
      Tmp : Particle_Iterator;
   begin
      while Iter /= null loop
         Tmp := Iter;
         Iter := Iter.Parent;
         Unchecked_Free (Tmp);
      end loop;
   end Free;

   -----------
   -- Start --
   -----------

   function Start (List : Particle_List) return Particle_Iterator is
      Iter : Particle_Iterator;
   begin
      Iter := new Particle_Iterator_Record'
        (Current => List.First, Parent  => null);

      while Iter.Current /= null
        and then Iter.Current.Typ = Particle_Group
      loop
         if Iter.Current.Group.Particles.First = null then
            Next (Iter);
            exit;
         else
            Iter := new Particle_Iterator_Record'
              (Current => Iter.Current.Group.Particles.First,
               Parent  => Iter);
         end if;
      end loop;

      return Iter;
   end Start;

   ----------
   -- Next --
   ----------

   procedure Next (Iter : in out Particle_Iterator) is
      Tmp : Particle_Iterator;
   begin
      Iter.Current := Iter.Current.Next;

      while Iter.Current = null loop
         Tmp := Iter;
         Iter := Iter.Parent;
         Unchecked_Free (Tmp);

         if Iter = null then
            return;
         end if;

         if Debug then
            Debug_Output ("--> End of group "
                          & Iter.Current.Group.Local_Name.all);
         end if;

         Iter.Current := Iter.Current.Next;
      end loop;

      while Iter.Current.Typ = Particle_Group loop
         if Iter.Current.Group.Particles.First = null then
            Next (Iter);
            exit;
         else
            if Debug then
               Debug_Output ("--> In group "
                             & Iter.Current.Group.Local_Name.all);
            end if;
            Iter := new Particle_Iterator_Record'
              (Current => Iter.Current.Group.Particles.First,
               Parent  => Iter);
         end if;
      end loop;
   end Next;

   ---------
   -- Get --
   ---------

   function Get (Iter : Particle_Iterator) return XML_Particle_Access is
   begin
      if Iter = null then
         return null;
      else
         return Iter.Current;
      end if;
   end Get;

   --------------------
   -- Get_Min_Occurs --
   --------------------

   function Get_Min_Occurs (Iter : Particle_Iterator) return Natural is
   begin
      if Iter.Parent /= null then
         return Iter.Parent.Current.Min_Occurs;
      else
         return Iter.Current.Min_Occurs;
      end if;
   end Get_Min_Occurs;

   --------------------
   -- Get_Max_Occurs --
   --------------------

   function Get_Max_Occurs (Iter : Particle_Iterator) return Integer is
--      Tmp : Particle_Iterator := Iter;
   begin
      --  If we have a <sequence> or <choice> inside a group, we need to
      --  consider the number of occurrences defined at the group level.
      --     <complexType name="explicitGroup" >
      --       <sequence>
      --         <group ref="nestedParticle" maxOccurs="unbounded" />
      --     <group name="nestedParticle">
      --       <choice>
      --         <element name="...">

--        Debug_Push_Prefix ("Get_Max_Occurs");
--        while Tmp /= null loop
--           if Tmp.Current.Typ /= Particle_Group then
--              Debug_Output (Tmp.Current.Typ'Img & ' '
--                            & Type_Model (Tmp, First_Only => True)
--                            & Tmp.Current.Max_Occurs'Img);
--           else
--              Debug_Output (Tmp.Current.Typ'Img & ' '
--                            & Tmp.Current.Max_Occurs'Img);
--           end if;
--           Tmp := Tmp.Parent;
--        end loop;
--        Debug_Pop_Prefix;

      if Iter.Current.Typ = Particle_Nested
        and then Iter.Parent /= null
        and then Iter.Parent.Current.Typ = Particle_Group
      then
         return Iter.Parent.Current.Max_Occurs;
      else
         return Iter.Current.Max_Occurs;
      end if;
   end Get_Max_Occurs;

   ------------------
   -- Global_Check --
   ------------------

   procedure Global_Check
     (Reader  : access Abstract_Validation_Reader'Class;
      Grammar : XML_Grammar_NS)
   is
      use Elements_Htable, Types_Htable, Attributes_Htable, Groups_Htable;
      use Attribute_Groups_Htable;
      Elem_Iter : Elements_Htable.Iterator := First (Grammar.Elements.all);
      Type_Iter : Types_Htable.Iterator := First (Grammar.Types.all);
      Attr_Iter : Attributes_Htable.Iterator :=
        First (Grammar.Attributes.all);
      Group_Iter : Groups_Htable.Iterator := First (Grammar.Groups.all);
      Attr_Group_Iter : Attribute_Groups_Htable.Iterator :=
        First (Grammar.Attribute_Groups.all);

      Elem : XML_Element_Access;
      Typ  : XML_Type;
      Attr : Named_Attribute_Validator;
      Group : XML_Group;
      Attr_Group : XML_Attribute_Group;
   begin
      if Grammar.Checked then
         return;
      end if;

      Grammar.Checked := True;

      while Type_Iter /= Types_Htable.No_Iterator loop
         Typ := Current (Type_Iter);
         if Get_Validator (Typ) = null then
            Validation_Error
              (Reader, "Type """
               & To_QName (Grammar.Namespace_URI.all, Typ.Local_Name.all)
               & """ was referenced but never declared");
         end if;
         Next (Grammar.Types.all, Type_Iter);
      end loop;

      while Elem_Iter /= Elements_Htable.No_Iterator loop
         Elem := Current (Elem_Iter);

         if Elem.Of_Type = No_Type then
            Validation_Error
              (Reader, "Element """
               & To_QName (Grammar.Namespace_URI.all, Elem.Local_Name.all)
               & """ was referenced but never declared");
         end if;

         Next (Grammar.Elements.all, Elem_Iter);
      end loop;

      while Attr_Iter /= Attributes_Htable.No_Iterator loop
         Attr := Current (Attr_Iter);
         if Get_Type (Attr.all) = No_Type then
            Validation_Error
              (Reader, "Attribute """
               & To_QName (Grammar.Namespace_URI.all, Attr.Local_Name.all)
               & """ is referenced, but not defined");
         end if;

         Next (Grammar.Attributes.all, Attr_Iter);
      end loop;

      while Group_Iter /= Groups_Htable.No_Iterator loop
         Group := Current (Group_Iter);
         if Group.Is_Forward_Decl then
            Validation_Error
              (Reader, "Group """
               & To_QName (Grammar.Namespace_URI.all,
                 Group.Local_Name.all)
               & """ is referenced, but not defined");
         end if;

         Next (Grammar.Groups.all, Group_Iter);
      end loop;

      while Attr_Group_Iter /= Attribute_Groups_Htable.No_Iterator loop
         Attr_Group := Current (Attr_Group_Iter);
         if Attr_Group.Is_Forward_Decl then
            Validation_Error
              (Reader, "attributeGroup """
               & To_QName (Grammar.Namespace_URI.all,
                 Attr_Group.Local_Name.all)
               & """ is referenced, but not defined");
         end if;
         Next (Grammar.Attribute_Groups.all, Attr_Group_Iter);
      end loop;
   end Global_Check;

   ----------------
   -- Create_All --
   ----------------

   function Create_All
     (G          : XML_Grammar_NS;
      Min_Occurs : Natural := 1;
      Max_Occurs : Integer := 1) return XML_All
   is
      Result : constant XML_All := new XML_All_Record'
        (XML_Validator_Record with
         Particles  => Empty_Particle_List,
         Min_Occurs => Min_Occurs,
         Max_Occurs => Max_Occurs);
   begin
      Register (G, Result);
      return Result;
   end Create_All;

   ----------
   -- Free --
   ----------

   procedure Free (Any : in out XML_Any_Record) is
   begin
      Free (Any.Namespace);
      Free (XML_Validator_Record (Any));
   end Free;

   ----------------
   -- Create_Any --
   ----------------

   function Create_Any
     (Process_Contents : Process_Contents_Type := Process_Strict;
      Namespace        : Unicode.CES.Byte_Sequence;
      Target_NS        : XML_Grammar_NS) return XML_Any
   is
      Result : constant XML_Any := new XML_Any_Record;
   begin
      Result.Process_Contents := Process_Contents;
      Result.Namespace := new Byte_Sequence'(Namespace);
      Result.Target_NS := Target_NS;
      Register (Target_NS, Result);
      return Result;
   end Create_Any;

   ------------------
   -- Add_Particle --
   ------------------

   procedure Add_Particle
     (Validator  : access XML_All_Record;
      Reader     : access Abstract_Validation_Reader'Class;
      Item       : XML_Element;
      Min_Occurs : Zero_Or_One := 1; Max_Occurs : Zero_Or_One := 1) is
   begin
      Append (Validator.Particles, Reader, XML_Particle'
                (Typ        => Particle_Element,
                 Element    => Item,
                 Min_Occurs => Min_Occurs,
                 Max_Occurs => Max_Occurs,
                 Next       => null));
   end Add_Particle;

   ----------------------------
   -- Validate_Start_Element --
   ----------------------------

   procedure Validate_Start_Element
     (Validator         : access XML_All_Record;
      Reader            : access Abstract_Validation_Reader'Class;
      Local_Name        : Unicode.CES.Byte_Sequence;
      Namespace_URI     : Unicode.CES.Byte_Sequence;
      NS                : XML_Grammar_NS;
      Data              : Validator_Data;
      Grammar           : XML_Grammar;
      Element_Validator : out XML_Element)
   is
      Tmp     : Particle_Iterator := Start (Validator.Particles);
      D       : constant All_Data_Access := All_Data_Access (Data);
      Count   : Positive := 1;
   begin
      --  Check whether there are still some candidates. It isn't the case if
      --  we have already matches all elements as many times as possible

      Debug_Push_Prefix
        ("Validate_Start_Element <all>: " & Get_Name (Validator));

      while Get (Tmp) /= null loop
         if D.All_Elements (Count) < Get_Max_Occurs (Tmp) then
            exit;
         end if;
         Count := Count + 1;
         Next (Tmp);
      end loop;

      if Get (Tmp) = null then
         if Debug then
            Debug_Output
              ("<all> can no longer match, all particles have been matched");
         end if;
         D.All_Elements := (others => 0);
         D.Num_Occurs   := D.Num_Occurs + 1;
         Element_Validator := No_Element;
         Free (Tmp);
         Debug_Pop_Prefix;
         return;
      end if;

      Free (Tmp);

      Tmp := Start (Validator.Particles);
      Count := 1;

      while Get (Tmp) /= null loop
         Element_Validator := Check_Substitution_Groups
           (Get (Tmp).Element.Elem, Reader, Local_Name, Namespace_URI, NS,
            Grammar, Is_Local_Element => not Get (Tmp).Element.Is_Ref);

         if Element_Validator /= No_Element then
            D.All_Elements (Count) := D.All_Elements (Count) + 1;
            if D.All_Elements (Count) > Get_Max_Occurs (Tmp) then
               Validation_Error
                 (Reader,
                  "Element """ & Local_Name & """ is repeated too many times,"
                  & " maximum number of occurrences is"
                  & Integer'Image (Get_Max_Occurs (Tmp)));
            end if;

            exit;
         end if;

         Count := Count + 1;
         Next (Tmp);
      end loop;

      Free (Tmp);

      Debug_Pop_Prefix;

   exception
      when others =>
         Debug_Pop_Prefix;
         Free (Tmp);
         raise;
   end Validate_Start_Element;

   --------------------------
   -- Validate_End_Element --
   --------------------------

   procedure Validate_End_Element
     (Validator      : access XML_All_Record;
      Reader         : access Abstract_Validation_Reader'Class;
      Local_Name     : Unicode.CES.Byte_Sequence;
      Data           : Validator_Data)
   is
      pragma Unreferenced (Local_Name);
      Tmp   : Particle_Iterator := Start (Validator.Particles);
      D     : constant All_Data_Access := All_Data_Access (Data);
      Count : Positive := 1;
   begin
      Debug_Push_Prefix ("Validate_End_Element <all> "
                         & Get_Name (Validator));

      while Get (Tmp) /= null loop
         if D.All_Elements (Count) < Get_Min_Occurs (Tmp) then
            declare
               L   : constant String := Get (Tmp).Element.Elem.Local_Name.all;
               Min : constant Integer := Get_Min_Occurs (Tmp);
            begin
               Free (Tmp);
               Validation_Error
                 (Reader, "Element """ & L & """ must appear at least"
                  & Integer'Image (Min) & " times");
            end;
         end if;

         Count := Count + 1;
         Next (Tmp);
      end loop;

      Free (Tmp);
      D.Num_Occurs := D.Num_Occurs + 1;

      if D.Num_Occurs < Validator.Min_Occurs then
         Validation_Error
           (Reader, "<all> element must be repeated at least"
            & Integer'Image (Validator.Min_Occurs) & " times");
      elsif Validator.Max_Occurs /= Unbounded
        and then D.Num_Occurs > Validator.Max_Occurs
      then
         Validation_Error
           (Reader,
            "<all> element was repeated too many times, expecting at most"
            & Integer'Image (Validator.Max_Occurs) & " times");
      end if;

      Debug_Pop_Prefix;

   exception
      when others =>
         Debug_Pop_Prefix;
         raise;
   end Validate_End_Element;

   ---------------------------
   -- Create_Validator_Data --
   ---------------------------

   function Create_Validator_Data
     (Validator : access XML_All_Record) return Validator_Data
   is
      Count : Natural := 0;
      Tmp   : Particle_Iterator := Start (Validator.Particles);
   begin
      while Get (Tmp) /= null loop
         Count := Count + 1;
         Next (Tmp);
      end loop;
      Free (Tmp);

      return new All_Data'
        (Group_Model_Data_Record with
         Num_Elements => Count,
         Num_Occurs   => 0,
         All_Elements => (others => 0));
   end Create_Validator_Data;

   ----------------
   -- Type_Model --
   ----------------

   function Type_Model
     (Validator  : access XML_All_Record;
      First_Only : Boolean) return Unicode.CES.Byte_Sequence is
   begin
      return Internal_Type_Model
        (Validator, "&", First_Only, All_In_List => True);
   end Type_Model;

   ------------------
   -- Can_Be_Empty --
   ------------------

   function Can_Be_Empty
     (Group : access XML_All_Record) return Boolean
   is
      Current : Particle_Iterator;
   begin
      Debug_Push_Prefix ("Can_Be_Empty " & Get_Name (Group));

      Current := Start (Group.Particles);
      while Get (Current) /= null loop
         if not Is_Optional (Current) then
            Free (Current);
            if Debug then
               Debug_Output ("Cannot be empty");
            end if;
            Debug_Pop_Prefix;
            return False;
         end if;

         Next (Current);
      end loop;

      Free (Current);

      if Debug then
         Debug_Output ("Can be empty");
      end if;
      Debug_Pop_Prefix;
      return True;

   exception
      when others =>
         Debug_Pop_Prefix;
         raise;
   end Can_Be_Empty;

   ----------------------------
   -- Validate_Start_Element --
   ----------------------------

   procedure Validate_Start_Element
     (Validator         : access XML_Any_Record;
      Reader            : access Abstract_Validation_Reader'Class;
      Local_Name        : Unicode.CES.Byte_Sequence;
      Namespace_URI     : Unicode.CES.Byte_Sequence;
      NS                : XML_Grammar_NS;
      Data              : Validator_Data;
      Grammar           : XML_Grammar;
      Element_Validator : out XML_Element)
   is
      pragma Unreferenced (Data);
      Start, Last, Index : Integer;
      C : Unicode_Char;
      Valid : Boolean := False;
   begin
      if Debug then
         Debug_Push_Prefix ("Validate ANY: namespaces="
                            & Validator.Namespace.all
                            & " " & Validator.Process_Contents'Img);
      end if;

      --  Do not check qualification, there is a special handling for
      --  namespaces

      if Validator.Namespace.all = "##any" then
         null;

      elsif Validator.Namespace.all = "##other" then
         if Namespace_URI = Validator.Target_NS.Namespace_URI.all then
            Validation_Error
              (Reader,
               "Namespace should be different from "
               & Validator.Target_NS.Namespace_URI.all);
         elsif Namespace_URI = "" then
            Validation_Error (Reader, "Element must specify a namespace");
         end if;

      else
         Index := Validator.Namespace'First;
         while Index <= Validator.Namespace'Last loop
            while Index <= Validator.Namespace'Last loop
               Start := Index;
               Encoding.Read (Validator.Namespace.all, Index, C);
               exit when not Is_White_Space (C);
            end loop;

            while Index <= Validator.Namespace'Last loop
               Last := Index;
               Encoding.Read (Validator.Namespace.all, Index, C);
               exit when Is_White_Space (C);
            end loop;

            if Index > Validator.Namespace'Last then
               Last := Validator.Namespace'Last + 1;
            end if;

            if Validator.Namespace
              (Start .. Last - 1) = "##targetNamespace"
            then
               Valid := Namespace_URI = Validator.Target_NS.Namespace_URI.all;
            elsif Validator.Namespace (Start .. Last - 1) = "##local" then
               Valid := Namespace_URI = "";
            else
               Valid :=
                 Namespace_URI = Validator.Namespace (Start .. Last - 1);
            end if;

            exit when Valid;
         end loop;

         if not Valid then
            Validation_Error
              (Reader, "Invalid namespace for this element: """
               & Namespace_URI & """ not in """
               & Validator.Namespace.all & """");
         end if;
      end if;

      Validate_Start_Element
        (Get_Validator
           (Get_Type (Get_UR_Type_Element
            (Grammar, Validator.Process_Contents))),
         Reader,
         Local_Name, Namespace_URI, NS, null,
         Grammar, Element_Validator);

      Debug_Pop_Prefix;

   exception
      when others =>
         Debug_Pop_Prefix;
         raise;
   end Validate_Start_Element;

   ----------
   -- Free --
   ----------

   procedure Free (Id : in out Id_Ref) is
   begin
      Free (Id.Key);
   end Free;

   -------------
   -- Get_Key --
   -------------

   function Get_Key (Id : Id_Ref) return Unicode.CES.Byte_Sequence is
   begin
      return Id.Key.all;
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
      List   : NS_List := Empty_NS_List) return Attribute_Validator
   is
      Result : constant Attribute_Validator :=
        new Any_Attribute_Validator'
          (NS_Count         => List'Length,
           Process_Contents => Process_Contents,
           Kind             => Kind,
           Next             => null,
           NS               => In_NS,
           List             => List);
   begin
      Register (In_NS, Result);
      return Result;
   end Create_Any_Attribute;

   ------------------------
   -- Validate_Attribute --
   ------------------------

   procedure Validate_Attribute
     (Validator : Any_Attribute_Validator;
      Reader    : access Abstract_Validation_Reader'Class;
      Atts      : in out Sax.Attributes.Attributes'Class;
      Index     : Natural)
   is
      URI  : constant Byte_Sequence := Get_URI (Atts, Index);
      Attr : Attribute_Validator;
      G    : XML_Grammar_NS;
      NS_Matches : Boolean := False;
   begin
      if Debug then
         Debug_Output ("Validate_Attribute, anyAttribute: "
                       & Validator.Kind'Img & " "
                       & Validator.Process_Contents'Img & " "
                       & Get_Local_Name (Atts, Index));
      end if;

      --  See 3.10.1 for interpretation of processContent.
      --  See also 3.4.2 for the intersection of <anyAttribute> elements

      case Validator.Kind is
         when Namespace_Other =>
            NS_Matches := URI /= ""
              and then URI /= Validator.NS.Namespace_URI.all;
         when Namespace_Any =>
            NS_Matches := True;
         when Namespace_List =>
            for N in Validator.List'Range loop
               if Validator.List (N).all = "##local" then
                  NS_Matches := NS_Matches or else URI = "";
               elsif Validator.List (N).all = "##targetNamespace" then
                  NS_Matches := NS_Matches
                    or else URI = Validator.NS.Namespace_URI.all;
               else
                  NS_Matches := NS_Matches
                    or else URI = Validator.List (N).all;
               end if;
            end loop;
      end case;

      if not NS_Matches then
         Validation_Error (Reader, "Invalid namespace for "
                           & To_QName
                             (Get_URI (Atts, Index),
                              Get_Local_Name (Atts, Index)));
      end if;

      case Validator.Process_Contents is
         when Process_Strict =>
            Get_NS (Reader.Grammar, URI, G);
            Attr := Lookup_Attribute
              (G, Reader,
               Get_Local_Name (Atts, Index), Create_If_Needed => False);
            if Attr = null then
               Validation_Error (Reader, "No definition provided");
            else
               Validate_Attribute (Attr.all, Reader, Atts, Index);

               if Is_ID (Attr.all) then
                  Set_Type (Atts, Index, Sax.Attributes.Id);
               end if;
            end if;

         when Process_Lax =>
            Get_NS (Reader.Grammar, URI, G);
            Attr := Lookup_Attribute
              (G, Reader,
               Get_Local_Name (Atts, Index), Create_If_Needed => False);
            if Attr = null then
               if Debug then
                  Debug_Output
                    ("Definition not found for attribute "
                     & To_QName (Get_URI (Atts, Index),
                                 Get_Local_Name (Atts, Index)));
               end if;
            else
               Validate_Attribute (Attr.all, Reader, Atts, Index);
               if Is_ID (Attr.all) then
                  Set_Type (Atts, Index, Sax.Attributes.Id);
               end if;
            end if;

         when Process_Skip =>
            null;
      end case;
   end Validate_Attribute;

   ----------
   -- Free --
   ----------

   procedure Free (Validator : in out Any_Attribute_Validator) is
   begin
      for N in Validator.List'Range loop
         Free (Validator.List (N));
      end loop;
   end Free;

   ------------------
   -- Set_Abstract --
   ------------------

   procedure Set_Abstract
     (Element     : XML_Element;
      Is_Abstract : Boolean) is
   begin
      Element.Elem.Is_Abstract := Is_Abstract;
   end Set_Abstract;

   -----------------
   -- Is_Abstract --
   -----------------

   function Is_Abstract (Element : XML_Element) return Boolean is
   begin
      return Element.Elem.Is_Abstract;
   end Is_Abstract;

   ------------------
   -- Set_Nillable --
   ------------------

   procedure Set_Nillable
     (Element  : XML_Element;
      Nillable : Boolean) is
   begin
      Element.Elem.Nillable := Nillable;
   end Set_Nillable;

   -----------------
   -- Is_Nillable --
   -----------------

   function Is_Nillable (Element : XML_Element) return Boolean is
   begin
      return Element.Elem.Nillable;
   end Is_Nillable;

   ---------------
   -- Set_Final --
   ---------------

   procedure Set_Final
     (Element : XML_Element;
      On_Restriction : Boolean;
      On_Extension   : Boolean;
      On_Unions      : Boolean;
      On_Lists       : Boolean)
   is
      pragma Unreferenced (On_Unions, On_Lists);
   begin
      Element.Elem.Final_Restriction := On_Restriction;
      Element.Elem.Final_Extension   := On_Extension;
   end Set_Final;

   ---------------
   -- Set_Final --
   ---------------

   procedure Set_Final
     (Typ            : XML_Type;
      On_Restriction : Boolean;
      On_Extension   : Boolean;
      On_Unions      : Boolean;
      On_Lists       : Boolean)
   is
      pragma Unreferenced (On_Unions, On_Lists);
   begin
      Typ.Final_Restriction := On_Restriction;
      Typ.Final_Extension   := On_Extension;
   end Set_Final;

   ---------------
   -- Set_Block --
   ---------------

   procedure Set_Block
     (Element : XML_Element;
      Blocks  : Block_Status) is
   begin
      Element.Elem.Blocks_Is_Set := True;
      Element.Elem.Blocks := Blocks;
   end Set_Block;

   ---------------
   -- Has_Block --
   ---------------

   function Has_Block (Element : XML_Element) return Boolean is
   begin
      return Element.Elem.Blocks_Is_Set;
   end Has_Block;

   ---------------
   -- Get_Block --
   ---------------

   function Get_Block (Element : XML_Element) return Block_Status is
   begin
      return Element.Elem.Blocks;
   end Get_Block;

   -----------------------
   -- Check_Replacement --
   -----------------------

   procedure Check_Replacement
     (Validator       : access XML_Validator_Record;
      Element         : XML_Element;
      Typ             : XML_Type;
      Valid           : out Boolean;
      Had_Restriction : in out Boolean;
      Had_Extension   : in out Boolean)
   is
      pragma Unreferenced (Validator, Element, Had_Restriction, Had_Extension);
   begin
      Valid := Is_Wildcard (Get_Validator (Typ));
   end Check_Replacement;

   -----------------------
   -- Free_Nested_Group --
   -----------------------

   procedure Free_Nested_Group (Data : Group_Model_Data) is
   begin
      Data.Nested := null;
      Free (Data.Nested_Data);
   end Free_Nested_Group;

   ----------
   -- Free --
   ----------

   procedure Free (Data : in out Group_Model_Data_Record) is
   begin
      Free (Data.Nested_Data);
      Free (Validator_Data_Record (Data));
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Data : in out Group_Model_Record) is
   begin
      Free (Data.Particles);
      Free (XML_Validator_Record (Data));
   end Free;

   ------------------------
   -- Check_Content_Type --
   ------------------------

   procedure Check_Content_Type
     (Validator        : access XML_Validator_Record;
      Reader           : access Abstract_Validation_Reader'Class;
      Should_Be_Simple : Boolean)
   is
      pragma Unreferenced (Validator);
   begin
      if Should_Be_Simple then
         Validation_Error
           (Reader,
            "Type specified in a simpleContent context must not have a "
            & "complexContent");
      end if;
   end Check_Content_Type;

   ---------------
   -- Is_Global --
   ---------------

   function Is_Global (Element : XML_Element) return Boolean is
   begin
      return Element.Elem.Is_Global;
   end Is_Global;

   -----------------------
   -- Debug_Push_Prefix --
   -----------------------

   procedure Debug_Push_Prefix (Append : String) is
   begin
      if Debug then
         Debug_Output (ASCII.ESC & "[36m" & Append
                       & ASCII.ESC & "[39m");
         Debug_Prefixes_Level := Debug_Prefixes_Level + 1;
      end if;
   end Debug_Push_Prefix;

   ----------------------
   -- Debug_Pop_Prefix --
   ----------------------

   procedure Debug_Pop_Prefix is
   begin
      if Debug then
         Debug_Prefixes_Level := Debug_Prefixes_Level - 1;
      end if;
   end Debug_Pop_Prefix;

   -------------------------
   -- Check_Qualification --
   -------------------------

   procedure Check_Qualification
     (Grammar       : XML_Grammar;
      Reader        : access Abstract_Validation_Reader'Class;
      Element       : XML_Element;
      Namespace_URI : Unicode.CES.Byte_Sequence) is
   begin
      if not Is_Global (Element)
        and then Element.Elem.Form = Unqualified
        and then Namespace_URI /= ""
      then
         Validation_Error
           (Reader, "Namespace specification not authorized in this context");

      elsif Element.Elem.Form = Qualified
        and then Namespace_URI = ""
        and then Get (Grammar).Target_NS /= null
      then
         Validation_Error
           (Reader, "Namespace specification is required in this context");
      end if;
   end Check_Qualification;

   ------------------
   -- Can_Be_Empty --
   ------------------

   function Can_Be_Empty
     (Group : access Group_Model_Record) return Boolean
   is
      pragma Unreferenced (Group);
   begin
      return False;
   end Can_Be_Empty;

   ------------------
   -- Can_Be_Empty --
   ------------------

   function Can_Be_Empty
     (Group : access Sequence_Record) return Boolean
   is
      Current : Particle_Iterator;
   begin
      Debug_Push_Prefix ("Can_Be_Empty " & Get_Name (Group));

      Current := Start (Group.Particles);
      while Get (Current) /= null loop
         if not Is_Optional (Current) then
            Free (Current);
            if Debug then
               Debug_Output ("Cannot be empty");
            end if;
            Debug_Pop_Prefix;
            return False;
         end if;
         Next (Current);
      end loop;

      Free (Current);

      if Debug then
         Debug_Output ("Can be empty");
      end if;
      Debug_Pop_Prefix;
      return True;

   exception
      when others =>
         Debug_Pop_Prefix;
         raise;
   end Can_Be_Empty;

   ------------------
   -- Can_Be_Empty --
   ------------------

   function Can_Be_Empty
     (Group : access Choice_Record) return Boolean
   is
      Current : Particle_Iterator;
   begin
      Debug_Push_Prefix ("Can_Be_Empty " & Get_Name (Group));

      Current := Start (Group.Particles);
      while Get (Current) /= null loop
         if Is_Optional (Current) then
            Free (Current);
            if Debug then
               Debug_Output ("Can be empty");
            end if;
            Debug_Pop_Prefix;
            return True;
         end if;

         Next (Current);
      end loop;

      Free (Current);

      if Debug then
         Debug_Output ("Cannot be empty");
      end if;
      Debug_Pop_Prefix;
      return False;

   exception
      when others =>
         Debug_Pop_Prefix;
         raise;
   end Can_Be_Empty;

   ------------------------
   -- Check_Content_Type --
   ------------------------

   procedure Check_Content_Type
     (Typ              : XML_Type;
      Reader           : access Abstract_Validation_Reader'Class;
      Should_Be_Simple : Boolean) is
   begin
      if Debug then
         Debug_Output ("Check_Content_Type: " & To_QName (Typ)
                       & " " & Typ.Simple_Type'Img
                       & " Expect_simple=" & Should_Be_Simple'Img);
      end if;

      if Typ.Simple_Type = Unknown_Content then
         if Typ.Validator /= null then
            Check_Content_Type (Typ.Validator, Reader, Should_Be_Simple);
         end if;

         --  If we matched, we now know the content type
         if Should_Be_Simple then
            Typ.Simple_Type := Simple_Content;
         else
            Typ.Simple_Type := Complex_Content;
         end if;

      elsif Should_Be_Simple and Typ.Simple_Type = Complex_Content then
         if Typ.Local_Name /= null then
            Validation_Error
              (Reader,
               To_QName (Typ) & " specified in a simpleContent context"
               & " must not have a complexContext");
         else
            Validation_Error
              (Reader, "Expecting simple type, got complex type");
         end if;
      elsif not Should_Be_Simple and Typ.Simple_Type = Simple_Content then
         Validation_Error (Reader, "Expecting complex type, got simple type");
      end if;
   end Check_Content_Type;

   --------------------
   -- Is_Simple_Type --
   --------------------

   function Is_Simple_Type
     (Reader : access Abstract_Validation_Reader'Class;
      Typ    : XML_Type) return Boolean is
   begin
      if Typ.Simple_Type = Unknown_Content then
         begin
            Check_Content_Type
              (Typ.Validator, Reader, Should_Be_Simple => True);
            Typ.Simple_Type := Simple_Content;
         exception
            when XML_Validation_Error =>
               Typ.Simple_Type := Complex_Content;
         end;
      end if;

      return Typ.Simple_Type = Simple_Content;
   end Is_Simple_Type;

   ----------------
   -- Type_Model --
   ----------------

   function Type_Model
     (Validator : access Group_Model_Record;
      First_Only : Boolean) return Byte_Sequence is
   begin
      return Internal_Type_Model
        (Validator, "", First_Only, All_In_List => True);
   end Type_Model;

   ----------------
   -- Type_Model --
   ----------------

   function Type_Model
     (Validator  : access Sequence_Record;
      First_Only : Boolean) return Byte_Sequence is
   begin
      return Internal_Type_Model
        (Validator, ",", First_Only, All_In_List => not First_Only);
   end Type_Model;

   ----------------
   -- Type_Model --
   ----------------

   function Type_Model
     (Validator : access Choice_Record;
      First_Only : Boolean) return Byte_Sequence is
   begin
      return Internal_Type_Model
        (Validator, "|", First_Only => First_Only, All_In_List => True);
   end Type_Model;

   -------------------------
   -- Internal_Type_Model --
   -------------------------

   function Internal_Type_Model
     (Validator : access Group_Model_Record'Class;
      Separator : Byte_Sequence;
      First_Only : Boolean;
      All_In_List : Boolean) return Byte_Sequence
   is
      Str : Unbounded_String;
      Iter : Particle_Iterator := Start (Validator.Particles);
      First : Boolean := True;
   begin
      if not All_In_List then
         Str := Str & Type_Model (Iter, First_Only);
      else
         while Get (Iter) /= null loop
            if not First then
               Str := Str & Separator & Type_Model (Iter, First_Only);
            else
               Str := Str & Type_Model (Iter, First_Only);
               First := False;
            end if;

            Next (Iter);
         end loop;
      end if;
      Free (Iter);

      return To_String (Str);
   end Internal_Type_Model;

   ----------------
   -- Type_Model --
   ----------------

   function Type_Model
     (Iter       : Particle_Iterator;
      First_Only : Boolean) return Byte_Sequence
   is
      Particle : constant XML_Particle_Access := Get (Iter);
      Str : Unbounded_String;
   begin
      case Particle.Typ is
         when Particle_Element =>
            Str := Str & To_QName (Particle.Element);
         when Particle_Nested =>
            Str := Str & "("
              & Type_Model (Particle.Validator, First_Only) & ")";
         when Particle_Group | Particle_XML_Type =>
            raise Program_Error;
         when Particle_Any =>
            Str := Str & "<any>";
      end case;

      if not First_Only then
         if Get_Min_Occurs (Iter) = 0 then
            if Get_Max_Occurs (Iter) = Unbounded then
               Str := Str & "*";
            elsif Get_Max_Occurs (Iter) = 1 then
               Str := Str & "?";
            else
               Str := Str & "{0,"
                 & Integer'Image (Get_Max_Occurs (Iter)) & "}";
            end if;

         elsif Get_Min_Occurs (Iter) = 1 then
            if Get_Max_Occurs (Iter) = Unbounded then
               Str := Str & "+";
            elsif Get_Max_Occurs (Iter) /= 1 then
               Str := Str & "{"
                 & Integer'Image (Get_Min_Occurs (Iter)) & ","
                 & Integer'Image (Get_Max_Occurs (Iter)) & "}";
            end if;
         else
            Str := Str & "{"
              & Integer'Image (Get_Min_Occurs (Iter)) & ","
              & Integer'Image (Get_Max_Occurs (Iter)) & "}";
         end if;
      end if;

      return To_String (Str);
   end Type_Model;

   -----------------------
   -- Get_Namespace_URI --
   -----------------------

   function Get_Namespace_URI
     (Grammar : XML_Grammar_NS) return Unicode.CES.Byte_Sequence is
   begin
      if Grammar = null then
         return "";
      else
         return Grammar.Namespace_URI.all;
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

   ------------------------------
   -- Get_Final_On_Restriction --
   ------------------------------

   function Get_Final_On_Restriction (Typ : XML_Type) return Boolean is
   begin
      return Typ.Final_Restriction;
   end Get_Final_On_Restriction;

   ----------------------------
   -- Get_Final_On_Extension --
   ----------------------------

   function Get_Final_On_Extension (Typ : XML_Type) return Boolean is
   begin
      return Typ.Final_Extension;
   end Get_Final_On_Extension;

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

   ------------------------------------------
   -- Get_Namespace_From_Parent_For_Locals --
   ------------------------------------------

   function Get_Namespace_From_Parent_For_Locals
     (Validator : access XML_Any_Record) return Boolean
   is
      pragma Unreferenced (Validator);
   begin
      return False;
   end Get_Namespace_From_Parent_For_Locals;

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
      Result : constant XML_Validator := new XML_Union_Record;
   begin
      Register (G, Result);
      return Result;
   end Create_Union;

   ---------------
   -- Add_Union --
   ---------------

   procedure Add_Union
     (Validator : access XML_Validator_Record'Class;
      Reader    : access Abstract_Validation_Reader'Class;
      Part      : XML_Type) is
   begin
      Schema.Validators.Simple_Types.Add_Union
        (XML_Union (Validator), Reader, Part);
   end Add_Union;

   --------------
   -- To_QName --
   --------------

   function To_QName (Namespace_URI, Local_Name : String) return String is
   begin
      if Namespace_URI = "" then
         return Local_Name;
      else
         return '{' & Namespace_URI & '}' & Local_Name;
      end if;
   end To_QName;

   --------------
   -- Check_Id --
   --------------

   procedure Check_Id
     (Reader    : access Abstract_Validation_Reader'Class;
      Validator : access XML_Validator_Record'Class;
      Value     : Unicode.CES.Byte_Sequence)
   is
   begin
      if Is_ID (Validator.all) then
         if Reader.Id_Table = null then
            Reader.Id_Table := new Id_Htable.HTable (101);
         else
            if Id_Htable.Get
              (Reader.Id_Table.all, Value) /= No_Id
            then
               Validation_Error
                 (Reader, "ID """ & Value & """ already defined");
            end if;
         end if;

         Id_Htable.Set
           (Reader.Id_Table.all,
            Id_Ref'(Key => new Byte_Sequence'(Value)));
      end if;
   end Check_Id;

   -------------------
   -- Set_System_Id --
   -------------------

   procedure Set_System_Id
     (Grammar   : XML_Grammar_NS;
      System_Id : Unicode.CES.Byte_Sequence) is
   begin
      Free (Grammar.System_ID);
      Grammar.System_ID := new Byte_Sequence'(System_Id);
   end Set_System_Id;

   -------------------
   -- Get_System_Id --
   -------------------

   function Get_System_Id
     (Grammar : XML_Grammar_NS) return Unicode.CES.Byte_Sequence is
   begin
      if Grammar.System_ID = null then
         return "";
      else
         return Grammar.System_ID.all;
      end if;
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

   function Is_Extension_Of
     (Element : XML_Element; Base : XML_Element) return Boolean is
   begin
      return Is_Extension_Of
        (Element.Elem.Of_Type.Validator.all,
         Base.Elem.Of_Type.Validator);
   end Is_Extension_Of;

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

   function Is_Restriction_Of
     (Element : XML_Element; Base : XML_Element) return Boolean is
   begin
      return Is_Restriction_Of
        (Element.Elem.Of_Type.Validator.all,
         Base.Elem.Of_Type.Validator);
   end Is_Restriction_Of;

   --------------------
   -- Compute_Blocks --
   --------------------

   procedure Compute_Blocks
     (Value  : Unicode.CES.Byte_Sequence;
      Reader : access Abstract_Validation_Reader'Class;
      Blocks : out Block_Status)
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
              (Reader, "Invalid value for block: """ & Str & """");
         end if;
      end On_Item;

      procedure For_Each
        is new Schema.Validators.Lists.For_Each_Item (On_Item);
   begin
      Blocks := (others => False);
      For_Each (Value);
   end Compute_Blocks;

   -------------------
   -- Compute_Final --
   -------------------

   procedure Compute_Final
     (Value         : Unicode.CES.Byte_Sequence;
      Reader        : access Abstract_Validation_Reader'Class;
      Restrictions  : out Boolean;
      Extensions    : out Boolean;
      Unions        : out Boolean;
      Lists         : out Boolean)
   is
      procedure On_Item (Str : Byte_Sequence);
      procedure On_Item (Str : Byte_Sequence) is
      begin
         if Str = "restriction" then
            Restrictions := True;
         elsif Str = "extension" then
            Extensions := True;
         elsif Str = "#all" then
            Restrictions := True;
            Extensions := True;
            Unions     := True;
            Lists      := True;
         elsif Str = "union" then
            Unions := True;
         elsif Str = "list" then
            Lists := True;
         else
            Validation_Error
              (Reader, "Invalid value for final: """ & Str & """");
         end if;
      end On_Item;

      procedure For_Each
         is new Schema.Validators.Lists.For_Each_Item (On_Item);
   begin
      Restrictions := False;
      Extensions   := False;
      Unions       := False;
      Lists        := False;
      For_Each (Value);
   end Compute_Final;

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

   --------------------------------
   -- Check_Replacement_For_Type --
   --------------------------------

   procedure Check_Replacement_For_Type
     (Validator         : access XML_Validator_Record'Class;
      Element           : XML_Element;
      Valid             : out Boolean;
      Had_Restriction   : in out Boolean;
      Had_Extension     : in out Boolean)
   is
      Typ : constant XML_Type := Get_Type (Element);
   begin
      if Get_Validator (Typ).all in XML_Union_Record'Class then
         Check_Replacement_For_Union
           (Validator,
            XML_Union_Record (Get_Validator (Typ).all),
            Element         => Element,
            Valid           => Valid,
            Had_Restriction => Had_Restriction,
            Had_Extension   => Had_Extension);

      else
         Check_Replacement
           (Validator,
            Element         => Element,
            Typ             => Typ,
            Valid           => Valid,
            Had_Restriction => Had_Restriction,
            Had_Extension   => Had_Extension);
      end if;
   end Check_Replacement_For_Type;

   ----------
   -- Free --
   ----------

   procedure Free (Reader : access Abstract_Validation_Reader) is
   begin
      Free (Reader.Error_Msg);
   end Free;

   -----------------------
   -- Get_Error_Message --
   -----------------------

   function Get_Error_Message
     (Reader : Abstract_Validation_Reader)
      return Unicode.CES.Byte_Sequence is
   begin
      if Reader.Error_Msg = null then
         return "";
      else
         return Reader.Error_Msg.all;
      end if;
   end Get_Error_Message;

end Schema.Validators;
