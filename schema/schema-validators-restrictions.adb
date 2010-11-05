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

with Schema.Validators.Facets;  use Schema.Validators.Facets;
with Sax.Symbols;               use Sax.Symbols;

package body Schema.Validators.Restrictions is

   type Restriction_XML_Validator is new XML_Validator_Record with record
      Base              : XML_Type;
      Restriction       : XML_Validator;
      Facets            : Facets_Description;
   end record;
   type Restriction_Type is access Restriction_XML_Validator'Class;

   overriding procedure Validate_Characters
     (Validator     : access Restriction_XML_Validator;
      Reader        : access Abstract_Validation_Reader'Class;
      Ch            : Unicode.CES.Byte_Sequence;
      Empty_Element : Boolean;
      Mask          : in out Facets_Mask);
   overriding procedure Get_Attribute_Lists
     (Validator   : access Restriction_XML_Validator;
      List        : out Attribute_Validator_List_Access;
      Dependency1 : out XML_Validator;
      Ignore_Wildcard_In_Dep1 : out Boolean;
      Dependency2 : out XML_Validator;
      Must_Match_All_Any_In_Dep2 : out Boolean);
   overriding procedure Add_Facet
     (Validator   : access Restriction_XML_Validator;
      Reader      : access Abstract_Validation_Reader'Class;
      Facet_Name  : Symbol;
      Facet_Value : Unicode.CES.Byte_Sequence);
   overriding procedure Check_Replacement
     (Validator         : access Restriction_XML_Validator;
      Element           : XML_Element;
      Typ               : XML_Type;
      Valid             : out Boolean;
      Had_Restriction   : in out Boolean;
      Had_Extension     : in out Boolean);
   overriding procedure Check_Content_Type
     (Validator        : access Restriction_XML_Validator;
      Reader           : access Abstract_Validation_Reader'Class;
      Should_Be_Simple : Boolean);
   overriding function Get_Facets
     (Validator : access Restriction_XML_Validator;
      Reader    : access Abstract_Validation_Reader'Class)
      return Facets_Description;
   overriding procedure Free (Validator : in out Restriction_XML_Validator);
   overriding function Is_ID
     (Validator : Restriction_XML_Validator) return Boolean;
   overriding function Is_Restriction_Of
     (Validator : Restriction_XML_Validator;
      Base      : access XML_Validator_Record'Class) return Boolean;
   overriding function Equal
     (Validator      : access Restriction_XML_Validator;
      Reader         : access Abstract_Validation_Reader'Class;
      Value1, Value2 : Unicode.CES.Byte_Sequence) return Boolean;
   --  See doc from inherited subprograms

   -----------
   -- Is_ID --
   -----------

   function Is_ID (Validator : Restriction_XML_Validator) return Boolean is
   begin
      return Is_ID (Validator.Base);
   end Is_ID;

   ----------
   -- Free --
   ----------

   procedure Free (Validator : in out Restriction_XML_Validator) is
   begin
      Free (Validator.Facets);
      Free (XML_Validator_Record (Validator));
   end Free;

   ----------------
   -- Get_Facets --
   ----------------

   function Get_Facets
     (Validator : access Restriction_XML_Validator;
      Reader    : access Abstract_Validation_Reader'Class)
      return Facets_Description
   is
      Base_Facets : Facets_Description;
   begin
      if Validator.Facets = null then
         if Validator.Base.Validator /= null then
            Base_Facets := Get_Facets (Validator.Base.Validator, Reader);
            if Base_Facets /= null then
               --  ??? Doesn't work if we do not know the full facets for the
               --  parent or if we modify any of them later on.
               Validator.Facets := new Facets_Description_Record'Class'
                 (Base_Facets.all);

               Copy (From => Base_Facets.all, To => Validator.Facets.all);

               --  "pattern" are overridden in the context of restriction.
               --  ??? If not defined in the restriction, we should still use
               --  the parent's pattern
               Common_Facets_Description (Validator.Facets.all)
                 .Pattern_String := No_Symbol;
               Common_Facets_Description (Validator.Facets.all)
                 .Mask (Facet_Pattern) := False;
            end if;
         else
            Validation_Error
              (Reader, "#The type """ & To_QName (Validator.Base)
               & """ isn't known at this point. Please check the name and"
               & " namespace");
         end if;
      end if;

      return Validator.Facets;
   end Get_Facets;

   -------------------------
   -- Get_Attribute_Lists --
   -------------------------

   procedure Get_Attribute_Lists
     (Validator   : access Restriction_XML_Validator;
      List        : out Attribute_Validator_List_Access;
      Dependency1 : out XML_Validator;
      Ignore_Wildcard_In_Dep1 : out Boolean;
      Dependency2 : out XML_Validator;
      Must_Match_All_Any_In_Dep2 : out Boolean) is
   begin
      --  A restriction has the same list of attributes as the base type
      --  (as per Primer 4.4) plus possibly some new ones, but it doesn't
      --  inherit the wildcards

      List := Validator.Attributes;
      Dependency1 := Validator.Base.Validator;
      Ignore_Wildcard_In_Dep1 := True;

      Dependency2 := Validator.Restriction;
      Must_Match_All_Any_In_Dep2 := True;
   end Get_Attribute_Lists;

   -----------
   -- Equal --
   -----------

   function Equal
     (Validator      : access Restriction_XML_Validator;
      Reader         : access Abstract_Validation_Reader'Class;
      Value1, Value2 : Unicode.CES.Byte_Sequence) return Boolean is
   begin
      return Equal (Get_Validator (Validator.Base), Reader, Value1, Value2);
   end Equal;

   ---------------
   -- Add_Facet --
   ---------------

   overriding procedure Add_Facet
     (Validator   : access Restriction_XML_Validator;
      Reader      : access Abstract_Validation_Reader'Class;
      Facet_Name  : Symbol;
      Facet_Value : Unicode.CES.Byte_Sequence)
   is
      Applies : Boolean;
      Facets  : constant Facets_Description := Get_Facets (Validator, Reader);
   begin
      if Facets = null then
         Validation_Error (Reader, "#No facet overridable for this type");
      end if;

      Add_Facet (Facets.all, Reader, Facet_Name, Facet_Value, Applies);
      if not Applies then
         Validation_Error (Reader, "#Invalid facet: " & Get (Facet_Name).all);
      end if;
   end Add_Facet;

   -------------------------
   -- Validate_Characters --
   -------------------------

   overriding procedure Validate_Characters
     (Validator     : access Restriction_XML_Validator;
      Reader        : access Abstract_Validation_Reader'Class;
      Ch            : Unicode.CES.Byte_Sequence;
      Empty_Element : Boolean;
      Mask          : in out Facets_Mask)
   is
      Orig_Mask : constant Facets_Mask := Mask;
      Mask2     : Facets_Mask;
   begin
      if Debug then
         Debug_Push_Prefix ("Validate_Chars (restr) " & Get_Name (Validator));
      end if;

      if Validator.Facets /= null then
         Check_Facet (Validator.Facets.all, Reader, Ch, Mask);
      end if;

      if Validator.Restriction /= null then
         Validate_Characters
           (Validator.Restriction, Reader, Ch, Empty_Element, Mask);
      else
         --  We need to match the restrictions of the parent too
         Mask2 := Orig_Mask;
         Validate_Characters
           (Get_Validator (Validator.Base), Reader,
            Ch, Empty_Element, Mask2);
         Mask := Mask or Mask2;
      end if;

      Debug_Pop_Prefix;
   exception
      when XML_Validation_Error =>
         Debug_Pop_Prefix;
         raise;
   end Validate_Characters;

   -----------------------
   -- Check_Replacement --
   -----------------------

   overriding procedure Check_Replacement
     (Validator       : access Restriction_XML_Validator;
      Element         : XML_Element;
      Typ             : XML_Type;
      Valid           : out Boolean;
      Had_Restriction : in out Boolean;
      Had_Extension   : in out Boolean)
   is
      B : constant XML_Validator := Get_Validator (Typ);
      Block : Block_Status;
   begin
      --  See rule in extensions implementation
      --  As per 3.3.4.3, "block" comes from either the element declaration, or
      --  if there is none, from the type.

      if Has_Block (Element) then
         Block := Get_Block (Element);
      else
         Block := Get_Block (Typ);
      end if;

      Valid := (XML_Validator (Validator) = B    --  1
                or else not Block (Block_Restriction));

      if Valid then
         Valid := XML_Validator (Validator) = B        --  2.1
           or else Get_Validator (Validator.Base) = B; --  2.2

         if not Valid
           and then not Is_Wildcard (Get_Validator (Validator.Base)) --  2.3.1
         then
            Check_Replacement                       --  2.3.2
              (Get_Validator (Validator.Base), Element,
               Typ, Valid, Had_Restriction, Had_Extension);
         end if;
      end if;

      Had_Restriction := True;
   end Check_Replacement;

   --------------------
   -- Is_Simple_Type --
   --------------------

   overriding procedure Check_Content_Type
     (Validator        : access Restriction_XML_Validator;
      Reader           : access Abstract_Validation_Reader'Class;
      Should_Be_Simple : Boolean) is
   begin
      Check_Content_Type (Validator.Base, Reader, Should_Be_Simple);
   end Check_Content_Type;

   ---------------------------
   -- Create_Restriction_Of --
   ---------------------------

   function Create_Restriction_Of
     (G           : XML_Grammar_NS;
      Reader      : access Abstract_Validation_Reader'Class;
      Base        : XML_Type;
      Restriction : XML_Validator := null) return XML_Validator
   is
      Result : constant Restriction_Type := new Restriction_XML_Validator;
   begin
      if Get_Final (Base)(Final_Restriction) then
         Validation_Error
           (Reader, "#Type """ & To_QName (Base) & """ forbids restrictions");
      end if;

      Register (G, Base);
      Result.Base        := Base;
      if Restriction /= null then
         Register (G, Restriction);
      end if;
      Result.Restriction := Restriction;
      Register (G, Result);
      return XML_Validator (Result);
   end Create_Restriction_Of;

   -----------------------
   -- Is_Restriction_Of --
   -----------------------

   overriding function Is_Restriction_Of
     (Validator : Restriction_XML_Validator;
      Base      : access XML_Validator_Record'Class) return Boolean
   is
   begin
      return Validator.Base.Validator = XML_Validator (Base)
        or else Is_Restriction_Of
          (Validator.Base.Validator.all, Base => Base);
   end Is_Restriction_Of;

end Schema.Validators.Restrictions;
