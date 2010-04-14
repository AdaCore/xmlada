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

with Schema.Validators.Facets;  use Schema.Validators.Facets;

package body Schema.Validators.Restrictions is

   type Restriction_XML_Validator is new XML_Validator_Record with record
      Base              : XML_Type;
      Restriction       : XML_Validator;
      Facets            : Facets_Description;
   end record;
   type Restriction_Type is access Restriction_XML_Validator'Class;
   type Restriction_Data is new Validator_Data_Record with record
      Restriction_Data : Validator_Data;
   end record;
   type Restriction_Data_Access is access all Restriction_Data'Class;

   procedure Free (Data : in out Restriction_Data);
   function Create_Validator_Data
     (Validator : access Restriction_XML_Validator) return Validator_Data;
   procedure Validate_Start_Element
     (Validator         : access Restriction_XML_Validator;
      Local_Name        : Unicode.CES.Byte_Sequence;
      Namespace_URI     : Unicode.CES.Byte_Sequence;
      NS                : XML_Grammar_NS;
      Data              : Validator_Data;
      Grammar           : XML_Grammar;
      Element_Validator : out XML_Element);
   procedure Validate_End_Element
     (Validator      : access Restriction_XML_Validator;
      Local_Name     : Unicode.CES.Byte_Sequence;
      Data           : Validator_Data);
   procedure Validate_Characters
     (Validator     : access Restriction_XML_Validator;
      Ch            : Unicode.CES.Byte_Sequence;
      Empty_Element : Boolean;
      Context       : in out Validation_Context);
   procedure Get_Attribute_Lists
     (Validator   : access Restriction_XML_Validator;
      List        : out Attribute_Validator_List_Access;
      Dependency1 : out XML_Validator;
      Ignore_Wildcard_In_Dep1 : out Boolean;
      Dependency2 : out XML_Validator;
      Must_Match_All_Any_In_Dep2 : out Boolean);
   procedure Add_Facet
     (Validator   : access Restriction_XML_Validator;
      Facet_Name  : Unicode.CES.Byte_Sequence;
      Facet_Value : Unicode.CES.Byte_Sequence);
   procedure Check_Replacement
     (Validator         : access Restriction_XML_Validator;
      Typ               : XML_Type;
      Had_Restriction   : in out Boolean;
      Had_Extension     : in out Boolean);
   procedure Check_Content_Type
     (Validator        : access Restriction_XML_Validator;
      Should_Be_Simple : Boolean);
   function Get_Facets
     (Validator : access Restriction_XML_Validator) return Facets_Description;
   procedure Free (Validator : in out Restriction_XML_Validator);
   function Is_ID (Validator : Restriction_XML_Validator) return Boolean;
   function Is_Restriction_Of
     (Validator : Restriction_XML_Validator;
      Base      : access XML_Validator_Record'Class) return Boolean;
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
     (Validator : access Restriction_XML_Validator) return Facets_Description
   is
      Base_Facets : Facets_Description;
   begin
      if Validator.Facets = null then
         if Validator.Base.Validator /= null then
            Base_Facets := Get_Facets (Validator.Base.Validator);
            if Base_Facets /= null then
               --  ??? Doesn't work if we do not know the full facets for the
               --  parent or if we modify any of them later on.
               Validator.Facets := new Facets_Description_Record'Class'
                 (Base_Facets.all);

               Copy (From => Base_Facets.all, To => Validator.Facets.all);

               --  "pattern" are overridden in the context of restriction.
               --  ??? If not defined in the restriction, we should still use
               --  the parent's pattern
               Unicode.CES.Free
                 (Common_Facets_Description (Validator.Facets.all)
                  .Pattern_String);
               Common_Facets_Description (Validator.Facets.all)
                 .Mask (Facet_Pattern) := False;
            end if;
         else
            Validation_Error
              ("The type """ & Get_Local_Name (Validator.Base)
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

   ----------------------------
   -- Validate_Start_Element --
   ----------------------------

   procedure Validate_Start_Element
     (Validator         : access Restriction_XML_Validator;
      Local_Name        : Unicode.CES.Byte_Sequence;
      Namespace_URI     : Unicode.CES.Byte_Sequence;
      NS                : XML_Grammar_NS;
      Data              : Validator_Data;
      Grammar           : XML_Grammar;
      Element_Validator : out XML_Element)
   is
      D : constant Restriction_Data_Access := Restriction_Data_Access (Data);
   begin
      if Debug then
         Debug_Push_Prefix
           ("Validate_Start_Element for restriction " & Get_Name (Validator));
      end if;

      if Validator.Restriction /= null then
         Validate_Start_Element
           (Validator.Restriction, Local_Name, Namespace_URI, NS,
            D.Restriction_Data, Grammar, Element_Validator);

         if Debug then
            if Element_Validator /= No_Element then
               Debug_Output
                 ("Validate_Start_Element: end of restriction, result="
                  & Element_Validator.Elem.Local_Name.all);
            else
               Debug_Output
                 ("Validate_Start_Element: end of restriction, no"
                  & " match from restriction");
            end if;
         end if;

      else
         --  We never need to analyze the base, since all relevant elements are
         --  already duplicates in the restriction part (or else the element
         --  must be null)
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
     (Validator      : access Restriction_XML_Validator;
      Local_Name     : Unicode.CES.Byte_Sequence;
      Data           : Validator_Data)
   is
      D : constant Restriction_Data_Access := Restriction_Data_Access (Data);
   begin
      if Validator.Restriction /= null then
         Validate_End_Element (Validator.Restriction, Local_Name,
                               D.Restriction_Data);
      else
         Validate_End_Element
           (Get_Validator (Validator.Base), Local_Name, D.Restriction_Data);
      end if;
   end Validate_End_Element;

   ---------------
   -- Add_Facet --
   ---------------

   procedure Add_Facet
     (Validator   : access Restriction_XML_Validator;
      Facet_Name  : Unicode.CES.Byte_Sequence;
      Facet_Value : Unicode.CES.Byte_Sequence)
   is
      Applies : Boolean;
      Facets  : constant Facets_Description := Get_Facets (Validator);
   begin
      if Facets = null then
         Validation_Error ("No facet overridable for this type");
      end if;

      Add_Facet (Facets.all, Facet_Name, Facet_Value, Applies);
      if not Applies then
         Validation_Error ("Invalid facet: " & Facet_Name);
      end if;
   end Add_Facet;

   -------------------------
   -- Validate_Characters --
   -------------------------

   procedure Validate_Characters
     (Validator     : access Restriction_XML_Validator;
      Ch            : Unicode.CES.Byte_Sequence;
      Empty_Element : Boolean;
      Context       : in out Validation_Context) is
   begin
      if Debug then
         Debug_Output
           ("Validate_Characters for restriction " & Get_Name (Validator)
            & " --" & Ch & "--");
      end if;

      if Validator.Facets /= null then
         Check_Facet (Validator.Facets.all, Ch);
      end if;

      if Validator.Restriction /= null then
         Validate_Characters
           (Validator.Restriction, Ch, Empty_Element, Context);
      else
         Validate_Characters
           (Get_Validator (Validator.Base), Ch, Empty_Element, Context);
      end if;
   end Validate_Characters;

   ----------
   -- Free --
   ----------

   procedure Free (Data : in out Restriction_Data) is
   begin
      Free (Data.Restriction_Data);
      Free (Validator_Data_Record (Data));
   end Free;

   ---------------------------
   -- Create_Validator_Data --
   ---------------------------

   function Create_Validator_Data
     (Validator : access Restriction_XML_Validator) return Validator_Data
   is
      D : constant Restriction_Data_Access := new Restriction_Data;
   begin
      Free (D.Restriction_Data);
      if Validator.Restriction /= null then
         D.Restriction_Data := Create_Validator_Data (Validator.Restriction);
      else
         D.Restriction_Data := Create_Validator_Data
           (Get_Validator (Validator.Base));
      end if;
      return Validator_Data (D);
   end Create_Validator_Data;

   -----------------------
   -- Check_Replacement --
   -----------------------

   procedure Check_Replacement
     (Validator         : access Restriction_XML_Validator;
      Typ               : XML_Type;
      Had_Restriction   : in out Boolean;
      Had_Extension     : in out Boolean) is
   begin
      Had_Restriction := True;

      if Validator.Base.Block_Restriction then
         Validation_Error
           ("Restrictions of type """
            & Get_Local_Name (Validator.Base) & """ are forbidden");
      end if;

      if Validator.Base.Block_Extension and then Had_Extension then
         Validation_Error
           ("Extensions of type """
            & Get_Local_Name (Validator.Base) & """ are forbidden");
      end if;

      if Validator.Base /= Typ then
         Check_Replacement
           (Get_Validator (Validator.Base), Typ,
            Had_Restriction => Had_Restriction,
            Had_Extension   => Had_Extension);
      end if;
   end Check_Replacement;

   --------------------
   -- Is_Simple_Type --
   --------------------

   procedure Check_Content_Type
     (Validator        : access Restriction_XML_Validator;
      Should_Be_Simple : Boolean) is
   begin
      Check_Content_Type (Validator.Base, Should_Be_Simple);
   end Check_Content_Type;

   ---------------------------
   -- Create_Restriction_Of --
   ---------------------------

   function Create_Restriction_Of
     (G           : XML_Grammar_NS;
      Base        : XML_Type;
      Restriction : XML_Validator := null) return XML_Validator
   is
      Result : constant Restriction_Type := new Restriction_XML_Validator;
   begin
      if Get_Final_On_Restriction (Base) then
         Validation_Error
           ("Type """ & Get_Local_Name (Base) & """ forbids restrictions");
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

   function Is_Restriction_Of
     (Validator : Restriction_XML_Validator;
      Base      : access XML_Validator_Record'Class) return Boolean
   is
   begin
      return Validator.Base.Validator = XML_Validator (Base)
        or else Is_Restriction_Of
          (Validator.Base.Validator.all, Base => Base);
   end Is_Restriction_Of;

end Schema.Validators.Restrictions;
