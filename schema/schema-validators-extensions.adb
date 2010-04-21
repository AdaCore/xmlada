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

package body Schema.Validators.Extensions is

   type Extension_XML_Validator is new XML_Validator_Record with record
      Base      : XML_Type;
      Extension : XML_Validator;
      Facets_Merged : Boolean := False;
   end record;
   type Extension_Type is access Extension_XML_Validator'Class;
   type Extension_Data is new Validator_Data_Record with record
      Validating_Base : Boolean := True;
      Base_Data       : Validator_Data;
      Extension_Data  : Validator_Data;
   end record;
   type Extension_Data_Access is access all Extension_Data'Class;

   procedure Free (Data : in out Extension_Data);
   function Create_Validator_Data
     (Validator : access Extension_XML_Validator) return Validator_Data;
   procedure Validate_Start_Element
     (Validator         : access Extension_XML_Validator;
      Reader            : access Abstract_Validation_Reader'Class;
      Local_Name        : Unicode.CES.Byte_Sequence;
      Namespace_URI     : Unicode.CES.Byte_Sequence;
      NS                : XML_Grammar_NS;
      Data              : Validator_Data;
      Grammar           : XML_Grammar;
      Element_Validator : out XML_Element);
   procedure Validate_End_Element
     (Validator      : access Extension_XML_Validator;
      Reader         : access Abstract_Validation_Reader'Class;
      Local_Name     : Unicode.CES.Byte_Sequence;
      Data           : Validator_Data);
   procedure Validate_Characters
     (Validator     : access Extension_XML_Validator;
      Reader        : access Abstract_Validation_Reader'Class;
      Ch            : Unicode.CES.Byte_Sequence;
      Empty_Element : Boolean;
      Mask          : in out Facets_Mask);
   procedure Get_Attribute_Lists
     (Validator   : access Extension_XML_Validator;
      List        : out Attribute_Validator_List_Access;
      Dependency1 : out XML_Validator;
      Ignore_Wildcard_In_Dep1 : out Boolean;
      Dependency2 : out XML_Validator;
      Must_Match_All_Any_In_Dep2 : out Boolean);
   procedure Check_Replacement
     (Validator       : access Extension_XML_Validator;
      Element         : XML_Element;
      Typ             : XML_Type;
      Valid           : out Boolean;
      Had_Restriction : in out Boolean;
      Had_Extension   : in out Boolean);
   procedure Check_Content_Type
     (Validator        : access Extension_XML_Validator;
      Reader           : access Abstract_Validation_Reader'Class;
      Should_Be_Simple : Boolean);
   function Is_Extension_Of
     (Validator : Extension_XML_Validator;
      Base      : access XML_Validator_Record'Class) return Boolean;
   function Get_Mixed_Content
     (Validator : access Extension_XML_Validator) return Boolean;
   function Get_Facets
     (Validator : access Extension_XML_Validator;
      Reader : access Abstract_Validation_Reader'Class)
      return Facets_Description;
   function Equal
     (Validator : access Extension_XML_Validator;
      Reader : access Abstract_Validation_Reader'Class;
      Value1, Value2 : Unicode.CES.Byte_Sequence) return Boolean;
   --  See doc from inherited subprograms

   -------------------------
   -- Get_Attribute_Lists --
   -------------------------

   procedure Get_Attribute_Lists
     (Validator   : access Extension_XML_Validator;
      List        : out Attribute_Validator_List_Access;
      Dependency1 : out XML_Validator;
      Ignore_Wildcard_In_Dep1 : out Boolean;
      Dependency2 : out XML_Validator;
      Must_Match_All_Any_In_Dep2 : out Boolean) is
   begin
      List := Validator.Attributes;
      Dependency1 := Validator.Extension;
      Ignore_Wildcard_In_Dep1 := False;
      Dependency2 := Validator.Base.Validator;
      Must_Match_All_Any_In_Dep2 := False;
   end Get_Attribute_Lists;

   ----------------
   -- Get_Facets --
   ----------------

   function Get_Facets
     (Validator : access Extension_XML_Validator;
      Reader : access Abstract_Validation_Reader'Class)
      return Facets_Description is
   begin
      if Validator.Base.Validator /= null then
         return Get_Facets (Validator.Base.Validator, Reader);
      end if;

      return null;
   end Get_Facets;

   ----------
   -- Free --
   ----------

   procedure Free (Data : in out Extension_Data) is
   begin
      Free (Data.Base_Data);
      Free (Data.Extension_Data);
      Free (Validator_Data_Record (Data));
   end Free;

   ---------------------------
   -- Create_Validator_Data --
   ---------------------------

   function Create_Validator_Data
     (Validator : access Extension_XML_Validator) return Validator_Data
   is
      D : constant Extension_Data_Access := new Extension_Data;
   begin
      if Validator.Extension /= null then
         D.Extension_Data := Create_Validator_Data (Validator.Extension);
      end if;
      D.Base_Data   := Create_Validator_Data (Get_Validator (Validator.Base));
      return Validator_Data (D);
   end Create_Validator_Data;

   ----------------------------
   -- Validate_Start_Element --
   ----------------------------

   procedure Validate_Start_Element
     (Validator         : access Extension_XML_Validator;
      Reader            : access Abstract_Validation_Reader'Class;
      Local_Name        : Unicode.CES.Byte_Sequence;
      Namespace_URI     : Unicode.CES.Byte_Sequence;
      NS                : XML_Grammar_NS;
      Data              : Validator_Data;
      Grammar           : XML_Grammar;
      Element_Validator : out XML_Element)
   is
      D : constant Extension_Data_Access := Extension_Data_Access (Data);
   begin
      if Debug then
         Debug_Push_Prefix
           ("Validate_Start_Element for extension " & Get_Name (Validator));
      end if;

      Element_Validator := No_Element;

      --  If we have a sequence with optional elements, it is possible that
      --  none of these matched, but this isn't an error. In this case, we keep
      --  looking in the base type

      if D.Validating_Base then
         begin
            if Debug then
               Debug_Output ("Validating base part of the extension ("
                             & Get_Name (Get_Validator (Validator.Base))
                             & ')');
            end if;
            Validate_Start_Element
              (Get_Validator (Validator.Base), Reader,
               Local_Name, Namespace_URI, NS,
               D.Base_Data, Grammar, Element_Validator);
         exception
            when XML_Validation_Error =>
               if Debug then
                  Debug_Output ("Validation error in base, testing extension");
               end if;
               Element_Validator := No_Element;
         end;
      end if;

      if Element_Validator = No_Element then
         D.Validating_Base := False;
         if Validator.Extension /= null then
            if Debug then
               Debug_Output ("Validating extension part of the extension");
            end if;
            Validate_Start_Element
              (Validator.Extension, Reader, Local_Name, Namespace_URI, NS,
               D.Extension_Data, Grammar, Element_Validator);
         elsif Debug then
            Debug_Output ("Base part didn't match, but no extension defined");
         end if;
      end if;

      Debug_Pop_Prefix;
   exception
      when others =>
         Debug_Pop_Prefix;
   end Validate_Start_Element;

   --------------------------
   -- Validate_End_Element --
   --------------------------

   procedure Validate_End_Element
     (Validator      : access Extension_XML_Validator;
      Reader         : access Abstract_Validation_Reader'Class;
      Local_Name     : Unicode.CES.Byte_Sequence;
      Data           : Validator_Data)
   is
      D : constant Extension_Data_Access := Extension_Data_Access (Data);
   begin
      Debug_Push_Prefix ("Validate_End_Element <extension> "
                         & Get_Name (Validator));
      if D.Validating_Base then
         Validate_End_Element
           (Get_Validator (Validator.Base), Reader, Local_Name, D.Base_Data);
      end if;

      if Validator.Extension /= null then
         Validate_End_Element
           (Validator.Extension, Reader, Local_Name, D.Extension_Data);
      end if;

      Debug_Pop_Prefix;

   exception
      when others =>
         Debug_Pop_Prefix;
         raise;
   end Validate_End_Element;

   -----------------------
   -- Get_Mixed_Content --
   -----------------------

   function Get_Mixed_Content
     (Validator : access Extension_XML_Validator) return Boolean is
   begin
      return Get_Mixed_Content (XML_Validator_Record (Validator.all)'Access)
        or else Get_Mixed_Content (Get_Validator (Validator.Base));
   end Get_Mixed_Content;

   -------------------------
   -- Validate_Characters --
   -------------------------

   procedure Validate_Characters
     (Validator     : access Extension_XML_Validator;
      Reader        : access Abstract_Validation_Reader'Class;
      Ch            : Unicode.CES.Byte_Sequence;
      Empty_Element : Boolean;
      Mask          : in out Facets_Mask) is
   begin
      if Debug then
         Debug_Push_Prefix
           ("Validate_Characters (ext) " & Get_Name (Validator));
      end if;

      if Validator.Extension /= null then
         Validate_Characters
           (Validator.Extension, Reader, Ch, Empty_Element, Mask);
      else
         if Debug then
            Debug_Output ("Validate_Characters (ext), testing base "
                          & Get_Name (Validator));
         end if;

         Validate_Characters
           (Get_Validator (Validator.Base), Reader, Ch, Empty_Element, Mask);
      end if;

      Debug_Pop_Prefix;

   exception
      when XML_Validation_Error =>
         --  If null, we have already tested
         if Validator.Extension /= null then
            if Debug then
               Debug_Output ("Validation error (ext), testing base");
            end if;
            Validate_Characters
              (Get_Validator (Validator.Base), Reader,
               Ch, Empty_Element, Mask);
            Debug_Pop_Prefix;

         else
            Debug_Pop_Prefix;
            raise;
         end if;

      when others =>
         Debug_Pop_Prefix;
         raise;
   end Validate_Characters;

   -----------
   -- Equal --
   -----------

   function Equal
     (Validator : access Extension_XML_Validator;
      Reader : access Abstract_Validation_Reader'Class;
      Value1, Value2 : Unicode.CES.Byte_Sequence) return Boolean is
   begin
      return Equal (Get_Validator (Validator.Base), Reader, Value1, Value2);
   end Equal;

   -----------------------
   -- Check_Replacement --
   -----------------------

   procedure Check_Replacement
     (Validator       : access Extension_XML_Validator;
      Element         : XML_Element;
      Typ             : XML_Type;
      Valid           : out Boolean;
      Had_Restriction : in out Boolean;
      Had_Extension   : in out Boolean)
   is
      B : constant XML_Validator := Get_Validator (Typ);
   begin
      --  From 3.4.6.5 "Type Derivation OK (Complex)".
      --  D is "Validator", B is "Typ" (not necessarily the base type of D).
      --  All of the following must be true.
      --  1. If B /= D, then the {derivation method} of D is not in the subset
      --  2 One or more of the following is true:
      --  2.1 B = D
      --  2.2 B = D.base
      --  2.3 All of the following are true:
      --  2.3.1 D.{base type definition} /= xs:anyType
      --  2.3.2 D.Base is validly derived from B

      Valid := (XML_Validator (Validator) = B    --  1
                or else not Typ.Blocks (Block_Extension));
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

      Had_Extension := True;
   end Check_Replacement;

   ------------------------
   -- Check_Content_Type --
   ------------------------

   procedure Check_Content_Type
     (Validator        : access Extension_XML_Validator;
      Reader           : access Abstract_Validation_Reader'Class;
      Should_Be_Simple : Boolean) is
   begin
      Check_Content_Type (Validator.Base, Reader, Should_Be_Simple);
   end Check_Content_Type;

   -------------------------
   -- Create_Extension_Of --
   -------------------------

   function Create_Extension_Of
     (G         : XML_Grammar_NS;
      Base      : XML_Type;
      Extension : XML_Validator := null) return XML_Validator
   is
      Result : constant Extension_Type := new Extension_XML_Validator;
   begin
      Register (G, Base);
      if Extension /= null then
         Register (G, Extension);
      end if;
      Result.Base      := Base;
      Result.Extension := Extension;
      Register (G, Result);
      return XML_Validator (Result);
   end Create_Extension_Of;

   -------------------------
   -- Create_Extension_Of --
   -------------------------

   function Create_Extension_Of
     (G          : XML_Grammar_NS;
      Reader     : access Abstract_Validation_Reader'Class;
      Base       : XML_Type;
      Group      : XML_Group;
      Min_Occurs : Natural := 1;
      Max_Occurs : Integer := 1) return XML_Validator
   is
      Result : constant Extension_Type := new Extension_XML_Validator;
      C      : Sequence;
   begin
      if Get_Final_On_Extension (Base) then
         Validation_Error
           (Reader, "Type """ & To_QName (Base) & """ forbids extensions");
      end if;

      Register (G, Result);
      Register (G, Base);
      Result.Base      := Base;
      C := Create_Sequence (G);
      Set_Debug_Name (C, "automatic_extension_sequence");
      Add_Particle (C, Reader, Group, Min_Occurs, Max_Occurs);
      Result.Extension := XML_Validator (C);
      return XML_Validator (Result);
   end Create_Extension_Of;

   ---------------------
   -- Is_Extension_Of --
   ---------------------

   function Is_Extension_Of
     (Validator : Extension_XML_Validator;
      Base      : access XML_Validator_Record'Class) return Boolean
   is
   begin
      return Validator.Base.Validator = XML_Validator (Base)
        or else Is_Extension_Of
          (Validator.Base.Validator.all, Base => Base);
   end Is_Extension_Of;

end Schema.Validators.Extensions;
