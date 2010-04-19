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

with Schema.Validators.Facets;

private package Schema.Validators.Simple_Types is

   procedure Register_Predefined_Types (G, XML_G : XML_Grammar_NS);
   --  Register all the predefined types

   type Valued_Facets is new Schema.Validators.Facets.Common_Facets_Description
      with null record;
   function Equal
     (Facet : Valued_Facets;
      Value1, Value2 : Unicode.CES.Byte_Sequence) return Boolean;
   --  Compare the two values

   -------------------------------
   -- Any_Simple_XML_Validator --
   -------------------------------

   type Any_Simple_XML_Validator_Record is new XML_Validator_Record with record
      Facets : Facets_Description;
   end record;
   type Any_Simple_XML_Validator
     is access all Any_Simple_XML_Validator_Record'Class;
   --  Validates a "SimpleType" XML datatype, ie accepts any contents but
   --  elements and attributes

   procedure Validate_Start_Element
     (Validator              : access Any_Simple_XML_Validator_Record;
      Local_Name             : Unicode.CES.Byte_Sequence;
      Namespace_URI          : Unicode.CES.Byte_Sequence;
      NS                     : XML_Grammar_NS;
      Data                   : Validator_Data;
      Grammar                : XML_Grammar;
      Element_Validator      : out XML_Element);
   procedure Validate_Characters
     (Validator      : access Any_Simple_XML_Validator_Record;
      Ch             : Unicode.CES.Byte_Sequence;
      Empty_Element  : Boolean;
      Mask           : in out Facets_Mask;
      Context        : in out Validation_Context);
   procedure Validate_End_Element
     (Validator  : access Any_Simple_XML_Validator_Record;
      Local_Name : Unicode.CES.Byte_Sequence;
      Data       : Validator_Data);
   procedure Check_Content_Type
     (Validator        : access Any_Simple_XML_Validator_Record;
      Should_Be_Simple : Boolean);
   function Get_Facets
     (Validator : access Any_Simple_XML_Validator_Record)
      return Facets_Description;
   function Get_Mixed_Content
     (Validator : access Any_Simple_XML_Validator_Record) return Boolean;
   procedure Free (Validator : in out Any_Simple_XML_Validator_Record);
   procedure Check_Replacement
     (Validator       : access Any_Simple_XML_Validator_Record;
      Element         : XML_Element;
      Typ             : XML_Type;
      Valid           : out Boolean;
      Had_Restriction : in out Boolean;
      Had_Extension   : in out Boolean);
   --  See doc from inherited subprograms

   ---------------
   -- XML_Union --
   ---------------

   type XML_Union_Record is new Any_Simple_XML_Validator_Record with record
      Unions : Particle_List := Empty_Particle_List;
   end record;
   type XML_Union is access all XML_Union_Record'Class;

   procedure Free (Union : in out XML_Union_Record);
   --  See inherited documentation

   procedure Add_Union
     (Validator : access XML_Union_Record; Part : XML_Type);
   --  Add a new element to the union in Validator

   procedure Validate_Characters
     (Union         : access XML_Union_Record;
      Ch            : Unicode.CES.Byte_Sequence;
      Empty_Element : Boolean;
      Mask          : in out Facets_Mask;
      Context       : in out Validation_Context);
   function Get_Facets
     (Validator : access XML_Union_Record) return Facets_Description;
   function Equal
     (Validator : access XML_Union_Record;
      Value1, Value2 : Unicode.CES.Byte_Sequence) return Boolean;

   procedure Check_Replacement_For_Union
     (Validator         : access XML_Validator_Record'Class;
      Union             : XML_Union_Record;
      Element           : XML_Element;
      Valid             : out Boolean;
      Had_Restriction   : in out Boolean;
      Had_Extension     : in out Boolean);
   --  Whether Validator can replace Union

end Schema.Validators.Simple_Types;
