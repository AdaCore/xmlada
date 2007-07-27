-----------------------------------------------------------------------
--                XML/Ada - An XML suite for Ada95                   --
--                                                                   --
--                       Copyright (C) 2004-2007, AdaCore            --
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

private package Schema.Validators.Simple_Types is

   procedure Register_Predefined_Types (G, XML_G : XML_Grammar_NS);
   --  Register all the predefined types

   -------------------------------
   -- Any_Simple_XML_Validator --
   -------------------------------

   type Any_Simple_XML_Validator_Record is new XML_Validator_Record
   with null record;
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
      Empty_Element  : Boolean);
   procedure Validate_End_Element
     (Validator  : access Any_Simple_XML_Validator_Record;
      Local_Name : Unicode.CES.Byte_Sequence;
      Data       : Validator_Data);
   procedure Check_Content_Type
     (Validator        : access Any_Simple_XML_Validator_Record;
      Should_Be_Simple : Boolean);
   function Get_Facets_Description
     (Validator : access Any_Simple_XML_Validator_Record)
      return Facets_Description;
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
      Empty_Element : Boolean);
   --  See doc from inherited subprograms

end Schema.Validators.Simple_Types;
