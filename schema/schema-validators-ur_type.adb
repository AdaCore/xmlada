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

package body Schema.Validators.UR_Type is

   type UR_Type_Validator is new XML_Validator_Record with record
      Process_Contents : Process_Contents_Type := Process_Strict;
   end record;
   type UR_Type_Access is access all UR_Type_Validator'Class;

   procedure Validate_End_Element
     (Validator      : access UR_Type_Validator;
      Local_Name     : Unicode.CES.Byte_Sequence;
      Data           : Validator_Data);
   procedure Validate_Attributes
     (Validator         : access UR_Type_Validator;
      Atts              : in out Sax.Attributes.Attributes'Class;
      Id_Table          : access Id_Htable_Access;
      Nillable          : Boolean;
      Is_Nil            : out Boolean;
      Grammar           : in out XML_Grammar);
   procedure Validate_Start_Element
     (Validator         : access UR_Type_Validator;
      Local_Name        : Unicode.CES.Byte_Sequence;
      Namespace_URI     : Unicode.CES.Byte_Sequence;
      NS                : XML_Grammar_NS;
      Data              : Validator_Data;
      Grammar           : XML_Grammar;
      Element_Validator : out XML_Element);
   procedure Validate_Characters
     (Validator      : access UR_Type_Validator;
      Ch             : Unicode.CES.Byte_Sequence;
      Empty_Element  : Boolean);
   --  See doc for inherited subprograms

   -------------------------
   -- Validate_Characters --
   -------------------------

   procedure Validate_Characters
     (Validator      : access UR_Type_Validator;
      Ch             : Unicode.CES.Byte_Sequence;
      Empty_Element  : Boolean)
   is
   begin
      if Debug then
         Debug_Output
           ("Validate_Characters for UR_Type Process_Contents="
            & Validator.Process_Contents'Img & ' ' & Ch
            & ' ' & Empty_Element'Img);
      end if;
   end Validate_Characters;

   ----------------------------
   -- Validate_Start_Element --
   ----------------------------

   procedure Validate_Start_Element
     (Validator              : access UR_Type_Validator;
      Local_Name             : Unicode.CES.Byte_Sequence;
      Namespace_URI          : Unicode.CES.Byte_Sequence;
      NS                     : XML_Grammar_NS;
      Data                   : Validator_Data;
      Grammar                : XML_Grammar;
      Element_Validator      : out XML_Element)
   is
      pragma Unreferenced (Data);
   begin
      if Debug then
         Debug_Output
           ("Validate_Start_Element UR_Type Process_Contents="
            & Validator.Process_Contents'Img);
      end if;

      --  ur-Type and anyType accept anything

      case Validator.Process_Contents is
         when Process_Strict =>
            Element_Validator := Lookup_Element
              (NS, Local_Name, Create_If_Needed => False);
            if Element_Validator = No_Element then
               Validation_Error
                 ("No definition provided for """ & Local_Name & """");
            else
               Check_Qualification
                 (Grammar, Element_Validator, Namespace_URI);
            end if;

         when Process_Lax =>
            Element_Validator := Lookup_Element
              (NS, Local_Name, Create_If_Needed => False);
            if Element_Validator = No_Element then
               if Debug then
                  Debug_Output ("Definition not found for " & Local_Name);
               end if;
               Element_Validator := Get_UR_Type_Element
                 (Grammar, Validator.Process_Contents);
            else
               if Debug then
                  Debug_Output ("Definition found for " & Local_Name);
               end if;
            end if;

         when Process_Skip =>
            Element_Validator := Get_UR_Type_Element
              (Grammar, Validator.Process_Contents);
      end case;

   end Validate_Start_Element;

   -------------------------
   -- Validate_Attributes --
   -------------------------

   procedure Validate_Attributes
     (Validator         : access UR_Type_Validator;
      Atts              : in out Sax.Attributes.Attributes'Class;
      Id_Table          : access Id_Htable_Access;
      Nillable          : Boolean;
      Is_Nil            : out Boolean;
      Grammar           : in out XML_Grammar)
   is
      pragma Unreferenced (Validator, Atts, Id_Table, Nillable, Grammar);
   begin
      Is_Nil := False;
   end Validate_Attributes;

   --------------------------
   -- Validate_End_Element --
   --------------------------

   procedure Validate_End_Element
     (Validator      : access UR_Type_Validator;
      Local_Name     : Unicode.CES.Byte_Sequence;
      Data           : Validator_Data)
   is
      pragma Unreferenced (Validator, Local_Name, Data);
   begin
      null;
   end Validate_End_Element;

   -------------------------
   -- Get_UR_Type_Element --
   -------------------------

   function Get_UR_Type_Element
     (Grammar          : XML_Grammar;
      Process_Contents : Process_Contents_Type) return XML_Element
   is
   begin
      return Get (Grammar).UR_Type_Elements (Process_Contents);
   end Get_UR_Type_Element;

   -----------------------------
   -- Create_UR_Type_Elements --
   -----------------------------

   procedure Create_UR_Type_Elements
     (Schema_NS : Schema.Validators.XML_Grammar_NS;
      Grammar   : XML_Grammar)
   is
      Validator : UR_Type_Access;
   begin
      for P in Process_Contents_Type loop
         Validator := new UR_Type_Validator;
         Validator.Process_Contents := P;
         Get (Grammar).UR_Type_Elements (P) := Create_Local_Element
           ("", Schema_NS, Create_Local_Type (Schema_NS, Validator),
            Qualified);
      end loop;
   end Create_UR_Type_Elements;

end Schema.Validators.UR_Type;
