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
with Sax.Readers;    use Sax.Readers;

package body Schema.Validators.UR_Type is

   type UR_Type_Validator is new XML_Validator_Record with record
      Process_Contents : Process_Contents_Type := Process_Strict;
   end record;
   type UR_Type_Access is access all UR_Type_Validator'Class;

   overriding function Is_Wildcard
     (Validator : access UR_Type_Validator) return Boolean;
   overriding function Get_Mixed_Content
     (Validator : access UR_Type_Validator) return Boolean;
   --  See doc for inherited subprograms

   -----------------------
   -- Get_Mixed_Content --
   -----------------------

   overriding function Get_Mixed_Content
     (Validator : access UR_Type_Validator) return Boolean
   is
      pragma Unreferenced (Validator);
   begin
      return True;
   end Get_Mixed_Content;

   ----------------------------
   -- Validate_Start_Element --
   ----------------------------

--     overriding procedure Validate_Start_Element
--       (Validator         : access UR_Type_Validator;
--        Reader            : access Abstract_Validation_Reader'Class;
--        Local_Name        : Symbol;
--        NS                : XML_Grammar_NS;
--        Data              : Validator_Data;
--        Element_Validator : out XML_Element)
--     is
--        pragma Unreferenced (Data);
--     begin
--        if Debug then
--           Debug_Output
--             ("Validate_Start_Element " & To_QName (NS, Local_Name)
--              & " (parent=UR_Type, " & Validator.Process_Contents'Img & ")");
--        end if;
--
--        --  ur-Type and anyType accept anything
--
--        case Validator.Process_Contents is
--           when Process_Strict =>
--              Element_Validator := Lookup_Element
--                (NS, Reader, Local_Name, Create_If_Needed => False);
--              if Element_Validator = No_Element then
--                 Validation_Error
--                   (Reader,
--                    "#No definition provided for """
--                    & Get (Local_Name).all & """");
--              else
--                 Check_Qualification (Reader, Element_Validator, NS);
--              end if;
--
--           when Process_Lax =>
--              Element_Validator := Lookup_Element
--                (NS, Reader, Local_Name, Create_If_Needed => False);
--
--              if Element_Validator = No_Element then
--                 if Debug then
--                    Debug_Output ("Definition not found for "
--                                  & Get (Local_Name).all);
--                 end if;
--                 Element_Validator := Get_UR_Type_Element
--                   (Reader.Grammar, Validator.Process_Contents);
--              else
--                 if Debug then
--                    Debug_Output ("Definition found for "
--                                  & Get (Local_Name).all);
--                 end if;
--              end if;
--
--           when Process_Skip =>
--              if Debug then
--                 Debug_Output
--               ("Children will be validated with UR-Type, because of SKIP");
--              end if;
--
--              Element_Validator := Get_UR_Type_Element
--                (Reader.Grammar, Validator.Process_Contents);
--        end case;
--     end Validate_Start_Element;

   -------------------------
   -- Validate_Attributes --
   -------------------------

--     overriding procedure Validate_Attributes
--       (Validator : access UR_Type_Validator;
--        Reader    : access Abstract_Validation_Reader'Class;
--        Atts      : in out Sax.Readers.Sax_Attribute_List;
--        Nillable  : Boolean;
--        Is_Nil    : out Boolean)
--     is
--        pragma Unreferenced (Validator, Nillable, Atts, Reader);
--     begin
--        Is_Nil := False;
--     end Validate_Attributes;

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

   -----------------
   -- Is_Wildcard --
   -----------------

   overriding function Is_Wildcard
     (Validator : access UR_Type_Validator) return Boolean
   is
      pragma Unreferenced (Validator);
   begin
      return True;
   end Is_Wildcard;

   -----------------------------
   -- Create_UR_Type_Elements --
   -----------------------------

   procedure Create_UR_Type_Elements
     (Reader    : access Schema.Validators.Abstract_Validation_Reader'Class;
      Schema_NS : Schema.Validators.XML_Grammar_NS;
      Grammar   : XML_Grammar)
   is
      pragma Unreferenced (Grammar);
      Validator : UR_Type_Access;
      Typ       : XML_Type;
   begin
      for P in Process_Contents_Type loop
         Validator := new UR_Type_Validator;
         Validator.Process_Contents := P;
         Typ := Create_Local_Type (Schema_NS, Validator);
         Typ.Local_Name := Find_Symbol (Reader.all, "ur-Type" & P'Img);
--           Get (Grammar).UR_Type_Elements (P) := Create_Local_Element
--             (Empty_String, Schema_NS, Typ, Qualified);
      end loop;
   end Create_UR_Type_Elements;

end Schema.Validators.UR_Type;
