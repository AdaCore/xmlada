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

with Schema.Validators.Facets;       use Schema.Validators.Facets;
with Schema.Validators.Simple_Types; use  Schema.Validators.Simple_Types;
with Unicode;                        use Unicode;
with Sax.Encodings;                  use Sax.Encodings;

package body Schema.Validators.Lists is

   type List_Facets_Description is new Common_Facets_Description with record
      Length     : Natural;
      Min_Length : Natural;
      Max_Length : Natural;
   end record;
   procedure Add_Facet
     (Facets      : in out List_Facets_Description;
      Facet_Name  : Unicode.CES.Byte_Sequence;
      Facet_Value : Unicode.CES.Byte_Sequence;
      Applied     : out Boolean);
   procedure Check_Facet
     (Facets : in out List_Facets_Description;
      Value  : Unicode.CES.Byte_Sequence);

   type List_Validator_Record is new Any_Simple_XML_Validator_Record with
      record
         Facets : List_Facets_Description;
         Base   : XML_Type;
      end record;
   type List_Validator is access all List_Validator_Record'Class;
   procedure Validate_Characters
     (Validator     : access List_Validator_Record;
      Ch            : Unicode.CES.Byte_Sequence;
      Empty_Element : Boolean);
   function Get_Facets_Description
     (Validator : access List_Validator_Record) return Facets_Description;
   procedure Free (Validator : in out List_Validator_Record);
   --  See doc from inherited subprogram

   ---------------
   -- Add_Facet --
   ---------------

   procedure Add_Facet
     (Facets      : in out List_Facets_Description;
      Facet_Name  : Unicode.CES.Byte_Sequence;
      Facet_Value : Unicode.CES.Byte_Sequence;
      Applied     : out Boolean)
   is
   begin
      Add_Facet
        (Common_Facets_Description (Facets), Facet_Name, Facet_Value, Applied);
      if Applied then
         null;
      elsif Facet_Name = "length" then
         Facets.Mask (Facet_Length) := True;
         Facets.Length := Natural'Value (Facet_Value);
         Applied := True;
      elsif Facet_Name = "minLength" then
         Facets.Mask (Facet_Min_Length) := True;
         Facets.Min_Length := Natural'Value (Facet_Value);
         Applied := True;
      elsif Facet_Name = "maxLength" then
         Facets.Mask (Facet_Max_Length) := True;
         Facets.Max_Length := Natural'Value (Facet_Value);
         Applied := True;
      else
         Applied := False;
      end if;
   end Add_Facet;

   -----------------
   -- Check_Facet --
   -----------------

   procedure Check_Facet
     (Facets : in out List_Facets_Description;
      Value  : Unicode.CES.Byte_Sequence)
   is
      Length : Natural := 1;
   begin
      Check_Facet (Common_Facets_Description (Facets), Value);

      --  Ch has already been normalized by the SAX parser
      for C in Value'Range loop
         if Value (C) = ' ' then
            Length := Length + 1;
         end if;
      end loop;

      if Facets.Mask (Facet_Length)
        and then Length /= Facets.Length
      then
         Validation_Error ("Invalid number of elements in list, expecting"
                           & Integer'Image (Facets.Length));
      end if;

      if Facets.Mask (Facet_Min_Length)
        and then Length < Facets.Min_Length
      then
         Validation_Error
           ("Invalid number of elements in list, expecting at least"
            & Integer'Image (Facets.Min_Length));
      end if;

      if Facets.Mask (Facet_Max_Length)
        and then Length > Facets.Max_Length
      then
         Validation_Error
           ("Invalid number of elements in list, expecting at most"
            & Integer'Image (Facets.Max_Length));
      end if;
   end Check_Facet;

   ----------
   -- Free --
   ----------

   procedure Free (Validator : in out List_Validator_Record) is
   begin
      Free (Validator.Facets);
      Free (XML_Validator_Record (Validator));
   end Free;

   ----------------------------
   -- Get_Facets_Description --
   ----------------------------

   function Get_Facets_Description
     (Validator : access List_Validator_Record) return Facets_Description
   is
      pragma Unreferenced (Validator);
   begin
      return Facets_Description'(new List_Facets_Description);
   end Get_Facets_Description;

   -------------
   -- List_Of --
   -------------

   function List_Of (Typ : XML_Type) return XML_Validator is
      Validator : List_Validator;
   begin
      Validator := new List_Validator_Record;
      Validator.Base := Typ;
      return XML_Validator (Validator);
   end List_Of;

   -------------------------
   -- Validate_Characters --
   -------------------------

   procedure Validate_Characters
     (Validator     : access List_Validator_Record;
      Ch            : Unicode.CES.Byte_Sequence;
      Empty_Element : Boolean)
   is
      Index : Integer := Ch'First;
      Last, Start  : Integer;
      C     : Unicode_Char;
   begin
      if Debug then
         Debug_Output ("Validate_Characters for list --" & Ch & "--"
                       & Get_Name (Validator));
      end if;

      Check_Facet (Validator.Facets, Ch);

      --  Ch might be from an attribute (in which case it might have been
      --  normalized first), or for the value of a mixed element, in which case
      --  no element has taken place. We therefore need to skip initial spaces

      while Index <= Ch'Last loop
         --  Skip leading spaces
         while Index <= Ch'Last loop
            Start := Index;
            Encoding.Read (Ch, Index, C);
            exit when not Is_White_Space (C);
            Start := Ch'Last + 1;
         end loop;

         if Start <= Ch'Last then
            --  Move to first whitespace after word
            while Index <= Ch'Last loop
               Last := Index;
               Encoding.Read (Ch, Index, C);
               if Is_White_Space (C) then
                  if Debug then
                     Debug_Output ("  In list: " & Ch (Start .. Last - 1));
                  end if;
                  Validate_Characters
                    (Get_Validator (Validator.Base),
                     Ch (Start .. Last - 1),
                     Empty_Element);
                  exit;
               end if;
            end loop;

            if Index > Ch'Last then
               if Debug then
                  Debug_Output ("  In list: " & Ch (Start .. Ch'Last));
               end if;
               Validate_Characters
                 (Get_Validator (Validator.Base),
                  Ch (Start .. Ch'Last),
                  Empty_Element);
            end if;
         end if;
      end loop;
   end Validate_Characters;

end Schema.Validators.Lists;
