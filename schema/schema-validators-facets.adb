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

with Unicode.CES;           use Unicode.CES;
with Ada.Strings.Unbounded;

package body Schema.Validators.Facets is

   --------------------
   -- Convert_Regexp --
   --------------------

   function Convert_Regexp (Regexp : String) return String is
      use Ada.Strings.Unbounded;
      Result : Unbounded_String;
      Tmp    : Unbounded_String;
      Pos    : Integer := Regexp'First;
   begin
      while Pos <= Regexp'Last loop
         if Regexp (Pos) = '[' then
            Append (Result, Regexp  (Pos));
            Pos := Pos + 1;

            Tmp := Null_Unbounded_String;

            while Pos <= Regexp'Last
              and then Regexp (Pos) /= ']'
            loop
               if Regexp (Pos) = '\' then
                  case Regexp (Pos + 1) is
                     when 'i' =>
                        --  rule [99] in XMLSchema specifications
                        Append (Tmp, "A-Za-z:_");
                        Pos := Pos + 1;

                     when 'c' =>
                        Append (Tmp, "a-z:A-Z0-9._-");
                        Pos    := Pos + 1;

                     when others =>
                        Append (Tmp, Regexp (Pos));
                  end case;
               else
                  Append (Tmp, Regexp (Pos));
               end if;
               Pos := Pos + 1;
            end loop;

            Append (Result, Tmp);
            if Pos <= Regexp'Last then
               Append (Result, Regexp (Pos));
            end if;

         --  ??? Some tests in the old w3c testsuite seem to imply that
         --  \c and \i are valid even outside character classes. Not sure about
         --  this though

         elsif Pos < Regexp'Last and then Regexp (Pos) = '\' then
            case Regexp (Pos + 1) is
               when 'i' =>
                  --  rule [99] in XMLSchema specifications
                  Append (Result, "[A-Za-z:_]");
                  Pos := Pos + 1;

               when 'c' =>
                  Append (Result, "[a-z:A-Z0-9._-]");
                  Pos    := Pos + 1;

               when others =>
                  Append (Result, Regexp (Pos));
            end case;

         else
            Append (Result, Regexp  (Pos));
         end if;

         Pos := Pos + 1;
      end loop;

      return To_String (Result);
   end Convert_Regexp;

   ----------
   -- Free --
   ----------

   procedure Free (Facets : in out Common_Facets_Description) is
   begin
      if Facets.Pattern /= null then
         Unchecked_Free (Facets.Pattern);
      end if;

      Free (Facets.Pattern_String);

      if Facets.Enumeration /= null then
         Free (Facets.Enumeration);
      end if;
   end Free;

   ----------
   -- Copy --
   ----------

   procedure Copy
     (From : Common_Facets_Description;
      To   : in out Facets_Description_Record'Class) is
   begin
      Common_Facets_Description (To).Mask := From.Mask;
      Common_Facets_Description (To).Whitespace := From.Whitespace;
      if From.Pattern /= null then
         Common_Facets_Description (To).Pattern :=
           new Pattern_Matcher'(From.Pattern.all);
      end if;

      if From.Pattern_String /= null then
         Common_Facets_Description (To).Pattern_String :=
           new Byte_Sequence'(From.Pattern_String.all);
      end if;

      Common_Facets_Description (To).Implicit_Enumeration :=
        From.Implicit_Enumeration;

      if From.Enumeration /= null then
         for L in From.Enumeration'Range loop
            Append
              (Common_Facets_Description (To).Enumeration,
               From.Enumeration (L).all);
         end loop;
      end if;
   end Copy;

   -----------------
   -- Check_Facet --
   -----------------

   procedure Check_Facet
     (Facets : in out Common_Facets_Description;
      Value  : Unicode.CES.Byte_Sequence)
   is
      Found : Boolean;
      Matched : Match_Array (0 .. 0);
   begin
      if Facets.Mask (Facet_Pattern) then
         Match (Facets.Pattern.all, String (Value), Matched);
         if Matched (0).First /= Value'First
           or else Matched (0).Last /= Value'Last
         then
            Validation_Error ("string pattern not matched: "
                              & Facets.Pattern_String.all);
         end if;
      end if;

      if Facets.Mask (Facet_Enumeration) then
         Found := False;
         for E in Facets.Enumeration'Range loop
            if Value = Facets.Enumeration (E).all then
               Found := True;
            end if;
         end loop;

         if not Found then
            Validation_Error ("Element's value not in the enumeration set");
         end if;
      end if;

      if Facets.Mask (Facet_Implicit_Enumeration) then
         if not Facets.Implicit_Enumeration (Value) then
            Validation_Error ("Invalid value: """ & Value & """");
         end if;
      end if;

      if Facets.Mask (Facet_Whitespace) then
         case Facets.Whitespace is
            when Preserve =>
               null; --  Always valid

            when Replace =>
               for C in Value'Range loop
                  if Value (C) = ASCII.HT
                    or else Value (C) = ASCII.LF
                    or else Value (C) = ASCII.CR
                  then
                     Validation_Error ("HT, LF and CR characters not allowed");
                  end if;
               end loop;

            when Collapse =>
               for C in Value'Range loop
                  if Value (C) = ASCII.HT
                    or else Value (C) = ASCII.LF
                    or else Value (C) = ASCII.CR
                  then
                     Validation_Error ("HT, LF and CR characters not allowed");

                  elsif Value (C) = ' '
                    and then C < Value'Last
                    and then Value (C + 1) = ' '
                  then
                     Validation_Error
                       ("Duplicate space characters not allowed");
                  end if;
               end loop;

               --  Leading or trailing white spaces are also forbidden
               if Value'Length /= 0 then
                  if Value (Value'First) = ' ' then
                     Validation_Error ("Leading whitespaces not allowed");
                  elsif Value (Value'Last) = ' ' then
                     Validation_Error ("Trailing whitespaces not allowed");
                  end if;
               end if;
         end case;
      end if;
   end Check_Facet;

   ---------------
   -- Add_Facet --
   ---------------

   procedure Add_Facet
     (Facets      : in out Common_Facets_Description;
      Facet_Name  : Unicode.CES.Byte_Sequence;
      Facet_Value : Unicode.CES.Byte_Sequence;
      Applied     : out Boolean) is
   begin
      Applied := False;
      if Facet_Name = "enumeration" then
         if not Facets.Settable (Facet_Enumeration) then
            Validation_Error ("Enumeration facet can't be set for this type");
         end if;
         Append (Facets.Enumeration, Facet_Value);
         Facets.Mask (Facet_Enumeration) := True;
         Applied := True;

      elsif Facet_Name = "whiteSpace" then
         if not Facets.Settable (Facet_Whitespace) then
            Validation_Error ("whiteSpace facet can't be set for this type");
         end if;
         if Facet_Value = "preserve" then
            Facets.Whitespace := Preserve;
         elsif Facet_Value = "replace" then
            Facets.Whitespace := Replace;
         elsif Facet_Value = "collapse" then
            Facets.Whitespace := Collapse;
         else
            Validation_Error
              ("Invalid value for whiteSpace facet: " & Facet_Value);
         end if;
         Facets.Mask (Facet_Whitespace) := True;
         Applied := True;

      elsif Facet_Name = "pattern" then
         if not Facets.Settable (Facet_Pattern) then
            Validation_Error ("pattern facet can't be set for this type");
         end if;
         Unchecked_Free (Facets.Pattern);
         Free (Facets.Pattern_String);

         Facets.Pattern_String := new Byte_Sequence'(Facet_Value);

         begin
            Facets.Pattern := new Pattern_Matcher '
              (Compile (Convert_Regexp (Facets.Pattern_String.all)));
         exception
            when  GNAT.Regpat.Expression_Error =>
               Validation_Error ("Invalid regular expression "
                                 & Facets.Pattern_String.all);
         end;

         Facets.Mask (Facet_Pattern) := True;
         Applied := True;
      end if;
   end Add_Facet;

   ------------------------------
   -- Set_Implicit_Enumeration --
   ------------------------------

   procedure Set_Implicit_Enumeration
     (Facets      : in out Common_Facets_Description;
      Validator   : Value_Validator) is
   begin
      Facets.Mask (Facet_Implicit_Enumeration) := True;
      Facets.Implicit_Enumeration := Validator;
   end Set_Implicit_Enumeration;

   --------------------
   -- Set_Whitespace --
   --------------------

   procedure Set_Whitespace
     (Facets     : in out Common_Facets_Description;
      Whitespace : Whitespace_Restriction) is
   begin
      Facets.Mask (Facet_Whitespace) := True;
      Facets.Whitespace := Whitespace;
   end Set_Whitespace;

   ------------
   -- Append --
   ------------

   procedure Append
     (List  : in out Byte_Sequence_List_Access;
      Value : Unicode.CES.Byte_Sequence)
   is
      L : Byte_Sequence_List_Access := List;
   begin
      if List /= null then
         L := new Byte_Sequence_List'(List.all & new Byte_Sequence'(Value));
         Unchecked_Free (List);
         List := L;
      else
         List := new Byte_Sequence_List'(1 => new Byte_Sequence'(Value));
      end if;
   end Append;

   ----------
   -- Free --
   ----------

   procedure Free (List : in out Byte_Sequence_List_Access) is
   begin
      if List /= null then
         for L in List'Range loop
            Free (List (L));
         end loop;

         Unchecked_Free (List);
      end if;
   end Free;

end Schema.Validators.Facets;
