-----------------------------------------------------------------------
--                XML/Ada - An XML suite for Ada95                   --
--                                                                   --
--                    Copyright (C) 2005-2011, AdaCore               --
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

--  This package provides the basic types to define the facets for an XSD type

pragma Ada_05;

with Sax.Symbols;
with Sax.Utils;
with Unicode.CES;

package Schema.Decimal is

   ---------------------------------
   -- Arbitrary_Precision_Numbers --
   ---------------------------------
   --  This type provides some minimal handling for arbitrary precision
   --  numbers as described by the "decimal" and "integer" types.
   --  This is *not* an arbitrary-precision library, which is not needed in the
   --  context of XML

   type Arbitrary_Precision_Number is private;
   Undefined_Number : constant Arbitrary_Precision_Number;

   function Image
     (Number : Arbitrary_Precision_Number) return Unicode.CES.Byte_Sequence;
   --  Return a displayable version of Number

   procedure Value
     (Symbols  : Sax.Utils.Symbol_Table;
      Ch       : Unicode.CES.Byte_Sequence;
      Val      : out Arbitrary_Precision_Number;
      Error    : out Sax.Symbols.Symbol);
   --  Convert Ch to a number.
   --  Raises a Validation_Error if this is not a valid number

   function Value (Val : Sax.Symbols.Symbol) return Arbitrary_Precision_Number;
   --  Assumes [Val] is a valid Arbitrary_Precision_Number.

   procedure Value_No_Exponent
     (Symbols  : Sax.Utils.Symbol_Table;
      Ch       : Unicode.CES.Byte_Sequence;
      Val      : out Arbitrary_Precision_Number;
      Error    : out Sax.Symbols.Symbol);
   --  Same as value, but does not allow a "E" part

   function "<"  (Num1, Num2 : Arbitrary_Precision_Number) return Boolean;
   function "<=" (Num1, Num2 : Arbitrary_Precision_Number) return Boolean;
   function "="  (Num1, Num2 : Arbitrary_Precision_Number) return Boolean;
   function ">=" (Num1, Num2 : Arbitrary_Precision_Number) return Boolean;
   function ">"  (Num1, Num2 : Arbitrary_Precision_Number) return Boolean;
   --  Compare two numbers

   function Check_Digits
     (Symbols                       : Sax.Utils.Symbol_Table;
      Num                           : Arbitrary_Precision_Number;
      Fraction_Digits, Total_Digits : Integer := -1)
      return Sax.Symbols.Symbol;
   --  Check whether the two facets Fraction_Digits and Total_Digits match.
   --  If any of the two values is negative, no check is done for it.
   --  Returns an error message or [No_Symbol].

   ---------------
   -- XML_Float --
   ---------------
   --  This type represents a floating point value (float or double in XSD),
   --  including infinity and NaN

   type XML_Float is private;
   Unknown_Float : constant XML_Float;

   function "<=" (F1, F2 : XML_Float) return Boolean;
   function "<" (F1, F2 : XML_Float) return Boolean;
   function ">=" (F1, F2 : XML_Float) return Boolean;
   function ">" (F1, F2 : XML_Float) return Boolean;
   --  Compare two numbers

   function Image (Value : XML_Float) return String;
   --  Return a displayable version of Number

   function Value (Str : String) return XML_Float;
   --  Return the float stored in Str (including +INF, -INF)

private
   type Arbitrary_Precision_Number is record
      Value : Sax.Symbols.Symbol;
   end record;
   Undefined_Number : constant Arbitrary_Precision_Number :=
     (Value => Sax.Symbols.No_Symbol);

   type XML_Float_Kind is (Plus_Infinity, Minus_Infinity, NaN, Standard_Float);
   type XML_Float (Kind : XML_Float_Kind := NaN) is record
      case Kind is
         when Standard_Float =>
            Mantiss : Long_Long_Float;
            Exp     : Integer;
         when others =>
            null;
      end case;
   end record;
   Unknown_Float : constant XML_Float := (Kind => NaN);

end Schema.Decimal;
