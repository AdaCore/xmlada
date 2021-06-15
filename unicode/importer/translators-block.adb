------------------------------------------------------------------------------
--                     XML/Ada - An XML suite for Ada95                     --
--                                                                          --
--                     Copyright (C) 2016, Nicolas Boulenguez               --
--                     Copyright (C) 2016-2017, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

pragma Ada_2012;
with Ada.Text_IO;

package body Translators.Block is

   function Default_Translation (Translator : A_Block_Translator;
                                 Original   : String)
                                return String is
      pragma Unreferenced (Translator);
      Word_Start : Boolean := True;
      Result     : String (Original'Range);
   begin
      pragma Assert (Original'Length /= 0);
      pragma Assert (Original (Original'First) in 'A' .. 'Z');
      pragma Assert
        (Original (Original'Last) in 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9');
      for I in Original'Range loop
         case Original (I) is
            when 'A' .. 'Z' =>
               pragma Assert (Word_Start);
               Result (I) := Original (I);
               Word_Start := False;
            when 'a' .. 'z' =>
               pragma Assert (not Word_Start);
               Result (I) := Original (I);
            when '0' .. '9' =>
               Result (I) := Original (I);
               Word_Start := False;
            when ' ' | '-' =>
               pragma Assert (not Word_Start);
               Result (I) := '_';
               Word_Start := True;
            when others =>
               pragma Assert (False);
         end case;
      end loop;
      return Result;
   exception
      when others =>
         Ada.Text_IO.Put_Line ("Cannot translate block """ & Original & '"');
         raise;
   end Default_Translation;

   procedure Set_Exceptions (Translator : in out A_Block_Translator) is
      procedure Add (Key, Value : String) with Inline;
      procedure Add (Key, Value : String) is
      begin
         Translator.Exceptions.Insert (Key, (Value'Length, Value, 0));
      end Add;
   begin
      Add ("Alphabetic Presentation Forms",
           "Alpha_Presentation_Forms");
      Add ("Ancient Greek Musical Notation",
           "Ancient_Greek_Music");
      Add ("Arabic Mathematical Alphabetic Symbols",
           "Arabic_Math_Alpha_Symb");
      --  TODO: why not Forms_A instead of FormsA?
      Add ("Arabic Presentation Forms-A",
           "Arabic_Present_FormsA");
      Add ("Arabic Presentation Forms-B",
           "Arabic_Present_FormsB");

      --  TODO: why not CJK instead of Cjk?
      Add ("CJK Compatibility Ideographs",
           "Cjk_Compat_Ideographs");
      Add ("CJK Compatibility Ideographs Supplement",
           "Cjk_Compat_Ideo_Sup");
      Add ("CJK Symbols and Punctuation",
           "Cjk_Symbols_And_Punct");
      Add ("Enclosed CJK Letters and Months",
           "Cjk_Letters_Months");

      Add ("Combining Diacritical Marks",
           "Combining_Diacritical");
      Add ("Combining Diacritical Marks Extended",
           "Combining_Diacritical_Ext");
      Add ("Combining Diacritical Marks Supplement",
           "Combining_Diacritical_Sup");
      Add ("Combining Diacritical Marks for Symbols",
           "Combining_Diacritical_Sym");
      Add ("Cuneiform Numbers and Punctuation",
           "Cuneiform_Num_Punctuation");
      Add ("Enclosed Alphanumeric Supplement",
           "Enclosed_Alphanum_Sup");
      Add ("Enclosed Ideographic Supplement",
           "Enclosed_Ideographic_Sup");
      Add ("Halfwidth and Fullwidth Forms",
           "Half_Full_Width_Forms");
      Add ("Greek and Coptic",
           "Greek_And_Coptic");
      Add ("Ideographic Description Characters",
           "Ideograph_Descr_Chars");
      Add ("Katakana Phonetic Extensions",
           "Katakana_Phonetic_Ext");
      Add ("Mathematical Alphanumeric Symbols",
           "Math_Alphanumeric_Symb");
      Add ("Miscellaneous Mathematical Symbols-A",
           "Misc_Math_Symbols_A");
      Add ("Miscellaneous Mathematical Symbols-B",
           "Misc_Math_Symbols_B");
      Add ("Miscellaneous Symbols and Arrows",
           "Misc_Symbols_And_Arrows");
      Add ("Miscellaneous Symbols and Pictographs",
           "Misc_Symbols_Pictographs");
      Add ("Optical Character Recognition",
           "Optical_Chars_Recognition");
      Add ("Phonetic Extensions Supplement",
           "Phonetic_Ext_Sup");
      Add ("Superscripts and Subscripts",
           "Super_And_Sub_Scripts");
      Add ("Supplemental Mathematical Operators",
           "Sup_Math_Operators");
      Add ("Supplemental Symbols and Pictographs",
           "Sup_Symbols_Pictographs");
      Add ("Unified Canadian Aboriginal Syllabics",
           "Canadian_Aboriginal");
      Add ("Unified Canadian Aboriginal Syllabics Extended",
           "Canadian_Aboriginal_Ext");
      Add ("Variation Selectors Supplement",
           "Variation_Selectors_Sup");

      --  Motivated case exceptions.
      Add ("Transport and Map Symbols",
           "Transport_And_Map_Symbols");
      Add ("Sutton SignWriting",
           "Sutton_Signwriting");

      --  TODO: those should not be case exceptions, case makes sense.
      Add ("IPA Extensions",
           "Ipa_Extensions");
      Add ("NKo",
           "Nko");
      Add ("CJK Radicals Supplement",
           "Cjk_Radicals_Supplement");
      Add ("CJK Strokes",
           "Cjk_Strokes");
      Add ("CJK Compatibility",
           "Cjk_Compatibility");
      Add ("Phags-pa",
           "Phags_Pa");
      Add ("CJK Compatibility Forms",
           "Cjk_Compatibility_Forms");

      --  Unicode 9.
      Add ("Ideographic Symbols and Punctuation",
           "Ideograph_Symb_Punct");
      --  Unicode 12.
      Add ("Egyptian Hieroglyph Format Controls",
           "Egypt_Hieroglyph_Fmt_Ctrl");
      Add ("Symbols and Pictographs Extended-A",
           "Symbols_Pictographs_Ext_A");
      --  Unicode 13
      Add ("Symbols for Legacy Computing",
           "Symbols_Legacy_Computing");
   end Set_Exceptions;

end Translators.Block;
