-----------------------------------------------------------------------
--                XML/Ada - An XML suite for Ada95                   --
--                                                                   --
--                       Copyright (C) 2001-2002                     --
--                            ACT-Europe                             --
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

with Unicode.Names.Basic_Latin;      use Unicode.Names.Basic_Latin;
with Unicode.Names.Latin_Extended_A; use Unicode.Names.Latin_Extended_A;

package body Unicode is

   --------------------
   -- Is_White_Space --
   --------------------

   function Is_White_Space (Char : Unicode_Char) return Boolean is
   begin
      return    Char = Space
        or else Char = Horizontal_Tabulation
        or else Char = Line_Feed
        or else Char = Carriage_Return;
   end Is_White_Space;

   --------------------
   -- Is_Ideographic --
   --------------------

   function Is_Ideographic (Char : Unicode_Char) return Boolean is
   begin
      return Char in 16#4E00# .. 16#9FA5#
        or else Char = 16#3007#
        or else Char in 16#3021# .. 16#3029#;
   end Is_Ideographic;

   ---------------
   -- Is_Letter --
   ---------------

   function Is_Letter (Char : Unicode_Char) return Boolean is
   begin
      return Is_Base_Char (Char) or else Is_Ideographic (Char);
   end Is_Letter;

   ------------------
   -- Is_Base_Char --
   ------------------

   function Is_Base_Char (Char : Unicode_Char) return Boolean is
   begin
      case Char is
         when Nul .. 16#00FF# =>
            return Char in Latin_Capital_Letter_A .. Latin_Capital_Letter_Z
              or else Char in Latin_Small_Letter_A .. Latin_Small_Letter_Z
              or else Char in 16#00C0# .. 16#00D6#
              or else Char in 16#00D8# .. 16#00F6#
              or else Char in 16#00F8# .. 16#00FF#;

         when Latin_Capital_Letter_A_With_Macron .. 16#0217# =>
            return Char in Latin_Capital_Letter_A_With_Macron
                           .. Latin_Small_Letter_Dotless_I
              or else Char in Latin_Capital_Letter_J_With_Circumflex
                           .. Latin_Small_Letter_L_With_Caron
              or else Char in Latin_Capital_Letter_L_With_Stroke
                           .. Latin_Small_Letter_N_With_Caron
              or else Char in Latin_Capital_Letter_Eng
                           .. Latin_Small_Letter_Z_With_Caron
              or else Char in 16#0180# .. 16#01C3#
              or else Char in 16#01CD# .. 16#01F0#
              or else Char in 16#01F4# .. 16#01F5#
              or else Char in 16#01FA# .. 16#0217#;

         when 16#0218# .. 16#03FF# =>
            return Char in 16#0250# .. 16#02A8#
              or else Char in 16#02BB# .. 16#02C1#
              or else Char = 16#0386#
              or else Char in 16#0388# .. 16#038A#
              or else Char = 16#038c#
              or else Char in 16#038E# .. 16#03A1#
              or else Char in 16#03A3# .. 16#03CE#
              or else Char in 16#03D0# .. 16#03D6#
              or else Char = 16#03DA#
              or else Char = 16#03DC#
              or else Char = 16#03DE#
              or else Char = 16#03E0#
              or else Char in 16#03E2# .. 16#03F3#;

         when 16#0400# .. 16#04FF# =>
            return Char in 16#0401# .. 16#040C#
              or else Char in 16#040E# .. 16#044F#
              or else Char in 16#0451# .. 16#045C#
              or else Char in 16#045E# .. 16#0481#
              or else Char in 16#0490# .. 16#04C4#
              or else Char in 16#04C7# .. 16#04C8#
              or else Char in 16#04CB# .. 16#04CC#
              or else Char in 16#04D0# .. 16#04EB#
              or else Char in 16#04EE# .. 16#04F5#
              or else Char in 16#04F8# .. 16#04F9#;

         when 16#0500# .. 16#05FF# =>
            return Char in 16#0531# .. 16#0556#
              or else Char = 16#0559#
              or else Char in 16#0561# .. 16#0586#
              or else Char in 16#05D0# .. 16#05EA#
              or else Char in 16#05F0# .. 16#05F2#;

         when 16#0600# .. 16#06FF# =>
            return Char in 16#0621# .. 16#063A#
              or else Char in 16#0641# .. 16#064A#
              or else Char in 16#0671# .. 16#06B7#
              or else Char in 16#06BA# .. 16#06BE#
              or else Char in 16#06C0# .. 16#06CE#
              or else Char in 16#06D0# .. 16#06D3#
              or else Char = 16#06D5#
              or else Char in 16#06E5# .. 16#06E6#;

         when 16#0700# .. 16#09FF# =>
            return Char in 16#0905# .. 16#0939#
              or else Char = 16#093D#
              or else Char in 16#0958# .. 16#0961#
              or else Char in 16#0985# .. 16#098C#
              or else Char in 16#098F# .. 16#0990#
              or else Char in 16#0993# .. 16#09A8#
              or else Char in 16#09AA# .. 16#09B0#
              or else Char = 16#09B2#
              or else Char in 16#09B6# .. 16#09B9#
              or else Char in 16#09DC# .. 16#09DD#
              or else Char in 16#09DF# .. 16#09E1#
              or else Char in 16#09F0# .. 16#09F1#;

         when 16#0A00# .. 16#0AFF# =>
            return Char in 16#0A05# .. 16#0A0A#
              or else Char in 16#0A0F# .. 16#0A10#
              or else Char in 16#0A13# .. 16#0A28#
              or else Char in 16#0A2A# .. 16#0A30#
              or else Char in 16#0A32# .. 16#0A33#
              or else Char in 16#0A35# .. 16#0A36#
              or else Char in 16#0A38# .. 16#0A39#
              or else Char in 16#0A59# .. 16#0A5C#
              or else Char = 16#0A5E#
              or else Char in 16#0A72# .. 16#0A74#
              or else Char in 16#0A85# .. 16#0A8B#
              or else Char = 16#0A8D#
              or else Char in 16#0A8F# .. 16#0A91#
              or else Char in 16#0A93# .. 16#0AA8#
              or else Char in 16#0AAA# .. 16#0AB0#
              or else Char in 16#0AB2# .. 16#0AB3#
              or else Char in 16#0AB5# .. 16#0AB9#
              or else Char = 16#0ABD#
              or else Char = 16#0AE0#;

         when 16#0B00# .. 16#0BFF# =>
            return Char in 16#0B05# .. 16#0B0C#
              or else Char in 16#0B0F# .. 16#0B10#
              or else Char in 16#0B13# .. 16#0B28#
              or else Char in 16#0B2A# .. 16#0B30#
              or else Char in 16#0B32# .. 16#0B33#
              or else Char in 16#0B36# .. 16#0B39#
              or else Char = 16#0B3D#
              or else Char in 16#0B5C# .. 16#0B5D#
              or else Char in 16#0B5F# .. 16#0B61#
              or else Char in 16#0B85# .. 16#0B8A#
              or else Char in 16#0B8E# .. 16#0B90#
              or else Char in 16#0B92# .. 16#0B95#
              or else Char in 16#0B99# .. 16#0B9A#
              or else Char = 16#0B9C#
              or else Char in 16#0B9E# .. 16#0B9F#
              or else Char in 16#0BA3# .. 16#0BA4#
              or else Char in 16#0BA8# .. 16#0BAA#
              or else Char in 16#0BAE# .. 16#0BB5#
              or else Char in 16#0BB7# .. 16#0BB9#;

         when 16#0C00# .. 16#0CFF# =>
            return Char in 16#0C05# .. 16#0C0C#
              or else Char in 16#0C0E# .. 16#0C10#
              or else Char in 16#0C12# .. 16#0C28#
              or else Char in 16#0C2A# .. 16#0C33#
              or else Char in 16#0C35# .. 16#0C39#
              or else Char in 16#0C60# .. 16#0C61#
              or else Char in 16#0C85# .. 16#0C8C#
              or else Char in 16#0C8E# .. 16#0C90#
              or else Char in 16#0C92# .. 16#0CA8#
              or else Char in 16#0CAA# .. 16#0CB3#
              or else Char in 16#0CB5# .. 16#0CB9#
              or else Char = 16#0CDE#
              or else Char in 16#0CE0# .. 16#0CE1#;

         when 16#0D00# .. 16#0DFF# =>
            return Char in 16#0D05# .. 16#0D0C#
              or else Char in 16#0D0E# .. 16#0D10#
              or else Char in 16#0D12# .. 16#0D28#
              or else Char in 16#0D2A# .. 16#0D39#
              or else Char in 16#0D60# .. 16#0D61#;

         when 16#0E00# .. 16#0EFF# =>
            return Char in 16#0E01# .. 16#0E2E#
              or else Char = 16#0E30#
              or else Char in 16#0E32# .. 16#0E33#
              or else Char in 16#0E40# .. 16#0E45#
              or else Char in 16#0E81# .. 16#0E82#
              or else Char = 16#0E84#
              or else Char in 16#0E87# .. 16#0E88#
              or else Char = 16#0E8A#
              or else Char = 16#0E8D#
              or else Char in 16#0E94# .. 16#0E97#
              or else Char in 16#0E99# .. 16#0E9F#
              or else Char in 16#0EA1# .. 16#0EA3#
              or else Char = 16#0EA5#
              or else Char = 16#0EA7#
              or else Char in 16#0EAA# .. 16#0EAB#
              or else Char in 16#0EAD# .. 16#0EAE#
              or else Char = 16#0EB0#
              or else Char in 16#0EB2# .. 16#0EB3#
              or else Char = 16#0EBD#
              or else Char in 16#0EC0# .. 16#0EC4#;

         when 16#0F00# .. 16#0FFF# =>
            return Char in 16#0F40# .. 16#0F47#
              or else Char in 16#0F49# .. 16#0F69#;

         when 16#1000# .. 16#11FF# =>
            return Char in 16#10A0# .. 16#10C5#
              or else Char in 16#10D0# .. 16#10F6#
              or else Char = 16#1100#
              or else Char in 16#1102# .. 16#1103#
              or else Char in 16#1105# .. 16#1107#
              or else Char = 16#1109#
              or else Char in 16#110B# .. 16#110C#
              or else Char in 16#110E# .. 16#1112#
              or else Char = 16#113C#
              or else Char = 16#113E#
              or else Char = 16#1140#
              or else Char = 16#114C#
              or else Char = 16#114E#
              or else Char = 16#1150#
              or else Char in 16#1154# .. 16#1155#
              or else Char = 16#1159#
              or else Char in 16#115F# .. 16#1161#
              or else Char = 16#1163#
              or else Char = 16#1165#
              or else Char = 16#1167#
              or else Char = 16#1169#
              or else Char in 16#116D# .. 16#116E#
              or else Char in 16#1172# .. 16#1173#
              or else Char = 16#1175#
              or else Char = 16#119E#
              or else Char = 16#11A8#
              or else Char = 16#11AB#
              or else Char in 16#11AE# .. 16#11AF#
              or else Char in 16#11B7# .. 16#11B8#
              or else Char = 16#11BA#
              or else Char in 16#11BC# .. 16#11C2#
              or else Char = 16#11EB#
              or else Char = 16#11F0#
              or else Char = 16#11F9#;

         when 16#1200# .. 16#1FFF# =>
            return Char in 16#1E00# .. 16#1E9B#
              or else Char in 16#1EA0# .. 16#1EF9#
              or else Char in 16#1F00# .. 16#1F15#
              or else Char in 16#1F18# .. 16#1F1D#
              or else Char in 16#1F20# .. 16#1F45#
              or else Char in 16#1F48# .. 16#1F4D#
              or else Char in 16#1F50# .. 16#1F57#
              or else Char = 16#1F59#
              or else Char = 16#1F5B#
              or else Char = 16#1F5D#
              or else Char in 16#1F5F# .. 16#1F7D#
              or else Char in 16#1F80# .. 16#1FB4#
              or else Char in 16#1FB6# .. 16#1FBC#
              or else Char = 16#1FBE#
              or else Char in 16#1FC2# .. 16#1FC4#
              or else Char in 16#1FC6# .. 16#1FCC#
              or else Char in 16#1FD0# .. 16#1FD3#
              or else Char in 16#1FD6# .. 16#1FDB#
              or else Char in 16#1FE0# .. 16#1FEC#
              or else Char in 16#1FF2# .. 16#1FF4#
              or else Char in 16#1FF6# .. 16#1FFC#;

         when others =>
            return Char = 16#2126#
              or else Char in 16#212A# .. 16#212B#
              or else Char = 16#212E#
              or else Char in 16#2180# .. 16#2182#
              or else Char in 16#3041# .. 16#3094#
              or else Char in 16#30A1# .. 16#30FA#;
      end case;
   end Is_Base_Char;

   --------------
   -- Is_Digit --
   --------------

   function Is_Digit (Char : Unicode_Char) return Boolean is
   begin
      return Char in Digit_Zero .. Digit_Nine
        or else (Char > 16#FF#
                 and then (Char in  16#0660# .. 16#0669#
                           or else Char in  16#06F0# .. 16#06F9#
                           or else Char in  16#0966# .. 16#096F#
                           or else Char in  16#09E6# .. 16#09EF#
                           or else Char in  16#0A66# .. 16#0A6F#
                           or else Char in  16#0AE6# .. 16#0AEF#
                           or else Char in  16#0B66# .. 16#0B6F#
                           or else Char in  16#0BE7# .. 16#0BEF#
                           or else Char in  16#0C66# .. 16#0C6F#
                           or else Char in  16#0CE6# .. 16#0CEF#
                           or else Char in  16#0D66# .. 16#0D6F#
                           or else Char in  16#0E50# .. 16#0E59#
                           or else Char in  16#0ED0# .. 16#0ED9#
                           or else Char in  16#0F20# .. 16#0F29#));
   end Is_Digit;

   -----------------------
   -- Is_Combining_Char --
   -----------------------

   function Is_Combining_Char (Char : Unicode_Char) return Boolean is
   begin
      case Char is
         when Nul .. 16#02FF# =>
            return False;

         when 16#0300# .. 16#05FF# =>
            return Char in 16#0300# .. 16#0345#
              or else Char in 16#0360# .. 16#0361#
              or else Char in 16#0483# .. 16#0486#
              or else Char in 16#0591# .. 16#05A1#
              or else Char in 16#05A3# .. 16#05B9#
              or else Char in 16#05BB# .. 16#05BD#
              or else Char = 16#05BF#
              or else Char in 16#05C1# .. 16#05C2#
              or else Char = 16#05C4#;

         when 16#0600# .. 16#06FF# =>
            return Char in 16#064B# .. 16#0652#
              or else Char = 16#0670#
              or else Char in 16#06D6# .. 16#06DC#
              or else Char in 16#06DD# .. 16#06DF#
              or else Char in 16#06E0# .. 16#06E4#
              or else Char in 16#06E7# .. 16#06E8#
              or else Char in 16#06EA# .. 16#06ED#;

         when 16#0700# .. 16#09FF# =>
            return Char in 16#0901# .. 16#0903#
              or else Char = 16#093C#
              or else Char in 16#093E# .. 16#094C#
              or else Char = 16#094D#
              or else Char in 16#0951# .. 16#0954#
              or else Char in 16#0962# .. 16#0963#
              or else Char in 16#0981# .. 16#0983#
              or else Char = 16#09BC#
              or else Char = 16#09BE#
              or else Char = 16#09BF#
              or else Char in 16#09C0# .. 16#09C4#
              or else Char in 16#09C7# .. 16#09C8#
              or else Char in 16#09CB# .. 16#09CD#
              or else Char = 16#09D7#
              or else Char in 16#09E2# .. 16#09E3#;

         when 16#0A00# .. 16#0AFF# =>
            return Char = 16#0A02#
              or else Char = 16#0A3C#
              or else Char = 16#0A3E#
              or else Char = 16#0A3F#
              or else Char in 16#0A40# .. 16#0A42#
              or else Char in 16#0A47# .. 16#0A48#
              or else Char in 16#0A4B# .. 16#0A4D#
              or else Char in 16#0A70# .. 16#0A71#
              or else Char in 16#0A81# .. 16#0A83#
              or else Char = 16#0ABC#
              or else Char in 16#0ABE# .. 16#0AC5#
              or else Char in 16#0AC7# .. 16#0AC9#
              or else Char in 16#0ACB# .. 16#0ACD#;

         when 16#0B00# .. 16#0BFF# =>
            return Char in 16#0B01# .. 16#0B03#
              or else Char = 16#0B3C#
              or else Char in 16#0B3E# .. 16#0B43#
              or else Char in 16#0B47# .. 16#0B48#
              or else Char in 16#0B4B# .. 16#0B4D#
              or else Char in 16#0B56# .. 16#0B57#
              or else Char in 16#0B82# .. 16#0B83#
              or else Char in 16#0BBE# .. 16#0BC2#
              or else Char in 16#0BC6# .. 16#0BC8#
              or else Char in 16#0BCA# .. 16#0BCD#
              or else Char = 16#0BD7#;

         when 16#0C00# .. 16#0CFF# =>
            return Char in 16#0C01# .. 16#0C03#
              or else Char in 16#0C3E# .. 16#0C44#
              or else Char in 16#0C46# .. 16#0C48#
              or else Char in 16#0C4A# .. 16#0C4D#
              or else Char in 16#0C55# .. 16#0C56#
              or else Char in 16#0C82# .. 16#0C83#
              or else Char in 16#0CBE# .. 16#0CC4#
              or else Char in 16#0CC6# .. 16#0CC8#
              or else Char in 16#0CCA# .. 16#0CCD#
              or else Char in 16#0CD5# .. 16#0CD6#;

         when 16#0D00# .. 16#0DFF# =>
            return Char in 16#0D02# .. 16#0D03#
              or else Char in 16#0D3E# .. 16#0D43#
              or else Char in 16#0D46# .. 16#0D48#
              or else Char in 16#0D4A# .. 16#0D4D#
              or else Char = 16#0D57#;

         when 16#0E00# .. 16#0EFF# =>
            return Char = 16#0E31#
              or else Char in 16#0E34# .. 16#0E3A#
              or else Char in 16#0E47# .. 16#0E4E#
              or else Char = 16#0EB1#
              or else Char in 16#0EB4# .. 16#0EB9#
              or else Char in 16#0EBB# .. 16#0EBC#
              or else Char in 16#0EC8# .. 16#0ECD#;

         when 16#0F00# .. 16#0FFF# =>
            return Char in 16#0F18# .. 16#0F19#
              or else Char = 16#0F35#
              or else Char = 16#0F37#
              or else Char = 16#0F39#
              or else Char = 16#0F3E#
              or else Char = 16#0F3F#
              or else Char in 16#0F71# .. 16#0F84#
              or else Char in 16#0F86# .. 16#0F8B#
              or else Char in 16#0F90# .. 16#0F95#
              or else Char = 16#0F97#
              or else Char in 16#0F99# .. 16#0FAD#
              or else Char in 16#0FB1# .. 16#0FB7#
              or else Char = 16#0FB9#;

         when others =>
            return Char in 16#20D0# .. 16#20DC#
              or else Char = 16#20E1#
              or else Char in 16#302A# .. 16#302F#
              or else Char = 16#3099#
              or else Char = 16#309A#;
      end case;
   end Is_Combining_Char;

   -----------------
   -- Is_Extender --
   -----------------

   function Is_Extender (Char : Unicode_Char) return Boolean is
   begin
      return Char = 16#00B7#
        or else (Char > 16#FF#
                 and then (Char = 16#02D0#
                           or else Char = 16#02D1#
                           or else Char = 16#0387#
                           or else Char = 16#0640#
                           or else Char = 16#0E46#
                           or else Char = 16#0EC6#
                           or else Char = 16#3005#
                           or else Char in 16#3031# .. 16#3035#
                           or else Char in 16#309D# .. 16#309E#
                           or else Char in 16#30FC# .. 16#30FE#));
   end Is_Extender;

   ----------------
   -- To_Unicode --
   ----------------

   function To_Unicode (C : Character) return Unicode_Char is
   begin
      return Character'Pos (C);
   end To_Unicode;
end Unicode;
