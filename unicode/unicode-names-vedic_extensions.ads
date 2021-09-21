--  This file is built automatically from data found on the
--  unicode web site (http://www.unicode.org)
--  in version 8.0.0 and thus is a subject to unicode license:
--
--  UNICODE, INC. LICENSE AGREEMENT - DATA FILES AND SOFTWARE
--  See Terms of Use for definitions of Unicode Inc.'s
--  Data Files and Software.
--
--  NOTICE TO USER: Carefully read the following legal agreement.
--  BY DOWNLOADING, INSTALLING, COPYING OR OTHERWISE USING UNICODE INC.'S
--  DATA FILES ("DATA FILES"), AND/OR SOFTWARE ("SOFTWARE"),
--  YOU UNEQUIVOCALLY ACCEPT, AND AGREE TO BE BOUND BY, ALL OF THE
--  TERMS AND CONDITIONS OF THIS AGREEMENT.
--  IF YOU DO NOT AGREE, DO NOT DOWNLOAD, INSTALL, COPY, DISTRIBUTE OR USE
--  THE DATA FILES OR SOFTWARE.
--
--  COPYRIGHT AND PERMISSION NOTICE
--
--  Copyright © 1991-2021 Unicode, Inc. All rights reserved.
--  Distributed under the Terms of Use
--  in https://www.unicode.org/copyright.html.
--
--  Permission is hereby granted, free of charge, to any person obtaining
--  a copy of the Unicode data files and any associated documentation
--  (the "Data Files") or Unicode software and any associated documentation
--  (the "Software") to deal in the Data Files or Software
--  without restriction, including without limitation the rights to use,
--  copy, modify, merge, publish, distribute, and/or sell copies of
--  the Data Files or Software, and to permit persons to whom the Data Files
--  or Software are furnished to do so, provided that either
--  (a) this copyright and permission notice appear with all copies
--  of the Data Files or Software, or
--  (b) this copyright and permission notice appear in associated
--  Documentation.
--
--  THE DATA FILES AND SOFTWARE ARE PROVIDED "AS IS", WITHOUT WARRANTY OF
--  ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
--  WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
--  NONINFRINGEMENT OF THIRD PARTY RIGHTS.
--  IN NO EVENT SHALL THE COPYRIGHT HOLDER OR HOLDERS INCLUDED IN THIS
--  NOTICE BE LIABLE FOR ANY CLAIM, OR ANY SPECIAL INDIRECT OR CONSEQUENTIAL
--  DAMAGES, OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE,
--  DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
--  TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
--  PERFORMANCE OF THE DATA FILES OR SOFTWARE.
--
--  Except as contained in this notice, the name of a copyright holder
--  shall not be used in advertising or otherwise to promote the sale,
--  use or other dealings in these Data Files or Software without prior
--  written authorization of the copyright holder.
package Unicode.Names.Vedic_Extensions is
   pragma Preelaborate;
   pragma Style_Checks (Off);

   Vedic_Tone_Karshana                     : constant Unicode_Char := 16#1CD0#;
   Vedic_Tone_Shara                        : constant Unicode_Char := 16#1CD1#;
   Vedic_Tone_Prenkha                      : constant Unicode_Char := 16#1CD2#;
   Vedic_Sign_Nihshvasa                    : constant Unicode_Char := 16#1CD3#;
   Vedic_Sign_Yajurvedic_Midline_Svarita   : constant Unicode_Char := 16#1CD4#;
   Vedic_Tone_Yajurvedic_Aggravated_Independent_Svarita :
      constant Unicode_Char := 16#1CD5#;
   Vedic_Tone_Yajurvedic_Independent_Svarita :
      constant Unicode_Char := 16#1CD6#;
   Vedic_Tone_Yajurvedic_Kathaka_Independent_Svarita :
      constant Unicode_Char := 16#1CD7#;
   Vedic_Tone_Candra_Below                 : constant Unicode_Char := 16#1CD8#;
   Vedic_Tone_Yajurvedic_Kathaka_Independent_Svarita_Schroeder :
      constant Unicode_Char := 16#1CD9#;
   Vedic_Tone_Double_Svarita               : constant Unicode_Char := 16#1CDA#;
   Vedic_Tone_Triple_Svarita               : constant Unicode_Char := 16#1CDB#;
   Vedic_Tone_Kathaka_Anudatta             : constant Unicode_Char := 16#1CDC#;
   Vedic_Tone_Dot_Below                    : constant Unicode_Char := 16#1CDD#;
   Vedic_Tone_Two_Dots_Below               : constant Unicode_Char := 16#1CDE#;
   Vedic_Tone_Three_Dots_Below             : constant Unicode_Char := 16#1CDF#;
   Vedic_Tone_Rigvedic_Kashmiri_Independent_Svarita :
      constant Unicode_Char := 16#1CE0#;
   Vedic_Tone_Atharvavedic_Independent_Svarita :
      constant Unicode_Char := 16#1CE1#;
   Vedic_Sign_Visarga_Svarita              : constant Unicode_Char := 16#1CE2#;
   Vedic_Sign_Visarga_Udatta               : constant Unicode_Char := 16#1CE3#;
   Vedic_Sign_Reversed_Visarga_Udatta      : constant Unicode_Char := 16#1CE4#;
   Vedic_Sign_Visarga_Anudatta             : constant Unicode_Char := 16#1CE5#;
   Vedic_Sign_Reversed_Visarga_Anudatta    : constant Unicode_Char := 16#1CE6#;
   Vedic_Sign_Visarga_Udatta_With_Tail     : constant Unicode_Char := 16#1CE7#;
   Vedic_Sign_Visarga_Anudatta_With_Tail   : constant Unicode_Char := 16#1CE8#;
   Vedic_Sign_Anusvara_Antargomukha        : constant Unicode_Char := 16#1CE9#;
   Vedic_Sign_Anusvara_Bahirgomukha        : constant Unicode_Char := 16#1CEA#;
   Vedic_Sign_Anusvara_Vamagomukha         : constant Unicode_Char := 16#1CEB#;
   Vedic_Sign_Anusvara_Vamagomukha_With_Tail :
      constant Unicode_Char := 16#1CEC#;
   Vedic_Sign_Tiryak                       : constant Unicode_Char := 16#1CED#;
   Vedic_Sign_Hexiform_Long_Anusvara       : constant Unicode_Char := 16#1CEE#;
   Vedic_Sign_Long_Anusvara                : constant Unicode_Char := 16#1CEF#;
   Vedic_Sign_Rthang_Long_Anusvara         : constant Unicode_Char := 16#1CF0#;
   Vedic_Sign_Anusvara_Ubhayato_Mukha      : constant Unicode_Char := 16#1CF1#;
   Vedic_Sign_Ardhavisarga                 : constant Unicode_Char := 16#1CF2#;
   Vedic_Sign_Rotated_Ardhavisarga         : constant Unicode_Char := 16#1CF3#;
   Vedic_Tone_Candra_Above                 : constant Unicode_Char := 16#1CF4#;
   Vedic_Sign_Jihvamuliya                  : constant Unicode_Char := 16#1CF5#;
   Vedic_Sign_Upadhmaniya                  : constant Unicode_Char := 16#1CF6#;
   Vedic_Tone_Ring_Above                   : constant Unicode_Char := 16#1CF8#;
   Vedic_Tone_Double_Ring_Above            : constant Unicode_Char := 16#1CF9#;
end Unicode.Names.Vedic_Extensions;
