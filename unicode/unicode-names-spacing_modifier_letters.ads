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
package Unicode.Names.Spacing_Modifier_Letters is
   pragma Preelaborate;
   pragma Style_Checks (Off);

   Modifier_Letter_Small_H                 : constant Unicode_Char := 16#02B0#;
   Modifier_Letter_Small_H_With_Hook       : constant Unicode_Char := 16#02B1#;
   Modifier_Letter_Small_J                 : constant Unicode_Char := 16#02B2#;
   Modifier_Letter_Small_R                 : constant Unicode_Char := 16#02B3#;
   Modifier_Letter_Small_Turned_R          : constant Unicode_Char := 16#02B4#;
   Modifier_Letter_Small_Turned_R_With_Hook :
      constant Unicode_Char := 16#02B5#;
   Modifier_Letter_Small_Capital_Inverted_R :
      constant Unicode_Char := 16#02B6#;
   Modifier_Letter_Small_W                 : constant Unicode_Char := 16#02B7#;
   Modifier_Letter_Small_Y                 : constant Unicode_Char := 16#02B8#;
   Modifier_Letter_Prime                   : constant Unicode_Char := 16#02B9#;
   Modifier_Letter_Double_Prime            : constant Unicode_Char := 16#02BA#;
   Modifier_Letter_Turned_Comma            : constant Unicode_Char := 16#02BB#;
   Modifier_Letter_Apostrophe              : constant Unicode_Char := 16#02BC#;
   Modifier_Letter_Reversed_Comma          : constant Unicode_Char := 16#02BD#;
   Modifier_Letter_Right_Half_Ring         : constant Unicode_Char := 16#02BE#;
   Modifier_Letter_Left_Half_Ring          : constant Unicode_Char := 16#02BF#;
   Modifier_Letter_Glottal_Stop            : constant Unicode_Char := 16#02C0#;
   Modifier_Letter_Reversed_Glottal_Stop   : constant Unicode_Char := 16#02C1#;
   Modifier_Letter_Left_Arrowhead          : constant Unicode_Char := 16#02C2#;
   Modifier_Letter_Right_Arrowhead         : constant Unicode_Char := 16#02C3#;
   Modifier_Letter_Up_Arrowhead            : constant Unicode_Char := 16#02C4#;
   Modifier_Letter_Down_Arrowhead          : constant Unicode_Char := 16#02C5#;
   Modifier_Letter_Circumflex_Accent       : constant Unicode_Char := 16#02C6#;
   Caron                                   : constant Unicode_Char := 16#02C7#;
   Modifier_Letter_Vertical_Line           : constant Unicode_Char := 16#02C8#;
   Modifier_Letter_Macron                  : constant Unicode_Char := 16#02C9#;
   Modifier_Letter_Acute_Accent            : constant Unicode_Char := 16#02CA#;
   Modifier_Letter_Grave_Accent            : constant Unicode_Char := 16#02CB#;
   Modifier_Letter_Low_Vertical_Line       : constant Unicode_Char := 16#02CC#;
   Modifier_Letter_Low_Macron              : constant Unicode_Char := 16#02CD#;
   Modifier_Letter_Low_Grave_Accent        : constant Unicode_Char := 16#02CE#;
   Modifier_Letter_Low_Acute_Accent        : constant Unicode_Char := 16#02CF#;
   Modifier_Letter_Triangular_Colon        : constant Unicode_Char := 16#02D0#;
   Modifier_Letter_Half_Triangular_Colon   : constant Unicode_Char := 16#02D1#;
   Modifier_Letter_Centred_Right_Half_Ring : constant Unicode_Char := 16#02D2#;
   Modifier_Letter_Centred_Left_Half_Ring  : constant Unicode_Char := 16#02D3#;
   Modifier_Letter_Up_Tack                 : constant Unicode_Char := 16#02D4#;
   Modifier_Letter_Down_Tack               : constant Unicode_Char := 16#02D5#;
   Modifier_Letter_Plus_Sign               : constant Unicode_Char := 16#02D6#;
   Modifier_Letter_Minus_Sign              : constant Unicode_Char := 16#02D7#;
   Breve                                   : constant Unicode_Char := 16#02D8#;
   Dot_Above                               : constant Unicode_Char := 16#02D9#;
   Ring_Above                              : constant Unicode_Char := 16#02DA#;
   Ogonek                                  : constant Unicode_Char := 16#02DB#;
   Small_Tilde                             : constant Unicode_Char := 16#02DC#;
   Double_Acute_Accent                     : constant Unicode_Char := 16#02DD#;
   Modifier_Letter_Rhotic_Hook             : constant Unicode_Char := 16#02DE#;
   Modifier_Letter_Cross_Accent            : constant Unicode_Char := 16#02DF#;
   Modifier_Letter_Small_Gamma             : constant Unicode_Char := 16#02E0#;
   Modifier_Letter_Small_L                 : constant Unicode_Char := 16#02E1#;
   Modifier_Letter_Small_S                 : constant Unicode_Char := 16#02E2#;
   Modifier_Letter_Small_X                 : constant Unicode_Char := 16#02E3#;
   Modifier_Letter_Small_Reversed_Glottal_Stop :
      constant Unicode_Char := 16#02E4#;
   Modifier_Letter_Extra_High_Tone_Bar     : constant Unicode_Char := 16#02E5#;
   Modifier_Letter_High_Tone_Bar           : constant Unicode_Char := 16#02E6#;
   Modifier_Letter_Mid_Tone_Bar            : constant Unicode_Char := 16#02E7#;
   Modifier_Letter_Low_Tone_Bar            : constant Unicode_Char := 16#02E8#;
   Modifier_Letter_Extra_Low_Tone_Bar      : constant Unicode_Char := 16#02E9#;
   Modifier_Letter_Yin_Departing_Tone_Mark : constant Unicode_Char := 16#02EA#;
   Modifier_Letter_Yang_Departing_Tone_Mark :
      constant Unicode_Char := 16#02EB#;
   Modifier_Letter_Voicing                 : constant Unicode_Char := 16#02EC#;
   Modifier_Letter_Unaspirated             : constant Unicode_Char := 16#02ED#;
   Modifier_Letter_Double_Apostrophe       : constant Unicode_Char := 16#02EE#;
   Modifier_Letter_Low_Down_Arrowhead      : constant Unicode_Char := 16#02EF#;
   Modifier_Letter_Low_Up_Arrowhead        : constant Unicode_Char := 16#02F0#;
   Modifier_Letter_Low_Left_Arrowhead      : constant Unicode_Char := 16#02F1#;
   Modifier_Letter_Low_Right_Arrowhead     : constant Unicode_Char := 16#02F2#;
   Modifier_Letter_Low_Ring                : constant Unicode_Char := 16#02F3#;
   Modifier_Letter_Middle_Grave_Accent     : constant Unicode_Char := 16#02F4#;
   Modifier_Letter_Middle_Double_Grave_Accent :
      constant Unicode_Char := 16#02F5#;
   Modifier_Letter_Middle_Double_Acute_Accent :
      constant Unicode_Char := 16#02F6#;
   Modifier_Letter_Low_Tilde               : constant Unicode_Char := 16#02F7#;
   Modifier_Letter_Raised_Colon            : constant Unicode_Char := 16#02F8#;
   Modifier_Letter_Begin_High_Tone         : constant Unicode_Char := 16#02F9#;
   Modifier_Letter_End_High_Tone           : constant Unicode_Char := 16#02FA#;
   Modifier_Letter_Begin_Low_Tone          : constant Unicode_Char := 16#02FB#;
   Modifier_Letter_End_Low_Tone            : constant Unicode_Char := 16#02FC#;
   Modifier_Letter_Shelf                   : constant Unicode_Char := 16#02FD#;
   Modifier_Letter_Open_Shelf              : constant Unicode_Char := 16#02FE#;
   Modifier_Letter_Low_Left_Arrow          : constant Unicode_Char := 16#02FF#;
end Unicode.Names.Spacing_Modifier_Letters;
