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
package Unicode.Names.Vertical_Forms is
   pragma Preelaborate;
   pragma Style_Checks (Off);

   Presentation_Form_For_Vertical_Comma    : constant Unicode_Char := 16#FE10#;
   Presentation_Form_For_Vertical_Ideographic_Comma :
      constant Unicode_Char := 16#FE11#;
   Presentation_Form_For_Vertical_Ideographic_Full_Stop :
      constant Unicode_Char := 16#FE12#;
   Presentation_Form_For_Vertical_Colon    : constant Unicode_Char := 16#FE13#;
   Presentation_Form_For_Vertical_Semicolon :
      constant Unicode_Char := 16#FE14#;
   Presentation_Form_For_Vertical_Exclamation_Mark :
      constant Unicode_Char := 16#FE15#;
   Presentation_Form_For_Vertical_Question_Mark :
      constant Unicode_Char := 16#FE16#;
   Presentation_Form_For_Vertical_Left_White_Lenticular_Bracket :
      constant Unicode_Char := 16#FE17#;
   Presentation_Form_For_Vertical_Right_White_Lenticular_Brakcet :
      constant Unicode_Char := 16#FE18#;
   Presentation_Form_For_Vertical_Right_White_Lenticular_Bracket :
      Unicode_Char renames Presentation_Form_For_Vertical_Right_White_Lenticular_Brakcet;
   Presentation_Form_For_Vertical_Horizontal_Ellipsis :
      constant Unicode_Char := 16#FE19#;
end Unicode.Names.Vertical_Forms;
