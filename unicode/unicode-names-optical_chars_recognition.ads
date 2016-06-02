--  This file is built automatically from data found on the
--  unicode web site (http://www.unicode.org)
--  in version 8.0.0.
package Unicode.Names.Optical_Chars_Recognition is
   pragma Preelaborate;
   pragma Style_Checks (Off);

   Ocr_Hook                                : constant Unicode_Char := 16#2440#;
   Ocr_Chair                               : constant Unicode_Char := 16#2441#;
   Ocr_Fork                                : constant Unicode_Char := 16#2442#;
   Ocr_Inverted_Fork                       : constant Unicode_Char := 16#2443#;
   Ocr_Belt_Buckle                         : constant Unicode_Char := 16#2444#;
   Ocr_Bow_Tie                             : constant Unicode_Char := 16#2445#;
   Ocr_Branch_Bank_Identification          : constant Unicode_Char := 16#2446#;
   Ocr_Amount_Of_Check                     : constant Unicode_Char := 16#2447#;
   Ocr_Dash                                : constant Unicode_Char := 16#2448#;
   Micr_On_Us_Symbol                       : Unicode_Char renames Ocr_Dash;
   Ocr_Customer_Account_Number             : constant Unicode_Char := 16#2449#;
   Micr_Dash_Symbol                        :
      Unicode_Char renames Ocr_Customer_Account_Number;
   Ocr_Double_Backslash                    : constant Unicode_Char := 16#244A#;
end Unicode.Names.Optical_Chars_Recognition;
