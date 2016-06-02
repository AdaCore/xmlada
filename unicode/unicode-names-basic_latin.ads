--  This file is built automatically from data found on the
--  unicode web site (http://www.unicode.org)
--  in version 8.0.0.
package Unicode.Names.Basic_Latin is
   pragma Preelaborate;
   pragma Style_Checks (Off);

   --  Real Unicode name is NULL
   Unicode_Null                            : constant Unicode_Char := 16#0000#;
   Nul                                     : Unicode_Char renames Unicode_Null;
   Start_Of_Heading                        : constant Unicode_Char := 16#0001#;
   Soh                                     :
      Unicode_Char renames Start_Of_Heading;
   Start_Of_Text                           : constant Unicode_Char := 16#0002#;
   Stx                                     :
      Unicode_Char renames Start_Of_Text;
   End_Of_Text                             : constant Unicode_Char := 16#0003#;
   Etx                                     : Unicode_Char renames End_Of_Text;
   End_Of_Transmission                     : constant Unicode_Char := 16#0004#;
   Eot                                     :
      Unicode_Char renames End_Of_Transmission;
   Enquiry                                 : constant Unicode_Char := 16#0005#;
   Enq                                     : Unicode_Char renames Enquiry;
   Acknowledge                             : constant Unicode_Char := 16#0006#;
   Ack                                     : Unicode_Char renames Acknowledge;
   Alert                                   : constant Unicode_Char := 16#0007#;
   Bel                                     : Unicode_Char renames Alert;
   Backspace                               : constant Unicode_Char := 16#0008#;
   Bs                                      : Unicode_Char renames Backspace;
   Character_Tabulation                    : constant Unicode_Char := 16#0009#;
   Horizontal_Tabulation                   :
      Unicode_Char renames Character_Tabulation;
   Ht                                      :
      Unicode_Char renames Character_Tabulation;
   Tab                                     :
      Unicode_Char renames Character_Tabulation;
   Line_Feed                               : constant Unicode_Char := 16#000A#;
   New_Line                                : Unicode_Char renames Line_Feed;
   End_Of_Line                             : Unicode_Char renames Line_Feed;
   Lf                                      : Unicode_Char renames Line_Feed;
   Nl                                      : Unicode_Char renames Line_Feed;
   Eol                                     : Unicode_Char renames Line_Feed;
   Line_Tabulation                         : constant Unicode_Char := 16#000B#;
   Vertical_Tabulation                     :
      Unicode_Char renames Line_Tabulation;
   Vt                                      :
      Unicode_Char renames Line_Tabulation;
   Form_Feed                               : constant Unicode_Char := 16#000C#;
   Ff                                      : Unicode_Char renames Form_Feed;
   Carriage_Return                         : constant Unicode_Char := 16#000D#;
   Cr                                      :
      Unicode_Char renames Carriage_Return;
   Shift_Out                               : constant Unicode_Char := 16#000E#;
   Locking_Shift_One                       : Unicode_Char renames Shift_Out;
   So                                      : Unicode_Char renames Shift_Out;
   Shift_In                                : constant Unicode_Char := 16#000F#;
   Locking_Shift_Zero                      : Unicode_Char renames Shift_In;
   Si                                      : Unicode_Char renames Shift_In;
   Data_Link_Escape                        : constant Unicode_Char := 16#0010#;
   Dle                                     :
      Unicode_Char renames Data_Link_Escape;
   Device_Control_One                      : constant Unicode_Char := 16#0011#;
   Dc1                                     :
      Unicode_Char renames Device_Control_One;
   Device_Control_Two                      : constant Unicode_Char := 16#0012#;
   Dc2                                     :
      Unicode_Char renames Device_Control_Two;
   Device_Control_Three                    : constant Unicode_Char := 16#0013#;
   Dc3                                     :
      Unicode_Char renames Device_Control_Three;
   Device_Control_Four                     : constant Unicode_Char := 16#0014#;
   Dc4                                     :
      Unicode_Char renames Device_Control_Four;
   Negative_Acknowledge                    : constant Unicode_Char := 16#0015#;
   Nak                                     :
      Unicode_Char renames Negative_Acknowledge;
   Synchronous_Idle                        : constant Unicode_Char := 16#0016#;
   Syn                                     :
      Unicode_Char renames Synchronous_Idle;
   End_Of_Transmission_Block               : constant Unicode_Char := 16#0017#;
   Etb                                     :
      Unicode_Char renames End_Of_Transmission_Block;
   Cancel                                  : constant Unicode_Char := 16#0018#;
   Can                                     : Unicode_Char renames Cancel;
   End_Of_Medium                           : constant Unicode_Char := 16#0019#;
   Eom                                     :
      Unicode_Char renames End_Of_Medium;
   Substitute                              : constant Unicode_Char := 16#001A#;
   Sub                                     : Unicode_Char renames Substitute;
   Escape                                  : constant Unicode_Char := 16#001B#;
   Esc                                     : Unicode_Char renames Escape;
   Information_Separator_Four              : constant Unicode_Char := 16#001C#;
   File_Separator                          :
      Unicode_Char renames Information_Separator_Four;
   Fs                                      :
      Unicode_Char renames Information_Separator_Four;
   Information_Separator_Three             : constant Unicode_Char := 16#001D#;
   Group_Separator                         :
      Unicode_Char renames Information_Separator_Three;
   Gs                                      :
      Unicode_Char renames Information_Separator_Three;
   Information_Separator_Two               : constant Unicode_Char := 16#001E#;
   Record_Separator                        :
      Unicode_Char renames Information_Separator_Two;
   Rs                                      :
      Unicode_Char renames Information_Separator_Two;
   Information_Separator_One               : constant Unicode_Char := 16#001F#;
   Unit_Separator                          :
      Unicode_Char renames Information_Separator_One;
   Us                                      :
      Unicode_Char renames Information_Separator_One;
   Space                                   : constant Unicode_Char := 16#0020#;
   Sp                                      : Unicode_Char renames Space;
   Exclamation_Mark                        : constant Unicode_Char := 16#0021#;
   Quotation_Mark                          : constant Unicode_Char := 16#0022#;
   Number_Sign                             : constant Unicode_Char := 16#0023#;
   Dollar_Sign                             : constant Unicode_Char := 16#0024#;
   Percent_Sign                            : constant Unicode_Char := 16#0025#;
   Ampersand                               : constant Unicode_Char := 16#0026#;
   Apostrophe                              : constant Unicode_Char := 16#0027#;
   Left_Parenthesis                        : constant Unicode_Char := 16#0028#;
   Right_Parenthesis                       : constant Unicode_Char := 16#0029#;
   Asterisk                                : constant Unicode_Char := 16#002A#;
   Plus_Sign                               : constant Unicode_Char := 16#002B#;
   Comma                                   : constant Unicode_Char := 16#002C#;
   Hyphen_Minus                            : constant Unicode_Char := 16#002D#;
   Full_Stop                               : constant Unicode_Char := 16#002E#;
   Solidus                                 : constant Unicode_Char := 16#002F#;
   Digit_Zero                              : constant Unicode_Char := 16#0030#;
   Digit_One                               : constant Unicode_Char := 16#0031#;
   Digit_Two                               : constant Unicode_Char := 16#0032#;
   Digit_Three                             : constant Unicode_Char := 16#0033#;
   Digit_Four                              : constant Unicode_Char := 16#0034#;
   Digit_Five                              : constant Unicode_Char := 16#0035#;
   Digit_Six                               : constant Unicode_Char := 16#0036#;
   Digit_Seven                             : constant Unicode_Char := 16#0037#;
   Digit_Eight                             : constant Unicode_Char := 16#0038#;
   Digit_Nine                              : constant Unicode_Char := 16#0039#;
   Colon                                   : constant Unicode_Char := 16#003A#;
   Semicolon                               : constant Unicode_Char := 16#003B#;
   Less_Than_Sign                          : constant Unicode_Char := 16#003C#;
   Equals_Sign                             : constant Unicode_Char := 16#003D#;
   Greater_Than_Sign                       : constant Unicode_Char := 16#003E#;
   Question_Mark                           : constant Unicode_Char := 16#003F#;
   Commercial_At                           : constant Unicode_Char := 16#0040#;
   Latin_Capital_Letter_A                  : constant Unicode_Char := 16#0041#;
   Latin_Capital_Letter_B                  : constant Unicode_Char := 16#0042#;
   Latin_Capital_Letter_C                  : constant Unicode_Char := 16#0043#;
   Latin_Capital_Letter_D                  : constant Unicode_Char := 16#0044#;
   Latin_Capital_Letter_E                  : constant Unicode_Char := 16#0045#;
   Latin_Capital_Letter_F                  : constant Unicode_Char := 16#0046#;
   Latin_Capital_Letter_G                  : constant Unicode_Char := 16#0047#;
   Latin_Capital_Letter_H                  : constant Unicode_Char := 16#0048#;
   Latin_Capital_Letter_I                  : constant Unicode_Char := 16#0049#;
   Latin_Capital_Letter_J                  : constant Unicode_Char := 16#004A#;
   Latin_Capital_Letter_K                  : constant Unicode_Char := 16#004B#;
   Latin_Capital_Letter_L                  : constant Unicode_Char := 16#004C#;
   Latin_Capital_Letter_M                  : constant Unicode_Char := 16#004D#;
   Latin_Capital_Letter_N                  : constant Unicode_Char := 16#004E#;
   Latin_Capital_Letter_O                  : constant Unicode_Char := 16#004F#;
   Latin_Capital_Letter_P                  : constant Unicode_Char := 16#0050#;
   Latin_Capital_Letter_Q                  : constant Unicode_Char := 16#0051#;
   Latin_Capital_Letter_R                  : constant Unicode_Char := 16#0052#;
   Latin_Capital_Letter_S                  : constant Unicode_Char := 16#0053#;
   Latin_Capital_Letter_T                  : constant Unicode_Char := 16#0054#;
   Latin_Capital_Letter_U                  : constant Unicode_Char := 16#0055#;
   Latin_Capital_Letter_V                  : constant Unicode_Char := 16#0056#;
   Latin_Capital_Letter_W                  : constant Unicode_Char := 16#0057#;
   Latin_Capital_Letter_X                  : constant Unicode_Char := 16#0058#;
   Latin_Capital_Letter_Y                  : constant Unicode_Char := 16#0059#;
   Latin_Capital_Letter_Z                  : constant Unicode_Char := 16#005A#;
   Left_Square_Bracket                     : constant Unicode_Char := 16#005B#;
   Reverse_Solidus                         : constant Unicode_Char := 16#005C#;
   Right_Square_Bracket                    : constant Unicode_Char := 16#005D#;
   Circumflex_Accent                       : constant Unicode_Char := 16#005E#;
   Low_Line                                : constant Unicode_Char := 16#005F#;
   Grave_Accent                            : constant Unicode_Char := 16#0060#;
   Latin_Small_Letter_A                    : constant Unicode_Char := 16#0061#;
   Latin_Small_Letter_B                    : constant Unicode_Char := 16#0062#;
   Latin_Small_Letter_C                    : constant Unicode_Char := 16#0063#;
   Latin_Small_Letter_D                    : constant Unicode_Char := 16#0064#;
   Latin_Small_Letter_E                    : constant Unicode_Char := 16#0065#;
   Latin_Small_Letter_F                    : constant Unicode_Char := 16#0066#;
   Latin_Small_Letter_G                    : constant Unicode_Char := 16#0067#;
   Latin_Small_Letter_H                    : constant Unicode_Char := 16#0068#;
   Latin_Small_Letter_I                    : constant Unicode_Char := 16#0069#;
   Latin_Small_Letter_J                    : constant Unicode_Char := 16#006A#;
   Latin_Small_Letter_K                    : constant Unicode_Char := 16#006B#;
   Latin_Small_Letter_L                    : constant Unicode_Char := 16#006C#;
   Latin_Small_Letter_M                    : constant Unicode_Char := 16#006D#;
   Latin_Small_Letter_N                    : constant Unicode_Char := 16#006E#;
   Latin_Small_Letter_O                    : constant Unicode_Char := 16#006F#;
   Latin_Small_Letter_P                    : constant Unicode_Char := 16#0070#;
   Latin_Small_Letter_Q                    : constant Unicode_Char := 16#0071#;
   Latin_Small_Letter_R                    : constant Unicode_Char := 16#0072#;
   Latin_Small_Letter_S                    : constant Unicode_Char := 16#0073#;
   Latin_Small_Letter_T                    : constant Unicode_Char := 16#0074#;
   Latin_Small_Letter_U                    : constant Unicode_Char := 16#0075#;
   Latin_Small_Letter_V                    : constant Unicode_Char := 16#0076#;
   Latin_Small_Letter_W                    : constant Unicode_Char := 16#0077#;
   Latin_Small_Letter_X                    : constant Unicode_Char := 16#0078#;
   Latin_Small_Letter_Y                    : constant Unicode_Char := 16#0079#;
   Latin_Small_Letter_Z                    : constant Unicode_Char := 16#007A#;
   Left_Curly_Bracket                      : constant Unicode_Char := 16#007B#;
   Vertical_Line                           : constant Unicode_Char := 16#007C#;
   Right_Curly_Bracket                     : constant Unicode_Char := 16#007D#;
   Tilde                                   : constant Unicode_Char := 16#007E#;
   Delete                                  : constant Unicode_Char := 16#007F#;
   Del                                     : Unicode_Char renames Delete;
end Unicode.Names.Basic_Latin;
