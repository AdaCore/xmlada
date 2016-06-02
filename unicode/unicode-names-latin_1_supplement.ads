--  This file is built automatically from data found on the
--  unicode web site (http://www.unicode.org)
--  in version 8.0.0.
package Unicode.Names.Latin_1_Supplement is
   pragma Preelaborate;
   pragma Style_Checks (Off);

   Padding_Character                       : constant Unicode_Char := 16#0080#;
   Pad                                     :
      Unicode_Char renames Padding_Character;
   High_Octet_Preset                       : constant Unicode_Char := 16#0081#;
   Hop                                     :
      Unicode_Char renames High_Octet_Preset;
   Break_Permitted_Here                    : constant Unicode_Char := 16#0082#;
   Bph                                     :
      Unicode_Char renames Break_Permitted_Here;
   No_Break_Here                           : constant Unicode_Char := 16#0083#;
   Nbh                                     :
      Unicode_Char renames No_Break_Here;
   Index                                   : constant Unicode_Char := 16#0084#;
   Ind                                     : Unicode_Char renames Index;
   Next_Line                               : constant Unicode_Char := 16#0085#;
   Nel                                     : Unicode_Char renames Next_Line;
   Start_Of_Selected_Area                  : constant Unicode_Char := 16#0086#;
   Ssa                                     :
      Unicode_Char renames Start_Of_Selected_Area;
   End_Of_Selected_Area                    : constant Unicode_Char := 16#0087#;
   Esa                                     :
      Unicode_Char renames End_Of_Selected_Area;
   Character_Tabulation_Set                : constant Unicode_Char := 16#0088#;
   Horizontal_Tabulation_Set               :
      Unicode_Char renames Character_Tabulation_Set;
   Hts                                     :
      Unicode_Char renames Character_Tabulation_Set;
   Character_Tabulation_With_Justification : constant Unicode_Char := 16#0089#;
   Horizontal_Tabulation_With_Justification :
      Unicode_Char renames Character_Tabulation_With_Justification;
   Htj                                     :
      Unicode_Char renames Character_Tabulation_With_Justification;
   Line_Tabulation_Set                     : constant Unicode_Char := 16#008A#;
   Vertical_Tabulation_Set                 :
      Unicode_Char renames Line_Tabulation_Set;
   Vts                                     :
      Unicode_Char renames Line_Tabulation_Set;
   Partial_Line_Forward                    : constant Unicode_Char := 16#008B#;
   Partial_Line_Down                       :
      Unicode_Char renames Partial_Line_Forward;
   Pld                                     :
      Unicode_Char renames Partial_Line_Forward;
   Partial_Line_Backward                   : constant Unicode_Char := 16#008C#;
   Partial_Line_Up                         :
      Unicode_Char renames Partial_Line_Backward;
   Plu                                     :
      Unicode_Char renames Partial_Line_Backward;
   Reverse_Line_Feed                       : constant Unicode_Char := 16#008D#;
   Reverse_Index                           :
      Unicode_Char renames Reverse_Line_Feed;
   Ri                                      :
      Unicode_Char renames Reverse_Line_Feed;
   Single_Shift_Two                        : constant Unicode_Char := 16#008E#;
   Single_Shift_2                          :
      Unicode_Char renames Single_Shift_Two;
   Ss2                                     :
      Unicode_Char renames Single_Shift_Two;
   Single_Shift_Three                      : constant Unicode_Char := 16#008F#;
   Single_Shift_3                          :
      Unicode_Char renames Single_Shift_Three;
   Ss3                                     :
      Unicode_Char renames Single_Shift_Three;
   Device_Control_String                   : constant Unicode_Char := 16#0090#;
   Dcs                                     :
      Unicode_Char renames Device_Control_String;
   Private_Use_One                         : constant Unicode_Char := 16#0091#;
   Private_Use_1                           :
      Unicode_Char renames Private_Use_One;
   Pu1                                     :
      Unicode_Char renames Private_Use_One;
   Private_Use_Two                         : constant Unicode_Char := 16#0092#;
   Private_Use_2                           :
      Unicode_Char renames Private_Use_Two;
   Pu2                                     :
      Unicode_Char renames Private_Use_Two;
   Set_Transmit_State                      : constant Unicode_Char := 16#0093#;
   Sts                                     :
      Unicode_Char renames Set_Transmit_State;
   Cancel_Character                        : constant Unicode_Char := 16#0094#;
   Cch                                     :
      Unicode_Char renames Cancel_Character;
   Message_Waiting                         : constant Unicode_Char := 16#0095#;
   Mw                                      :
      Unicode_Char renames Message_Waiting;
   Start_Of_Guarded_Area                   : constant Unicode_Char := 16#0096#;
   Start_Of_Protected_Area                 :
      Unicode_Char renames Start_Of_Guarded_Area;
   Spa                                     :
      Unicode_Char renames Start_Of_Guarded_Area;
   End_Of_Guarded_Area                     : constant Unicode_Char := 16#0097#;
   End_Of_Protected_Area                   :
      Unicode_Char renames End_Of_Guarded_Area;
   Epa                                     :
      Unicode_Char renames End_Of_Guarded_Area;
   Start_Of_String                         : constant Unicode_Char := 16#0098#;
   Sos                                     :
      Unicode_Char renames Start_Of_String;
   Single_Graphic_Character_Introducer     : constant Unicode_Char := 16#0099#;
   Sgc                                     :
      Unicode_Char renames Single_Graphic_Character_Introducer;
   Single_Character_Introducer             : constant Unicode_Char := 16#009A#;
   Sci                                     :
      Unicode_Char renames Single_Character_Introducer;
   Control_Sequence_Introducer             : constant Unicode_Char := 16#009B#;
   Csi                                     :
      Unicode_Char renames Control_Sequence_Introducer;
   String_Terminator                       : constant Unicode_Char := 16#009C#;
   St                                      :
      Unicode_Char renames String_Terminator;
   Operating_System_Command                : constant Unicode_Char := 16#009D#;
   Osc                                     :
      Unicode_Char renames Operating_System_Command;
   Privacy_Message                         : constant Unicode_Char := 16#009E#;
   Pm                                      :
      Unicode_Char renames Privacy_Message;
   Application_Program_Command             : constant Unicode_Char := 16#009F#;
   Apc                                     :
      Unicode_Char renames Application_Program_Command;
   No_Break_Space                          : constant Unicode_Char := 16#00A0#;
   Nbsp                                    :
      Unicode_Char renames No_Break_Space;
   Inverted_Exclamation_Mark               : constant Unicode_Char := 16#00A1#;
   Cent_Sign                               : constant Unicode_Char := 16#00A2#;
   Pound_Sign                              : constant Unicode_Char := 16#00A3#;
   Currency_Sign                           : constant Unicode_Char := 16#00A4#;
   Yen_Sign                                : constant Unicode_Char := 16#00A5#;
   Broken_Bar                              : constant Unicode_Char := 16#00A6#;
   Section_Sign                            : constant Unicode_Char := 16#00A7#;
   Diaeresis                               : constant Unicode_Char := 16#00A8#;
   Copyright_Sign                          : constant Unicode_Char := 16#00A9#;
   Feminine_Ordinal_Indicator              : constant Unicode_Char := 16#00AA#;
   Left_Pointing_Double_Angle_Quotation_Mark :
      constant Unicode_Char := 16#00AB#;
   Not_Sign                                : constant Unicode_Char := 16#00AC#;
   Soft_Hyphen                             : constant Unicode_Char := 16#00AD#;
   Shy                                     : Unicode_Char renames Soft_Hyphen;
   Registered_Sign                         : constant Unicode_Char := 16#00AE#;
   Macron                                  : constant Unicode_Char := 16#00AF#;
   Degree_Sign                             : constant Unicode_Char := 16#00B0#;
   Plus_Minus_Sign                         : constant Unicode_Char := 16#00B1#;
   Superscript_Two                         : constant Unicode_Char := 16#00B2#;
   Superscript_Three                       : constant Unicode_Char := 16#00B3#;
   Acute_Accent                            : constant Unicode_Char := 16#00B4#;
   Micro_Sign                              : constant Unicode_Char := 16#00B5#;
   Pilcrow_Sign                            : constant Unicode_Char := 16#00B6#;
   Middle_Dot                              : constant Unicode_Char := 16#00B7#;
   Cedilla                                 : constant Unicode_Char := 16#00B8#;
   Superscript_One                         : constant Unicode_Char := 16#00B9#;
   Masculine_Ordinal_Indicator             : constant Unicode_Char := 16#00BA#;
   Right_Pointing_Double_Angle_Quotation_Mark :
      constant Unicode_Char := 16#00BB#;
   Vulgar_Fraction_One_Quarter             : constant Unicode_Char := 16#00BC#;
   Vulgar_Fraction_One_Half                : constant Unicode_Char := 16#00BD#;
   Vulgar_Fraction_Three_Quarters          : constant Unicode_Char := 16#00BE#;
   Inverted_Question_Mark                  : constant Unicode_Char := 16#00BF#;
   Latin_Capital_Letter_A_With_Grave       : constant Unicode_Char := 16#00C0#;
   Latin_Capital_Letter_A_With_Acute       : constant Unicode_Char := 16#00C1#;
   Latin_Capital_Letter_A_With_Circumflex  : constant Unicode_Char := 16#00C2#;
   Latin_Capital_Letter_A_With_Tilde       : constant Unicode_Char := 16#00C3#;
   Latin_Capital_Letter_A_With_Diaeresis   : constant Unicode_Char := 16#00C4#;
   Latin_Capital_Letter_A_With_Ring_Above  : constant Unicode_Char := 16#00C5#;
   Latin_Capital_Letter_Ae                 : constant Unicode_Char := 16#00C6#;
   Latin_Capital_Letter_C_With_Cedilla     : constant Unicode_Char := 16#00C7#;
   Latin_Capital_Letter_E_With_Grave       : constant Unicode_Char := 16#00C8#;
   Latin_Capital_Letter_E_With_Acute       : constant Unicode_Char := 16#00C9#;
   Latin_Capital_Letter_E_With_Circumflex  : constant Unicode_Char := 16#00CA#;
   Latin_Capital_Letter_E_With_Diaeresis   : constant Unicode_Char := 16#00CB#;
   Latin_Capital_Letter_I_With_Grave       : constant Unicode_Char := 16#00CC#;
   Latin_Capital_Letter_I_With_Acute       : constant Unicode_Char := 16#00CD#;
   Latin_Capital_Letter_I_With_Circumflex  : constant Unicode_Char := 16#00CE#;
   Latin_Capital_Letter_I_With_Diaeresis   : constant Unicode_Char := 16#00CF#;
   Latin_Capital_Letter_Eth                : constant Unicode_Char := 16#00D0#;
   Latin_Capital_Letter_N_With_Tilde       : constant Unicode_Char := 16#00D1#;
   Latin_Capital_Letter_O_With_Grave       : constant Unicode_Char := 16#00D2#;
   Latin_Capital_Letter_O_With_Acute       : constant Unicode_Char := 16#00D3#;
   Latin_Capital_Letter_O_With_Circumflex  : constant Unicode_Char := 16#00D4#;
   Latin_Capital_Letter_O_With_Tilde       : constant Unicode_Char := 16#00D5#;
   Latin_Capital_Letter_O_With_Diaeresis   : constant Unicode_Char := 16#00D6#;
   Multiplication_Sign                     : constant Unicode_Char := 16#00D7#;
   Latin_Capital_Letter_O_With_Stroke      : constant Unicode_Char := 16#00D8#;
   Latin_Capital_Letter_U_With_Grave       : constant Unicode_Char := 16#00D9#;
   Latin_Capital_Letter_U_With_Acute       : constant Unicode_Char := 16#00DA#;
   Latin_Capital_Letter_U_With_Circumflex  : constant Unicode_Char := 16#00DB#;
   Latin_Capital_Letter_U_With_Diaeresis   : constant Unicode_Char := 16#00DC#;
   Latin_Capital_Letter_Y_With_Acute       : constant Unicode_Char := 16#00DD#;
   Latin_Capital_Letter_Thorn              : constant Unicode_Char := 16#00DE#;
   Latin_Small_Letter_Sharp_S              : constant Unicode_Char := 16#00DF#;
   Latin_Small_Letter_A_With_Grave         : constant Unicode_Char := 16#00E0#;
   Latin_Small_Letter_A_With_Acute         : constant Unicode_Char := 16#00E1#;
   Latin_Small_Letter_A_With_Circumflex    : constant Unicode_Char := 16#00E2#;
   Latin_Small_Letter_A_With_Tilde         : constant Unicode_Char := 16#00E3#;
   Latin_Small_Letter_A_With_Diaeresis     : constant Unicode_Char := 16#00E4#;
   Latin_Small_Letter_A_With_Ring_Above    : constant Unicode_Char := 16#00E5#;
   Latin_Small_Letter_Ae                   : constant Unicode_Char := 16#00E6#;
   Latin_Small_Letter_C_With_Cedilla       : constant Unicode_Char := 16#00E7#;
   Latin_Small_Letter_E_With_Grave         : constant Unicode_Char := 16#00E8#;
   Latin_Small_Letter_E_With_Acute         : constant Unicode_Char := 16#00E9#;
   Latin_Small_Letter_E_With_Circumflex    : constant Unicode_Char := 16#00EA#;
   Latin_Small_Letter_E_With_Diaeresis     : constant Unicode_Char := 16#00EB#;
   Latin_Small_Letter_I_With_Grave         : constant Unicode_Char := 16#00EC#;
   Latin_Small_Letter_I_With_Acute         : constant Unicode_Char := 16#00ED#;
   Latin_Small_Letter_I_With_Circumflex    : constant Unicode_Char := 16#00EE#;
   Latin_Small_Letter_I_With_Diaeresis     : constant Unicode_Char := 16#00EF#;
   Latin_Small_Letter_Eth                  : constant Unicode_Char := 16#00F0#;
   Latin_Small_Letter_N_With_Tilde         : constant Unicode_Char := 16#00F1#;
   Latin_Small_Letter_O_With_Grave         : constant Unicode_Char := 16#00F2#;
   Latin_Small_Letter_O_With_Acute         : constant Unicode_Char := 16#00F3#;
   Latin_Small_Letter_O_With_Circumflex    : constant Unicode_Char := 16#00F4#;
   Latin_Small_Letter_O_With_Tilde         : constant Unicode_Char := 16#00F5#;
   Latin_Small_Letter_O_With_Diaeresis     : constant Unicode_Char := 16#00F6#;
   Division_Sign                           : constant Unicode_Char := 16#00F7#;
   Latin_Small_Letter_O_With_Stroke        : constant Unicode_Char := 16#00F8#;
   Latin_Small_Letter_U_With_Grave         : constant Unicode_Char := 16#00F9#;
   Latin_Small_Letter_U_With_Acute         : constant Unicode_Char := 16#00FA#;
   Latin_Small_Letter_U_With_Circumflex    : constant Unicode_Char := 16#00FB#;
   Latin_Small_Letter_U_With_Diaeresis     : constant Unicode_Char := 16#00FC#;
   Latin_Small_Letter_Y_With_Acute         : constant Unicode_Char := 16#00FD#;
   Latin_Small_Letter_Thorn                : constant Unicode_Char := 16#00FE#;
   Latin_Small_Letter_Y_With_Diaeresis     : constant Unicode_Char := 16#00FF#;
end Unicode.Names.Latin_1_Supplement;
