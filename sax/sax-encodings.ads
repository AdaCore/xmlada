with Unicode.CES.Utf8;
with Unicode.CES.Utf32;

package Sax.Encodings is
   Encoding : constant Unicode.CES.Encoding_Scheme :=
     Unicode.CES.Utf8.Utf8_Encoding;

   function From_Utf32
     (Str : Unicode.CES.Utf32.Utf32_LE_String)
      return Unicode.CES.Utf8.Utf8_String
      renames Unicode.CES.Utf8.From_Utf32;

   ----------------
   --  Constants --
   ----------------
   --  This is a set of constant strings that need to be defined for the
   --  parser. We do not initialize them through calls to Encoding.Encode, for
   --  efficiency reasons, and so that we know in advance the length of the
   --  byte_sequence (no memory allocation).
   --  These strings must be encoded with the default encoding.

   Amp_Sequence           : constant Unicode.CES.Byte_Sequence := "amp";
   Amp_DOM_Sequence       : constant Unicode.CES.Byte_Sequence := "&amp;";
   Any_Sequence           : constant Unicode.CES.Byte_Sequence := "ANY";
   Apos_Sequence          : constant Unicode.CES.Byte_Sequence := "apos";
   Attlist_Sequence       : constant Unicode.CES.Byte_Sequence := "ATTLIST";
   Cdata_Sequence         : constant Unicode.CES.Byte_Sequence := "CDATA";
   Cr_Sequence            : constant Unicode.CES.Byte_Sequence := "&#13;";
   Doctype_Sequence       : constant Unicode.CES.Byte_Sequence := "DOCTYPE";
   Element_Sequence       : constant Unicode.CES.Byte_Sequence := "LEMENT";
   Empty_Sequence         : constant Unicode.CES.Byte_Sequence := "EMPTY";
   Encoding_Sequence      : constant Unicode.CES.Byte_Sequence := "encoding";
   Entit_Sequence         : constant Unicode.CES.Byte_Sequence := "ENTIT";
   Id_Sequence            : constant Unicode.CES.Byte_Sequence := "ID";
   Ies_Sequence           : constant Unicode.CES.Byte_Sequence := "IES";
   Fixed_Sequence         : constant Unicode.CES.Byte_Sequence := "FIXED";
   Gt_Sequence            : constant Unicode.CES.Byte_Sequence := "gt";
   Gt_DOM_Sequence        : constant Unicode.CES.Byte_Sequence := "&gt;";
   Implied_Sequence       : constant Unicode.CES.Byte_Sequence := "IMPLIED";
   Include_Sequence       : constant Unicode.CES.Byte_Sequence := "INCLUDE";
   Ignore_Sequence        : constant Unicode.CES.Byte_Sequence := "IGNORE";
   Lang_Sequence          : constant Unicode.CES.Byte_Sequence := "lang";
   Lf_Sequence            : constant Unicode.CES.Byte_Sequence := "&#10;";
   Lt_Sequence            : constant Unicode.CES.Byte_Sequence := "lt";
   Lt_DOM_Sequence        : constant Unicode.CES.Byte_Sequence := "&lt;";
   Mtoken_Sequence        : constant Unicode.CES.Byte_Sequence := "MTOKEN";
   Ndata_Sequence         : constant Unicode.CES.Byte_Sequence := "NDATA";
   Otation_Sequence       : constant Unicode.CES.Byte_Sequence := "OTATION";
   No_Sequence            : constant Unicode.CES.Byte_Sequence := "no";
   Notation_Sequence      : constant Unicode.CES.Byte_Sequence := "NOTATION";
   Ntity_Sequence         : constant Unicode.CES.Byte_Sequence := "NTITY";
   Pcdata_Sequence        : constant Unicode.CES.Byte_Sequence := "#PCDATA";
   Public_Sequence        : constant Unicode.CES.Byte_Sequence := "PUBLIC";
   Quot_Sequence          : constant Unicode.CES.Byte_Sequence := "quot";
   Quot_DOM_Sequence      : constant Unicode.CES.Byte_Sequence := "&quot;";
   Ref_Sequence           : constant Unicode.CES.Byte_Sequence := "REF";
   Required_Sequence      : constant Unicode.CES.Byte_Sequence := "REQUIRED";
   Standalone_Sequence    : constant Unicode.CES.Byte_Sequence := "standalone";
   Tab_Sequence           : constant Unicode.CES.Byte_Sequence := "&#9;";
   System_Sequence        : constant Unicode.CES.Byte_Sequence := "SYSTEM";
   Version_Sequence       : constant Unicode.CES.Byte_Sequence := "version";
   Xml_Sequence           : constant Unicode.CES.Byte_Sequence := "xml";
   Xmlns_Sequence         : constant Unicode.CES.Byte_Sequence := "xmlns";
   Yes_Sequence           : constant Unicode.CES.Byte_Sequence := "yes";
   Vertical_Line_Sequence : constant Unicode.CES.Byte_Sequence := "|";
   Comma_Sequence         : constant Unicode.CES.Byte_Sequence := ",";
   Closing_Parenthesis_Sequence : constant Unicode.CES.Byte_Sequence := ")";
   Opening_Parenthesis_Sequence : constant Unicode.CES.Byte_Sequence := "(";
   Star_Sequence          : constant Unicode.CES.Byte_Sequence := "*";
   Question_Mark_Sequence : constant Unicode.CES.Byte_Sequence := "?";
   Plus_Sign_Sequence     : constant Unicode.CES.Byte_Sequence := "+";
   Colon_Sequence         : constant Unicode.CES.Byte_Sequence := ":";
   Percent_Sign_Sequence  : constant Unicode.CES.Byte_Sequence := "%";
   Space_Sequence         : constant Unicode.CES.Byte_Sequence := " ";
   Less_Than_Sequence     : constant Unicode.CES.Byte_Sequence := "<";
   Greater_Than_Sequence  : constant Unicode.CES.Byte_Sequence := ">";
   Equals_Sign_Sequence   : constant Unicode.CES.Byte_Sequence := "=";
   Quotation_Mark_Sequence      : constant Unicode.CES.Byte_Sequence := """";
   Slash_Sequence         : constant Unicode.CES.Byte_Sequence := "/";

end Sax.Encodings;

