with Unicode.CES.Utf8;
with Unicode.CES.Utf32;

package Encodings is
   Encoding : constant Unicode.CES.Encoding_Scheme :=
     Unicode.CES.Utf8.Utf8_Encoding;

   function From_Utf32
     (Str : Unicode.CES.Utf32.Utf32_LE_String)
      return Unicode.CES.Utf8.Utf8_String
     renames Unicode.CES.Utf8.From_Utf32;

end Encodings;

