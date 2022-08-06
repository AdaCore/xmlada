--
--  Copyright (C) 2017, AdaCore
--

with Input_Sources.Strings; use Input_Sources.Strings;
with Unicode.CES;           use Unicode.CES;
with Unicode.CES.Utf8;

with Sax.Readers;        use Sax.Readers;
with SaxExample;         use SaxExample;

procedure SaxExample_Main is
   My_Reader : SaxExample.Reader;
   Input     : String_Input;

   XML_Source_String : constant Unicode.CES.Byte_Sequence :=
     "<?xml version=""1.0"" ?>"
     & "<preferences>"
     & "<pref name=""pref1"">Value1</pref>"
     & "<pref name=""pref2"">Value2</pref>"
     & "</preferences> ";
begin
   Set_Public_Id (Input, "Preferences file");
   Set_System_Id (Input, "pref.xml");
   Open (XML_Source_String, Unicode.CES.Utf8.Utf8_Encoding, Input);

   Set_Feature (My_Reader, Namespace_Prefixes_Feature, False);
   Set_Feature (My_Reader, Namespace_Feature, False);
   Set_Feature (My_Reader, Validation_Feature, False);

   Parse (My_Reader, Input);

   Close (Input);
end SaxExample_Main;
