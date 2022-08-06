--
--  Copyright (C) 2017, AdaCore
--

with Input_Sources.Strings; use Input_Sources.Strings;
with Unicode.CES;           use Unicode.CES;
with Unicode.CES.Utf8;

with GNAT.IO;            use GNAT.IO;
with Sax.Readers;        use Sax.Readers;
with Schema.Readers;     use Schema.Readers;
with Schema.Schema_Readers; use Schema.Schema_Readers;
with Schema.Validators;  use Schema.Validators;

procedure SchemaExample is
   Input, Schema_Input : String_Input;
   My_Reader : Validating_Reader;

   Grammar           : XML_Grammar;
   Schema_From_String  : Schema_Reader;

   XML_Source_String : constant Unicode.CES.Byte_Sequence :=
     "<?xml version=""1.0"" ?>"
     & "<preferences>"
     & "<pref name=""pref1"">Value1</pref>"
     & "<pref name=""pref2"">Value2</pref>"
     & "</preferences> ";

   XSD_Source_String : constant Unicode.CES.Byte_Sequence :=
     "<?xml version=""1.0"" ?>"
     & "<xsd:schema xmlns:xsd=""http://www.w3.org/2001/XMLSchema"">"
     & "  <xsd:element name=""preferences"">"
     & "    <xsd:complexType>"
     & "      <xsd:sequence>"
     & "         <xsd:element ref=""pref"" minOccurs=""1"" "
     & "maxOccurs=""unbounded""/>"
     & "      </xsd:sequence>"
     & "    </xsd:complexType>"
     & "  </xsd:element>"
     & "  <xsd:element name=""pref"">"
     & "    <xsd:complexType mixed=""true"">"
     & "       <xsd:attribute name=""name"" use=""required"" "
     & "type=""xsd:string"" />"
     & "    </xsd:complexType>"
     & "  </xsd:element>"
     & "</xsd:schema>";
begin
   Open (XSD_Source_String, Unicode.CES.Utf8.Utf8_Encoding, Schema_Input);
   Parse (Schema_From_String, Schema_Input);
   Close (Schema_Input);

   Grammar := Get_Grammar (Schema_From_String);

   Set_Public_Id (Input, "Preferences file");
   Open (XML_Source_String, Unicode.CES.Utf8.Utf8_Encoding, Input);

   Set_Feature (My_Reader, Schema_Validation_Feature, True);
   My_Reader.Set_Grammar (Grammar);

   Parse (My_Reader, Input);

   Close (Input);

   Put_Line ("OK");

exception
   when Schema.Validators.XML_Validation_Error =>
      Put_Line ("ERROR: " & Get_Error_Message (My_Reader));
end SchemaExample;
