with Schema.Readers;    use Schema.Readers;
with Schema.Validators; use Schema.Validators;
with Input_Sources.File; use Input_Sources.File;
with Ada.Exceptions;     use Ada.Exceptions;
with GNAT.IO;            use GNAT.IO;
with Sax.Readers;        use Sax.Readers;

procedure TestSchema is
   Read : File_Input;
   My_Reader : Validating_Reader;
--     Schema : Schema_Reader;
   Grammar : XML_Grammar;
   Str_Type : Type_Validator;
   AdaString, Strs, Strs2, Strs4 : Sequence;
   Zip, Strs3 : Choice;
   SizeElement, ContentsElement : XML_Element;
begin
--     Open ("test.xsd", Read);
--     Set_Public_Id (Read, "test.xsd");
--     Set_System_Id (Read, "test.xsd");
--     Parse (Schema, Read);
--     Close (Read);

   --  <element name="root" type="anyType"/>

   Initialize (Grammar);

   Str_Type := Lookup_Type (Grammar, "string");

   Register_Element
     (Grammar, Create_Element ("root", Lookup_Type (Grammar, "anyType")));

   --  <complexType name="AdaString">
   --    <sequence>
   --     <element name="size"     type="nonNegativeInteger"/>
   --     <element name="contents" type="string" />
   --    </sequence>
   --  </complexType>
   AdaString       := Create_Sequence;
   SizeElement     := Create_Element
     ("size",     Lookup_Type (Grammar, "nonNegativeInteger"));
   ContentsElement := Create_Element ("contents", Str_Type);
   Add_Sequence (AdaString, SizeElement);
   Add_Sequence (AdaString, ContentsElement);
   Register_Type
     (Grammar, Create_Type ("AdaString", Type_Validator (AdaString)));

   --  <element name="my_string" type="AdaString" />
   Register_Element
     (Grammar,
      Create_Element ("my_string", Lookup_Type (Grammar, "AdaString")));

   --  <complexType name="zip">
   --    <choice>
   --     <element name="zipcode" type="string"/>
   --     <element name="code_postal" type="string"/>
   --    </choice>
   --  </complexType>
   Zip := Create_Choice (Min_Occurs => 1, Max_Occurs => 2);
   Add_Choice (Zip, Create_Element ("zipcode", Str_Type));
   Add_Choice (Zip, Create_Element ("code_postal", Str_Type));
   Register_Type (Grammar, Create_Type ("zip", Type_Validator (Zip)));

   --  <element name="zip" type="zip" />
   Register_Element
     (Grammar, Create_Element ("zip", Lookup_Type (Grammar, "zip")));

   --  <complexType name="strs">
   --    <sequence>
   --     <sequence>
   --       <element name="str" type="string"/>
   --       <element name="str2" type="string"/>
   --     </sequence>
   --     <element name="str" type="string"/>
   --    </sequence>
   --  </complexType>
   Strs := Create_Sequence;
   Strs2 := Create_Sequence;
   Add_Sequence (Strs2, Create_Element ("str", Str_Type));
   Add_Sequence (Strs2, Create_Element ("str2", Str_Type));
   Add_Sequence (Strs, Strs2);
   Add_Sequence (Strs, Create_Element ("str", Str_Type));
   Register_Type (Grammar, Create_Type ("strs", Type_Validator (Strs)));

   --  <element name="strs" type="strs" />
   Register_Element (Grammar, Create_Element ("strs", Strs));

   --  <complexType name="strs2">
   --    <choice>
   --     <sequence>
   --       <element name="str" type="string"/>
   --       <element name="str2" type="string"/>
   --     </sequence>
   --     <element name="str3" type="string"/>
   --    </choice>
   --  </complexType>
   Strs3 := Create_Choice;
   Add_Choice (Strs3, Strs2);
   Add_Choice (Strs3, Create_Element ("str3", Str_Type));
   Register_Type (Grammar, Create_Type ("strs2", Type_Validator (Strs3)));

   --  <element name="strs2" type="strs2" />
   Register_Element (Grammar, Create_Element ("strs2", Strs3));

   --  <complexType name="strs3">
   --    <sequence>
   --     <choice>
   --       <element name="str" type="string"/>
   --       <element name="str2" type="string"/>
   --     </schoice>
   --     <element name="str3" type="string"/>
   --    </sequence>
   --  </complexType>
   Strs3 := Create_Choice;
   Add_Choice (Strs3, Create_Element ("str", Str_Type));
   Add_Choice (Strs3, Create_Element ("str2", Str_Type));
   Strs4 := Create_Sequence;
   Add_Sequence (Strs4, Strs3);
   Add_Sequence (Strs4, Create_Element ("str3", Str_Type));
   Register_Type (Grammar, Create_Type ("strs3", Type_Validator (Strs4)));

   --  <element name="strs3" type="strs3" />
   Register_Element (Grammar, Create_Element ("strs3", Strs4));

   My_Reader := Create (Grammar);
   Open ("test.xml", Read);
   Set_Public_Id (Read, "test.xml");
   Set_System_Id (Read, "test.xml");
   Parse (My_Reader, Read);
   Close (Read);

exception
   when E : XML_Validation_Error | XML_Fatal_Error =>
      Put_Line (Exception_Message (E));
      Close (Read);
end TestSchema;
