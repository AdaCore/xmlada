with Schema.Readers;        use Schema.Readers;
with Schema.Schema_Readers; use Schema.Schema_Readers;
with Schema.Validators;     use Schema.Validators;
with Input_Sources.File;    use Input_Sources.File;
with Ada.Exceptions;        use Ada.Exceptions;
with GNAT.IO;               use GNAT.IO;
with Sax.Readers;           use Sax.Readers;

procedure TestSchema is
   Read : File_Input;
   My_Reader : Validating_Reader;
   Schema : Schema_Reader;
   Grammar : XML_Grammar;
   Str_String : XML_Type;
   AdaString, Strs, Strs2, Strs4 : Sequence;
   Zip, Strs3 : Choice;
begin
   Open ("test.xsd", Read);
   Set_Public_Id (Read, "test.xsd");
   Set_System_Id (Read, "test.xsd");
   Parse (Schema, Read);
   Close (Read);



   --  <element name="root" type="anyType"/>

   Initialize (Grammar);

   Str_String := Lookup (Grammar, "string");

   Register
     (Grammar, Create_Element ("root", Lookup (Grammar, "anyType")));

   --  <complexType name="AdaString">
   --    <sequence>
   --     <element name="size"     type="nonNegativeInteger"/>
   --     <element name="contents" type="string" />
   --    </sequence>
   --  </complexType>
   AdaString       := Create_Sequence;
   Add_Particle (AdaString, Create_Element
                   ("size",     Lookup (Grammar, "nonNegativeInteger")));
   Add_Particle (AdaString,  Create_Element ("contents", Str_String));
   Register (Grammar, Create_Type ("AdaString", AdaString));

   --  <element name="my_string" type="AdaString" />
   Register
     (Grammar,
      Create_Element ("my_string", Lookup (Grammar, "AdaString")));

   --  <complexType name="zip">
   --    <choice>
   --     <element name="zipcode" type="string"/>
   --     <element name="code_postal" type="string"/>
   --    </choice>
   --  </complexType>
   Zip := Create_Choice (Min_Occurs => 1, Max_Occurs => 2);
   Add_Particle (Zip, Create_Element ("zipcode", Str_String));
   Add_Particle (Zip, Create_Element ("code_postal", Str_String));
   Register (Grammar, Create_Type ("zip", Zip));

   --  <element name="zip" type="zip" />
   Register (Grammar, Create_Element ("zip", Lookup (Grammar, "zip")));

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
   Set_Debug_Name (Strs, "strs outer");
   Strs2 := Create_Sequence;
   Set_Debug_Name (Strs2, "strs inner");
   Add_Particle (Strs2, Create_Element ("str", Str_String));
   Add_Particle (Strs2, Create_Element ("str2", Str_String));
   Add_Particle (Strs, Strs2);
   Add_Particle (Strs, Create_Element ("str", Str_String));
   Register (Grammar, Create_Type ("strs", Strs));

   --  <element name="strs" type="strs" />
   Register (Grammar, Create_Element ("strs", Lookup (Grammar, "strs")));

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
   Set_Debug_Name (Strs3, "strs2 choice");
   Add_Particle (Strs3, Strs2);
   Add_Particle (Strs3, Create_Element ("str3", Str_String));
   Register (Grammar, Create_Type ("strs2", Strs3));

   --  <element name="strs2" type="strs2" />
   Register (Grammar, Create_Element ("strs2", Lookup (Grammar, "strs2")));

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
   Add_Particle (Strs3, Create_Element ("str", Str_String));
   Add_Particle (Strs3, Create_Element ("str2", Str_String));
   Strs4 := Create_Sequence;
   Add_Particle (Strs4, Strs3);
   Add_Particle (Strs4, Create_Element ("str3", Str_String));
   Register (Grammar, Create_Type ("strs3", Strs4));

   --  <element name="strs3" type="strs3" />
   Register (Grammar, Create_Element ("strs3", Lookup (Grammar, "strs3")));

   Set_Grammar (My_Reader, Grammar);
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
