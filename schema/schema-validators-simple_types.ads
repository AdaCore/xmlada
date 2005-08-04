
private package Schema.Validators.Simple_Types is

   procedure Register_Predefined_Types (G, XML_G : XML_Grammar_NS);
   --  Register all the predefined types

   -------------------------------
   -- Any_Simple_XML_Validator --
   -------------------------------

   type Any_Simple_XML_Validator_Record is new XML_Validator_Record
   with null record;
   type Any_Simple_XML_Validator
     is access all Any_Simple_XML_Validator_Record'Class;
   --  Validates a "SimpleType" XML datatype, ie accepts any contents but
   --  elements and attributes

   procedure Validate_Start_Element
     (Validator              : access Any_Simple_XML_Validator_Record;
      Local_Name             : Unicode.CES.Byte_Sequence;
      Namespace_URI          : Unicode.CES.Byte_Sequence;
      NS                     : XML_Grammar_NS;
      Data                   : Validator_Data;
      Schema_Target_NS       : XML_Grammar_NS;
      Element_Validator      : out XML_Element);
   procedure Validate_End_Element
     (Validator  : access Any_Simple_XML_Validator_Record;
      Local_Name : Unicode.CES.Byte_Sequence;
      Data       : Validator_Data);
   procedure Check_Content_Type
     (Validator        : access Any_Simple_XML_Validator_Record;
      Should_Be_Simple : Boolean);
   function Get_Facets_Description
     (Validator : access Any_Simple_XML_Validator_Record)
      return Facets_Description;
   --  See doc from inherited subprograms

   ---------------
   -- XML_Union --
   ---------------

   type XML_Union_Record is new Any_Simple_XML_Validator_Record with record
      Unions : Particle_List := Empty_Particle_List;
   end record;
   type XML_Union is access all XML_Union_Record'Class;

   procedure Add_Union
     (Validator : access XML_Union_Record; Part : XML_Type);
   --  Add a new element to the union in Validator

   procedure Validate_Characters
     (Union         : access XML_Union_Record;
      Ch            : Unicode.CES.Byte_Sequence;
      Empty_Element : Boolean);
   --  See doc from inherited subprograms

end Schema.Validators.Simple_Types;
