with Schema.Validators.Facets; use Schema.Validators.Facets;

private package Schema.Validators.Simple_Types is

   -----------------------------------
   --  Common_Simple_XML_Validator --
   -----------------------------------
   --  For all simple types with pattern, enumeration and whitespace faces

   type Common_Simple_XML_Validator is new Any_Simple_XML_Validator_Record
   with record
      Facets : Facets_Value;
   end record;
   type Common_Simple_Validator
     is access all Common_Simple_XML_Validator'Class;

   procedure Validate_Characters
     (Validator     : access Common_Simple_XML_Validator;
      Ch            : Unicode.CES.Byte_Sequence;
      Empty_Element : Boolean);
   procedure Free
     (Validator : in out Common_Simple_XML_Validator);
   procedure Add_Facet
     (Validator   : access Common_Simple_XML_Validator;
      Facet_Name  : Unicode.CES.Byte_Sequence;
      Facet_Value : Unicode.CES.Byte_Sequence);
   function Is_Simple_Type
     (Validator : access Common_Simple_XML_Validator) return Boolean;
   --  See doc from inherited subprogram

   -----------------------
   -- Boolean_Validator --
   -----------------------

   type Boolean_Validator_Record is new Common_Simple_XML_Validator with
     null record;
   procedure Validate_Characters
     (Validator     : access Boolean_Validator_Record;
      Ch            : Unicode.CES.Byte_Sequence;
      Empty_Element : Boolean);
   procedure Add_Facet
     (Validator   : access Boolean_Validator_Record;
      Facet_Name  : Unicode.CES.Byte_Sequence;
      Facet_Value : Unicode.CES.Byte_Sequence);
   --   See doc from inherited subprograms

end Schema.Validators.Simple_Types;
