package body Schema.Validators.UR_Type is

   UR_Type_Element   : array (Process_Contents_Type) of XML_Element :=
     (others => No_Element);


   type UR_Type_Validator is new XML_Validator_Record with record
      Process_Contents : Process_Contents_Type := Process_Strict;
   end record;
   type UR_Type_Access is access all UR_Type_Validator'Class;

   procedure Validate_End_Element
     (Validator      : access UR_Type_Validator;
      Local_Name     : Unicode.CES.Byte_Sequence;
      Data           : Validator_Data);
   procedure Validate_Attributes
     (Validator         : access UR_Type_Validator;
      Atts              : Sax.Attributes.Attributes'Class;
      Id_Table          : in out Id_Htable_Access;
      Nillable          : Boolean;
      Is_Nil            : out Boolean);
   procedure Validate_Start_Element
     (Validator         : access UR_Type_Validator;
      Local_Name        : Unicode.CES.Byte_Sequence;
      Namespace_URI     : Unicode.CES.Byte_Sequence;
      Data              : Validator_Data;
      Grammar           : in out XML_Grammar;
      Element_Validator : out XML_Element);
   --  See doc for inherited subprograms


   ----------------------------
   -- Validate_Start_Element --
   ----------------------------

   procedure Validate_Start_Element
     (Validator         : access UR_Type_Validator;
      Local_Name        : Unicode.CES.Byte_Sequence;
      Namespace_URI     : Unicode.CES.Byte_Sequence;
      Data              : Validator_Data;
      Grammar           : in out XML_Grammar;
      Element_Validator : out XML_Element)
   is
      pragma Unreferenced (Data);
      G : XML_Grammar_NS;
   begin
      Debug_Output
        ("Validate_Start_Element UR_Type Process_Contents="
         & Validator.Process_Contents'Img);

      --  ur-Type and anyType accept anything

      case Validator.Process_Contents is
         when Process_Strict =>
            Get_NS (Grammar, Namespace_URI, G);
            Element_Validator := Lookup_Element
              (G, Local_Name, Create_If_Needed => False);
            if Element_Validator = No_Element then
               Validation_Error
                 ("No definition provided for """ & Local_Name & """");
            end if;

         when Process_Lax =>
            Get_NS (Grammar, Namespace_URI, G);
            Element_Validator := Lookup_Element
              (G, Local_Name, Create_If_Needed => False);
            if Element_Validator = No_Element then
               Debug_Output ("Definition not found for " & Local_Name);
               Element_Validator :=
                 Get_UR_Type_Element (Validator.Process_Contents);
            else
               Debug_Output ("Definition found for " & Local_Name);
            end if;

         when Process_Skip =>
            Element_Validator :=
              Get_UR_Type_Element (Validator.Process_Contents);
      end case;
   end Validate_Start_Element;

   -------------------------
   -- Validate_Attributes --
   -------------------------

   procedure Validate_Attributes
     (Validator         : access UR_Type_Validator;
      Atts              : Sax.Attributes.Attributes'Class;
      Id_Table          : in out Id_Htable_Access;
      Nillable          : Boolean;
      Is_Nil            : out Boolean)
   is
      pragma Unreferenced (Validator, Atts, Id_Table, Nillable);
   begin
      Is_Nil := False;
   end Validate_Attributes;

   --------------------------
   -- Validate_End_Element --
   --------------------------

   procedure Validate_End_Element
     (Validator      : access UR_Type_Validator;
      Local_Name     : Unicode.CES.Byte_Sequence;
      Data           : Validator_Data)
   is
      pragma Unreferenced (Validator, Local_Name, Data);
   begin
      null;
   end Validate_End_Element;

   -------------------------
   -- Get_UR_Type_Element --
   -------------------------

   function Get_UR_Type_Element
     (Process_Contents : Process_Contents_Type)
         return XML_Element
   is
      Validator : UR_Type_Access;
   begin
      if UR_Type_Element (UR_Type_Element'First) = No_Element then
         for P in Process_Contents_Type loop
            Validator := new UR_Type_Validator;
            Validator.Process_Contents := P;
            UR_Type_Element (P)  := Create_Element
              ("", Create_Type ("ur-Type", Validator), Qualified);
         end loop;
      end if;

      return UR_Type_Element (Process_Contents);
   end Get_UR_Type_Element;

end Schema.Validators.UR_Type;
