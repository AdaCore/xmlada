package body Schema.Validators.Extensions is

   type Extension_XML_Validator is new XML_Validator_Record with record
      Base      : XML_Type;
      Extension : XML_Validator;
   end record;
   type Extension_Type is access Extension_XML_Validator'Class;
   type Extension_Data is new Validator_Data_Record with record
      Validating_Base : Boolean := True;
      Base_Data       : Validator_Data;
      Extension_Data  : Validator_Data;
   end record;
   type Extension_Data_Access is access all Extension_Data'Class;

   procedure Free (Data : in out Extension_Data);
   function Create_Validator_Data
     (Validator : access Extension_XML_Validator) return Validator_Data;
   procedure Validate_Start_Element
     (Validator         : access Extension_XML_Validator;
      Local_Name        : Unicode.CES.Byte_Sequence;
      Namespace_URI     : Unicode.CES.Byte_Sequence;
      NS                : XML_Grammar_NS;
      Data              : Validator_Data;
      Schema_Target_NS  : XML_Grammar_NS;
      Element_Validator : out XML_Element);
   procedure Validate_End_Element
     (Validator      : access Extension_XML_Validator;
      Local_Name     : Unicode.CES.Byte_Sequence;
      Data           : Validator_Data);
   procedure Validate_Characters
     (Validator     : access Extension_XML_Validator;
      Ch            : Unicode.CES.Byte_Sequence;
      Empty_Element : Boolean);
   procedure Get_Attribute_Lists
     (Validator   : access Extension_XML_Validator;
      List        : out Attribute_Validator_List_Access;
      Dependency1 : out XML_Validator;
      Dependency2 : out XML_Validator);
   procedure Check_Replacement
     (Validator         : access Extension_XML_Validator;
      Typ               : XML_Type;
      Had_Restriction   : in out Boolean;
      Had_Extension     : in out Boolean);
   procedure Check_Content_Type
     (Validator        : access Extension_XML_Validator;
      Should_Be_Simple : Boolean);
   --  See doc from inherited subprograms

   -------------------------
   -- Get_Attribute_Lists --
   -------------------------

   procedure Get_Attribute_Lists
     (Validator   : access Extension_XML_Validator;
      List        : out Attribute_Validator_List_Access;
      Dependency1 : out XML_Validator;
      Dependency2 : out XML_Validator) is
   begin
      List := Validator.Attributes;
      Dependency1 := Validator.Extension;
      Dependency2 := Validator.Base.Validator;
   end Get_Attribute_Lists;

   ----------
   -- Free --
   ----------

   procedure Free (Data : in out Extension_Data) is
   begin
      Free (Data.Base_Data);
      Free (Data.Extension_Data);
   end Free;

   ---------------------------
   -- Create_Validator_Data --
   ---------------------------

   function Create_Validator_Data
     (Validator : access Extension_XML_Validator) return Validator_Data
   is
      D : constant Extension_Data_Access := new Extension_Data;
   begin
      if Validator.Extension /= null then
         D.Extension_Data := Create_Validator_Data (Validator.Extension);
      end if;
      D.Base_Data   := Create_Validator_Data (Get_Validator (Validator.Base));
      return Validator_Data (D);
   end Create_Validator_Data;

   ----------------------------
   -- Validate_Start_Element --
   ----------------------------

   procedure Validate_Start_Element
     (Validator         : access Extension_XML_Validator;
      Local_Name        : Unicode.CES.Byte_Sequence;
      Namespace_URI     : Unicode.CES.Byte_Sequence;
      NS                : XML_Grammar_NS;
      Data              : Validator_Data;
      Schema_Target_NS  : XML_Grammar_NS;
      Element_Validator : out XML_Element)
   is
      D : constant Extension_Data_Access := Extension_Data_Access (Data);
   begin
      Debug_Output
        ("Validate_Start_Element for extension " & Get_Name (Validator));

      Element_Validator := No_Element;

      --  If we have a sequence with optional elements, it is possible that
      --  none of these matched, but this isn't an error. In this case, we keep
      --  looking in the base type

      if D.Validating_Base then
         begin
            Debug_Output ("Validating base part of the extension");
            Validate_Start_Element
              (Get_Validator (Validator.Base), Local_Name, Namespace_URI, NS,
               D.Base_Data, Schema_Target_NS, Element_Validator);
         exception
            when XML_Validation_Error =>
               Debug_Output ("Validation error in base, testing extension");
               Element_Validator := No_Element;
         end;
      end if;

      if Element_Validator = No_Element then
         D.Validating_Base := False;
         if Validator.Extension /= null then
            Debug_Output ("Validating extension part of the extension");
            Validate_Start_Element
              (Validator.Extension, Local_Name, Namespace_URI, NS,
               D.Extension_Data, Schema_Target_NS, Element_Validator);
         end if;
      end if;
   end Validate_Start_Element;

   --------------------------
   -- Validate_End_Element --
   --------------------------

   procedure Validate_End_Element
     (Validator      : access Extension_XML_Validator;
      Local_Name     : Unicode.CES.Byte_Sequence;
      Data           : Validator_Data)
   is
      D : constant Extension_Data_Access := Extension_Data_Access (Data);
   begin
      Debug_Push_Prefix ("Validate_End_Element <extension> "
                         & Get_Name (Validator));
      if D.Validating_Base then
         Validate_End_Element
           (Get_Validator (Validator.Base), Local_Name, D.Base_Data);
      end if;

      if Validator.Extension /= null then
         Validate_End_Element
           (Validator.Extension, Local_Name, D.Extension_Data);
      end if;

      Debug_Pop_Prefix;
   end Validate_End_Element;

   -------------------------
   -- Validate_Characters --
   -------------------------

   procedure Validate_Characters
     (Validator     : access Extension_XML_Validator;
      Ch            : Unicode.CES.Byte_Sequence;
      Empty_Element : Boolean) is
   begin
      Debug_Output ("Validate_Characters for extension "
                    & Get_Name (Validator));

      if Validator.Extension /= null then
         Validate_Characters (Validator.Extension, Ch, Empty_Element);
      else
         Validate_Characters
           (Get_Validator (Validator.Base), Ch, Empty_Element);
      end if;

   exception
      when XML_Validation_Error =>
         Debug_Output ("Validation error in extension, testing base");
         Validate_Characters
           (Get_Validator (Validator.Base), Ch, Empty_Element);
   end Validate_Characters;

   -----------------------
   -- Check_Replacement --
   -----------------------

   procedure Check_Replacement
     (Validator         : access Extension_XML_Validator;
      Typ               : XML_Type;
      Had_Restriction   : in out Boolean;
      Had_Extension     : in out Boolean) is
   begin
      Had_Extension := True;

      if Validator.Base.Block_Restriction and then Had_Restriction then
         Validation_Error
           ("Restrictions of type """
            & Get_Local_Name (Validator.Base) & """ are forbidden");
      end if;

      if Validator.Base.Block_Extension and then Had_Extension then
         Validation_Error
           ("Extensions of type """
            & Get_Local_Name (Validator.Base) & """ are forbidden");
      end if;

      if Validator.Base /= Typ then
         Check_Replacement
           (Get_Validator (Validator.Base), Typ,
            Had_Restriction => Had_Restriction,
            Had_Extension   => Had_Extension);
      end if;
   end Check_Replacement;

   ------------------------
   -- Check_Content_Type --
   ------------------------

   procedure Check_Content_Type
     (Validator : access Extension_XML_Validator;
      Should_Be_Simple : Boolean) is
   begin
      Check_Content_Type (Validator.Base, Should_Be_Simple);
   end Check_Content_Type;

   -------------------------
   -- Create_Extension_Of --
   -------------------------

   function Create_Extension_Of
     (Base      : XML_Type;
      Extension : XML_Validator := null) return XML_Validator
   is
      Result : constant Extension_Type := new Extension_XML_Validator;
   begin
      Result.Base      := Base;
      Result.Extension := Extension;
      return XML_Validator (Result);
   end Create_Extension_Of;

   -------------------------
   -- Create_Extension_Of --
   -------------------------

   function Create_Extension_Of
     (Base       : XML_Type;
      Group      : XML_Group;
      Min_Occurs : Natural := 1;
      Max_Occurs : Integer := 1) return XML_Validator
   is
      Result : constant Extension_Type := new Extension_XML_Validator;
      C      : Sequence;
   begin
      Result.Base      := Base;
      C := Create_Sequence;
      Set_Debug_Name (C, "automatic_extension_sequence");
      Add_Particle (C, Group, Min_Occurs, Max_Occurs);
      Result.Extension := XML_Validator (C);
      return XML_Validator (Result);
   end Create_Extension_Of;

end Schema.Validators.Extensions;
