with Schema.Validators.Facets; use Schema.Validators.Facets;

package body Schema.Validators.Restrictions is

   type Restriction_XML_Validator is new XML_Validator_Record with record
      Base              : XML_Type;
      Restriction       : XML_Validator;
      Facets            : Facets_Value;
   end record;
   type Restriction_Type is access Restriction_XML_Validator'Class;
   type Restriction_Data is new Validator_Data_Record with record
      Restriction_Data : Validator_Data;
   end record;
   type Restriction_Data_Access is access all Restriction_Data'Class;

   procedure Free (Data : in out Restriction_Data);
   function Create_Validator_Data
     (Validator : access Restriction_XML_Validator) return Validator_Data;
   procedure Validate_Start_Element
     (Validator         : access Restriction_XML_Validator;
      Local_Name        : Unicode.CES.Byte_Sequence;
      Namespace_URI     : Unicode.CES.Byte_Sequence;
      Data              : Validator_Data;
      Grammar           : in out XML_Grammar;
      Element_Validator : out XML_Element);
   procedure Validate_Characters
     (Validator     : access Restriction_XML_Validator;
      Ch            : Unicode.CES.Byte_Sequence;
      Empty_Element : Boolean);
   procedure Get_Attribute_Lists
     (Validator   : access Restriction_XML_Validator;
      List        : out Attribute_Validator_List_Access;
      Dependency1 : out XML_Validator;
      Dependency2 : out XML_Validator);
   procedure Add_Facet
     (Validator   : access Restriction_XML_Validator;
      Facet_Name  : Unicode.CES.Byte_Sequence;
      Facet_Value : Unicode.CES.Byte_Sequence);
   procedure Check_Replacement
     (Validator         : access Restriction_XML_Validator;
      Typ               : XML_Type;
      Had_Restriction   : in out Boolean;
      Had_Extension     : in out Boolean);
   procedure Check_Content_Type
     (Validator        : access Restriction_XML_Validator;
      Should_Be_Simple : Boolean);
   --  See doc from inherited subprograms

   -------------------------
   -- Get_Attribute_Lists --
   -------------------------

   procedure Get_Attribute_Lists
     (Validator   : access Restriction_XML_Validator;
      List        : out Attribute_Validator_List_Access;
      Dependency1 : out XML_Validator;
      Dependency2 : out XML_Validator) is
   begin
      List := Validator.Attributes;
      Dependency1 := Validator.Restriction;
      Dependency2 := Validator.Base.Validator;
   end Get_Attribute_Lists;

   ----------------------------
   -- Validate_Start_Element --
   ----------------------------

   procedure Validate_Start_Element
     (Validator         : access Restriction_XML_Validator;
      Local_Name        : Unicode.CES.Byte_Sequence;
      Namespace_URI     : Unicode.CES.Byte_Sequence;
      Data              : Validator_Data;
      Grammar           : in out XML_Grammar;
      Element_Validator : out XML_Element)
   is
      D : constant Restriction_Data_Access := Restriction_Data_Access (Data);
   begin
      if Validator.Restriction /= null then
         Validate_Start_Element
           (Validator.Restriction, Local_Name, Namespace_URI,
            D.Restriction_Data, Grammar, Element_Validator);

         if Element_Validator /= No_Element then
            Debug_Output ("Validate_Start_Element: end of restriction, result="
                          & Element_Validator.Elem.Local_Name.all);
         else
            Debug_Output ("Validate_Start_Element: end of restriction, no"
                          & " match from restriction");
         end if;
      else
         Validate_Start_Element
           (Get_Validator (Validator.Base), Local_Name, Namespace_URI,
            D.Restriction_Data, Grammar, Element_Validator);
      end if;
   end Validate_Start_Element;

   ---------------
   -- Add_Facet --
   ---------------

   procedure Add_Facet
     (Validator   : access Restriction_XML_Validator;
      Facet_Name  : Unicode.CES.Byte_Sequence;
      Facet_Value : Unicode.CES.Byte_Sequence) is
   begin
      --  We won't allow to set a facet that the ancestor doesn't know.
      --  ??? Problem: The ancestor might not be known yet at that point...
      if Validator.Restriction = null then
         Validator.Facets.Settable := (others => True);
--           Validator.Facets.Settable := Common_Simple_XML_Validator
--             (Validator.Base.Validator.all).Facets.Settable;
      else
         Validator.Facets.Settable := (others => False);
      end if;
      Add_Facet (Validator.Facets, Facet_Name, Facet_Value);
   end Add_Facet;

   -------------------------
   -- Validate_Characters --
   -------------------------

   procedure Validate_Characters
     (Validator     : access Restriction_XML_Validator;
      Ch            : Unicode.CES.Byte_Sequence;
      Empty_Element : Boolean) is
   begin
      Debug_Output ("Validate_Characters for restriction --" & Ch & "--"
                    & Get_Name (Validator));

      Check_Facet (Validator.Facets, Ch);
      if Validator.Restriction /= null then
         Validate_Characters (Validator.Restriction, Ch, Empty_Element);
      end if;
   end Validate_Characters;

   ----------
   -- Free --
   ----------

   procedure Free (Data : in out Restriction_Data) is
   begin
      Free (Data.Restriction_Data);
   end Free;

   ---------------------------
   -- Create_Validator_Data --
   ---------------------------

   function Create_Validator_Data
     (Validator : access Restriction_XML_Validator) return Validator_Data
   is
      D : constant Restriction_Data_Access := new Restriction_Data;
   begin
      if Validator.Restriction /= null then
         D.Restriction_Data := Create_Validator_Data (Validator.Restriction);
      else
         D.Restriction_Data := Create_Validator_Data
           (Get_Validator (Validator.Base));
      end if;
      return Validator_Data (D);
   end Create_Validator_Data;

   -----------------------
   -- Check_Replacement --
   -----------------------

   procedure Check_Replacement
     (Validator         : access Restriction_XML_Validator;
      Typ               : XML_Type;
      Had_Restriction   : in out Boolean;
      Had_Extension     : in out Boolean) is
   begin
      Had_Restriction := True;

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

   --------------------
   -- Is_Simple_Type --
   --------------------

   procedure Check_Content_Type
     (Validator        : access Restriction_XML_Validator;
      Should_Be_Simple : Boolean) is
   begin
      Check_Content_Type (Validator.Base, Should_Be_Simple);
   end Check_Content_Type;

   ---------------------------
   -- Create_Restriction_Of --
   ---------------------------

   function Create_Restriction_Of
     (Base        : XML_Type;
      Restriction : XML_Validator := null) return XML_Validator
   is
      Result : constant Restriction_Type := new Restriction_XML_Validator;
   begin
      Result.Base        := Base;
      Result.Restriction := Restriction;
      return XML_Validator (Result);
   end Create_Restriction_Of;

end Schema.Validators.Restrictions;
