with Schema.Validators.Facets; use Schema.Validators.Facets;

package body Schema.Validators.Simple_Types is

   -------------------------
   -- Validate_Characters --
   -------------------------

   procedure Validate_Characters
     (Validator     : access Common_Simple_XML_Validator;
      Ch            : Unicode.CES.Byte_Sequence;
      Empty_Element : Boolean)
   is
      pragma Unreferenced (Empty_Element);
   begin
      Debug_Output ("Validate_Character for common: --" & Ch & "--"
                    & Get_Name (Validator));

      Check_Facet (Validator.Facets, Ch);
   end Validate_Characters;

   -------------------------
   -- Validate_Characters --
   -------------------------

   procedure Validate_Characters
     (Validator     : access Boolean_Validator_Record;
      Ch            : Unicode.CES.Byte_Sequence;
      Empty_Element : Boolean) is
   begin
      Debug_Output ("Validate_Characters for boolean --" & Ch & "--"
                    & Get_Name (Validator));

      if Ch /= "true"
        and then Ch /= "false"
        and then Ch /= "0"
        and then Ch /= "1"
      then
         Validation_Error ("Invalid value for boolean type: """ & Ch & """");
      end if;
      Validate_Characters
        (Common_Simple_XML_Validator (Validator.all)'Access, Ch,
         Empty_Element);
   end Validate_Characters;

   ---------------
   -- Add_Facet --
   ---------------

   procedure Add_Facet
     (Validator   : access Common_Simple_XML_Validator;
      Facet_Name  : Unicode.CES.Byte_Sequence;
      Facet_Value : Unicode.CES.Byte_Sequence) is
   begin
      Add_Facet (Validator.Facets, Facet_Name, Facet_Value);
   end Add_Facet;

   ---------------
   -- Add_Facet --
   ---------------

   procedure Add_Facet
     (Validator   : access Boolean_Validator_Record;
      Facet_Name  : Unicode.CES.Byte_Sequence;
      Facet_Value : Unicode.CES.Byte_Sequence)
   is
   begin
      if Facet_Name /= "whiteSpace"
        and then Facet_Name /= "pattern"
      then
         Validation_Error
           ("Invalid restriction for boolean type: " & Facet_Name);
      else
         Add_Facet
           (Common_Simple_XML_Validator (Validator.all)'Access,
            Facet_Name, Facet_Value);
      end if;
   end Add_Facet;

   ----------
   -- Free --
   ----------

   procedure Free (Validator : in out Common_Simple_XML_Validator) is
   begin
      Free (Validator.Facets);
      Free (Any_Simple_XML_Validator_Record (Validator));
   end Free;

   --------------------
   -- Is_Simple_Type --
   --------------------

   function Is_Simple_Type
     (Validator : access Common_Simple_XML_Validator) return Boolean
   is
      pragma Unreferenced (Validator);
   begin
      return True;
   end Is_Simple_Type;

end Schema.Validators.Simple_Types;

