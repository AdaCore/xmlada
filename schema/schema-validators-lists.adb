with Schema.Validators.Facets; use Schema.Validators.Facets;

package body Schema.Validators.Lists is

   type List_Facets_Names is (Facet_Length,
                              Facet_Min_Length,
                              Facet_Max_Length);
   type List_Facets_Mask is array (List_Facets_Names) of Boolean;
   pragma Pack (List_Facets_Mask);

   type List_Facets_Description is new Facets_Description_Record with record
      Common     : Common_Facets_Description;
      Mask       : List_Facets_Mask := (others => False);
      Length     : Natural;
      Min_Length : Natural;
      Max_Length : Natural;
   end record;
   procedure Add_Facet
     (Facets      : in out List_Facets_Description;
      Facet_Name  : Unicode.CES.Byte_Sequence;
      Facet_Value : Unicode.CES.Byte_Sequence;
      Applied     : out Boolean);
   procedure Check_Facet
     (Facets : in out List_Facets_Description;
      Value  : Unicode.CES.Byte_Sequence);
   procedure Free (Facets : in out List_Facets_Description);

   type List_Validator_Record is new Any_Simple_XML_Validator_Record with
      record
         Facets : List_Facets_Description;
         Base   : XML_Type;
      end record;
   type List_Validator is access all List_Validator_Record'Class;
   procedure Validate_Characters
     (Validator     : access List_Validator_Record;
      Ch            : Unicode.CES.Byte_Sequence;
      Empty_Element : Boolean);
   function Get_Facets_Description
     (Validator : access List_Validator_Record) return Facets_Description;
   procedure Free (Validator : in out List_Validator_Record);
   --  See doc from inherited subprogram

   ---------------
   -- Add_Facet --
   ---------------

   procedure Add_Facet
     (Facets      : in out List_Facets_Description;
      Facet_Name  : Unicode.CES.Byte_Sequence;
      Facet_Value : Unicode.CES.Byte_Sequence;
      Applied     : out Boolean)
   is
   begin
      Add_Facet (Facets.Common, Facet_Name, Facet_Value, Applied);
      if Applied then
         null;
      elsif Facet_Name = "length" then
         Facets.Mask (Facet_Length) := True;
         Facets.Length := Natural'Value (Facet_Value);
         Applied := True;
      elsif Facet_Name = "minLength" then
         Facets.Mask (Facet_Min_Length) := True;
         Facets.Min_Length := Natural'Value (Facet_Value);
         Applied := True;
      elsif Facet_Name = "maxLength" then
         Facets.Mask (Facet_Max_Length) := True;
         Facets.Max_Length := Natural'Value (Facet_Value);
         Applied := True;
      else
         Applied := False;
      end if;
   end Add_Facet;

   -----------------
   -- Check_Facet --
   -----------------

   procedure Check_Facet
     (Facets : in out List_Facets_Description;
      Value  : Unicode.CES.Byte_Sequence)
   is
      Length : Natural := 1;
   begin
      Check_Facet (Facets.Common, Value);

      --  Ch has already been normalized by the SAX parser
      for C in Value'Range loop
         if Value (C) = ' ' then
            Length := Length + 1;
         end if;
      end loop;

      if Facets.Mask (Facet_Length)
        and then Length /= Facets.Length
      then
         Validation_Error ("Invalid number of elements in list, expecting"
                           & Integer'Image (Facets.Length));
      end if;

      if Facets.Mask (Facet_Min_Length)
        and then Length < Facets.Min_Length
      then
         Validation_Error
           ("Invalid number of elements in list, expecting at least"
            & Integer'Image (Facets.Min_Length));
      end if;

      if Facets.Mask (Facet_Max_Length)
        and then Length > Facets.Max_Length
      then
         Validation_Error
           ("Invalid number of elements in list, expecting at most"
            & Integer'Image (Facets.Max_Length));
      end if;
   end Check_Facet;

   ----------
   -- Free --
   ----------

   procedure Free (Facets : in out List_Facets_Description) is
   begin
      Free (Facets.Common);
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Validator : in out List_Validator_Record) is
   begin
      Free (Validator.Facets);
   end Free;

   ----------------------------
   -- Get_Facets_Description --
   ----------------------------

   function Get_Facets_Description
     (Validator : access List_Validator_Record) return Facets_Description
   is
      pragma Unreferenced (Validator);
   begin
      return new List_Facets_Description;
   end Get_Facets_Description;

   -------------
   -- List_Of --
   -------------

   function List_Of (Typ : XML_Type) return XML_Validator is
      Validator : List_Validator;
   begin
      Validator := new List_Validator_Record;
      Validator.Base := Typ;
      return XML_Validator (Validator);
   end List_Of;

   -------------------------
   -- Validate_Characters --
   -------------------------

   procedure Validate_Characters
     (Validator     : access List_Validator_Record;
      Ch            : Unicode.CES.Byte_Sequence;
      Empty_Element : Boolean)
   is
      Start : Integer := Ch'First;
   begin
      Debug_Output ("Validate_Characters for list --" & Ch & "--"
                    & Get_Name (Validator));

      Check_Facet (Validator.Facets, Ch);

      --  Ch has already been normalized by the SAX parser
      for C in Ch'Range loop
         if Ch (C) = ' ' then
            if C /= Ch'First then
               Validate_Characters
                 (Get_Validator (Validator.Base), Ch (Start .. C - 1),
                  Empty_Element);
            end if;
            Start := C + 1;
         end if;
      end loop;

      if Start <= Ch'Last then
         Validate_Characters
           (Get_Validator (Validator.Base), Ch (Start .. Ch'Last),
            Empty_Element);
      end if;
   end Validate_Characters;

end Schema.Validators.Lists;
