private package Schema.Validators.Restrictions is

   function Create_Restriction_Of
     (Base        : XML_Type;
      Restriction : XML_Validator := null) return XML_Validator;
   --  Create new Restrictions of Base, either through a type or a group.

end Schema.Validators.Restrictions;
