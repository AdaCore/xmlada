private package Schema.Validators.Extensions is

   function Create_Extension_Of
     (Base      : XML_Type;
      Extension : XML_Validator := null) return XML_Validator;
   function Create_Extension_Of
     (Base       : XML_Type;
      Group      : XML_Group;
      Min_Occurs : Natural := 1;
      Max_Occurs : Integer := 1) return XML_Validator;
   --  Create new extensions of Base, either through a type or a group.

end Schema.Validators.Extensions;
