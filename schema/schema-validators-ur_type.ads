private package Schema.Validators.UR_Type is

   procedure Create_UR_Type_Elements
     (Schema_NS : Schema.Validators.XML_Grammar_NS);
   --  Create all the ur-Type elements

   function Get_UR_Type_Element
     (Process_Contents : Process_Contents_Type) return XML_Element;
   --  Return an element of type ur-Type, which will match its contents with
   --  a given policy

end Schema.Validators.UR_Type;
