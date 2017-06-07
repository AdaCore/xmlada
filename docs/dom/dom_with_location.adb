--
--  Copyright (C) 2017, AdaCore
--

with DOM.Core;            use DOM.Core;
with DOM.Core.Attrs;      use DOM.Core.Attrs;
with DOM.Core.Documents;  use DOM.Core.Documents;
with DOM.Core.Elements;   use DOM.Core.Elements;
with Sax.Locators;        use Sax.Locators;

package body DOM_With_Location is

   overriding procedure Start_Element
      (Handler     : in out Tree_Reader_With_Location;
       NS          : Sax.Utils.XML_NS;
       Local_Name  : Sax.Symbols.Symbol;
       Atts        : Sax_Attribute_List)
   is
      Att, Att2 : Attr;
   begin
      --  First create the node as usual
      Start_Element (Tree_Reader (Handler), NS, Local_Name, Atts);

      --  Then add the new attribute
      Att := Create_Attribute_NS
         (Get_Tree (Handler),
          Namespace_URI  => "http://mydomain.com",
          Qualified_Name => "mydomain:location");
      Set_Value (Att, To_String (Current_Location (Handler)));

      Att2 := Set_Attribute_Node (Handler.Current_Node, Att);
   end Start_Element;

end DOM_With_Location;
