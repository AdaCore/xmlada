--
--  Copyright (C) 2017, AdaCore
--

with DOM.Readers;       use DOM.Readers;
with Sax.Utils;         use Sax.Utils;
with Sax.Readers;       use Sax.Readers;
with Sax.Symbols;       use Sax.Symbols;

package DOM_With_Location is

   type Tree_Reader_With_Location is new Tree_Reader with null record;
   overriding procedure Start_Element
      (Handler     : in out Tree_Reader_With_Location;
       NS          : Sax.Utils.XML_NS;
       Local_Name  : Sax.Symbols.Symbol;
       Atts        : Sax.Readers.Sax_Attribute_List);

end DOM_With_Location;
