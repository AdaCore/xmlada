--
--  Copyright (C) 2017, AdaCore
--

with Input_Sources.Strings; use Input_Sources.Strings;
with Unicode.CES;           use Unicode.CES;
with Unicode.CES.Utf8;

with Sax.Readers;        use Sax.Readers;
with DOM.Readers;        use DOM.Readers;
with DOM.Core;           use DOM.Core;
with DOM.Core.Documents; use DOM.Core.Documents;
with DOM.Core.Nodes;     use DOM.Core.Nodes;
with DOM.Core.Attrs;     use DOM.Core.Attrs;
with GNAT.IO;            use GNAT.IO;

procedure DomExample is
   Input  : String_Input;
   Reader : Tree_Reader;
   Doc    : Document;
   List   : Node_List;
   N      : Node;
   A      : Attr;

   XML_Source_String : constant Unicode.CES.Byte_Sequence :=
     "<?xml version=""1.0"" ?>"
     & "<preferences>"
     & "<pref name=""pref1"">Value1</pref>"
     & "<pref name=""pref2"">Value2</pref>"
     & "</preferences> ";
begin
   Set_Public_Id (Input, "Preferences file");
   Open (XML_Source_String, Unicode.CES.Utf8.Utf8_Encoding, Input);

   Set_Feature (Reader, Validation_Feature, False);
   Set_Feature (Reader, Namespace_Feature, False);

   Parse (Reader, Input);
   Close (Input);

   Doc := Get_Tree (Reader);

   List := Get_Elements_By_Tag_Name (Doc, "pref");

   for Index in 1 .. Length (List) loop
      N := Item (List, Index - 1);
      A := Get_Named_Item (Attributes (N), "name");
      Put_Line ("Value of """ & Value (A) & """ is "
                & Node_Value (First_Child (N)));
   end loop;

   Free (List);

   Free (Reader);
end DomExample;
