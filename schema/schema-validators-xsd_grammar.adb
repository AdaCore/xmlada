-----------------------------------------------------------------------
--                XML/Ada - An XML suite for Ada95                   --
--                                                                   --
--                       Copyright (C) 2004-2010, AdaCore            --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

with Unicode.CES;                    use Unicode.CES;
with Schema.Validators;              use Schema.Validators;
with Schema.Validators.Simple_Types; use Schema.Validators.Simple_Types;
with Schema.Validators.UR_Type;      use Schema.Validators.UR_Type;

package body Schema.Validators.XSD_Grammar is

   procedure Add_Schema_For_Schema
     (R : access Schema.Validators.Abstract_Validation_Reader'Class)
   is
      G, XML_G, XML_IG : XML_Grammar_NS;
      Tmp2          : XML_Validator;
      Typ, Typ2     : XML_Validator;
      Seq1, Seq2    : Sequence;
      Choice1       : Choice;
      All_Validator : XML_Type;
      Elem          : XML_Element;
      Gr            : XML_Group;
      Union, Union2 : XML_Validator;
      Attr          : XML_Attribute_Group;
   begin
      Get_NS (R.Grammar, XML_Schema_URI,   Result => G);
      Get_NS (R.Grammar, XML_URI,          Result => XML_G);
      Get_NS (R.Grammar, XML_Instance_URI, Result => XML_IG);

      Create_UR_Type_Elements (G, R.Grammar);

      --  As per 3.4.7, ur-Type (ie anyType) uses a Lax processing for its
      --  children node (ie uses the grammar definition if one is found)
      Create_Global_Type
        (G, R, "ur-Type",
         Get_Validator
           (Get_Type (Get_UR_Type_Element (R.Grammar, Process_Lax))));

      Create_Global_Type
        (G, R, "anyType",
         Get_Validator
           (Get_Type (Get_UR_Type_Element (R.Grammar, Process_Lax))));

      Tmp2 := new Any_Simple_XML_Validator_Record;
      Create_Global_Type (G, R, "anySimpleType", Tmp2);

      Schema.Validators.Simple_Types.Register_Predefined_Types (G, XML_G, R);

      --  The "formChoice" type of schema.xsd
      Typ := Restriction_Of (G, R, Lookup (G, R, "NMTOKEN"));
      Add_Facet (Typ, R, "enumeration", "qualified");
      Add_Facet (Typ, R, "enumeration", "unqualified");
      Create_Global_Type (G, R, "formChoice", Typ);

      --  The "derivationControl" type
      Typ := Restriction_Of (G, R, Lookup (G, R, "NMTOKEN"));
      Add_Facet (Typ, R, "enumeration", "substitution");
      Add_Facet (Typ, R, "enumeration", "extension");
      Add_Facet (Typ, R, "enumeration", "restriction");
      Create_Global_Type (G, R, "derivationControl", Typ);

      --  The "blockSet" type
      Typ := Restriction_Of (G, R, Lookup (G, R, "token"));
      Add_Facet (Typ, R, "enumeration", "#all");
      All_Validator := Create_Local_Type (G, Typ);

      Union := Create_Union (G);
      Add_Union (Union, R, All_Validator);
      Add_Union (Union, R, List_Of (G, Lookup (G, R, "derivationControl")));
      Create_Global_Type (G, R, "blockSet", Union);

      --  The "reducedDerivationControl" type
      Typ := Restriction_Of (G, R, Lookup (G, R, "derivationControl"));
      Add_Facet (Typ, R, "enumeration", "extension");
      Add_Facet (Typ, R, "enumeration", "restriction");
      Create_Global_Type (G, R, "reducedDerivationControl", Typ);

      --  The "derivationSet" type
      Union := Create_Union (G);
      Add_Union (Union, R, All_Validator);
      Add_Union
        (Union, R, List_Of (G, Lookup (G, R, "reducedDerivationControl")));
      Create_Global_Type (G, R, "derivationSet", Union);

      --  The "openAttrs" type
      Typ := Restriction_Of (G, R, Lookup (G, R, "anyType"));
      Add_Attribute
        (Typ, Create_Any_Attribute (G, Process_Lax, Kind => Namespace_Other));
      Create_Global_Type (G, R, "openAttrs", Typ);

      --  The "annotated" type
      Seq1 := Create_Sequence (G);
      Set_Debug_Name (Seq1, "annotated_seq");
      Add_Particle
        (Seq1, R, Lookup_Element (G, R, "annotation"), Min_Occurs => 0);
      Typ := Extension_Of
        (G, Lookup (G, R, "openAttrs"), XML_Validator (Seq1));
      Add_Attribute
        (Typ, Create_Local_Attribute ("id", G, Lookup (G, R, "ID")));
      Create_Global_Type (G, R, "annotated", Typ);

      --  The "schemaTop" element  ??? Missing abstract
      Set_Type (Create_Global_Element (G, R, "schemaTop", Qualified), R,
                Lookup (G, R, "annotated"));

      --  The "include" element
      Typ := Restriction_Of (G, R, Lookup (G, R, "annotated"));
      Add_Attribute
        (Typ, Create_Local_Attribute ("schemaLocation", G,
                                      Lookup (G, R, "uriReference"),
                                      Attribute_Use => Required));
      Set_Type (Create_Global_Element (G, R, "include", Qualified), R,
                Create_Local_Type (G, Typ));

      --  The "import" element
      Typ := Restriction_Of (G, R, Lookup (G, R, "annotated"));
      Add_Attribute
        (Typ, Create_Local_Attribute ("namespace", G,
                                      Lookup (G, R, "uriReference")));
      Add_Attribute
        (Typ, Create_Local_Attribute ("schemaLocation", G,
                                Lookup (G, R, "uriReference")));
      Set_Type (Create_Global_Element (G, R, "import", Qualified), R,
                Create_Local_Type (G, Typ));

      --  The "schema" element
      Choice1 := Create_Choice (G);
      Set_Debug_Name (Choice1, "schema_choice1");
      Add_Particle (Choice1, R, Lookup_Element (G, R, "include"));
      Add_Particle (Choice1, R, Lookup_Element (G, R, "import"));
      Add_Particle (Choice1, R, Lookup_Element (G, R, "redefine"));
      Add_Particle (Choice1, R, Lookup_Element (G, R, "annotation"));
      Seq1    := Create_Sequence (G);
      Set_Debug_Name (Seq1, "schema_seq1");
      Add_Particle (Seq1, R, Lookup_Element (G, R, "schemaTop"));
      Add_Particle (Seq1, R, Lookup_Element (G, R, "annotation"),
                    Min_Occurs => 0, Max_Occurs => Unbounded);
      Seq2    := Create_Sequence (G);
      Set_Debug_Name (Seq2, "schema_seq2");
      Add_Particle
        (Seq2, R, Choice1, Min_Occurs => 0, Max_Occurs => Unbounded);
      Add_Particle
        (Seq2, R, Seq1, Min_Occurs => 0, Max_Occurs => Unbounded);
      Add_Attribute
        (Seq2, Create_Local_Attribute
           ("targetNamespace", G, Lookup (G, R, "uriReference")));
      Add_Attribute
        (Seq2,
         Create_Local_Attribute ("version", G, Lookup (G, R, "token")));
      Add_Attribute
        (Seq2, Create_Local_Attribute
           ("finalDefault", G, Lookup (G, R, "derivationSet"),
            Attribute_Use     => Default,
            Value             => ""));
      Add_Attribute
        (Seq2, Create_Local_Attribute
           ("blockDefault", G, Lookup (G, R, "blockSet"),
            Attribute_Use     => Default,
            Value             => ""));
      Add_Attribute
        (Seq2, Create_Local_Attribute
           ("attributeFormDefault", G, Lookup (G, R, "formChoice"),
            Attribute_Use  => Default,
            Value          => "unqualified"));
      Add_Attribute
        (Seq2, Create_Local_Attribute
           ("elementFormDefault", G, Lookup (G, R, "formChoice"),
            Attribute_Use     => Default,
            Value             => "unqualified"));
      Add_Attribute
        (Seq2, Create_Local_Attribute ("id", G, Lookup (G, R, "ID")));
      Add_Attribute (Seq2, Lookup_Attribute
         (XML_G, R, Local_Name => "lang"));
      Set_Type (Create_Global_Element (G, R, "schema", Qualified), R,
                Create_Local_Type (G, Seq2));

      --  The "localComplexType" type
      Seq1 := Create_Sequence (G);
      Set_Debug_Name (Seq1, "localComplexType_seq");
      Add_Particle
        (Seq1, R, Lookup_Element (G, R, "annotation"), Min_Occurs => 0);
      Add_Particle (Seq1, R, Lookup_Group (G, R, "complexTypeModel"));
      Typ := Restriction_Of
        (G, R, Lookup (G, R, "complexType"), XML_Validator (Seq1));
      Add_Attribute
        (Typ, Create_Local_Attribute ("name", G, Attribute_Use => Prohibited));
      Create_Global_Type (G, R, "localComplexType", Typ);

      --  The "keybase" type
      Seq1 := Create_Sequence (G);
      Set_Debug_Name (Seq1, "keybase_seq");
      Add_Particle (Seq1, R, Lookup_Element (G, R, "selector"));
      Add_Particle (Seq1, R, Lookup_Element (G, R, "field"),
                    Min_Occurs => 1, Max_Occurs => Unbounded);
      Typ := Extension_Of
        (G, Lookup (G, R, "annotated"), XML_Validator (Seq1));
      Add_Attribute
        (Typ, Create_Local_Attribute ("name", G, Lookup (G, R, "NCName"),
                                      Attribute_Use => Required));
      Create_Global_Type (G, R, "keybase", Typ);

      --  The "identityConstraint" element  ??? abstract=true
      Set_Type (Create_Global_Element (G, R, "identityConstraint", Qualified),
                R, Lookup (G, R, "keybase"));

      --  The "unique" element
      Elem := Create_Global_Element (G, R, "unique", Qualified);
      Set_Type
        (Elem, R, Get_Type (Lookup_Element (G, R, "identityConstraint")));
      Set_Substitution_Group
        (Elem, R, Lookup_Element (G, R, "identityConstraint"));

      --  The "keyref" element
      Typ := Extension_Of (G, Lookup (G, R, "keybase"));
      Add_Attribute (Typ, Create_Local_Attribute
                       ("refer", G, Lookup (G, R, "QName"),
                        Attribute_Use => Required));
      Elem := Create_Global_Element (G, R, "keyref", Qualified);
      Set_Type (Elem, R, Create_Local_Type (G, Typ));
      Set_Substitution_Group
        (Elem, R, Lookup_Element (G, R, "identityConstraint"));

      --  The "key" element
      Elem := Create_Global_Element (G, R, "key", Qualified);
      Set_Type (Elem,
                R, Get_Type (Lookup_Element (G, R, "identityConstraint")));
      Set_Substitution_Group
        (Elem, R, Lookup_Element (G, R, "identityConstraint"));

      --  The "XPathExprApprox" type  Incorrect pattern
      Typ := Restriction_Of (G, R, Lookup (G, R, "string"));
--    Add_Facet (Typ, "pattern", "(/|//|\.|\.\.|:|::|\||(\w-[.:/|])+)+");
      Create_Global_Type (G, R, "XPathExprApprox", Typ);

      --  The "XPathSpec" type"
      Typ := Restriction_Of (G, R, Lookup (G, R, "annotated"));
      Add_Attribute (Typ, Create_Local_Attribute ("xpath", G,
                                            Lookup (G, R, "XPathExprApprox")));
      Create_Global_Type (G, R, "XPathSpec", Typ);

      --  The "selector" element
      Set_Type (Create_Global_Element (G, R, "selector", Qualified), R,
                Lookup (G, R, "XPathSpec"));

      --  The "field" element
      Set_Type (Create_Global_Element (G, R, "field", Qualified), R,
                Lookup (G, R, "XPathSpec"));

      --  The "allNNI" type"
      Union := Create_Union (G);
      Add_Union (Union, R, Lookup (G, R, "nonNegativeInteger"));
      Typ := Restriction_Of (G, R, Lookup (G, R, "NMTOKEN"));
      Add_Facet (Typ, R, "enumeration", "unbounded");
      Add_Union (Union, R, Create_Local_Type (G, Typ));
      Create_Global_Type (G, R, "allNNI", Union);

      --  The "occurs" AttributeGroup
      Attr := Create_Global_Attribute_Group (G, R, "occurs");
      Add_Attribute
        (Attr,
         Create_Local_Attribute ("minOccurs", G,
           Lookup (G, R, "nonNegativeInteger"),
           Attribute_Use => Default, Value => "1"));
      Add_Attribute
        (Attr,
         Create_Local_Attribute ("maxOccurs", G,
           Lookup (G, R, "allNNI"),
           Attribute_Use => Default, Value => "1"));

      --  From AttributeGroup "defRef"
      Attr := Create_Global_Attribute_Group (G, R, "defRef");
      Add_Attribute (Attr, Create_Local_Attribute
                       ("name", G, Lookup (G, R, "NCName")));
      Add_Attribute (Attr, Create_Local_Attribute
                       ("ref", G, Lookup (G, R, "QName")));

      --  The "element" type   ??? abstract=true
      Seq1 := Create_Sequence (G);
      Set_Debug_Name (Seq1, "element_seq");
      Choice1 := Create_Choice (G);
      Set_Debug_Name (Choice1, "element_choice");
      Add_Particle (Choice1, R, Create_Local_Element
                      ("simpleType", G, Lookup (G, R, "localSimpleType"),
                       Qualified));
      Add_Particle (Choice1, R, Create_Local_Element
                      ("complexType", G, Lookup (G, R, "localComplexType"),
                       Qualified));
      Add_Particle (Seq1, R, Choice1, Min_Occurs => 0);
      Add_Particle (Seq1, R, Lookup_Element (G, R, "identityConstraint"),
                    Min_Occurs => 0, Max_Occurs => Unbounded);
      Typ := Extension_Of
        (G, Lookup (G, R, "annotated"), XML_Validator (Seq1));
      Set_Debug_Name (Typ, "element_ext");
      Create_Global_Type (G, R, "element", Typ);
      Add_Attribute_Group (Typ, R, Lookup_Attribute_Group (G, R, "occurs"));
      Add_Attribute_Group (Typ, R, Lookup_Attribute_Group (G, R, "defRef"));
      Add_Attribute
        (Typ,
         Create_Local_Attribute ("type", G, Lookup (G, R, "QName")));
      Add_Attribute
        (Typ, Create_Local_Attribute
           ("substitutionGroup", G, Lookup (G, R, "QName")));
      Add_Attribute
        (Typ, Create_Local_Attribute ("default", G, Lookup (G, R, "string")));
      Add_Attribute (Typ, Create_Local_Attribute
                       ("fixed", G, Lookup (G, R, "string")));
      Add_Attribute
        (Typ, Create_Local_Attribute ("nillable", G, Lookup (G, R, "boolean"),
                                Attribute_Use => Default, Value => "false"));
      Add_Attribute
        (Typ, Create_Local_Attribute ("abstract", G, Lookup (G, R, "boolean"),
                                Attribute_Use => Default, Value => "false"));
      Add_Attribute
        (Typ,
         Create_Local_Attribute ("final", G, Lookup (G, R, "derivationSet"),
           Attribute_Use => Default, Value => ""));
      Add_Attribute
        (Typ,
         Create_Local_Attribute ("block", G, Lookup (G, R, "blockSet"),
           Attribute_Use => Default, Value => ""));
      Add_Attribute
        (Typ,
         Create_Local_Attribute ("form", G, Lookup (G, R, "formChoice")));

      --  The "appinfo" element"
      Seq1 := Create_Sequence (G);
      Set_Debug_Name (Seq1, "appinfo_seq");
      Seq2 := Create_Sequence (G);
      Add_Particle (Seq1, R, Seq2, Min_Occurs => 0, Max_Occurs => Unbounded);
      Set_Debug_Name (Seq2, "appinfo_seq2");
      Add_Particle
        (Seq2, R,
         Create_Any (Process_Contents => Process_Lax,
                     Namespace        => "##any",
                     Target_NS        => XML_G));
      Add_Attribute
        (Seq1, Create_Local_Attribute
           ("source", G, Lookup (G, R, "uriReference")));
      Set_Mixed_Content (Seq1, True);
      Set_Type (Create_Global_Element (G, R, "appinfo", Qualified), R,
                Create_Local_Type (G, Seq1));

      --  The "documentation" element
      Seq1 := Create_Sequence (G);
      Seq2 := Create_Sequence (G);
      Add_Particle (Seq1, R, Seq2, Min_Occurs => 0, Max_Occurs => Unbounded);
      Set_Debug_Name (Seq1, "documentation_seq");
      Set_Debug_Name (Seq2, "documentation_seq2");
      Add_Particle
        (Seq2, R,
         Create_Any (Process_Contents => Process_Lax,
                     Namespace        => "##any",
                     Target_NS        => XML_G));
      Add_Attribute
        (Seq1, Create_Local_Attribute
           ("source", G, Lookup (G, R, "uriReference")));
      Add_Attribute (Seq1, Lookup_Attribute (XML_G, R, "lang"));
      Set_Mixed_Content (Seq1, True);
      Set_Type (Create_Global_Element (G, R, "documentation", Qualified), R,
                Create_Local_Type (G, Seq1));

      --  The "annotation" element  ??? invalid
      Seq1 := Create_Sequence (G);
      Choice1 := Create_Choice (G);
      Add_Particle
        (Seq1, R, Choice1, Min_Occurs => 0, Max_Occurs => Unbounded);
      Set_Debug_Name (Seq1, "annotation_seq");
      Set_Debug_Name (Choice1, "annotation_choice");
      Add_Particle (Choice1, R, Lookup_Element (G, R, "appinfo"));
      Add_Particle (Choice1, R, Lookup_Element (G, R, "documentation"));
      Typ := Extension_Of
        (G, Lookup (G, R, "openAttrs"), XML_Validator (Seq1));
      Add_Attribute
        (Typ, Create_Local_Attribute ("id", G, Lookup (G, R, "ID")));
      Set_Type (Create_Global_Element (G, R, "annotation", Qualified), R,
                Create_Local_Type (G, Typ));

      --  The "topLevelElement" type
      Seq1 := Create_Sequence (G);
      Set_Debug_Name (Seq1, "topLevelElement_seq");
      Add_Particle
        (Seq1, R, Lookup_Element (G, R, "annotation"), Min_Occurs => 0);
      Choice1 := Create_Choice (G);
      Set_Debug_Name (Choice1, "topLevelElement_choice");
      Add_Particle (Seq1, R, Choice1, Min_Occurs => 0);
      Add_Particle (Choice1, R, Create_Local_Element
                      ("simpleType", G, Lookup (G, R, "localSimpleType"),
                       Qualified));
      Add_Particle (Choice1, R, Create_Local_Element
                      ("complexType", G, Lookup (G, R, "localComplexType"),
                       Qualified));
      Add_Particle (Seq1, R, Lookup_Element (G, R, "identityConstraint"),
                    Min_Occurs => 0, Max_Occurs => Unbounded);
      Typ := Restriction_Of
        (G, R, Lookup (G, R, "element"), XML_Validator (Seq1));
      Set_Debug_Name (Typ, "topLevelElement restriction");
      Add_Attribute
        (Typ, Create_Local_Attribute ("ref", G, Attribute_Use => Prohibited));
      Add_Attribute
        (Typ, Create_Local_Attribute ("form", G, Attribute_Use => Prohibited));
      Add_Attribute
        (Typ, Create_Local_Attribute
           ("minOccurs", G, Attribute_Use => Prohibited));
      Add_Attribute
        (Typ, Create_Local_Attribute
           ("maxOccurs", G, Attribute_Use => Prohibited));
      Add_Attribute
        (Typ, Create_Local_Attribute ("name", G, Lookup (G, R, "NCName"),
                                Attribute_Use => Required));
      Create_Global_Type (G, R, "topLevelElement", Typ);

      --  The "element" element
      Elem := Create_Global_Element (G, R, "element", Qualified);
      Set_Type (Elem, R, Lookup (G, R, "topLevelElement"));
      Set_Substitution_Group (Elem, R, Lookup_Element (G, R, "schemaTop"));

      --  The "attribute" element
      Elem := Create_Global_Element (G, R, "attribute", Qualified);
      Set_Type (Elem, R, Lookup (G, R, "topLevelAttribute"));
      Set_Substitution_Group (Elem, R, Lookup_Element (G, R, "schemaTop"));

      --  The "redefinable" element  --  abstract=true
      Elem := Create_Global_Element (G, R, "redefinable", Qualified);
      Set_Type
        (Elem, R, Get_Type (Lookup_Element (G, R, "schemaTop")));
      Set_Substitution_Group (Elem, R, Lookup_Element (G, R, "schemaTop"));

      --  The "all" element
      Seq1 := Create_Sequence (G);
      Set_Debug_Name (Seq1, "all_seq");
      Add_Particle
        (Seq1, R, Lookup_Element (G, R, "annotation"), Min_Occurs => 0);

      Seq2 := Create_Sequence (G);
      Set_Debug_Name (Seq2, "all_seq2");
      Add_Particle
        (Seq2, R, Lookup_Element (G, R, "annotation"), Min_Occurs => 0);
      Choice1 := Create_Choice (G);
      Set_Debug_Name (Choice1, "all_choice");
      Add_Particle (Seq2, R, Choice1, Min_Occurs => 0);
      Add_Particle (Choice1, R, Create_Local_Element
                      ("simpleType", G, Lookup (G, R, "localSimpleType"),
                       Qualified));
      Add_Particle (Choice1, R, Create_Local_Element
                      ("complexType", G, Lookup (G, R, "localComplexType"),
                       Qualified));
      Add_Particle (Seq2, R, Lookup_Element (G, R, "identityConstraint"),
                    Min_Occurs => 0, Max_Occurs => Unbounded);
      Typ2 := Restriction_Of
        (G, R, Lookup (G, R, "localElement"), XML_Validator (Seq2));

      Typ := Restriction_Of (G, R, Lookup (G, R, "nonNegativeInteger"));
      Add_Facet (Typ, R, "enumeration", "0");
      Add_Facet (Typ, R, "enumeration", "1");
      Add_Attribute
        (Typ2, Create_Local_Attribute
           ("minOccurs", G, Create_Local_Type (G, Typ),
            Attribute_Use => Default, Value => "1"));

      Typ := Restriction_Of (G, R, Lookup (G, R, "allNNI"));
      Add_Facet (Typ, R, "enumeration", "0");
      Add_Facet (Typ, R, "enumeration", "1");
      Add_Attribute
        (Typ2, Create_Local_Attribute
           ("maxOccurs", G, Create_Local_Type (G, Typ),
            Attribute_Use => Default, Value => "1"));

      Add_Particle (Seq1, R,
                    Create_Local_Element
                      ("element", G, Create_Local_Type (G, Typ2), Qualified),
                    Min_Occurs => 0, Max_Occurs => Unbounded);

      Typ := Restriction_Of
        (G, R, Lookup (G, R, "explicitGroup"), XML_Validator (Seq1));

      Typ2 := Restriction_Of (G, R, Lookup (G, R, "nonNegativeInteger"));
      Add_Facet (Typ2, R, "enumeration", "1");
      Add_Attribute
        (Typ, Create_Local_Attribute
           ("minOccurs", G, Create_Local_Type (G, Typ2),
            Attribute_Use => Default, Value => "1"));

      Typ2 := Restriction_Of (G, R, Lookup (G, R, "allNNI"));
      Add_Facet (Typ2, R, "enumeration", "1");
      Add_Attribute
        (Typ, Create_Local_Attribute
           ("maxOccurs", G, Create_Local_Type (G, Typ2),
            Attribute_Use => Default, Value => "1"));

      Set_Type (Create_Global_Element (G, R, "all", Qualified), R,
                Create_Local_Type (G, Typ));

      --  The "localElement" type
      Seq1 := Create_Sequence (G);
      Set_Debug_Name (Seq1, "localElement_seq");
      Add_Particle
        (Seq1, R, Lookup_Element (G, R, "annotation"), Min_Occurs => 0);
      Choice1 := Create_Choice (G);
      Set_Debug_Name (Choice1, "localElement_choice");
      Add_Particle (Seq1, R, Choice1, Min_Occurs => 0);
      Add_Particle
        (Choice1, R, Create_Local_Element
           ("simpleType", G, Lookup (G, R, "localSimpleType"), Qualified));
      Add_Particle
        (Choice1, R, Create_Local_Element
           ("complexType", G, Lookup (G, R, "localComplexType"), Qualified));
      Add_Particle (Seq1, R, Lookup_Element (G, R, "identityConstraint"),
                    Min_Occurs => 0, Max_Occurs => Unbounded);
      Add_Attribute (Seq1, Create_Local_Attribute ("substitutionGroup", G,
                                             Attribute_Use => Prohibited));
      Add_Attribute (Seq1, Create_Local_Attribute ("final", G,
                                             Attribute_Use => Prohibited));
      Typ := Restriction_Of
        (G, R, Lookup (G, R, "element"), XML_Validator (Seq1));
      Create_Global_Type (G, R, "localElement", Typ);

      --  The "particle" group
      Gr := Create_Global_Group (G, R, "particle");
      Choice1 := Create_Choice (G);
      Set_Debug_Name (Choice1, "particle_choice");
      Add_Particle (Gr, R, Choice1);
      Add_Particle
        (Choice1, R, Create_Local_Element
           ("element", G, Lookup (G, R, "localElement"), Qualified));
      Add_Particle
        (Choice1, R, Create_Local_Element
           ("group", G, Lookup (G, R, "groupRef"), Qualified));
      Add_Particle (Choice1, R, Lookup_Element (G, R, "all"));
      Add_Particle (Choice1, R, Lookup_Element (G, R, "choice"));
      Add_Particle (Choice1, R, Lookup_Element (G, R, "sequence"));
      Add_Particle (Choice1, R, Lookup_Element (G, R, "any"));

      --  "group" type
      Typ := Extension_Of
        (G, R,
         Lookup (G, R, "annotated"),
         Lookup_Group (G, R, "particle"),
         Min_Occurs => 0, Max_Occurs => Unbounded);
      Set_Debug_Name (Typ, "group_ext");
      Create_Global_Type (G, R, "group", Typ);
      Add_Attribute_Group (Typ, R, Lookup_Attribute_Group (G, R, "defRef"));
      Add_Attribute_Group (Typ, R, Lookup_Attribute_Group (G, R, "occurs"));

      --  The "nestedParticle" element
      Gr := Create_Global_Group (G, R, "nestedParticle");
      Choice1 := Create_Choice (G);
      Set_Debug_Name (Choice1, "nestedParticle_choice");
      Add_Particle (Gr, R, Choice1);
      Add_Particle
        (Choice1, R, Create_Local_Element
           ("element", G, Lookup (G, R, "localElement"), Qualified));
      Add_Particle
        (Choice1, R, Create_Local_Element
           ("group", G, Lookup (G, R, "groupRef"), Qualified));
      Add_Particle (Choice1, R, Lookup_Element (G, R, "choice"));
      Add_Particle (Choice1, R, Lookup_Element (G, R, "sequence"));
      Add_Particle (Choice1, R, Lookup_Element (G, R, "any"));

      --  "explicitGroup" type
      Seq1 := Create_Sequence (G);
      Set_Debug_Name (Seq1, "explicitGroup_seq");
      Add_Particle (Seq1, R, Lookup_Element (G, R, "annotation"),
                    Min_Occurs => 0);
      Add_Particle (Seq1, R, Lookup_Group (G, R, "nestedParticle"),
                    Min_Occurs => 0, Max_Occurs => Unbounded);
      Typ := Restriction_Of
        (G, R, Lookup (G, R, "group"), XML_Validator (Seq1));
      Create_Global_Type (G, R, "explicitGroup", Typ);
      Add_Attribute
        (Typ, Create_Local_Attribute
           ("name", G, Lookup (G, R, "NCName"), Attribute_Use => Prohibited));
      Add_Attribute
        (Typ, Create_Local_Attribute
           ("ref", G, Lookup (G, R, "QName"), Attribute_Use => Prohibited));

      --  The "choice" element
      Set_Type (Create_Global_Element (G, R, "choice", Qualified), R,
                Lookup (G, R, "explicitGroup"));

      --  The "sequence" element
      Set_Type (Create_Global_Element (G, R, "sequence", Qualified), R,
                Lookup (G, R, "explicitGroup"));

      --  "groupDefParticle" group
      Gr := Create_Global_Group (G, R, "groupDefParticle");
      Choice1 := Create_Choice (G);
      Set_Debug_Name (Choice1, "groupDefParticle_choice");
      Add_Particle (Gr, R, Choice1);
      Add_Particle (Choice1, R, Lookup_Element (G, R, "all"));
      Add_Particle (Choice1, R, Lookup_Element (G, R, "choice"));
      Add_Particle (Choice1, R, Lookup_Element (G, R, "sequence"));

      --  The "realGroup" type
      Seq1 := Create_Sequence (G);
      Set_Debug_Name (Seq1, "realGroup_seq");
      Add_Particle
        (Seq1, R, Lookup_Element (G, R, "annotation"), Min_Occurs => 0);
      Add_Particle (Seq1, R, Lookup_Group (G, R, "groupDefParticle"),
                    Min_Occurs => 0, Max_Occurs => 1);
      Typ := Restriction_Of
        (G, R, Lookup (G, R, "group"), XML_Validator (Seq1));
      Create_Global_Type (G, R, "realGroup", Typ);

      --  The "groupRef" type
      Seq1 := Create_Sequence (G);
      Set_Debug_Name (Seq1, "groupRef_seq");
      Add_Particle
        (Seq1, R, Lookup_Element (G, R, "annotation"), Min_Occurs => 0);
      Typ := Restriction_Of
        (G, R, Lookup (G, R, "realGroup"), XML_Validator (Seq1));
      Create_Global_Type (G, R, "groupRef", Typ);
      Add_Attribute
        (Typ, Create_Local_Attribute
           ("ref", G, Lookup (G, R, "QName"), Attribute_Use => Required));
      Add_Attribute
        (Typ, Create_Local_Attribute ("name", G, Attribute_Use => Prohibited));

      --  The "group" element
      Elem := Create_Global_Element (G, R, "group", Qualified);
      Set_Type (Elem, R, Lookup (G, R, "namedGroup"));
      Set_Substitution_Group (Elem, R, Lookup_Element (G, R, "redefinable"));

      --  The "namedGroup" type
      Seq1 := Create_Sequence (G);
      Set_Debug_Name (Seq1, "namedGroup_seq");
      Add_Particle
        (Seq1, R, Lookup_Element (G, R, "annotation"), Min_Occurs => 0);
      Add_Particle (Seq1, R, Lookup_Group (G, R, "groupDefParticle"),
                    Min_Occurs => 1, Max_Occurs => 1);
      Typ := Restriction_Of
        (G, R, Lookup (G, R, "realGroup"), XML_Validator (Seq1));
      Add_Attribute (Typ, Create_Local_Attribute
                       ("name", G, Lookup (G, R, "NCName"),
                        Attribute_Use => Required));
      Add_Attribute (Typ, Create_Local_Attribute
                       ("ref", G, Attribute_Use => Prohibited));
      Add_Attribute (Typ, Create_Local_Attribute
                       ("minOccurs", G, Attribute_Use => Prohibited));
      Add_Attribute (Typ, Create_Local_Attribute
                       ("maxOccurs", G, Attribute_Use => Prohibited));
      Create_Global_Type (G, R, "namedGroup", Typ);

      --  The "attributeGroup" type
      Seq1 := Create_Sequence (G);
      Set_Debug_Name (Seq1, "attributeGroup_seq");
      Add_Particle (Seq1, R, Lookup_Group (G, R, "attrDecls"));
      Typ := Extension_Of
        (G, Lookup (G, R, "annotated"), XML_Validator (Seq1));
      Add_Attribute_Group (Typ, R, Lookup_Attribute_Group (G, R, "defRef"));
      Create_Global_Type (G, R, "attributeGroup", Typ);

      --  The "namedAttributeGroup" type
      Seq1 := Create_Sequence (G);
      Set_Debug_Name (Seq1, "namedAttributeGroup_seq");
      Add_Particle
        (Seq1, R, Lookup_Element (G, R, "annotation"), Min_Occurs => 0);
      Add_Particle (Seq1, R, Lookup_Group (G, R, "attrDecls"));
      Typ := Restriction_Of
        (G, R, Lookup (G, R, "attributeGroup"), XML_Validator (Seq1));
      Add_Attribute
        (Typ, Create_Local_Attribute ("name", G, Lookup (G, R, "NCName"),
                                Attribute_Use => Required));
      Add_Attribute
        (Typ, Create_Local_Attribute ("ref", G, Attribute_Use => Prohibited));
      Create_Global_Type (G, R, "namedAttributeGroup", Typ);

      --  The "attributeGroup" element
      Elem := Create_Global_Element (G, R, "attributeGroup", Qualified);
      Set_Type (Elem, R, Lookup (G, R, "namedAttributeGroup"));
      Set_Substitution_Group (Elem, R, Lookup_Element (G, R, "redefinable"));

      --  The "typeDefParticle" group
      Gr := Create_Global_Group (G, R, "typeDefParticle");
      Choice1 := Create_Choice (G);
      Set_Debug_Name (Choice1, "typeDefParticle_choice");
      Add_Particle (Gr, R, Choice1);
      Add_Particle (Choice1, R, Create_Local_Element
                      ("group", G, Lookup (G, R, "groupRef"), Qualified));
      Add_Particle (Choice1, R, Lookup_Element (G, R, "all"));
      Add_Particle (Choice1, R, Lookup_Element (G, R, "choice"));
      Add_Particle (Choice1, R, Lookup_Element (G, R, "sequence"));

      --  The "attribute" type
      Seq1 := Create_Sequence (G);
      Set_Debug_Name (Seq1, "attribute_seq");
      Add_Particle (Seq1, R, Create_Local_Element
                      ("simpleType", G,
                       Lookup (G, R, "localSimpleType"), Qualified),
                    Min_Occurs => 0);
      Typ := Extension_Of
        (G, Lookup (G, R, "annotated"), XML_Validator (Seq1));
      Set_Debug_Name (Typ, "attribute_ext");
      Create_Global_Type (G, R, "attribute", Typ);
      Add_Attribute
        (Typ,
         Create_Local_Attribute ("type", G, Lookup (G, R, "QName")));

      Typ2 := Restriction_Of (G, R, Lookup (G, R, "NMTOKEN"));
      Add_Facet (Typ2, R, "enumeration", "prohibited");
      Add_Facet (Typ2, R, "enumeration", "optional");
      Add_Facet (Typ2, R, "enumeration", "required");
      Add_Attribute_Group (Typ, R, Lookup_Attribute_Group (G, R, "defRef"));
      Add_Attribute (Typ, Create_Local_Attribute
                       ("use", G, Create_Local_Type (G, Typ2),
                        Attribute_Use => Default,
                        Value => "optional"));
      Add_Attribute (Typ, Create_Local_Attribute
                       ("default", G, Lookup (G, R, "string"),
                        Attribute_Use => Optional));
      Add_Attribute (Typ, Create_Local_Attribute
                       ("fixed", G, Lookup (G, R, "string"),
                        Attribute_Use => Optional));
      Add_Attribute
        (Typ,
         Create_Local_Attribute ("form", G, Lookup (G, R, "formChoice")));

      --  The "topLevelAttribute" type
      Seq1 := Create_Sequence (G);
      Set_Debug_Name (Seq1, "topLevelAttribute_seq");
      Add_Particle
        (Seq1, R, Lookup_Element (G, R, "annotation"), Min_Occurs => 0);
      Add_Particle (Seq1, R, Create_Local_Element
                      ("simpleType", G, Lookup (G, R, "localSimpleType"),
                       Qualified),
                    Min_Occurs => 0);
      Typ := Restriction_Of
        (G, R, Lookup (G, R, "attribute"), XML_Validator (Seq1));
      Set_Debug_Name (Typ, "topLevelAttribute restriction");
      Create_Global_Type (G, R, "topLevelAttribute", Typ);
      Add_Attribute
        (Typ, Create_Local_Attribute ("ref", G, Attribute_Use => Prohibited));
      Add_Attribute
        (Typ, Create_Local_Attribute ("form", G, Attribute_Use => Prohibited));
      Add_Attribute
        (Typ, Create_Local_Attribute ("use", G, Attribute_Use => Prohibited));
      Add_Attribute
        (Typ, Create_Local_Attribute ("name", G, Lookup (G, R, "NCName"),
                                      Attribute_Use => Required));

      --  The "anyAttributes" element
      Set_Type (Create_Global_Element (G, R, "anyAttribute", Qualified), R,
                Lookup (G, R, "wildcard"));

      --  The "namespaceList" type   ??? Incomplete
      Union := Create_Union (G);
      Typ := Restriction_Of (G, R, Lookup (G, R, "token"));
      Add_Facet (Typ, R, "enumeration", "##any");
      Add_Facet (Typ, R, "enumeration", "##other");
      Add_Union (Union, R, Create_Local_Type (G, Typ));

      Union2 := Create_Union (G);
      Add_Union (Union, R, Create_Local_Type (G, Union2));
      Add_Union (Union2, R, Lookup (G, R, "uriReference"));
      Typ := Restriction_Of (G, R, Lookup (G, R, "token"));
      Add_Facet (Typ, R, "enumeration", "##targetNamespace");
      Add_Facet (Typ, R, "enumeration", "##local");
      Add_Union (Union2, R, Create_Local_Type (G, Typ));

      Create_Global_Type (G, R, "namespaceList", Union);

      --  The "wildcard" type
      Typ := Extension_Of (G, Lookup (G, R, "annotated"));
      Set_Debug_Name (Typ, "annotated_restr_in_wildcard");
      Add_Attribute (Typ, Create_Local_Attribute ("namespace", G,
                                            Lookup (G, R, "namespaceList"),
                                            Attribute_Use => Default,
                                            Value => "##any"));
      Typ2 := Restriction_Of (G, R, Lookup (G, R, "NMTOKEN"));
      Add_Facet (Typ2, R, "enumeration", "skip");
      Add_Facet (Typ2, R, "enumeration", "lax");
      Add_Facet (Typ2, R, "enumeration", "strict");
      Add_Attribute (Typ, Create_Local_Attribute ("processContents", G,
                                            Create_Local_Type (G, Typ2),
                                            Attribute_Use => Default,
                                            Value => "strict"));
      Create_Global_Type (G, R, "wildcard", Typ);

      --  The "any" element   ??? Error if you put before "wildcard"
      Typ := Extension_Of (G, Lookup (G, R, "wildcard"));
      Set_Debug_Name (Typ, "wildcard_ext");
      Add_Attribute_Group (Typ, R, Lookup_Attribute_Group (G, R, "occurs"));
      Set_Type (Create_Global_Element (G, R, "any", Qualified), R,
                Create_Local_Type (G, Typ));

      --  The "attributeGroupRef"  ??? invalid
      Seq1 := Create_Sequence (G);
      Set_Debug_Name (Seq1, "attributeGroupRef_seq");
      Add_Particle
        (Seq1, R, Lookup_Element (G, R, "annotation"), Min_Occurs => 0);
      Typ := Restriction_Of
        (G, R, Lookup (G, R, "attributeGroup"), XML_Validator (Seq1));
      Add_Attribute
        (Typ, Create_Local_Attribute ("ref", G, Lookup (G, R, "QName"),
                                Attribute_Use => Required));
      Add_Attribute
        (Typ, Create_Local_Attribute ("name", G, Attribute_Use => Prohibited));
      Create_Global_Type (G, R, "attributeGroupRef", Typ);

      --  The "attrDecls" group
      Gr := Create_Global_Group (G, R, "attrDecls");
      Seq1 := Create_Sequence (G);
      Set_Debug_Name (Seq1, "attrDecls_seq");
      Add_Particle (Gr, R, Seq1);
      Choice1 := Create_Choice (G);
      Set_Debug_Name (Choice1, "attrDecls_choice");
      Add_Particle
        (Seq1, R, Choice1, Min_Occurs => 0, Max_Occurs => Unbounded);
      Add_Particle
        (Choice1, R, Create_Local_Element
           ("attribute", G, Lookup (G, R, "attribute"), Qualified));
      Add_Particle
        (Choice1, R, Create_Local_Element
           ("attributeGroup", G,
            Lookup (G, R, "attributeGroupRef"), Qualified));
      Add_Particle
        (Seq1, R, Lookup_Element (G, R, "anyAttribute"), Min_Occurs => 0);

      --  The "extensionType" type
      Seq1 := Create_Sequence (G);
      Set_Debug_Name (Seq1, "extensionType_seq");
      Add_Particle
        (Seq1, R, Lookup_Group (G, R, "typeDefParticle"), Min_Occurs => 0);
      Add_Particle
        (Seq1, R, Lookup_Group (G, R, "attrDecls"));
      Typ := Extension_Of
        (G, Lookup (G, R, "annotated"), XML_Validator (Seq1));
      Set_Debug_Name (Typ, "extensionType_ext");
      Add_Attribute (Typ, Create_Local_Attribute
                       ("base", G, Lookup (G, R, "QName")));
      Create_Global_Type (G, R, "extensionType", Typ);

      --  The "restrictionType" type
      Seq1 := Create_Sequence (G);
      Set_Debug_Name (Seq1, "restrictionType_seq");
      Choice1 := Create_Choice (G);
      Set_Debug_Name (Choice1, "restrictionType_choice");
      Add_Particle
        (Choice1, R, Lookup_Group (G, R, "typeDefParticle"), Min_Occurs => 0);
      Add_Particle
        (Choice1, R,
         Lookup_Group (G, R, "simpleRestrictionModel"), Min_Occurs => 0);
      Add_Particle (Seq1, R, Choice1);
      Add_Particle (Seq1, R, Lookup_Group (G, R, "attrDecls"));
      Typ := Extension_Of
        (G, Lookup (G, R, "annotated"), XML_Validator (Seq1));
      Add_Attribute (Typ, Create_Local_Attribute
                       ("base", G, Lookup (G, R, "QName"),
                        Attribute_Use => Required));
      Create_Global_Type (G, R, "restrictionType", Typ);

      --  The "simpleRestrictionModel" group
      Gr := Create_Global_Group (G, R, "simpleRestrictionModel");
      Seq1 := Create_Sequence (G);
      Set_Debug_Name (Seq1, "simpleRestrictionModel_seq");
      Add_Particle (Seq1, R, Create_Local_Element ("simpleType", G,
                                          Lookup (G, R, "localSimpleType"),
                                          Qualified),
                    Min_Occurs => 0);
      Add_Particle (Seq1, R, Lookup_Element (G, R, "facet"),
                    Min_Occurs => 0, Max_Occurs => Unbounded);
      Add_Particle (Gr, R, Seq1);

      --  The "simpleExtensionType"
      Seq1 := Create_Sequence (G);
      Set_Debug_Name (Seq1, "simpleExtensionType_seq");
      Add_Particle
        (Seq1, R, Lookup_Element (G, R, "annotation"), Min_Occurs => 0);
      Add_Particle (Seq1, R, Lookup_Group (G, R, "attrDecls"));
      Create_Global_Type (G, R, "simpleExtensionType",
                Restriction_Of (G, R, Lookup (G, R, "extensionType"),
                                XML_Validator (Seq1)));

      --  The "simpleRestrictionType"
      Seq1 := Create_Sequence (G);
      Set_Debug_Name (Seq1, "simpleRestrictionType_seq");
      Add_Particle
        (Seq1, R, Lookup_Element (G, R, "annotation"), Min_Occurs => 0);
      Add_Particle (Seq1, R, Lookup_Group (G, R, "simpleRestrictionModel"),
                    Min_Occurs => 0);
      Add_Particle (Seq1, R, Lookup_Group (G, R, "attrDecls"));
      Create_Global_Type (G, R, "simpleRestrictionType",
                Restriction_Of (G, R, Lookup (G, R, "restrictionType"),
                                XML_Validator (Seq1)));

      --  The "simpleContent" element
      Choice1 := Create_Choice (G);
      Set_Debug_Name (Choice1, "simpleContent_choice");
      Add_Particle (Choice1, R, Create_Local_Element
                      ("restriction", G,
                       Lookup (G, R, "simpleRestrictionType"),
                       Qualified));
      Add_Particle (Choice1, R, Create_Local_Element
                      ("extension", G,
                       Lookup (G, R, "simpleExtensionType"),
                       Qualified));
      Typ := Extension_Of
        (G, Lookup (G, R, "annotated"), XML_Validator (Choice1));
      Set_Debug_Name (Typ, "simpleContent_ext");
      Set_Type (Create_Global_Element (G, R, "simpleContent", Qualified), R,
                Create_Local_Type (G, Typ));

      --  The "complexRestrictionType" type
      Seq1 := Create_Sequence (G);
      Set_Debug_Name (Seq1, "complexRestrictionType_seq");
      Add_Particle
        (Seq1, R, Lookup_Element (G, R, "annotation"), Min_Occurs => 0);
      Add_Particle
        (Seq1, R, Lookup_Group (G, R, "typeDefParticle"), Min_Occurs => 0);
      Add_Particle (Seq1, R, Lookup_Group (G, R, "attrDecls"));
      Typ := Restriction_Of (G, R, Lookup (G, R, "restrictionType"),
                             XML_Validator (Seq1));
      Create_Global_Type (G, R, "complexRestrictionType", Typ);

      --  The "complexContent" element
      Choice1 := Create_Choice (G);
      Set_Debug_Name (Choice1, "complexContent_choice");
      Add_Particle
        (Choice1, R, Create_Local_Element
           ("restriction", G, Lookup (G, R, "complexRestrictionType"),
            Qualified));
      Add_Particle
        (Choice1, R, Create_Local_Element
           ("extension", G, Lookup (G, R, "extensionType"), Qualified));
      Add_Attribute
        (Choice1,
         Create_Local_Attribute ("mixed", G, Lookup (G, R, "boolean")));
      Typ := Extension_Of
        (G, Lookup (G, R, "annotated"), XML_Validator (Choice1));
      Set_Type (Create_Global_Element (G, R, "complexContent", Qualified), R,
                Create_Local_Type (G, Typ));

      --  The "complexTypeModel" group
      Gr := Create_Global_Group (G, R, "complexTypeModel");
      Choice1 := Create_Choice (G);
      Set_Debug_Name (Choice1, "complexTypeModel_choice");
      Add_Particle (Gr, R, Choice1);
      Add_Particle (Choice1, R, Lookup_Element (G, R, "simpleContent"));
      Add_Particle (Choice1, R, Lookup_Element (G, R, "complexContent"));
      Seq1 := Create_Sequence (G);
      Set_Debug_Name (Seq1, "complexTypeModel_seq");
      Add_Particle (Choice1, R, Seq1);
      Add_Particle
        (Seq1, R, Lookup_Group (G, R, "typeDefParticle"), Min_Occurs => 0);
      Add_Particle (Seq1, R, Lookup_Group (G, R, "attrDecls"));

      --  The "complexType" type  ??? abstract=true
      Typ := Extension_Of (G, R,
                           Lookup (G, R, "annotated"),
                           Lookup_Group (G, R, "complexTypeModel"));
      Set_Debug_Name (Typ, "complexType_ext");
      Create_Global_Type (G, R, "complexType", Typ);
      Add_Attribute (Typ, Create_Local_Attribute
                       ("name", G, Lookup (G, R, "NCName")));
      Add_Attribute (Typ, Create_Local_Attribute
                       ("mixed", G, Lookup (G, R, "boolean"),
                        Attribute_Use => Default,
                        Value => "false"));
      Add_Attribute (Typ, Create_Local_Attribute
                       ("abstract", G, Lookup (G, R, "boolean"),
                        Attribute_Use => Default,
                        Value => "false"));
      Add_Attribute
        (Typ, Create_Local_Attribute
           ("final", G, Lookup (G, R, "derivationSet")));
      Add_Attribute
        (Typ,
         Create_Local_Attribute ("block", G, Lookup (G, R, "derivationSet"),
           Attribute_Use => Default,
           Value => ""));

      --  The "topLevelComplexType" type
      Seq1 := Create_Sequence (G);
      Set_Debug_Name (Seq1, "topLevelComplexType_seq");
      Add_Particle
        (Seq1, R, Lookup_Element (G, R, "annotation"), Min_Occurs => 0);
      Add_Particle (Seq1, R, Lookup_Group (G, R, "complexTypeModel"));
      Typ := Restriction_Of (G, R, Lookup (G, R, "complexType"),
                             XML_Validator (Seq1));
      Add_Attribute (Typ, Create_Local_Attribute
                       ("name", G, Lookup (G, R, "NCName"),
                        Attribute_Use => Required));
      Create_Global_Type (G, R, "topLevelComplexType", Typ);

      --  The "complexType" element
      Elem := Create_Global_Element (G, R, "complexType", Qualified);
      Set_Type (Elem, R, Lookup (G, R, "topLevelComplexType"));
      Set_Substitution_Group (Elem, R, Lookup_Element (G, R, "redefinable"));

      --  The "notation" element
      Typ := Restriction_Of (G, R, Lookup (G, R, "annotated"));
      Add_Attribute (Typ, Create_Local_Attribute
                       ("name", G, Lookup (G, R, "NCName"),
                        Attribute_Use => Required));
      Add_Attribute (Typ, Create_Local_Attribute
                       ("public", G, Lookup (G, R, "public"),
                        Attribute_Use => Required));
      Add_Attribute (Typ, Create_Local_Attribute
                       ("system", G, Lookup (G, R, "uriReference")));
      Elem := Create_Global_Element (G, R, "notation", Qualified);
      Set_Type (Elem, R, Create_Local_Type (G, Typ));
      Set_Substitution_Group (Elem, R, Lookup_Element (G, R, "schemaTop"));

      --  The "public" type
      Create_Global_Type
        (G, R, "public", Get_Validator (Lookup (G, R, "token")));

      --  The "redefine" element
      Seq1 := Create_Sequence (G);
      Choice1 := Create_Choice (G);
      Add_Particle
        (Seq1, R, Choice1, Min_Occurs => 0, Max_Occurs => Unbounded);
      Set_Debug_Name (Choice1, "redefine_choice");
      Set_Debug_Name (Seq1, "redefine_seq");
      Add_Particle (Choice1, R, Lookup_Element (G, R, "annotation"));
      Add_Particle (Choice1, R, Lookup_Element (G, R, "redefinable"));
      Add_Attribute
        (Seq1, Create_Local_Attribute ("schemaLocation", G,
                                    Lookup (G, R, "uriReference"),
                                    Attribute_Use => Required));
      Typ := Extension_Of
        (G, Lookup (G, R, "openAttrs"), XML_Validator (Seq1));
      Set_Type (Create_Global_Element (G, R, "redefine", Qualified), R,
                Create_Local_Type (G, Typ));

      --  From datatypes.xsd

      --  The "localSimpleType" type
      Seq1 := Create_Sequence (G);
      Set_Debug_Name (Seq1, "localSimpleType_seq");
      Add_Particle
        (Seq1, R, Lookup_Element (G, R, "annotation"), Min_Occurs => 0);
      Add_Particle
        (Seq1, R, Lookup_Element (G, R, "simpleDerivation"));
      Typ := Restriction_Of (G, R, Lookup (G, R, "simpleType"),
                             XML_Validator (Seq1));
      Add_Attribute (Typ, Create_Local_Attribute
                       ("name", G, Attribute_Use => Prohibited));
      Create_Global_Type (G, R, "localSimpleType", Typ);

      --  The "simpleDerivation" element  ??? abstract=true
      Set_Type (Create_Global_Element (G, R, "simpleDerivation", Qualified),
                R, Lookup (G, R, "annotated"));

      --  The "simpleDerivationSet" type
      Union := Create_Union (G);
      Typ := Restriction_Of (G, R, Lookup (G, R, "token"));
      Add_Facet (Typ, R, "enumeration", "#all");
      Add_Union (Union, R, Create_Local_Type (G, Typ));
      Typ := Restriction_Of (G, R, Lookup (G, R, "derivationControl"));
      Add_Facet (Typ, R, "enumeration", "list");
      Add_Facet (Typ, R, "enumeration", "union");
      Add_Facet (Typ, R, "enumeration", "restriction");
      Add_Facet (Typ, R, "enumeration", "extension");
      Add_Union (Union, R, List_Of (G, Create_Local_Type (G, Typ)));
      Create_Global_Type (G, R, "simpleDerivationSet", Union);

      --  The "simpleType" type  ??? abstract=true
      Seq1 := Create_Sequence (G);
      Set_Debug_Name (Seq1, "simpleType_seq");
      Add_Particle (Seq1, R, Lookup_Element (G, R, "simpleDerivation"));
      Typ := Extension_Of
        (G, Lookup (G, R, "annotated"), XML_Validator (Seq1));
      Add_Attribute
        (Typ, Create_Local_Attribute ("name", G, Lookup (G, R, "NCName")));
      Add_Attribute
        (Typ, Create_Local_Attribute
           ("final", G, Lookup (G, R, "simpleDerivationSet")));
      Create_Global_Type (G, R, "simpleType", Typ);

      --  The "topLevelSimpleType" type
      Seq1 := Create_Sequence (G);
      Set_Debug_Name (Seq1, "topLevelSimpleType_seq");
      Add_Particle
        (Seq1, R, Lookup_Element (G, R, "annotation"), Min_Occurs => 0);
      Add_Particle (Seq1, R, Lookup_Element (G, R, "simpleDerivation"));
      Typ := Restriction_Of (G, R, Lookup (G, R, "simpleType"),
                             XML_Validator (Seq1));
      Create_Global_Type (G, R, "topLevelSimpleType", Typ);
      Add_Attribute
        (Typ, Create_Local_Attribute ("name", G, Lookup (G, R, "NCName"),
         Attribute_Use => Required));
      Add_Attribute
        (Typ, Create_Any_Attribute (G, Process_Lax, Kind => Namespace_Other));

      --  The "simpleType" element
      Elem := Create_Global_Element (G, R, "simpleType", Qualified);
      Set_Type (Elem, R, Lookup (G, R, "topLevelSimpleType"));
      Set_Substitution_Group (Elem, R, Lookup_Element (G, R, "redefinable"));

      --  The "restriction" element
      Typ := Extension_Of (G, R,
                           Lookup (G, R, "annotated"),
                           Lookup_Group (G, R, "simpleRestrictionModel"));
      Add_Attribute
        (Typ, Create_Local_Attribute ("base", G, Lookup (G, R, "QName"),
                                      Attribute_Use => Optional));
      Set_Debug_Name (Typ, "restriction_ext");
      Elem := Create_Global_Element (G, R, "restriction", Qualified);
      Set_Type (Elem, R, Create_Local_Type (G, Typ));
      Set_Substitution_Group
        (Elem, R, Lookup_Element (G, R, "simpleDerivation"));

      --  The "union" element
      Seq1 := Create_Sequence (G);
      Set_Debug_Name (Seq1, "union_seq");
      Add_Particle (Seq1, R,
                    Create_Local_Element
                      ("simpleType", G, Lookup (G, R, "localSimpleType"),
                       Qualified),
                    Min_Occurs => 0, Max_Occurs => Unbounded);
      Typ := Extension_Of
        (G, Lookup (G, R, "annotated"), XML_Validator (Seq1));
      Add_Attribute
        (Typ, Create_Local_Attribute ("memberTypes", G,
                                List_Of (G, Lookup (G, R, "QName")),
                                Attribute_Use => Optional));
      Elem := Create_Global_Element (G, R, "union", Qualified);
      Set_Type (Elem, R, Create_Local_Type (G, Typ));
      Set_Substitution_Group
        (Elem, R, Lookup_Element (G, R, "simpleDerivation"));

      --  The "list" element
      Seq1 := Create_Sequence (G);
      Set_Debug_Name (Seq1, "list_seq");
      Add_Particle (Seq1, R, Create_Local_Element
                      ("simpleType", G, Lookup (G, R, "localSimpleType"),
                       Qualified),
                    Min_Occurs => 0);
      Typ := Extension_Of
        (G, Lookup (G, R, "annotated"), XML_Validator (Seq1));
      Add_Attribute
        (Typ, Create_Local_Attribute ("itemType", G, Lookup (G, R, "QName"),
         Attribute_Use => Optional));
      Elem := Create_Global_Element (G, R, "list", Qualified);
      Set_Type (Elem, R, Create_Local_Type (G, Typ));
      Set_Substitution_Group
        (Elem, R, Lookup_Element (G, R, "simpleDerivation"));

      --  The "facet" type
      Typ := Restriction_Of (G, R, Lookup (G, R, "annotated"));
      Add_Attribute
        (Typ, Create_Local_Attribute ("value", G,
                                Lookup (G, R, "anySimpleType"),
                                Attribute_Use => Required));
      Add_Attribute
        (Typ, Create_Local_Attribute ("fixed", G, Lookup (G, R, "boolean"),
                                Attribute_Use => Optional));
      Create_Global_Type (G, R, "facet", Typ);

      --  The "numFacet" type
      Seq1 := Create_Sequence (G);
      Set_Debug_Name (Seq1, "numFacet_seq");
      Add_Particle
        (Seq1, R, Lookup_Element (G, R, "annotation"), Min_Occurs => 0);
      Typ := Restriction_Of
        (G, R, Lookup (G, R, "facet"), XML_Validator (Seq1));
      Add_Attribute
        (Typ, Create_Local_Attribute
           ("value", G, Lookup (G, R, "nonNegativeInteger")));
      Create_Global_Type (G, R, "numFacet", Typ);

      --  The "facet" element  ??? abstract=true
      Set_Type (Create_Global_Element (G, R, "facet", Qualified),
                R, Lookup (G, R, "facet"));

      --  The "enumeration" element
      Elem := Create_Global_Element (G, R, "enumeration", Qualified);
      Set_Type (Elem, R, Get_Type (Lookup_Element (G, R, "facet")));
      Set_Substitution_Group (Elem, R, Lookup_Element (G, R, "facet"));

      --  The "pattern" element
      Elem := Create_Global_Element (G, R, "pattern", Qualified);
      Set_Type (Elem, R, Get_Type (Lookup_Element (G, R, "facet")));
      Set_Substitution_Group (Elem, R, Lookup_Element (G, R, "facet"));

      --  The "maxLength" element
      Elem := Create_Global_Element (G, R, "maxLength", Qualified);
      Set_Type (Elem, R, Lookup (G, R, "numFacet"));
      Set_Substitution_Group (Elem, R, Lookup_Element (G, R, "facet"));

      --  The "minLength" element
      Elem := Create_Global_Element (G, R, "minLength", Qualified);
      Set_Type (Elem, R, Lookup (G, R, "numFacet"));
      Set_Substitution_Group (Elem, R, Lookup_Element (G, R, "facet"));

      --  The "length" element
      Elem := Create_Global_Element (G, R, "length", Qualified);
      Set_Type (Elem, R, Lookup (G, R, "numFacet"));
      Set_Substitution_Group (Elem, R, Lookup_Element (G, R, "facet"));

      --  The "minBound" element
      Elem := Create_Global_Element (G, R, "minBound", Qualified);
      Set_Type (Elem, R, Lookup (G, R, "facet"));
      Set_Abstract (Elem, True);
      Set_Substitution_Group (Elem, R, Lookup_Element (G, R, "facet"));

      --  The "minExclusive" element
      Elem := Create_Global_Element (G, R, "minExclusive", Qualified);
      Set_Type (Elem, R, Lookup (G, R, "facet"));
      Set_Substitution_Group (Elem, R, Lookup_Element (G, R, "minBound"));

      --  The "minInclusive" element
      Elem := Create_Global_Element (G, R, "minInclusive", Qualified);
      Set_Type (Elem, R, Lookup (G, R, "facet"));
      Set_Substitution_Group (Elem, R, Lookup_Element (G, R, "minBound"));

      --  The "maxBound" element
      Elem := Create_Global_Element (G, R, "maxBound", Qualified);
      Set_Type (Elem, R, Lookup (G, R, "facet"));
      Set_Abstract (Elem, True);
      Set_Substitution_Group (Elem, R, Lookup_Element (G, R, "facet"));

      --  The "maxExclusive" element
      Elem := Create_Global_Element (G, R, "maxExclusive", Qualified);
      Set_Type (Elem, R, Lookup (G, R, "facet"));
      Set_Substitution_Group (Elem, R, Lookup_Element (G, R, "maxBound"));

      --  The "maxInclusive" element
      Elem := Create_Global_Element (G, R, "maxInclusive", Qualified);
      Set_Type (Elem, R, Lookup (G, R, "facet"));
      Set_Substitution_Group (Elem, R, Lookup_Element (G, R, "maxBound"));

      --  The "whiteSpace" element
      Elem := Create_Global_Element (G, R, "whiteSpace", Qualified);
      Set_Substitution_Group (Elem, R, Lookup_Element (G, R, "facet"));
      Seq1 := Create_Sequence (G);
      Add_Particle
        (Seq1, R, Lookup_Element (G, R, "annotation"), Min_Occurs => 0);
      Typ := Restriction_Of
        (G, R, Lookup (G, R, "facet"), XML_Validator (Seq1));
      Typ2 := Restriction_Of (G, R, Lookup (G, R, "NMTOKEN"));
      Add_Facet (Typ2, R, "enumeration", "preserve");
      Add_Facet (Typ2, R, "enumeration", "replace");
      Add_Facet (Typ2, R, "enumeration", "collapse");
      Add_Attribute
        (Typ, Create_Local_Attribute ("value", G,
                                      Create_Local_Type (G, Typ2)));
      Set_Type (Elem, R, Create_Local_Type (G, Typ));

      --  The "totalDigits" element
      Elem := Create_Global_Element (G, R, "totalDigits", Qualified);
      Set_Substitution_Group (Elem, R, Lookup_Element (G, R, "facet"));
      Seq1 := Create_Sequence (G);
      Set_Debug_Name (Seq1, "totalDigits_seq");
      Add_Particle
        (Seq1, R, Lookup_Element (G, R, "annotation"), Min_Occurs => 0);
      Typ := Restriction_Of
        (G, R, Lookup (G, R, "numFacet"), XML_Validator (Seq1));
      Add_Attribute
        (Typ, Create_Local_Attribute
           ("value", G, Lookup (G, R, "positiveInteger"),
           Attribute_Use => Required));
      Add_Attribute
        (Typ, Create_Any_Attribute (G, Process_Lax, Kind => Namespace_Other));
      Set_Type (Elem, R, Create_Local_Type (G, Typ));

      --  The "fractionDigits" element
      Elem := Create_Global_Element (G, R, "fractionDigits", Qualified);
      Set_Type (Elem, R, Lookup (G, R, "numFacet"));
      Set_Substitution_Group (Elem, R, Lookup_Element (G, R, "facet"));

      --  The namespace schema

      Create_Global_Attribute (XML_G, R, "base", Lookup (G, R, "string"));

      --  The schema instance namespace

      Create_Global_Attribute (XML_IG, R, "nil", Lookup (G, R, "boolean"));
      Create_Global_Attribute (XML_IG, R, "type", Lookup (G, R, "QName"));
      Create_Global_Attribute
        (XML_IG, R, "schemaLocation",
         List_Of (XML_IG, Lookup (G, R, "uriReference")));
      Create_Global_Attribute (XML_IG, R, "noNamespaceSchemaLocation",
                               Lookup (G, R, "uriReference"));

      Global_Check (R, G);
   end Add_Schema_For_Schema;

end Schema.Validators.XSD_Grammar;
