with Unicode.CES;       use Unicode.CES;
with Schema.Validators; use Schema.Validators;
with Schema.Validators; use Schema.Validators;

package body Schema.Schema_Grammar is

   function Create_Schema_For_Schema return XML_Grammar is
      Grammar                    : XML_Grammar;
      G, XML_G                   : XML_Grammar_NS;
      Typ, Typ2                  : XML_Validator;
      Seq1, Seq2                 : Sequence;
      Choice1                    : Choice;
      All_Validator              : XML_Type;
      Elem                       : XML_Element;
      Gr                         : XML_Group;
      Union, Union2              : XML_Validator;
      Attr                       : XML_Attribute_Group;
   begin
      Initialize (Grammar);
      Get_NS (Grammar, XML_Schema_URI, G);
      Get_NS (Grammar, XML_URI, XML_G);

      Set_Element_Form_Default (G, Qualified);

      --  The "formChoice" type of schema.xsd
      Typ := Restriction_Of (Lookup (G, "NMTOKEN"));
      Add_Facet (Typ, "enumeration", "qualified");
      Add_Facet (Typ, "enumeration", "unqualified");
      Create_Global_Type (G, "formChoice", Typ);

      --  The "derivationControl" type
      Typ := Restriction_Of (Lookup (G, "NMTOKEN"));
      Add_Facet (Typ, "enumeration", "substitution");
      Add_Facet (Typ, "enumeration", "extension");
      Add_Facet (Typ, "enumeration", "restriction");
      Create_Global_Type (G, "derivationControl", Typ);

      --  The "blockSet" type
      Typ := Restriction_Of (Lookup (G, "token"));
      Add_Facet (Typ, "enumeration", "#all");
      All_Validator := Create_Local_Type (Typ);

      Union := Create_Union;
      Add_Union (Union, All_Validator);
      Add_Union (Union, List_Of (Lookup (G, "derivationControl")));
      Create_Global_Type (G, "blockSet", Union);

      --  The "reducedDerivationControl" type
      Typ := Restriction_Of (Lookup (G, "derivationControl"));
      Add_Facet (Typ, "enumeration", "extension");
      Add_Facet (Typ, "enumeration", "restriction");
      Create_Global_Type (G, "reducedDerivationControl", Typ);

      --  The "derivationSet" type
      Union := Create_Union;
      Add_Union (Union, All_Validator);
      Add_Union (Union, List_Of (Lookup (G, "reducedDerivationControl")));
      Create_Global_Type (G, "derivationSet", Union);

      --  The "uriReference" type
      Typ := Restriction_Of (Lookup (G, "anySimpleType"));
      Add_Facet (Typ, "whiteSpace", "collapse");
      Create_Global_Type (G, "uriReference", Typ);

      --  The "openAttrs" type
      Typ := Restriction_Of (Lookup (G, "anyType"));
      Add_Attribute
        (Typ, Create_Any_Attribute
           (Process_Lax, NS => G, Kind => Namespace_Other));
      Create_Global_Type (G, "openAttrs", Typ);

      --  The "annotated" type
      Seq1 := Create_Sequence;
      Set_Debug_Name (Seq1, "annotated_seq");
      Add_Particle (Seq1, Lookup_Element (G, "annotation"), Min_Occurs => 0);
      Typ := Extension_Of (Lookup (G, "openAttrs"), XML_Validator (Seq1));
      Add_Attribute (Typ, Create_Local_Attribute
                       ("id", G, Lookup (G, "ID"), Is_ID => True));
      Create_Global_Type (G, "annotated", Typ);

      --  The "schemaTop" element  ??? Missing abstract
      Set_Type (Create_Global_Element (G, "schemaTop", Qualified),
                Lookup (G, "annotated"));

      --  The "include" element
      Typ := Restriction_Of (Lookup (G, "annotated"));
      Add_Attribute
        (Typ, Create_Local_Attribute ("schemaLocation", G,
                                      Lookup (G, "uriReference"),
                                      Attribute_Use => Required));
      Set_Type (Create_Global_Element (G, "include", Qualified),
                Create_Local_Type (Typ));

      --  The "import" element
      Typ := Restriction_Of (Lookup (G, "annotated"));
      Add_Attribute
        (Typ, Create_Local_Attribute ("namespace", G,
                                      Lookup (G, "uriReference")));
      Add_Attribute
        (Typ, Create_Local_Attribute ("schemaLocation", G,
                                Lookup (G, "uriReference")));
      Set_Type (Create_Global_Element (G, "import", Qualified),
                Create_Local_Type (Typ));

      --  The "schema" element
      Choice1 := Create_Choice;
      Set_Debug_Name (Choice1, "schema_choice1");
      Add_Particle (Choice1, Lookup_Element (G, "include"));
      Add_Particle (Choice1, Lookup_Element (G, "import"));
      Add_Particle (Choice1, Lookup_Element (G, "redefine"));
      Add_Particle (Choice1, Lookup_Element (G, "annotation"));
      Seq1    := Create_Sequence;
      Set_Debug_Name (Seq1, "schema_seq1");
      Add_Particle (Seq1, Lookup_Element (G, "schemaTop"));
      Add_Particle (Seq1, Lookup_Element (G, "annotation"),
                    Min_Occurs => 0, Max_Occurs => Unbounded);
      Seq2    := Create_Sequence;
      Set_Debug_Name (Seq2, "schema_seq2");
      Add_Particle (Seq2, Choice1, Min_Occurs => 0, Max_Occurs => Unbounded);
      Add_Particle (Seq2, Seq1, Min_Occurs => 0, Max_Occurs => Unbounded);
      Add_Attribute
        (Seq2, Create_Local_Attribute
           ("targetNamespace", G, Lookup (G, "uriReference")));
      Add_Attribute
        (Seq2, Create_Local_Attribute ("version", G, Lookup (G, "token")));
      Add_Attribute
        (Seq2, Create_Local_Attribute
           ("finalDefault", G, Lookup (G, "derivationSet"),
            Attribute_Use     => Default,
            Value             => ""));
      Add_Attribute
        (Seq2, Create_Local_Attribute
           ("blockDefault", G, Lookup (G, "blockSet"),
            Attribute_Use     => Default,
            Value             => ""));
      Add_Attribute
        (Seq2, Create_Local_Attribute
           ("attributeFormDefault", G, Lookup (G, "formChoice"),
            Attribute_Use  => Default,
            Value          => "unqualified"));
      Add_Attribute
        (Seq2, Create_Local_Attribute
           ("elementFormDefault", G, Lookup (G, "formChoice"),
            Attribute_Use     => Default,
            Value             => "unqualified"));
      Add_Attribute (Seq2, Create_Local_Attribute ("id", G, Lookup (G, "ID"),
                                                   Is_ID => True));
      Set_Type (Create_Global_Element (G, "schema", Qualified),
                Create_Local_Type (Seq2));

      --  The "localComplexType" type
      Seq1 := Create_Sequence;
      Set_Debug_Name (Seq1, "localComplexType_seq");
      Add_Particle (Seq1, Lookup_Element (G, "annotation"), Min_Occurs => 0);
      Add_Particle (Seq1, Lookup_Group (G, "complexTypeModel"));
      Typ := Restriction_Of (Lookup (G, "complexType"), XML_Validator (Seq1));
      Add_Attribute
        (Typ, Create_Local_Attribute ("name", G, Attribute_Use => Prohibited));
      Create_Global_Type (G, "localComplexType", Typ);

      --  The "keybase" type
      Seq1 := Create_Sequence;
      Set_Debug_Name (Seq1, "keybase_seq");
      Add_Particle (Seq1, Lookup_Element (G, "selector"));
      Add_Particle (Seq1, Lookup_Element (G, "field"),
                    Min_Occurs => 1, Max_Occurs => Unbounded);
      Typ := Extension_Of (Lookup (G, "annotated"), XML_Validator (Seq1));
      Add_Attribute
        (Typ, Create_Local_Attribute ("name", G, Lookup (G, "NCName"),
                                      Attribute_Use => Required));
      Create_Global_Type (G, "keybase", Typ);

      --  The "identityConstraint" element  ??? abstract=true
      Set_Type (Create_Global_Element (G, "identityConstraint", Qualified),
                Lookup (G, "keybase"));

      --  The "unique" element
      Elem := Create_Global_Element (G, "unique", Qualified);
      Set_Type (Elem, Get_Type (Lookup_Element (G, "identityConstraint")));
      Set_Substitution_Group (Elem, Lookup_Element (G, "identityConstraint"));

      --  The "keyref" element
      Typ := Extension_Of (Lookup (G, "keybase"));
      Add_Attribute (Typ, Create_Local_Attribute
                       ("refer", G, Lookup (G, "QName"),
                        Attribute_Use => Required));
      Elem := Create_Global_Element (G, "keyref", Qualified);
      Set_Type (Elem, Create_Local_Type (Typ));
      Set_Substitution_Group (Elem, Lookup_Element (G, "identityConstraint"));

      --  The "key" element
      Elem := Create_Global_Element (G, "key", Qualified);
      Set_Type (Elem,
                Get_Type (Lookup_Element (G, "identityConstraint")));
      Set_Substitution_Group
        (Elem, Lookup_Element (G, "identityConstraint"));

      --  The "XPathExprApprox" type  Incorrect pattern
      Typ := Restriction_Of (Lookup (G, "string"));
--    Add_Facet (Typ, "pattern", "(/|//|\.|\.\.|:|::|\||(\w-[.:/|])+)+");
      Create_Global_Type (G, "XPathExprApprox", Typ);

      --  The "XPathSpec" type"
      Typ := Restriction_Of (Lookup (G, "annotated"));
      Add_Attribute (Typ, Create_Local_Attribute ("xpath", G,
                                            Lookup (G, "XPathExprApprox")));
      Create_Global_Type (G, "XPathSpec", Typ);

      --  The "selector" element
      Set_Type (Create_Global_Element (G, "selector", Qualified),
                Lookup (G, "XPathSpec"));

      --  The "field" element
      Set_Type (Create_Global_Element (G, "field", Qualified),
                Lookup (G, "XPathSpec"));

      --  The "allNNI" type"
      Union := Create_Union;
      Add_Union (Union, Lookup (G, "nonNegativeInteger"));
      Typ := Restriction_Of (Lookup (G, "NMTOKEN"));
      Add_Facet (Typ, "enumeration", "unbounded");
      Add_Union (Union, Create_Local_Type (Typ));
      Create_Global_Type (G, "allNNI", Union);

      --  The "occurs" AttributeGroup
      Attr := Create_Global_Attribute_Group (G, "occurs");
      Add_Attribute (Attr,
                     Create_Local_Attribute ("minOccurs", G,
                                             Lookup (G, "nonNegativeInteger"),
                                             Attribute_Use => Default,
                                             Value => "1"));
      Add_Attribute (Attr,
                     Create_Local_Attribute ("maxOccurs", G,
                                             Lookup (G, "allNNI"),
                                             Attribute_Use => Default,
                                             Value => "1"));

      --  From AttributeGroup "defRef"
      Attr := Create_Global_Attribute_Group (G, "defRef");
      Add_Attribute (Attr, Create_Local_Attribute
                       ("name", G, Lookup (G, "NCName")));
      Add_Attribute (Attr, Create_Local_Attribute
                       ("ref", G, Lookup (G, "QName")));

      --  The "element" type   ??? abstract=true
      Seq1 := Create_Sequence;
      Set_Debug_Name (Seq1, "element_seq");
      Choice1 := Create_Choice;
      Set_Debug_Name (Choice1, "element_choice");
      Add_Particle (Choice1, Create_Local_Element
                      ("simpleType", G, Lookup (G, "localSimpleType"),
                       Qualified));
      Add_Particle (Choice1, Create_Local_Element
                      ("complexType", G, Lookup (G, "localComplexType"),
                       Qualified));
      Add_Particle (Seq1, Choice1, Min_Occurs => 0);
      Add_Particle (Seq1, Lookup_Element (G, "identityConstraint"),
                    Min_Occurs => 0, Max_Occurs => Unbounded);
      Typ := Extension_Of (Lookup (G, "annotated"), XML_Validator (Seq1));
      Set_Debug_Name (Typ, "element_ext");
      Create_Global_Type (G, "element", Typ);
      Add_Attribute_Group (Typ, Lookup_Attribute_Group (G, "occurs"));
      Add_Attribute_Group (Typ, Lookup_Attribute_Group (G, "defRef"));
      Add_Attribute (Typ,
                     Create_Local_Attribute ("type", G, Lookup (G, "QName")));
      Add_Attribute
        (Typ, Create_Local_Attribute
           ("substitutionGroup", G, Lookup (G, "QName")));
      Add_Attribute
        (Typ, Create_Local_Attribute ("default", G, Lookup (G, "string")));
      Add_Attribute (Typ, Create_Local_Attribute
                       ("fixed", G, Lookup (G, "string")));
      Add_Attribute
        (Typ, Create_Local_Attribute ("nillable", G, Lookup (G, "boolean"),
                                Attribute_Use => Default, Value => "false"));
      Add_Attribute
        (Typ, Create_Local_Attribute ("abstract", G, Lookup (G, "boolean"),
                                Attribute_Use => Default, Value => "false"));
      Add_Attribute
        (Typ, Create_Local_Attribute ("final", G, Lookup (G, "derivationSet"),
                                Attribute_Use => Default, Value => ""));
      Add_Attribute
        (Typ, Create_Local_Attribute ("block", G, Lookup (G, "blockSet"),
                                Attribute_Use => Default, Value => ""));
      Add_Attribute
        (Typ, Create_Local_Attribute ("form", G, Lookup (G, "formChoice")));

      --  The "appinfo" element"
      Seq1 := Create_Sequence;
      Set_Debug_Name (Seq1, "appinfo_seq");
      Seq2 := Create_Sequence;
      Add_Particle (Seq1, Seq2, Min_Occurs => 0, Max_Occurs => Unbounded);
      Set_Debug_Name (Seq2, "appinfo_seq2");
      --   <any processContents="lax" />
      Add_Attribute
        (Seq1, Create_Local_Attribute
           ("source", G, Lookup (G, "uriReference")));
      Set_Mixed_Content (Seq1, True);
      Set_Type (Create_Global_Element (G, "appinfo", Qualified),
                Create_Local_Type (Seq1));

      --  The "documentation" element
      Seq1 := Create_Sequence;
      Seq2 := Create_Sequence;
      Add_Particle (Seq1, Seq2, Min_Occurs => 0, Max_Occurs => Unbounded);
      Set_Debug_Name (Seq1, "documentation_seq");
      Set_Debug_Name (Seq2, "documentation_seq2");
      --   <any processContents="lax" />
      Add_Attribute
        (Seq1, Create_Local_Attribute
           ("source", G, Lookup (G, "uriReference")));
      Add_Attribute (Seq1, Lookup_Attribute (XML_G, "lang"));
      Set_Mixed_Content (Seq1, True);
      Set_Type (Create_Global_Element (G, "documentation", Qualified),
                Create_Local_Type (Seq1));

      --  The "annotation" element  ??? invalid
      Seq1 := Create_Sequence;
      Choice1 := Create_Choice;
      Add_Particle (Seq1, Choice1, Min_Occurs => 0, Max_Occurs => Unbounded);
      Set_Debug_Name (Seq1, "annotation_seq");
      Set_Debug_Name (Choice1, "annotation_choice");
      Add_Particle (Choice1, Lookup_Element (G, "appinfo"));
      Add_Particle (Choice1, Lookup_Element (G, "documentation"));
      Set_Type (Create_Global_Element (G, "annotation", Qualified),
                Create_Local_Type (Seq1));

      --  The "topLevelElement" type
      Seq1 := Create_Sequence;
      Set_Debug_Name (Seq1, "topLevelElement_seq");
      Add_Particle (Seq1, Lookup_Element (G, "annotation"), Min_Occurs => 0);
      Choice1 := Create_Choice;
      Set_Debug_Name (Choice1, "topLevelElement_choice");
      Add_Particle (Seq1, Choice1, Min_Occurs => 0);
      Add_Particle (Choice1, Create_Local_Element
                      ("simpleType", G, Lookup (G, "localSimpleType"),
                       Qualified));
      Add_Particle (Choice1, Create_Local_Element
                      ("complexType", G, Lookup (G, "localComplexType"),
                       Qualified));
      Add_Particle (Seq1, Lookup_Element (G, "identityConstraint"),
                    Min_Occurs => 0, Max_Occurs => Unbounded);
      Typ := Restriction_Of (Lookup (G, "element"), XML_Validator (Seq1));
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
        (Typ, Create_Local_Attribute ("name", G, Lookup (G, "NCName"),
                                Attribute_Use => Required));
      Create_Global_Type (G, "topLevelElement", Typ);

      --  The "element" element
      Elem := Create_Global_Element (G, "element", Qualified);
      Set_Type (Elem, Lookup (G, "topLevelElement"));
      Set_Substitution_Group (Elem, Lookup_Element (G, "schemaTop"));

      --  The "attribute" element
      Elem := Create_Global_Element (G, "attribute", Qualified);
      Set_Type (Elem, Lookup (G, "topLevelAttribute"));
      Set_Substitution_Group (Elem, Lookup_Element (G, "schemaTop"));

      --  The "redefinable" element  --  abstract=true
      Elem := Create_Global_Element (G, "redefinable", Qualified);
      Set_Type (Elem, Get_Type (Lookup_Element (G, "schemaTop")));
      Set_Substitution_Group (Elem, Lookup_Element (G, "schemaTop"));

      --  The "all" element
      Seq1 := Create_Sequence;
      Set_Debug_Name (Seq1, "all_seq");
      Add_Particle (Seq1, Lookup_Element (G, "annotation"), Min_Occurs => 0);

      Seq2 := Create_Sequence;
      Set_Debug_Name (Seq2, "all_seq2");
      Add_Particle (Seq2, Lookup_Element (G, "annotation"), Min_Occurs => 0);
      Choice1 := Create_Choice;
      Set_Debug_Name (Choice1, "all_choice");
      Add_Particle (Seq2, Choice1, Min_Occurs => 0);
      Add_Particle (Choice1, Create_Local_Element
                      ("simpleType", G, Lookup (G, "localSimpleType"),
                       Qualified));
      Add_Particle (Choice1, Create_Local_Element
                      ("complexType", G, Lookup (G, "localComplexType"),
                       Qualified));
      Add_Particle (Seq2, Lookup_Element (G, "identityConstraint"),
                    Min_Occurs => 0, Max_Occurs => Unbounded);
      Typ2 := Restriction_Of
        (Lookup (G, "localElement"), XML_Validator (Seq2));

      Typ := Restriction_Of (Lookup (G, "nonNegativeInteger"));
      Add_Facet (Typ, "enumeration", "0");
      Add_Facet (Typ, "enumeration", "1");
      Add_Attribute
        (Typ2, Create_Local_Attribute ("minOccurs", G, Create_Local_Type (Typ),
                                 Attribute_Use => Default, Value => "1"));

      Typ := Restriction_Of (Lookup (G, "allNNI"));
      Add_Facet (Typ, "enumeration", "0");
      Add_Facet (Typ, "enumeration", "1");
      Add_Attribute
        (Typ2, Create_Local_Attribute ("maxOccurs", G, Create_Local_Type (Typ),
                                 Attribute_Use => Default, Value => "1"));

      Add_Particle (Seq1,
                    Create_Local_Element
                      ("element", G, Create_Local_Type (Typ2), Qualified),
                    Min_Occurs => 0, Max_Occurs => Unbounded);

      Typ := Restriction_Of
        (Lookup (G, "explicitGroup"), XML_Validator (Seq1));

      Typ2 := Restriction_Of (Lookup (G, "nonNegativeInteger"));
      Add_Facet (Typ2, "enumeration", "1");
      Add_Attribute
        (Typ, Create_Local_Attribute ("minOccurs", G, Create_Local_Type (Typ2),
                                      Attribute_Use => Default, Value => "1"));

      Typ2 := Restriction_Of (Lookup (G, "allNNI"));
      Add_Facet (Typ2, "enumeration", "1");
      Add_Attribute
        (Typ, Create_Local_Attribute ("maxOccurs", G, Create_Local_Type (Typ2),
                                      Attribute_Use => Default, Value => "1"));

      Set_Type (Create_Global_Element (G, "all", Qualified),
                Create_Local_Type (Typ));

      --  The "localElement" type
      Seq1 := Create_Sequence;
      Set_Debug_Name (Seq1, "localElement_seq");
      Add_Particle (Seq1, Lookup_Element (G, "annotation"), Min_Occurs => 0);
      Choice1 := Create_Choice;
      Set_Debug_Name (Choice1, "localElement_choice");
      Add_Particle (Seq1, Choice1, Min_Occurs => 0);
      Add_Particle
        (Choice1, Create_Local_Element
           ("simpleType", G, Lookup (G, "localSimpleType"), Qualified));
      Add_Particle
        (Choice1, Create_Local_Element
           ("complexType", G, Lookup (G, "localComplexType"), Qualified));
      Add_Particle (Seq1, Lookup_Element (G, "identityConstraint"),
                    Min_Occurs => 0, Max_Occurs => Unbounded);
      Add_Attribute (Seq1, Create_Local_Attribute ("substitutionGroup", G,
                                             Attribute_Use => Prohibited));
      Add_Attribute (Seq1, Create_Local_Attribute ("final", G,
                                             Attribute_Use => Prohibited));
      Typ := Restriction_Of (Lookup (G, "element"), XML_Validator (Seq1));
      Create_Global_Type (G, "localElement", Typ);

      --  The "particle" group
      Gr := Create_Global_Group (G, "particle");
      Choice1 := Create_Choice;
      Set_Debug_Name (Choice1, "particle_choice");
      Add_Particle (Gr, Choice1);
      Add_Particle
        (Choice1, Create_Local_Element
           ("element", G, Lookup (G, "localElement"), Qualified));
      Add_Particle
        (Choice1, Create_Local_Element
           ("group", G, Lookup (G, "groupRef"), Qualified));
      Add_Particle (Choice1, Lookup_Element (G, "all"));
      Add_Particle (Choice1, Lookup_Element (G, "choice"));
      Add_Particle (Choice1, Lookup_Element (G, "sequence"));
      Add_Particle (Choice1, Lookup_Element (G, "any"));

      --  "group" type
      Typ := Extension_Of
        (Lookup (G, "annotated"),
         Lookup_Group (G, "particle"),
         Min_Occurs => 0, Max_Occurs => Unbounded);
      Set_Debug_Name (Typ, "group_ext");
      Create_Global_Type (G, "group", Typ);
      Add_Attribute_Group (Typ, Lookup_Attribute_Group (G, "defRef"));
      Add_Attribute_Group (Typ, Lookup_Attribute_Group (G, "occurs"));

      --  The "nestedParticle" element
      Gr := Create_Global_Group (G, "nestedParticle");
      Choice1 := Create_Choice;
      Set_Debug_Name (Choice1, "nestedParticle_choice");
      Add_Particle (Gr, Choice1);
      Add_Particle
        (Choice1, Create_Local_Element
           ("element", G, Lookup (G, "localElement"), Qualified));
      Add_Particle
        (Choice1, Create_Local_Element
           ("group", G, Lookup (G, "groupRef"), Qualified));
      Add_Particle (Choice1, Lookup_Element (G, "choice"));
      Add_Particle (Choice1, Lookup_Element (G, "sequence"));
      Add_Particle (Choice1, Lookup_Element (G, "any"));

      --  "explicitGroup" type
      Seq1 := Create_Sequence;
      Set_Debug_Name (Seq1, "explicitGroup_seq");
      Add_Particle (Seq1, Lookup_Element (G, "annotation"),
                    Min_Occurs => 0);
      Add_Particle (Seq1, Lookup_Group (G, "nestedParticle"),
                    Min_Occurs => 0, Max_Occurs => Unbounded);
      Typ := Restriction_Of (Lookup (G, "group"), XML_Validator (Seq1));
      Create_Global_Type (G, "explicitGroup", Typ);
      Add_Attribute
        (Typ, Create_Local_Attribute
           ("name", G, Lookup (G, "NCName"), Attribute_Use => Prohibited));
      Add_Attribute
        (Typ, Create_Local_Attribute
           ("ref", G, Lookup (G, "QName"), Attribute_Use => Prohibited));

      --  The "choice" element
      Set_Type (Create_Global_Element (G, "choice", Qualified),
                Lookup (G, "explicitGroup"));

      --  The "sequence" element
      Set_Type (Create_Global_Element (G, "sequence", Qualified),
                Lookup (G, "explicitGroup"));

      --  "groupDefParticle" group
      Gr := Create_Global_Group (G, "groupDefParticle");
      Choice1 := Create_Choice;
      Set_Debug_Name (Choice1, "groupDefParticle_choice");
      Add_Particle (Gr, Choice1);
      Add_Particle (Choice1, Lookup_Element (G, "all"));
      Add_Particle (Choice1, Lookup_Element (G, "choice"));
      Add_Particle (Choice1, Lookup_Element (G, "sequence"));

      --  The "realGroup" type
      Seq1 := Create_Sequence;
      Set_Debug_Name (Seq1, "realGroup_seq");
      Add_Particle (Seq1, Lookup_Element (G, "annotation"), Min_Occurs => 0);
      Add_Particle (Seq1, Lookup_Group (G, "groupDefParticle"),
                    Min_Occurs => 0, Max_Occurs => 1);
      Typ := Restriction_Of (Lookup (G, "group"), XML_Validator (Seq1));
      Create_Global_Type (G, "realGroup", Typ);

      --  The "groupRef" type
      Seq1 := Create_Sequence;
      Set_Debug_Name (Seq1, "groupRef_seq");
      Add_Particle (Seq1, Lookup_Element (G, "annotation"), Min_Occurs => 0);
      Typ := Restriction_Of (Lookup (G, "realGroup"), XML_Validator (Seq1));
      Create_Global_Type (G, "groupRef", Typ);
      Add_Attribute
        (Typ, Create_Local_Attribute
           ("ref", G, Lookup (G, "QName"), Attribute_Use => Required));
      Add_Attribute
        (Typ, Create_Local_Attribute ("name", G, Attribute_Use => Prohibited));

      --  The "group" element
      Elem := Create_Global_Element (G, "group", Qualified);
      Set_Type (Elem, Lookup (G, "namedGroup"));
      Set_Substitution_Group (Elem, Lookup_Element (G, "redefinable"));

      --  The "namedGroup" type
      Seq1 := Create_Sequence;
      Set_Debug_Name (Seq1, "namedGroup_seq");
      Add_Particle (Seq1, Lookup_Element (G, "annotation"), Min_Occurs => 0);
      Add_Particle (Seq1, Lookup_Group (G, "groupDefParticle"),
                    Min_Occurs => 1, Max_Occurs => 1);
      Typ := Restriction_Of (Lookup (G, "realGroup"), XML_Validator (Seq1));
      Add_Attribute (Typ, Create_Local_Attribute
                       ("name", G, Lookup (G, "NCName"),
                        Attribute_Use => Required));
      Add_Attribute (Typ, Create_Local_Attribute
                       ("ref", G, Attribute_Use => Prohibited));
      Add_Attribute (Typ, Create_Local_Attribute
                       ("minOccurs", G, Attribute_Use => Prohibited));
      Add_Attribute (Typ, Create_Local_Attribute
                       ("maxOccurs", G, Attribute_Use => Prohibited));
      Create_Global_Type (G, "namedGroup", Typ);

      --  The "attributeGroup" type
      Seq1 := Create_Sequence;
      Set_Debug_Name (Seq1, "attributeGroup_seq");
      Add_Particle (Seq1, Lookup_Group (G, "attrDecls"));
      Typ := Extension_Of (Lookup (G, "annotated"), XML_Validator (Seq1));
      Add_Attribute_Group (Typ, Lookup_Attribute_Group (G, "defRef"));
      Create_Global_Type (G, "attributeGroup", Typ);

      --  The "namedAttributeGroup" type
      Seq1 := Create_Sequence;
      Set_Debug_Name (Seq1, "namedAttributeGroup_seq");
      Add_Particle (Seq1, Lookup_Element (G, "annotation"), Min_Occurs => 0);
      Add_Particle (Seq1, Lookup_Group (G, "attrDecls"));
      Typ := Restriction_Of
        (Lookup (G, "attributeGroup"), XML_Validator (Seq1));
      Add_Attribute
        (Typ, Create_Local_Attribute ("name", G, Lookup (G, "NCName"),
                                Attribute_Use => Required));
      Add_Attribute
        (Typ, Create_Local_Attribute ("ref", G, Attribute_Use => Prohibited));
      Create_Global_Type (G, "namedAttributeGroup", Typ);

      --  The "attributeGroup" element
      Elem := Create_Global_Element (G, "attributeGroup", Qualified);
      Set_Type (Elem, Lookup (G, "namedAttributeGroup"));
      Set_Substitution_Group (Elem, Lookup_Element (G, "redefinable"));

      --  The "typeDefParticle" group
      Gr := Create_Global_Group (G, "typeDefParticle");
      Choice1 := Create_Choice;
      Set_Debug_Name (Choice1, "typeDefParticle_choice");
      Add_Particle (Gr, Choice1);
      Add_Particle (Choice1, Create_Local_Element
                      ("group", G, Lookup (G, "groupRef"), Qualified));
      Add_Particle (Choice1, Lookup_Element (G, "all"));
      Add_Particle (Choice1, Lookup_Element (G, "choice"));
      Add_Particle (Choice1, Lookup_Element (G, "sequence"));

      --  The "attribute" type
      Seq1 := Create_Sequence;
      Set_Debug_Name (Seq1, "attribute_seq");
      Add_Particle (Seq1, Create_Local_Element
                      ("simpleType", G,
                       Lookup (G, "localSimpleType"), Qualified),
                    Min_Occurs => 0);
      Typ := Extension_Of (Lookup (G, "annotated"), XML_Validator (Seq1));
      Set_Debug_Name (Typ, "attribute_ext");
      Create_Global_Type (G, "attribute", Typ);
      Add_Attribute (Typ,
                     Create_Local_Attribute ("type", G, Lookup (G, "QName")));

      Typ2 := Restriction_Of (Lookup (G, "NMTOKEN"));
      Add_Facet (Typ2, "enumeration", "prohibited");
      Add_Facet (Typ2, "enumeration", "optional");
      Add_Facet (Typ2, "enumeration", "required");
      Add_Attribute_Group (Typ, Lookup_Attribute_Group (G, "defRef"));
      Add_Attribute (Typ, Create_Local_Attribute
                       ("use", G, Create_Local_Type (Typ2),
                        Attribute_Use => Default,
                        Value => "optional"));
      Add_Attribute (Typ, Create_Local_Attribute
                       ("default", G, Lookup (G, "string"),
                        Attribute_Use => Optional));
      Add_Attribute (Typ, Create_Local_Attribute
                       ("fixed", G, Lookup (G, "string"),
                        Attribute_Use => Optional));
      Add_Attribute
        (Typ, Create_Local_Attribute ("form", G, Lookup (G, "formChoice")));

      --  The "topLevelAttribute" type
      Seq1 := Create_Sequence;
      Set_Debug_Name (Seq1, "topLevelAttribute_seq");
      Add_Particle (Seq1, Lookup_Element (G, "annotation"), Min_Occurs => 0);
      Add_Particle (Seq1, Create_Local_Element
                      ("simpleType", G, Lookup (G, "localSimpleType"),
                       Qualified),
                    Min_Occurs => 0);
      Typ := Restriction_Of (Lookup (G, "attribute"), XML_Validator (Seq1));
      Set_Debug_Name (Typ, "topLevelAttribute restriction");
      Create_Global_Type (G, "topLevelAttribute", Typ);
      Add_Attribute
        (Typ, Create_Local_Attribute ("ref", G, Attribute_Use => Prohibited));
      Add_Attribute
        (Typ, Create_Local_Attribute ("form", G, Attribute_Use => Prohibited));
      Add_Attribute
        (Typ, Create_Local_Attribute ("use", G, Attribute_Use => Prohibited));
      Add_Attribute
        (Typ, Create_Local_Attribute ("name", G, Lookup (G, "NCName"),
                                      Attribute_Use => Required));

      --  The "anyAttributes" element
      Set_Type (Create_Global_Element (G, "anyAttribute", Qualified),
                Lookup (G, "wildcard"));

      --  The "namespaceList" type   ??? Incomplete
      Union := Create_Union;
      Typ := Restriction_Of (Lookup (G, "token"));
      Add_Facet (Typ, "enumeration", "##any");
      Add_Facet (Typ, "enumeration", "##other");
      Add_Union (Union, Create_Local_Type (Typ));

      Union2 := Create_Union;
      Add_Union (Union, Create_Local_Type (Union2));
      Add_Union (Union2, Lookup (G, "uriReference"));
      Typ := Restriction_Of (Lookup (G, "token"));
      Add_Facet (Typ, "enumeration", "##targetNamespace");
      Add_Facet (Typ, "enumeration", "##local");
      Add_Union (Union2, Create_Local_Type (Typ));

      Create_Global_Type (G, "namespaceList", Union);

      --  The "wildcard" type
      Typ := Restriction_Of (Lookup (G, "annotated"));
      Add_Attribute (Typ, Create_Local_Attribute ("namespace", G,
                                            Lookup (G, "namespaceList"),
                                            Attribute_Use => Default,
                                            Value => "##any"));
      Typ2 := Restriction_Of (Lookup (G, "NMTOKEN"));
      Add_Facet (Typ2, "enumeration", "skip");
      Add_Facet (Typ2, "enumeration", "lax");
      Add_Facet (Typ2, "enumeration", "strict");
      Add_Attribute (Typ, Create_Local_Attribute ("processContents", G,
                                            Create_Local_Type (Typ2),
                                            Attribute_Use => Default,
                                            Value => "strict"));
      Create_Global_Type (G, "wildcard", Typ);

      --  The "any" element   ??? Error if you put before "wildcard"
      Typ := Restriction_Of (Lookup (G, "wildcard"));
      Add_Attribute_Group (Typ, Lookup_Attribute_Group (G, "occurs"));
      Set_Type (Create_Global_Element (G, "any", Qualified),
                Create_Local_Type (Typ));

      --  The "attributeGroupRef"  ??? invalid
      Seq1 := Create_Sequence;
      Set_Debug_Name (Seq1, "attributeGroupRef_seq");
      Add_Particle (Seq1, Lookup_Element (G, "annotation"), Min_Occurs => 0);
      Typ := Restriction_Of
        (Lookup (G, "attributeGroup"), XML_Validator (Seq1));
      Add_Attribute
        (Typ, Create_Local_Attribute ("ref", G, Lookup (G, "QName"),
                                Attribute_Use => Required));
      Add_Attribute
        (Typ, Create_Local_Attribute ("name", G, Attribute_Use => Prohibited));
      Create_Global_Type (G, "attributeGroupRef", Typ);

      --  The "attrDecls" group
      Gr := Create_Global_Group (G, "attrDecls");
      Seq1 := Create_Sequence;
      Set_Debug_Name (Seq1, "attrDecls_seq");
      Add_Particle (Gr, Seq1);
      Choice1 := Create_Choice;
      Set_Debug_Name (Choice1, "attrDecls_choice");
      Add_Particle (Seq1, Choice1, Min_Occurs => 0, Max_Occurs => Unbounded);
      Add_Particle
        (Choice1, Create_Local_Element
           ("attribute", G, Lookup (G, "attribute"), Qualified));
      Add_Particle
        (Choice1, Create_Local_Element
           ("attributeGroup", G, Lookup (G, "attributeGroupRef"), Qualified));
      Add_Particle
        (Seq1, Lookup_Element (G, "anyAttribute"), Min_Occurs => 0);

      --  The "extensionType" type
      Seq1 := Create_Sequence;
      Set_Debug_Name (Seq1, "extensionType_seq");
      Add_Particle
        (Seq1, Lookup_Group (G, "typeDefParticle"), Min_Occurs => 0);
      Add_Particle
        (Seq1, Lookup_Group (G, "attrDecls"));
      Typ := Extension_Of (Lookup (G, "annotated"), XML_Validator (Seq1));
      Set_Debug_Name (Typ, "extensionType_ext");
      Add_Attribute (Typ, Create_Local_Attribute
                       ("base", G, Lookup (G, "QName")));
      Create_Global_Type (G, "extensionType", Typ);

      --  The "restrictionType" type
      Seq1 := Create_Sequence;
      Set_Debug_Name (Seq1, "restrictionType_seq");
      Choice1 := Create_Choice;
      Set_Debug_Name (Choice1, "restrictionType_choice");
      Add_Particle
        (Choice1, Lookup_Group (G, "typeDefParticle"), Min_Occurs => 0);
      Add_Particle
        (Choice1, Lookup_Group (G, "simpleRestrictionModel"), Min_Occurs => 0);
      Add_Particle (Seq1, Choice1);
      Add_Particle (Seq1, Lookup_Group (G, "attrDecls"));
      Typ := Extension_Of (Lookup (G, "annotated"), XML_Validator (Seq1));
      Add_Attribute (Typ, Create_Local_Attribute
                       ("base", G, Lookup (G, "QName"),
                        Attribute_Use => Required));
      Create_Global_Type (G, "restrictionType", Typ);

      --  The "simpleRestrictionModel" group
      Gr := Create_Global_Group (G, "simpleRestrictionModel");
      Seq1 := Create_Sequence;
      Set_Debug_Name (Seq1, "simpleRestrictionModel_seq");
      Add_Particle (Seq1, Create_Local_Element ("simpleType", G,
                                          Lookup (G, "localSimpleType"),
                                          Qualified),
                    Min_Occurs => 0);
      Add_Particle (Seq1, Lookup_Element (G, "facet"),
                    Min_Occurs => 0, Max_Occurs => Unbounded);
      Add_Particle (Gr, Seq1);

      --  The "simpleExtensionType"
      Seq1 := Create_Sequence;
      Set_Debug_Name (Seq1, "simpleExtensionType_seq");
      Add_Particle (Seq1, Lookup_Element (G, "annotation"), Min_Occurs => 0);
      Add_Particle (Seq1, Lookup_Group (G, "attrDecls"));
      Create_Global_Type (G, "simpleExtensionType",
                Restriction_Of (Lookup (G, "extensionType"),
                                XML_Validator (Seq1)));

      --  The "simpleRestrictionType"
      Seq1 := Create_Sequence;
      Set_Debug_Name (Seq1, "simpleRestrictionType_seq");
      Add_Particle (Seq1, Lookup_Element (G, "annotation"), Min_Occurs => 0);
      Add_Particle (Seq1, Lookup_Group (G, "simpleRestrictionModel"),
                    Min_Occurs => 0);
      Add_Particle (Seq1, Lookup_Group (G, "attrDecls"));
      Create_Global_Type (G, "simpleRestrictionType",
                Restriction_Of (Lookup (G, "restrictionType"),
                                XML_Validator (Seq1)));

      --  The "simpleContent" element
      Choice1 := Create_Choice;
      Set_Debug_Name (Choice1, "simpleContent_choice");
      Add_Particle (Choice1, Create_Local_Element
                      ("restriction", G,
                       Lookup (G, "simpleRestrictionType"),
                       Qualified));
      Add_Particle (Choice1, Create_Local_Element
                      ("extension", G,
                       Lookup (G, "simpleExtensionType"),
                       Qualified));
      Typ := Extension_Of (Lookup (G, "annotated"), XML_Validator (Choice1));
      Set_Debug_Name (Typ, "simpleContent_ext");
      Set_Type (Create_Global_Element (G, "simpleContent", Qualified),
                Create_Local_Type (Typ));

      --  The "complexRestrictionType" type
      Seq1 := Create_Sequence;
      Set_Debug_Name (Seq1, "complexRestrictionType_seq");
      Add_Particle (Seq1, Lookup_Element (G, "annotation"), Min_Occurs => 0);
      Add_Particle
        (Seq1, Lookup_Group (G, "typeDefParticle"), Min_Occurs => 0);
      Add_Particle (Seq1, Lookup_Group (G, "attrDecls"));
      Typ := Restriction_Of (Lookup (G, "restrictionType"),
                             XML_Validator (Seq1));
      Create_Global_Type (G, "complexRestrictionType", Typ);

      --  The "complexContent" element
      Choice1 := Create_Choice;
      Set_Debug_Name (Choice1, "complexContent_choice");
      Add_Particle
        (Choice1, Create_Local_Element
           ("restriction", G, Lookup (G, "complexRestrictionType"),
            Qualified));
      Add_Particle
        (Choice1, Create_Local_Element
           ("extension", G, Lookup (G, "extensionType"), Qualified));
      Add_Attribute
        (Choice1, Create_Local_Attribute ("mixed", G, Lookup (G, "boolean")));
      Typ := Extension_Of (Lookup (G, "annotated"), XML_Validator (Choice1));
      Set_Type (Create_Global_Element (G, "complexContent", Qualified),
                Create_Local_Type (Typ));

      --  The "complexTypeModel" group
      Gr := Create_Global_Group (G, "complexTypeModel");
      Choice1 := Create_Choice;
      Set_Debug_Name (Choice1, "complexTypeModel_choice");
      Add_Particle (Gr, Choice1);
      Add_Particle (Choice1, Lookup_Element (G, "simpleContent"));
      Add_Particle (Choice1, Lookup_Element (G, "complexContent"));
      Seq1 := Create_Sequence;
      Set_Debug_Name (Seq1, "complexTypeModel_seq");
      Add_Particle (Choice1, Seq1);
      Add_Particle
        (Seq1, Lookup_Group (G, "typeDefParticle"), Min_Occurs => 0);
      Add_Particle (Seq1, Lookup_Group (G, "attrDecls"));

      --  The "complexType" type  ??? abstract=true
      Typ := Extension_Of (Lookup (G, "annotated"),
                           Lookup_Group (G, "complexTypeModel"));
      Set_Debug_Name (Typ, "complexType_ext");
      Create_Global_Type (G, "complexType", Typ);
      Add_Attribute (Typ, Create_Local_Attribute
                       ("name", G, Lookup (G, "NCName")));
      Add_Attribute (Typ, Create_Local_Attribute
                       ("mixed", G, Lookup (G, "boolean"),
                        Attribute_Use => Default,
                        Value => "false"));
      Add_Attribute (Typ, Create_Local_Attribute
                       ("abstract", G, Lookup (G, "boolean"),
                        Attribute_Use => Default,
                        Value => "false"));
      Add_Attribute
        (Typ, Create_Local_Attribute
           ("final", G, Lookup (G, "derivationSet")));
      Add_Attribute
        (Typ, Create_Local_Attribute ("block", G, Lookup (G, "derivationSet"),
                                Attribute_Use => Default,
                                Value => ""));

      --  The "topLevelComplexType" type
      Seq1 := Create_Sequence;
      Set_Debug_Name (Seq1, "topLevelComplexType_seq");
      Add_Particle (Seq1, Lookup_Element (G, "annotation"), Min_Occurs => 0);
      Add_Particle (Seq1, Lookup_Group (G, "complexTypeModel"));
      Typ := Restriction_Of (Lookup (G, "complexType"),
                             XML_Validator (Seq1));
      Add_Attribute (Typ, Create_Local_Attribute
                       ("name", G, Lookup (G, "NCName"),
                        Attribute_Use => Required));
      Create_Global_Type (G, "topLevelComplexType", Typ);

      --  The "complexType" element
      Elem := Create_Global_Element (G, "complexType", Qualified);
      Set_Type (Elem, Lookup (G, "topLevelComplexType"));
      Set_Substitution_Group (Elem, Lookup_Element (G, "redefinable"));

      --  The "notation" element
      Typ := Restriction_Of (Lookup (G, "annotated"));
      Add_Attribute (Typ, Create_Local_Attribute
                       ("name", G, Lookup (G, "NCName"),
                        Attribute_Use => Required));
      Add_Attribute (Typ, Create_Local_Attribute
                       ("public", G, Lookup (G, "public"),
                        Attribute_Use => Required));
      Add_Attribute (Typ, Create_Local_Attribute
                       ("system", G, Lookup (G, "uriReference")));
      Elem := Create_Global_Element (G, "notation", Qualified);
      Set_Type (Elem, Create_Local_Type (Typ));
      Set_Substitution_Group (Elem, Lookup_Element (G, "schemaTop"));

      --  The "public" type
      Create_Global_Type (G, "public", Get_Validator (Lookup (G, "token")));

      --  The "redefine" element
      Seq1 := Create_Sequence;
      Choice1 := Create_Choice;
      Add_Particle (Seq1, Choice1, Min_Occurs => 0, Max_Occurs => Unbounded);
      Set_Debug_Name (Choice1, "redefine_choice");
      Set_Debug_Name (Seq1, "redefine_seq");
      Add_Particle (Choice1, Lookup_Element (G, "annotation"));
      Add_Particle (Choice1, Lookup_Element (G, "redefinable"));
      Add_Attribute
        (Seq1, Create_Local_Attribute ("schemaLocation", G,
                                    Lookup (G, "uriReference"),
                                    Attribute_Use => Required));
      Typ := Extension_Of (Lookup (G, "openAttrs"), XML_Validator (Seq1));
      Set_Type (Create_Global_Element (G, "redefine", Qualified),
                Create_Local_Type (Typ));

      --  From datatypes.xsd

      --  The "localSimpleType" type
      Seq1 := Create_Sequence;
      Set_Debug_Name (Seq1, "localSimpleType_seq");
      Add_Particle (Seq1, Lookup_Element (G, "annotation"), Min_Occurs => 0);
      Add_Particle (Seq1, Lookup_Element (G, "simpleDerivation"));
      Typ := Restriction_Of (Lookup (G, "simpleType"),
                             XML_Validator (Seq1));
      Add_Attribute (Typ, Create_Local_Attribute
                       ("name", G, Attribute_Use => Prohibited));
      Create_Global_Type (G, "localSimpleType", Typ);

      --  The "simpleDerivation" element  ??? abstract=true
      Set_Type (Create_Global_Element (G, "simpleDerivation", Qualified),
                Lookup (G, "annotated"));

      --  The "simpleType" type  ??? abstract=true
      Seq1 := Create_Sequence;
      Set_Debug_Name (Seq1, "simpleType_seq");
      Add_Particle (Seq1, Lookup_Element (G, "simpleDerivation"));
      Typ := Extension_Of (Lookup (G, "annotated"), XML_Validator (Seq1));
      Add_Attribute
        (Typ, Create_Local_Attribute ("name", G, Lookup (G, "NCName")));
      Create_Global_Type (G, "simpleType", Typ);

      --  The "topLevelSimpleType" type
      Seq1 := Create_Sequence;
      Set_Debug_Name (Seq1, "topLevelSimpleType_seq");
      Add_Particle (Seq1, Lookup_Element (G, "annotation"), Min_Occurs => 0);
      Add_Particle (Seq1, Lookup_Element (G, "simpleDerivation"));
      Typ := Restriction_Of (Lookup (G, "simpleType"),
                             XML_Validator (Seq1));
      Create_Global_Type (G, "topLevelSimpleType", Typ);
      Add_Attribute
        (Typ, Create_Local_Attribute ("name", G, Lookup (G, "NCName"),
                                Attribute_Use => Required));

      --  The "simpleType" element
      Elem := Create_Global_Element (G, "simpleType", Qualified);
      Set_Type (Elem, Lookup (G, "topLevelSimpleType"));
      Set_Substitution_Group (Elem, Lookup_Element (G, "redefinable"));

      --  The "restriction" element
      Typ := Extension_Of (Lookup (G, "annotated"),
                           Lookup_Group (G, "simpleRestrictionModel"));
      Add_Attribute
        (Typ, Create_Local_Attribute ("base", G, Lookup (G, "QName"),
                                 Attribute_Use => Optional));
      Set_Debug_Name (Typ, "restriction_ext");
      Elem := Create_Global_Element (G, "restriction", Qualified);
      Set_Type (Elem, Create_Local_Type (Typ));
      Set_Substitution_Group (Elem, Lookup_Element (G, "simpleDerivation"));

      --  The "union" element
      Seq1 := Create_Sequence;
      Set_Debug_Name (Seq1, "union_seq");
      Add_Particle (Seq1,
                    Create_Local_Element
                      ("simpleType", G, Lookup (G, "localSimpleType"),
                       Qualified),
                    Min_Occurs => 0, Max_Occurs => Unbounded);
      Typ := Extension_Of (Lookup (G, "annotated"), XML_Validator (Seq1));
      Add_Attribute
        (Typ, Create_Local_Attribute ("memberTypes", G,
                                List_Of (Lookup (G, "QName")),
                                Attribute_Use => Optional));
      Elem := Create_Global_Element (G, "union", Qualified);
      Set_Type (Elem, Create_Local_Type (Typ));
      Set_Substitution_Group (Elem, Lookup_Element (G, "simpleDerivation"));

      --  The "list" element
      Seq1 := Create_Sequence;
      Set_Debug_Name (Seq1, "list_seq");
      Add_Particle (Seq1, Create_Local_Element
                      ("simpleType", G, Lookup (G, "localSimpleType"),
                       Qualified),
                    Min_Occurs => 0);
      Typ := Extension_Of (Lookup (G, "annotated"), XML_Validator (Seq1));
      Add_Attribute
        (Typ, Create_Local_Attribute ("itemType", G, Lookup (G, "QName"),
                                Attribute_Use => Optional));
      Elem := Create_Global_Element (G, "list", Qualified);
      Set_Type (Elem, Create_Local_Type (Typ));
      Set_Substitution_Group (Elem, Lookup_Element (G, "simpleDerivation"));

      --  The "facet" type
      Typ := Restriction_Of (Lookup (G, "annotated"));
      Add_Attribute
        (Typ, Create_Local_Attribute ("value", G,
                                Lookup (G, "anySimpleType"),
                                Attribute_Use => Required));
      Add_Attribute
        (Typ, Create_Local_Attribute ("fixed", G, Lookup (G, "boolean"),
                                Attribute_Use => Optional));
      Create_Global_Type (G, "facet", Typ);

      --  The "numFacet" type
      Seq1 := Create_Sequence;
      Set_Debug_Name (Seq1, "numFacet_seq");
      Add_Particle (Seq1, Lookup_Element (G, "annotation"), Min_Occurs => 0);
      Typ := Restriction_Of (Lookup (G, "facet"), XML_Validator (Seq1));
      Add_Attribute
        (Typ, Create_Local_Attribute
           ("value", G, Lookup (G, "nonNegativeInteger")));
      Create_Global_Type (G, "numFacet", Typ);

      --  The "facet" element  ??? abstract=true
      Set_Type (Create_Global_Element (G, "facet", Qualified),
                Lookup (G, "facet"));

      --  The "enumeration" element
      Elem := Create_Global_Element (G, "enumeration", Qualified);
      Set_Type (Elem, Get_Type (Lookup_Element (G, "facet")));
      Set_Substitution_Group (Elem, Lookup_Element (G, "facet"));

      --  The "pattern" element
      Elem := Create_Global_Element (G, "pattern", Qualified);
      Set_Type (Elem, Get_Type (Lookup_Element (G, "facet")));
      Set_Substitution_Group (Elem, Lookup_Element (G, "facet"));

      --  The "maxLength" element
      Elem := Create_Global_Element (G, "maxLength", Qualified);
      Set_Type (Elem, Lookup (G, "numFacet"));
      Set_Substitution_Group (Elem, Lookup_Element (G, "facet"));

      --  The "minLength" element
      Elem := Create_Global_Element (G, "minLength", Qualified);
      Set_Type (Elem, Lookup (G, "numFacet"));
      Set_Substitution_Group (Elem, Lookup_Element (G, "facet"));

      --  The "length" element
      Elem := Create_Global_Element (G, "length", Qualified);
      Set_Type (Elem, Lookup (G, "numFacet"));
      Set_Substitution_Group (Elem, Lookup_Element (G, "facet"));

      --  The "minBound" element
      Elem := Create_Global_Element (G, "minBound", Qualified);
      Set_Type (Elem, Lookup (G, "facet"));
      Set_Abstract (Elem, True);
      Set_Substitution_Group (Elem, Lookup_Element (G, "facet"));

      --  The "minExclusive" element
      Elem := Create_Global_Element (G, "minExclusive", Qualified);
      Set_Type (Elem, Lookup (G, "facet"));
      Set_Substitution_Group (Elem, Lookup_Element (G, "minBound"));

      --  The "minInclusive" element
      Elem := Create_Global_Element (G, "minInclusive", Qualified);
      Set_Type (Elem, Lookup (G, "facet"));
      Set_Substitution_Group (Elem, Lookup_Element (G, "minBound"));

      --  The "maxBound" element
      Elem := Create_Global_Element (G, "maxBound", Qualified);
      Set_Type (Elem, Lookup (G, "facet"));
      Set_Abstract (Elem, True);
      Set_Substitution_Group (Elem, Lookup_Element (G, "facet"));

      --  The "maxExclusive" element
      Elem := Create_Global_Element (G, "maxExclusive", Qualified);
      Set_Type (Elem, Lookup (G, "facet"));
      Set_Substitution_Group (Elem, Lookup_Element (G, "maxBound"));

      --  The "maxInclusive" element
      Elem := Create_Global_Element (G, "maxInclusive", Qualified);
      Set_Type (Elem, Lookup (G, "facet"));
      Set_Substitution_Group (Elem, Lookup_Element (G, "maxBound"));

      --  The "whiteSpace" element
      Elem := Create_Global_Element (G, "whiteSpace", Qualified);
      Set_Substitution_Group (Elem, Lookup_Element (G, "facet"));

      Seq1 := Create_Sequence;
      Add_Particle (Seq1, Lookup_Element (G, "annotation"), Min_Occurs => 0);
      Typ := Restriction_Of (Lookup (G, "facet"), XML_Validator (Seq1));
      Typ2 := Restriction_Of (Lookup (G, "NMTOKEN"));
      Add_Facet (Typ2, "enumeration", "preserve");
      Add_Facet (Typ2, "enumeration", "replace");
      Add_Facet (Typ2, "enumeration", "collapse");
      Add_Attribute
        (Typ, Create_Local_Attribute ("value", G,
                                      Create_Local_Type (Typ2)));
      Set_Type (Elem, Create_Local_Type (Typ));

      return Grammar;
   end Create_Schema_For_Schema;

end Schema.Schema_Grammar;
