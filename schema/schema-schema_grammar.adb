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
      Union                      : XML_Union;
      Attr                       : XML_Attribute_Group;
   begin
      Initialize (Grammar);
      Get_NS (Grammar, XML_Schema_URI, G);
      Get_NS (Grammar, XML_URI, XML_G);

      --  The "formChoice" type of schema.xsd
      Typ := Restriction_Of (Lookup (G, "NMTOKEN"));
      Add_Facet (Typ, "enumeration", "qualified");
      Add_Facet (Typ, "enumeration", "unqualified");
      Register (G, Create_Type ("formChoice", Typ));

      --  The "derivationControl" type
      Typ := Restriction_Of (Lookup (G, "NMTOKEN"));
      Add_Facet (Typ, "enumeration", "substitution");
      Add_Facet (Typ, "enumeration", "extension");
      Add_Facet (Typ, "enumeration", "restriction");
      Register (G, Create_Type ("derivationControl", Typ));

      --  The "blockSet" type
      Typ := Restriction_Of (Lookup (G, "token"));
      Add_Facet (Typ, "enumeration", "#all");
      All_Validator := Create_Type ("#all", Typ);

      Union := Create_Union;
      Add_Union (Union, All_Validator);
      Add_Union (Union, List_Of (Lookup (G, "derivationControl")));
      Register (G, Create_Type ("blockSet", Union));

      --  The "reducedDerivationControl" type
      Typ := Restriction_Of (Lookup (G, "derivationControl"));
      Add_Facet (Typ, "enumeration", "extension");
      Add_Facet (Typ, "enumeration", "restriction");
      Register (G, Create_Type ("reducedDerivationControl", Typ));

      --  The "derivationSet" type
      Union := Create_Union;
      Add_Union (Union, All_Validator);
      Add_Union (Union, List_Of (Lookup (G, "reducedDerivationControl")));
      Register (G, Create_Type ("derivationSet", Union));

      --  The "uriReference" type
      Typ := Restriction_Of (Lookup (G, "anySimpleType"));
      Add_Facet (Typ, "whiteSpace", "collapse");
      Register (G, Create_Type ("uriReference", Typ));

      --  The "openAttrs" type  --  ??? <anyAttribute>
      Typ := Restriction_Of (Lookup (G, "anyType"));
      Add_Attribute
        (Typ, Create_Any_Attribute (NS => G, Kind => Namespace_Other));
      Register (G, Create_Type ("openAttrs", Typ));

      --  The "annotated" type
      Seq1 := Create_Sequence;
      Add_Particle (Seq1, Lookup_Element (G, "annotation"), Min_Occurs => 0);
      Typ := Extension_Of (Lookup (G, "openAttrs"), XML_Validator (Seq1));
      Add_Attribute (Typ, Create_Attribute ("id", G, Lookup (G, "ID"),
                                            Is_ID => True));
      Register (G, Create_Type ("annotated", Typ));

      --  The "schemaTop" element  ??? Missing abstract
      Elem := Create_Element ("schemaTop", Lookup (G, "annotated"));
      Register (G, Elem);

      --  The "include" element
      Typ := Restriction_Of (Lookup (G, "annotated"));
      Add_Attribute
        (Typ, Create_Attribute ("schemaLocation", G,
                                Lookup (G, "uriReference"),
                                Attribute_Use => Required));
      Elem := Create_Element ("include", Create_Type ("", Typ));
      Register (G, Elem);

      --  The "import" element
      Typ := Restriction_Of (Lookup (G, "annotated"));
      Add_Attribute
        (Typ, Create_Attribute ("namespace", G,
                                Lookup (G, "uriReference")));
      Add_Attribute
        (Typ, Create_Attribute ("schemaLocation", G,
                                Lookup (G, "uriReference")));
      Elem := Create_Element ("import", Create_Type ("", Typ));
      Register (G, Elem);

      --  The "schema" element
      Choice1 := Create_Choice (Min_Occurs => 0, Max_Occurs => Unbounded);
      Set_Debug_Name (Choice1, "schema choice1");
      Add_Particle (Choice1, Lookup_Element (G, "include"));
      Add_Particle (Choice1, Lookup_Element (G, "import"));
      Add_Particle (Choice1, Lookup_Element (G, "redefine"));
      Add_Particle (Choice1, Lookup_Element (G, "annotation"));
      Seq1    := Create_Sequence (Min_Occurs => 0, Max_Occurs => Unbounded);
      Set_Debug_Name (Seq1, "schema seq1");
      Add_Particle (Seq1, Lookup_Element (G, "schemaTop"));
      Add_Particle (Seq1, Lookup_Element (G, "annotation"),
                    Min_Occurs => 0, Max_Occurs => Unbounded);
      Seq2    := Create_Sequence;
      Set_Debug_Name (Seq2, "schema seq2");
      Add_Particle (Seq2, Choice1);
      Add_Particle (Seq2, Seq1);
      Add_Attribute
        (Seq2, Create_Attribute
           ("targetNamespace", G, Lookup (G, "uriReference")));
      Add_Attribute
        (Seq2, Create_Attribute ("version", G, Lookup (G, "token")));
      Add_Attribute
        (Seq2, Create_Attribute
           ("finalDefault", G, Lookup (G, "derivationSet"),
            Attribute_Use     => Default,
            Value             => ""));
      Add_Attribute
        (Seq2, Create_Attribute
           ("blockDefault", G, Lookup (G, "blockSet"),
            Attribute_Use     => Default,
            Value             => ""));
      Add_Attribute
        (Seq2, Create_Attribute
           ("attributeFormDefault", G, Lookup (G, "formChoice"),
            Attribute_Use  => Default,
            Value          => "unqualified"));
      Add_Attribute
        (Seq2, Create_Attribute
           ("elementFormDefault", G, Lookup (G, "formChoice"),
            Attribute_Use     => Default,
            Value             => "unqualified"));
      Add_Attribute (Seq2, Create_Attribute ("id", G, Lookup (G, "ID"),
                                             Is_ID => True));
      Elem := Create_Element ("schema", Create_Type ("schema type", Seq2));
      Register (G, Elem);

      --  The "localComplexType" type
      Seq1 := Create_Sequence;
      Set_Debug_Name (Seq1, "localComplexType seq");
      Add_Particle (Seq1, Lookup_Element (G, "annotation"), Min_Occurs => 0);
      Add_Particle (Seq1, Lookup_Group (G, "complexTypeModel"));
      Typ := Restriction_Of (Lookup (G, "complexType"), XML_Validator (Seq1));
      Add_Attribute
        (Typ, Create_Attribute ("name", G, Attribute_Use => Prohibited));
      Register (G, Create_Type ("localComplexType", Typ));

      --  The "keybase" type
      Seq1 := Create_Sequence;
      Add_Particle (Seq1, Lookup_Element (G, "selector"));
      Add_Particle (Seq1, Lookup_Element (G, "field"),
                    Min_Occurs => 1, Max_Occurs => Unbounded);
      Typ := Extension_Of (Lookup (G, "annotated"), XML_Validator (Seq1));
      Add_Attribute
        (Typ, Create_Attribute ("name", G, Lookup (G, "NCName"),
                              Attribute_Use => Required));
      Register (G, Create_Type ("keybase", Typ));

      --  The "identityConstraint" element  ??? abstract=true
      Elem := Create_Element ("identityConstraint", Lookup (G, "keybase"));
      Register (G, Elem);

      --  The "unique" element
      Elem := Create_Element
        ("unique", Get_Type (Lookup_Element (G, "identityConstraint")));
      Set_Substitution_Group (Elem, Lookup_Element (G, "identityConstraint"));
      Register (G, Elem);

      --  The "keyref" element
      Typ := Extension_Of (Lookup (G, "keybase"));
      Add_Attribute (Typ, Create_Attribute
                       ("refer", G, Lookup (G, "QName"),
                        Attribute_Use => Required));
      Elem := Create_Element ("keyref", Create_Type ("", Typ));
      Set_Substitution_Group (Elem, Lookup_Element (G, "identityConstraint"));
      Register (G, Elem);

      --  The "key" element
      Elem := Create_Element
        ("key",
         Get_Type (Lookup_Element (G, "identityConstraint")));
      Set_Substitution_Group
        (Elem, Lookup_Element (G, "identityConstraint"));
      Register (G, Elem);

      --  The "XPathExprApprox" type  Incorrect pattern
      Typ := Restriction_Of (Lookup (G, "string"));
--    Add_Facet (Typ, "pattern", "(/|//|\.|\.\.|:|::|\||(\w-[.:/|])+)+");
      Register (G, Create_Type ("XPathExprApprox", Typ));

      --  The "XPathSpec" type"
      Typ := Restriction_Of (Lookup (G, "annotated"));
      Add_Attribute (Typ, Create_Attribute ("xpath", G,
                                            Lookup (G, "XPathExprApprox")));
      Register (G, Create_Type ("XPathSpec", Typ));

      --  The "selector" element
      Elem := Create_Element ("selector", Lookup (G, "XPathSpec"));
      Register (G, Elem);

      --  The "field" element
      Elem := Create_Element ("field", Lookup (G, "XPathSpec"));
      Register (G, Elem);

      --  The "allNNI" type"
      Union := Create_Union;
      Add_Union (Union, Lookup (G, "nonNegativeInteger"));
      Typ := Restriction_Of (Lookup (G, "NMTOKEN"));
      Add_Facet (Typ, "enumeration", "unbounded");
      Add_Union (Union, Create_Type ("", Typ));
      Register (G, Create_Type ("allNNI", Union));

      --  The "occurs" AttributeGroup
      Attr := Create_Attribute_Group ("occurs");
      Add_Attribute (Attr,
                     Create_Attribute ("minOccurs", G,
                                       Lookup (G, "nonNegativeInteger"),
                                       Attribute_Use => Default,
                                       Value => "1"));
      Add_Attribute (Attr,
                     Create_Attribute ("maxOccurs", G,
                                       Lookup (G, "allNNI"),
                                       Attribute_Use => Default,
                                       Value => "1"));
      Register (G, Attr);

      --  From AttributeGroup "defRef"
      Attr := Create_Attribute_Group ("defRef");
      Add_Attribute (Attr, Create_Attribute ("name", G, Lookup (G, "NCName")));
      Add_Attribute (Attr, Create_Attribute ("ref", G, Lookup (G, "QName")));
      Register (G, Attr);

      --  The "element" type   ??? abstract=true
      Seq1 := Create_Sequence;
      Set_Debug_Name (Seq1, "element seq");
      Choice1 := Create_Choice (Min_Occurs => 0);
      Add_Particle (Choice1, Create_Element
                      ("simpleType", Lookup (G, "localSimpleType")));
      Add_Particle (Choice1, Create_Element
                      ("complexType", Lookup (G, "localComplexType")));
      Add_Particle (Seq1, Choice1);
      Add_Particle (Seq1, Lookup_Element (G, "identityConstraint"),
                    Min_Occurs => 0, Max_Occurs => Unbounded);
      Typ := Extension_Of (Lookup (G, "annotated"), XML_Validator (Seq1));
      Set_Debug_Name (Typ, "element extension");
      Register (G, Create_Type ("element", Typ));
      Add_Attribute_Group (Typ, Lookup_Attribute_Group (G, "occurs"));
      Add_Attribute_Group (Typ, Lookup_Attribute_Group (G, "defRef"));
      Add_Attribute (Typ, Create_Attribute ("type", G, Lookup (G, "QName")));
      Add_Attribute
        (Typ, Create_Attribute ("substitutionGroup", G, Lookup (G, "QName")));
      Add_Attribute
        (Typ, Create_Attribute ("default", G, Lookup (G, "string")));
      Add_Attribute (Typ, Create_Attribute ("fixed", G, Lookup (G, "string")));
      Add_Attribute
        (Typ, Create_Attribute ("nillable", G, Lookup (G, "boolean"),
                                Attribute_Use => Default, Value => "false"));
      Add_Attribute
        (Typ, Create_Attribute ("abstract", G, Lookup (G, "boolean"),
                                Attribute_Use => Default, Value => "false"));
      Add_Attribute
        (Typ, Create_Attribute ("final", G, Lookup (G, "derivationSet"),
                                Attribute_Use => Default, Value => ""));
      Add_Attribute
        (Typ, Create_Attribute ("block", G, Lookup (G, "blockSet"),
                                Attribute_Use => Default, Value => ""));
      Add_Attribute
        (Typ, Create_Attribute ("form", G, Lookup (G, "formChoice")));

      --  The "appinfo" element"
      Seq1 := Create_Sequence (Min_Occurs => 0, Max_Occurs => Unbounded);
      --   <any processContents="lax" />
      Add_Attribute
        (Seq1, Create_Attribute ("source", G, Lookup (G, "uriReference")));
      Set_Mixed_Content (Seq1, True);
      Elem := Create_Element ("appinfo", Create_Type ("", Seq1));
      Register (G, Elem);

      --  The "documentation" element
      Seq1 := Create_Sequence (Min_Occurs => 0, Max_Occurs => Unbounded);
      --   <any processContents="lax" />
      Add_Attribute
        (Seq1, Create_Attribute ("source", G, Lookup (G, "uriReference")));
      Add_Attribute (Seq1, Lookup_Attribute (XML_G, "lang"));
      Set_Mixed_Content (Seq1, True);
      Elem := Create_Element ("documentation", Create_Type ("", Seq1));
      Register (G, Elem);

      --  The "annotation" element  ??? invalid
      Choice1 := Create_Choice (Min_Occurs => 0, Max_Occurs => Unbounded);
      Set_Debug_Name (Choice1, "annotation choice");
      Add_Particle (Choice1, Lookup_Element (G, "appinfo"));
      Add_Particle (Choice1, Lookup_Element (G, "documentation"));
      Elem := Create_Element ("annotation", Create_Type ("", Choice1));
      Register (G, Elem);

      --  The "topLevelElement" type
      Seq1 := Create_Sequence;
      Set_Debug_Name (Seq1, "topLevelElement seq");
      Add_Particle (Seq1, Lookup_Element (G, "annotation"), Min_Occurs => 0);
      Choice1 := Create_Choice (Min_Occurs => 0);
      Add_Particle (Seq1, Choice1);
      Add_Particle (Choice1, Create_Element
                      ("simpleType", Lookup (G, "localSimpleType")));
      Add_Particle (Choice1, Create_Element
                      ("complexType", Lookup (G, "localComplexType")));
      Add_Particle (Seq1, Lookup_Element (G, "identityConstraint"),
                    Min_Occurs => 0, Max_Occurs => Unbounded);
      Typ := Restriction_Of (Lookup (G, "element"), XML_Validator (Seq1));
      Set_Debug_Name (Typ, "topLevelElement restriction");
      Add_Attribute
        (Typ, Create_Attribute ("ref", G, Attribute_Use => Prohibited));
      Add_Attribute
        (Typ, Create_Attribute ("form", G, Attribute_Use => Prohibited));
      Add_Attribute
        (Typ, Create_Attribute ("minOccurs", G, Attribute_Use => Prohibited));
      Add_Attribute
        (Typ, Create_Attribute ("maxOccurs", G, Attribute_Use => Prohibited));
      Add_Attribute
        (Typ, Create_Attribute ("name", G, Lookup (G, "NCName"),
                                Attribute_Use => Required));
      Register (G, Create_Type ("topLevelElement", Typ));

      --  The "element" element
      Elem := Create_Element ("element", Lookup (G, "topLevelElement"));
      Set_Substitution_Group (Elem, Lookup_Element (G, "schemaTop"));
      Register (G, Elem);

      --  The "attribute" element
      Elem := Create_Element ("attribute", Lookup (G, "topLevelAttribute"));
      Set_Substitution_Group (Elem, Lookup_Element (G, "schemaTop"));
      Register (G, Elem);

      --  The "redefinable" element  --  abstract=true
      Elem := Create_Element ("redefinable", Lookup (G, "anyType"));
      Set_Substitution_Group (Elem, Lookup_Element (G, "schemaTop"));
      Register (G, Elem);

      --  The "all" element
      Seq1 := Create_Sequence;
      Add_Particle (Seq1, Lookup_Element (G, "annotation"), Min_Occurs => 0);

      Seq2 := Create_Sequence;
      Add_Particle (Seq2, Lookup_Element (G, "annotation"), Min_Occurs => 0);
      Choice1 := Create_Choice (Min_Occurs => 0);
      Add_Particle (Seq2, Choice1);
      Add_Particle (Choice1, Create_Element
                      ("simpleType", Lookup (G, "localSimpleType")));
      Add_Particle (Choice1, Create_Element
                      ("complexType", Lookup (G, "localComplexType")));
      Add_Particle (Seq2, Lookup_Element (G, "identityConstraint"),
                    Min_Occurs => 0, Max_Occurs => Unbounded);
      Typ2 := Restriction_Of
        (Lookup (G, "localElement"), XML_Validator (Seq2));

      Typ := Restriction_Of (Lookup (G, "nonNegativeInteger"));
      Add_Facet (Typ, "enumeration", "0");
      Add_Facet (Typ, "enumeration", "1");
      Add_Attribute
        (Typ2, Create_Attribute ("minOccurs", G, Create_Type ("", Typ),
                                 Attribute_Use => Default, Value => "1"));

      Typ := Restriction_Of (Lookup (G, "allNNI"));
      Add_Facet (Typ, "enumeration", "0");
      Add_Facet (Typ, "enumeration", "1");
      Add_Attribute
        (Typ2, Create_Attribute ("maxOccurs", G, Create_Type ("", Typ),
                                 Attribute_Use => Default, Value => "1"));

      Add_Particle (Seq1, Create_Element ("element", Create_Type ("", Typ2)),
                    Min_Occurs => 0, Max_Occurs => Unbounded);

      Typ := Restriction_Of
        (Lookup (G, "explicitGroup"), XML_Validator (Seq1));

      Typ2 := Restriction_Of (Lookup (G, "nonNegativeInteger"));
      Add_Facet (Typ2, "enumeration", "1");
      Add_Attribute
        (Typ, Create_Attribute ("minOccurs", G, Create_Type ("", Typ2),
                                Attribute_Use => Default, Value => "1"));

      Typ2 := Restriction_Of (Lookup (G, "allNNI"));
      Add_Facet (Typ2, "enumeration", "1");
      Add_Attribute
        (Typ, Create_Attribute ("maxOccurs", G, Create_Type ("", Typ2),
                                Attribute_Use => Default, Value => "1"));

      Elem := Create_Element ("all", Create_Type ("", Typ));
      Register (G, Elem);

      --  The "localElement" type
      Seq1 := Create_Sequence;
      Add_Particle (Seq1, Lookup_Element (G, "annotation"), Min_Occurs => 0);
      Choice1 := Create_Choice (Min_Occurs => 0);
      Add_Particle (Seq1, Choice1);
      Add_Particle (Choice1, Create_Element ("simpleType",
                                             Lookup (G, "localSimpleType")));
      Add_Particle (Choice1, Create_Element ("complexType",
                                             Lookup (G, "localComplexType")));
      Add_Particle (Seq1, Lookup_Element (G, "identityConstraint"),
                    Min_Occurs => 0, Max_Occurs => Unbounded);
      Add_Attribute (Seq1, Create_Attribute ("substitutionGroup", G,
                                             Attribute_Use => Prohibited));
      Add_Attribute (Seq1, Create_Attribute ("final", G,
                                             Attribute_Use => Prohibited));
      Typ := Restriction_Of (Lookup (G, "element"), XML_Validator (Seq1));
      Register (G, Create_Type ("localElement", Typ));

      --  The "particle" group
      Gr := Create_Group ("particle");
      Register (G, Gr);
      Choice1 := Create_Choice;
      Add_Particle (Gr, Choice1);
      Add_Particle
        (Choice1, Create_Element ("element", Lookup (G, "localElement")));
      Add_Particle
        (Choice1, Create_Element ("group", Lookup (G, "groupRef")));
      Add_Particle (Choice1, Lookup_Element (G, "all"));
      Add_Particle (Choice1, Lookup_Element (G, "choice"));
      Add_Particle (Choice1, Lookup_Element (G, "sequence"));
      Add_Particle (Choice1, Lookup_Element (G, "any"));

      --  "group" type
      Typ := Extension_Of
        (Lookup (G, "annotated"),
         Lookup_Group (G, "particle"),
         Min_Occurs => 0, Max_Occurs => Unbounded);
      Set_Debug_Name (Typ, "group extension");
      Register (G, Create_Type ("group", Typ));
      Add_Attribute_Group (Typ, Lookup_Attribute_Group (G, "defRef"));
      Add_Attribute_Group (Typ, Lookup_Attribute_Group (G, "occurs"));

      --  The "nestedParticle" element
      Gr := Create_Group ("nestedParticle");
      Register (G, Gr);
      Choice1 := Create_Choice;
      Add_Particle (Gr, Choice1);
      Set_Debug_Name (Choice1, "nestedParticle choice");
      Add_Particle
        (Choice1, Create_Element ("element", Lookup (G, "localElement")));
      Add_Particle
        (Choice1, Create_Element ("group", Lookup (G, "groupRef")));
      Add_Particle (Choice1, Lookup_Element (G, "choice"));
      Add_Particle (Choice1, Lookup_Element (G, "sequence"));
      Add_Particle (Choice1, Lookup_Element (G, "any"));

      --  "explicitGroup" type
      Seq1 := Create_Sequence;
      Set_Debug_Name (Seq1, "explicitGroup seq");
      Add_Particle (Seq1, Lookup_Element (G, "annotation"),
                    Min_Occurs => 0);
      Add_Particle (Seq1, Lookup_Group (G, "nestedParticle"),
                    Min_Occurs => 0, Max_Occurs => Unbounded);
      Typ := Restriction_Of (Lookup (G, "group"), XML_Validator (Seq1));
      Register (G, Create_Type ("explicitGroup", Typ));
      Add_Attribute
        (Typ, Create_Attribute
           ("name", G, Lookup (G, "NCName"), Attribute_Use => Prohibited));
      Add_Attribute
        (Typ, Create_Attribute
           ("ref", G, Lookup (G, "QName"), Attribute_Use => Prohibited));

      --  The "choice" element
      Elem := Create_Element ("choice", Lookup (G, "explicitGroup"));
      Register (G, Elem);

      --  The "sequence" element
      Elem := Create_Element ("sequence", Lookup (G, "explicitGroup"));
      Register (G, Elem);

      --  "groupDefParticle" group
      Gr := Create_Group ("groupDefParticle");
      Register (G, Gr);
      Choice1 := Create_Choice;
      Add_Particle (Gr, Choice1);
      Add_Particle (Choice1, Lookup_Element (G, "all"));
      Add_Particle (Choice1, Lookup_Element (G, "choice"));
      Add_Particle (Choice1, Lookup_Element (G, "sequence"));

      --  The "realGroup" type
      Seq1 := Create_Sequence;
      Set_Debug_Name (Seq1, "realGroup seq");
      Add_Particle (Seq1, Lookup_Element (G, "annotation"), Min_Occurs => 0);
      Add_Particle (Seq1, Lookup_Group (G, "groupDefParticle"),
                    Min_Occurs => 0, Max_Occurs => 1);
      Typ := Restriction_Of (Lookup (G, "group"), XML_Validator (Seq1));
      Register (G, Create_Type ("realGroup", Typ));

      --  The "groupRef" type
      Seq1 := Create_Sequence;
      Set_Debug_Name (Seq1, "groupRef seq");
      Add_Particle (Seq1, Lookup_Element (G, "annotation"), Min_Occurs => 0);
      Typ := Restriction_Of (Lookup (G, "realGroup"), XML_Validator (Seq1));
      Register (G, Create_Type ("groupRef", Typ));
      Add_Attribute
        (Typ, Create_Attribute
           ("ref", G, Lookup (G, "QName"), Attribute_Use => Required));
      Add_Attribute
        (Typ, Create_Attribute ("name", G, Attribute_Use => Prohibited));

      --  The "group" element
      Elem := Create_Element ("group", Lookup (G, "namedGroup"));
      Set_Substitution_Group (Elem, Lookup_Element (G, "redefinable"));
      Register (G, Elem);

      --  The "namedGroup" type
      Seq1 := Create_Sequence;
      Add_Particle (Seq1, Lookup_Element (G, "annotation"), Min_Occurs => 0);
      Add_Particle (Seq1, Lookup_Group (G, "groupDefParticle"),
                    Min_Occurs => 1, Max_Occurs => 1);
      Typ := Restriction_Of (Lookup (G, "realGroup"), XML_Validator (Seq1));
      Add_Attribute (Typ, Create_Attribute
                       ("name", G, Lookup (G, "NCName"),
                        Attribute_Use => Required));
      Add_Attribute (Typ, Create_Attribute
                       ("ref", G, Attribute_Use => Prohibited));
      Add_Attribute (Typ, Create_Attribute
                       ("minOccurs", G, Attribute_Use => Prohibited));
      Add_Attribute (Typ, Create_Attribute
                       ("maxOccurs", G, Attribute_Use => Prohibited));
      Register (G, Create_Type ("namedGroup", Typ));

      --  The "attributeGroup" type
      Seq1 := Create_Sequence;
      Add_Particle (Seq1, Lookup_Group (G, "attrDecls"));
      Typ := Extension_Of (Lookup (G, "annotated"), XML_Validator (Seq1));
      Add_Attribute_Group (Typ, Lookup_Attribute_Group (G, "defRef"));
      Register (G, Create_Type ("attributeGroup", Typ));

      --  The "namedAttributeGroup" type
      Seq1 := Create_Sequence;
      Add_Particle (Seq1, Lookup_Element (G, "annotation"), Min_Occurs => 0);
      Add_Particle (Seq1, Lookup_Group (G, "attrDecls"));
      Typ := Restriction_Of
        (Lookup (G, "attributeGroup"), XML_Validator (Seq1));
      Add_Attribute
        (Typ, Create_Attribute ("name", G, Lookup (G, "NCName"),
                                Attribute_Use => Required));
      Add_Attribute
        (Typ, Create_Attribute ("ref", G, Attribute_Use => Prohibited));
      Register (G, Create_Type ("namedAttributeGroup", Typ));

      --  The "attributeGroup" element
      Elem := Create_Element ("attributeGroup",
                              Lookup (G, "namedAttributeGroup"));
      Set_Substitution_Group (Elem, Lookup_Element (G, "redefinable"));
      Register (G, Elem);

      --  The "typeDefParticle" group
      Gr := Create_Group ("typeDefParticle");
      Register (G, Gr);
      Choice1 := Create_Choice;
      Add_Particle (Gr, Choice1);
      Set_Debug_Name (Choice1, "typeDefParticle choice");
      Add_Particle (Choice1, Create_Element ("group", Lookup (G, "groupRef")));
      Add_Particle (Choice1, Lookup_Element (G, "all"));
      Add_Particle (Choice1, Lookup_Element (G, "choice"));
      Add_Particle (Choice1, Lookup_Element (G, "sequence"));

      --  The "attribute" type
      Seq1 := Create_Sequence;
      Set_Debug_Name (Seq1, "attribute seq");
      Add_Particle (Seq1, Create_Element ("simpleType",
                                         Lookup (G, "localSimpleType")),
                    Min_Occurs => 0);
      Typ := Extension_Of (Lookup (G, "annotated"), XML_Validator (Seq1));
      Set_Debug_Name (Typ, "attribute extension");
      Register (G, Create_Type ("attribute", Typ));
      Add_Attribute (Typ, Create_Attribute ("type", G, Lookup (G, "QName")));

      Typ2 := Restriction_Of (Lookup (G, "NMTOKEN"));
      Add_Facet (Typ2, "enumeration", "prohibited");
      Add_Facet (Typ2, "enumeration", "optional");
      Add_Facet (Typ2, "enumeration", "required");
      Add_Attribute_Group (Typ, Lookup_Attribute_Group (G, "defRef"));
      Add_Attribute (Typ, Create_Attribute
                       ("use", G, Create_Type ("use_type", Typ2),
                        Attribute_Use => Default,
                        Value => "optional"));
      Add_Attribute (Typ, Create_Attribute
                       ("default", G, Lookup (G, "string"),
                        Attribute_Use => Optional));
      Add_Attribute (Typ, Create_Attribute
                       ("fixed", G, Lookup (G, "string"),
                        Attribute_Use => Optional));
      Add_Attribute
        (Typ, Create_Attribute ("form", G, Lookup (G, "formChoice")));

      --  The "topLevelAttribute" type
      Seq1 := Create_Sequence;
      Set_Debug_Name (Seq1, "topLevelAttribute seq");
      Add_Particle (Seq1, Lookup_Element (G, "annotation"), Min_Occurs => 0);
      Add_Particle (Seq1, Create_Element ("simpleType",
                                          Lookup (G, "localSimpleType")),
                    Min_Occurs => 0);
      Typ := Restriction_Of (Lookup (G, "attribute"), XML_Validator (Seq1));
      Set_Debug_Name (Typ, "topLevelAttribute restriction");
      Register (G, Create_Type ("topLevelAttribute", Typ));
      Add_Attribute
        (Typ, Create_Attribute ("ref", G, Attribute_Use => Prohibited));
      Add_Attribute
        (Typ, Create_Attribute ("form", G, Attribute_Use => Prohibited));
      Add_Attribute
        (Typ, Create_Attribute ("use", G, Attribute_Use => Prohibited));
      Add_Attribute
        (Typ, Create_Attribute ("name", G, Lookup (G, "NCName"),
                                Attribute_Use => Required));

      --  The "anyAttributes" element
      Elem := Create_Element ("anyAttribute", Lookup (G, "wildcard"));
      Register (G, Elem);

      --  The "namespaceList" type   ??? Incomplete
      Union := Create_Union;
      Typ := Restriction_Of (Lookup (G, "token"));
      Add_Facet (Typ, "enumeration", "##any");
      Add_Facet (Typ, "enumeration", "##other");
      Add_Union (Union, Create_Type ("", Typ));
      Register (G, Create_Type ("namespaceList", Union));

      --  The "wildcard" type
      Typ := Restriction_Of (Lookup (G, "annotated"));
      Add_Attribute (Typ, Create_Attribute ("namespace", G,
                                            Lookup (G, "namespaceList"),
                                            Attribute_Use => Default,
                                            Value => "##any"));
      Typ2 := Restriction_Of (Lookup (G, "NMTOKEN"));
      Add_Facet (Typ2, "enumeration", "skip");
      Add_Facet (Typ2, "enumeration", "lax");
      Add_Facet (Typ2, "enumeration", "strict");
      Add_Attribute (Typ, Create_Attribute ("processContents", G,
                                            Create_Type ("", Typ2),
                                            Attribute_Use => Default,
                                            Value => "strict"));
      Register (G, Create_Type ("wildcard", Typ));

      --  The "any" element   ??? Error if you put before "wildcard"
      Typ := Restriction_Of (Lookup (G, "wildcard"));
      Add_Attribute_Group (Typ, Lookup_Attribute_Group (G, "occurs"));
      Elem := Create_Element ("any", Create_Type ("", Typ));
      Register (G, Elem);

      --  The "attributeGroupRef"  ??? invalid
      Seq1 := Create_Sequence;
      Add_Particle (Seq1, Lookup_Element (G, "annotation"), Min_Occurs => 0);
      Typ := Restriction_Of
        (Lookup (G, "attributeGroup"), XML_Validator (Seq1));
      Add_Attribute
        (Typ, Create_Attribute ("ref", G, Lookup (G, "QName"),
                                Attribute_Use => Required));
      Add_Attribute
        (Typ, Create_Attribute ("name", G, Attribute_Use => Prohibited));
      Register (G, Create_Type ("attributeGroupRef", Typ));

      --  The "attrDecls" group
      Gr := Create_Group ("attrDecls");
      Register (G, Gr);
      Seq1 := Create_Sequence;
      Set_Debug_Name (Seq1, "attrDecls seq");
      Add_Particle (Gr, Seq1);
      Choice1 := Create_Choice (Min_Occurs => 0, Max_Occurs => Unbounded);
      Set_Debug_Name (Choice1, "attrDecls choice");
      Add_Particle (Seq1, Choice1);
      Add_Particle
        (Choice1, Create_Element ("attribute", Lookup (G, "attribute")));
      Add_Particle
        (Choice1, Create_Element ("attributeGroup",
                                  Lookup (G, "attributeGroupRef")));
      Add_Particle
        (Seq1, Lookup_Element (G, "anyAttribute"), Min_Occurs => 0);

      --  The "extensionType" type
      Seq1 := Create_Sequence;
      Set_Debug_Name (Seq1, "extensionType seq");
      Add_Particle
        (Seq1, Lookup_Group (G, "typeDefParticle"), Min_Occurs => 0);
      Add_Particle
        (Seq1, Lookup_Group (G, "attrDecls"));
      Typ := Extension_Of (Lookup (G, "annotated"), XML_Validator (Seq1));
      Set_Debug_Name (Typ, "extensionType extension");
      Add_Attribute (Typ, Create_Attribute ("base", G, Lookup (G, "QName")));
      Register (G, Create_Type ("extensionType", Typ));

      --  The "restrictionType" type
      Seq1 := Create_Sequence;
      Set_Debug_Name (Seq1, "restrictionType seq");
      Choice1 := Create_Choice;
      Set_Debug_Name (Choice1, "restrictionType choice");
      Add_Particle
        (Choice1, Lookup_Group (G, "typeDefParticle"), Min_Occurs => 0);
      Add_Particle
        (Choice1, Lookup_Group (G, "simpleRestrictionModel"), Min_Occurs => 0);
      Add_Particle (Seq1, Choice1);
      Add_Particle (Seq1, Lookup_Group (G, "attrDecls"));
      Typ := Extension_Of (Lookup (G, "annotated"), XML_Validator (Seq1));
      Add_Attribute (Typ, Create_Attribute ("base", G, Lookup (G, "QName"),
                                             Attribute_Use => Required));
      Register (G, Create_Type ("restrictionType", Typ));

      --  The "simpleRestrictionModel" group
      Gr := Create_Group ("simpleRestrictionModel");
      Seq1 := Create_Sequence;
      Set_Debug_Name (Seq1, "simpleRestrictionModel seq");
      Add_Particle (Seq1, Create_Element ("simpleType",
                                          Lookup (G, "localSimpleType")),
                    Min_Occurs => 0);
      Add_Particle (Seq1, Lookup_Element (G, "facet"),
                    Min_Occurs => 0, Max_Occurs => Unbounded);
      Add_Particle (Gr, Seq1);
      Register (G, Gr);

      --  The "simpleExtensionType"
      Seq1 := Create_Sequence;
      Set_Debug_Name (Seq1, "simpleExtensionType seq");
      Add_Particle (Seq1, Lookup_Element (G, "annotation"), Min_Occurs => 0);
      Add_Particle (Seq1, Lookup_Group (G, "attrDecls"));
      Register (G, Create_Type
                  ("simpleExtensionType",
                   Restriction_Of (Lookup (G, "extensionType"),
                                   XML_Validator (Seq1))));

      --  The "simpleRestrictionType"
      Seq1 := Create_Sequence;
      Set_Debug_Name (Seq1, "simpleRestrictionType seq");
      Add_Particle (Seq1, Lookup_Element (G, "annotation"), Min_Occurs => 0);
      Add_Particle (Seq1, Lookup_Group (G, "simpleRestrictionModel"),
                    Min_Occurs => 0);
      Add_Particle (Seq1, Lookup_Group (G, "attrDecls"));
      Register (G, Create_Type
                  ("simpleRestrictionType",
                   Restriction_Of (Lookup (G, "restrictionType"),
                                   XML_Validator (Seq1))));

      --  The "simpleContent" element
      Choice1 := Create_Choice;
      Set_Debug_Name (Choice1, "simpleContent choice");
      Add_Particle (Choice1, Create_Element
                      ("retriction",
                       Lookup (G, "simpleRestrictionType")));
      Add_Particle (Choice1, Create_Element
                      ("extension",
                       Lookup (G, "simpleExtensionType")));
      Typ := Extension_Of (Lookup (G, "annotated"), XML_Validator (Choice1));
      Set_Debug_Name (Typ, "simpleContent extension");
      Elem := Create_Element
        ("simpleContent", Create_Type ("simpleContent type", Typ));
      Register (G, Elem);

      --  The "complexRestrictionType" type
      Seq1 := Create_Sequence;
      Set_Debug_Name (Seq1, "complexRestrictionType seq");
      Add_Particle (Seq1, Lookup_Element (G, "annotation"), Min_Occurs => 0);
      Add_Particle
        (Seq1, Lookup_Group (G, "typeDefParticle"), Min_Occurs => 0);
      Add_Particle (Seq1, Lookup_Group (G, "attrDecls"));
      Typ := Restriction_Of (Lookup (G, "restrictionType"),
                             XML_Validator (Seq1));
      Register (G, Create_Type ("complexRestrictionType", Typ));

      --  The "complexContent" element
      Choice1 := Create_Choice;
      Set_Debug_Name (Choice1, "complexContent choice");
      Add_Particle (Choice1,
                    Create_Element ("restriction",
                                    Lookup (G, "complexRestrictionType")));
      Add_Particle (Choice1,
                    Create_Element ("extension",
                                    Lookup (G, "extensionType")));
      Add_Attribute
        (Choice1, Create_Attribute ("mixed", G, Lookup (G, "boolean")));
      Typ := Extension_Of (Lookup (G, "annotated"), XML_Validator (Choice1));
      Elem := Create_Element ("complexContent", Create_Type ("", Typ));
      Register (G, Elem);

      --  The "complexTypeModel" group
      Gr := Create_Group ("complexTypeModel");
      Register (G, Gr);
      Choice1 := Create_Choice;
      Set_Debug_Name (Choice1, "complexTypeModel choice");
      Add_Particle (Gr, Choice1);
      Add_Particle (Choice1, Lookup_Element (G, "simpleContent"));
      Add_Particle (Choice1, Lookup_Element (G, "complexContent"));
      Seq1 := Create_Sequence;
      Add_Particle (Choice1, Seq1);
      Set_Debug_Name (Seq1, "complexTypeModel seq");
      Add_Particle
        (Seq1, Lookup_Group (G, "typeDefParticle"), Min_Occurs => 0);
      Add_Particle (Seq1, Lookup_Group (G, "attrDecls"));

      --  The "complexType" type  ??? abstract=true
      Typ := Extension_Of (Lookup (G, "annotated"),
                           Lookup_Group (G, "complexTypeModel"));
      Set_Debug_Name (Typ, "complexType extension");
      Register (G, Create_Type ("complexType", Typ));
      Add_Attribute (Typ, Create_Attribute ("name", G, Lookup (G, "NCName")));
      Add_Attribute (Typ, Create_Attribute ("mixed", G, Lookup (G, "boolean"),
                                            Attribute_Use => Default,
                                            Value => "false"));
      Add_Attribute (Typ, Create_Attribute
                       ("abstract", G, Lookup (G, "boolean"),
                        Attribute_Use => Default,
                        Value => "false"));
      Add_Attribute
        (Typ, Create_Attribute ("final", G, Lookup (G, "derivationSet")));
      Add_Attribute
        (Typ, Create_Attribute ("block", G, Lookup (G, "derivationSet"),
                                Attribute_Use => Default,
                                Value => ""));


      --  The "topLevelComplexType" type
      Seq1 := Create_Sequence;
      Set_Debug_Name (Seq1, "topLevelComplexType seq");
      Add_Particle (Seq1, Lookup_Element (G, "annotation"), Min_Occurs => 0);
      Add_Particle (Seq1, Lookup_Group (G, "complexTypeModel"));
      Typ := Restriction_Of (Lookup (G, "complexType"),
                             XML_Validator (Seq1));
      Add_Attribute (Typ, Create_Attribute ("name", G, Lookup (G, "NCName"),
                                            Attribute_Use => Required));
      Register (G, Create_Type ("topLevelComplexType", Typ));

      --  The "complexType" element
      Elem := Create_Element
        ("complexType", Lookup (G, "topLevelComplexType"));
      Set_Substitution_Group (Elem, Lookup_Element (G, "redefinable"));
      Register (G, Elem);

      --  The "notation" element
      Typ := Restriction_Of (Lookup (G, "annotated"));
      Add_Attribute (Typ, Create_Attribute
                       ("name", G, Lookup (G, "NCName"),
                        Attribute_Use => Required));
      Add_Attribute (Typ, Create_Attribute
                       ("public", G, Lookup (G, "public"),
                        Attribute_Use => Required));
      Add_Attribute (Typ, Create_Attribute
                       ("system", G, Lookup (G, "uriReference")));
      Elem := Create_Element ("notation", Create_Type ("", Typ));
      Set_Substitution_Group (Elem, Lookup_Element (G, "schemaTop"));
      Register (G, Elem);

      --  The "public" type
      Register
        (G, Create_Type ("public", Get_Validator (Lookup (G, "token"))));


      --  From datatypes.xsd

      --  The "localSimpleType" type
      Seq1 := Create_Sequence;
      Add_Particle (Seq1, Lookup_Element (G, "annotation"), Min_Occurs => 0);
      Add_Particle (Seq1, Lookup_Element (G, "simpleDerivation"));
      Typ := Restriction_Of (Lookup (G, "simpleType"),
                             XML_Validator (Seq1));
      Add_Attribute (Typ, Create_Attribute
                       ("name", G, Attribute_Use => Prohibited));
      Register (G, Create_Type ("localSimpleType", Typ));

      --  The "simpleDerivation" element  ??? abstract=true
      Elem := Create_Element ("simpleDerivation", Lookup (G, "annotated"));
      Register (G, Elem);

      --  The "simpleType" type  ??? abstract=true
      Seq1 := Create_Sequence;
      Add_Particle (Seq1, Lookup_Element (G, "simpleDerivation"));
      Add_Attribute
        (Seq1, Create_Attribute ("name", G, Lookup (G, "NCName")));

      --  The "topLevelSimpleType" type
      Seq1 := Create_Sequence;
      Add_Particle (Seq1, Lookup_Element (G, "annotation"), Min_Occurs => 0);
      Add_Particle (Seq1, Lookup_Element (G, "simpleDerivation"));
      Typ := Restriction_Of (Lookup (G, "simpleType"),
                             XML_Validator (Seq1));
      Register (G, Create_Type ("topLevelSimpleType", Typ));
      Add_Attribute
        (Typ, Create_Attribute ("name", G, Lookup (G, "NCName"),
                                Attribute_Use => Required));

      --  The "simpleType" element
      Elem := Create_Element ("simpleType", Lookup (G, "topLevelSimpleType"));
      Set_Substitution_Group (Elem, Lookup_Element (G, "redefinable"));
      Register (G, Elem);

      --  The "restriction" element
      Seq1 := Create_Sequence;
      Set_Debug_Name (Seq1, "restriction seq");
      Add_Particle (Seq1, Lookup_Group (G, "simpleRestrictionModel"));
      Typ := Extension_Of (Lookup (G, "annotated"), XML_Validator (Seq1));
      Add_Attribute
        (Typ, Create_Attribute ("base", G, Lookup (G, "QName"),
                                 Attribute_Use => Optional));
      Set_Debug_Name (Typ, "restriction extension");
      Elem := Create_Element ("restriction", Create_Type ("", Typ));
      Set_Substitution_Group (Elem, Lookup_Element (G, "simpleDerivation"));
      Register (G, Elem);

      --  The "union" element
      Seq1 := Create_Sequence;
      Add_Particle (Seq1,
                    Create_Element
                      ("simpleType", Lookup (G, "localSimpleType")),
                    Min_Occurs => 0, Max_Occurs => Unbounded);
      Typ := Extension_Of (Lookup (G, "annotated"), XML_Validator (Seq1));
      Add_Attribute
        (Typ, Create_Attribute ("memberTypes", G,
                                List_Of (Lookup (G, "QName")),
                                Attribute_Use => Optional));
      Elem := Create_Element ("union", Create_Type ("", Typ));
      Set_Substitution_Group (Elem, Lookup_Element (G, "simpleDerivation"));
      Register (G, Elem);

      --  The "list" element
      Seq1 := Create_Sequence;
      Add_Particle (Seq1, Create_Element
                      ("simpleType", Lookup (G, "localSimpleType")),
                    Min_Occurs => 0);
      Typ := Extension_Of (Lookup (G, "annotated"), XML_Validator (Seq1));
      Add_Attribute
        (Typ, Create_Attribute ("itemType", G, Lookup (G, "QName"),
                                Attribute_Use => Optional));
      Elem := Create_Element ("list", Create_Type ("", Typ));
      Set_Substitution_Group (Elem, Lookup_Element (G, "simpleDerivation"));
      Register (G, Elem);

      --  The "facet" type
      Typ := Restriction_Of (Lookup (G, "annotated"));
      Add_Attribute
        (Typ, Create_Attribute ("value", G,
                                Lookup (G, "anySimpleType"),
                                Attribute_Use => Required));
      Add_Attribute
        (Typ, Create_Attribute ("fixed", G, Lookup (G, "boolean"),
                                Attribute_Use => Optional));
      Register (G, Create_Type ("facet", Typ));

      --  The "numFacet" type
      Seq1 := Create_Sequence;
      Add_Particle (Seq1, Lookup_Element (G, "annotation"), Min_Occurs => 0);
      Typ := Restriction_Of (Lookup (G, "facet"), XML_Validator (Seq1));
      Add_Attribute
        (Typ, Create_Attribute ("value", G, Lookup (G, "nonNegativeInteger")));
      Register (G, Create_Type ("numFacet", Typ));

      --  The "facet" element  ??? abstract=true
      Elem := Create_Element ("facet", Lookup (G, "facet"));
      Register (G, Elem);

      --  The "enumeration" element
      Elem := Create_Element ("enumeration",
                              Get_Type (Lookup_Element (G, "facet")));
      Set_Substitution_Group (Elem, Lookup_Element (G, "facet"));
      Register (G, Elem);

      --  The "pattern" element
      Elem := Create_Element ("pattern",
                              Get_Type (Lookup_Element (G, "facet")));
      Set_Substitution_Group (Elem, Lookup_Element (G, "facet"));
      Register (G, Elem);

      --  The "maxLength" element
      Elem := Create_Element ("maxLength", Lookup (G, "numFacet"));
      Set_Substitution_Group (Elem, Lookup_Element (G, "facet"));
      Register (G, Elem);

      --  The "minLength" element
      Elem := Create_Element ("minLength", Lookup (G, "numFacet"));
      Set_Substitution_Group (Elem, Lookup_Element (G, "facet"));
      Register (G, Elem);

      --  The "length" element
      Elem := Create_Element ("length", Lookup (G, "numFacet"));
      Set_Substitution_Group (Elem, Lookup_Element (G, "facet"));
      Register (G, Elem);

      return Grammar;
   end Create_Schema_For_Schema;

end Schema.Schema_Grammar;
