with Unicode.CES;       use Unicode.CES;
with Sax.Attributes;    use Sax.Attributes;
with Schema.Validators; use Schema.Validators;
with Schema.Readers;    use Schema.Readers;
with Schema.Validators; use Schema.Validators;

package body Schema.Schema_Readers is

   function Create_Schema_Grammar return XML_Grammar;
   --  Create the grammar to validate an XML Schema document

   -----------------
   -- Get_Grammar --
   -----------------

   function Get_Grammar
     (Reader : Schema_Reader) return Schema.Validators.XML_Grammar is
   begin
      return Reader.Grammar;
   end Get_Grammar;

   ---------------------------
   -- Create_Schema_Grammar --
   ---------------------------

   function Create_Schema_Grammar return XML_Grammar is
      G                          : XML_Grammar;
      Typ                        : XML_Validator;
      Seq1, Seq2                 : Sequence;
      Choice1                    : Choice;
      All_Validator              : XML_Type;
      Unknown_Valid              : XML_Validator;
      Unknown                    : XML_Type;
      Elem                       : XML_Element;
      Gr                         : XML_Group;
   begin
      Initialize (G);
      Unknown := Lookup (G, "debug");
      Unknown_Valid := Get_Validator (Lookup (G, "debug"));

      --  The "formChoice" type of schema.xsd
      Typ := Clone (Get_Validator (Lookup (G, "NMTOKEN")));
      Add_Restriction (Typ, "enumeration", "qualified");
      Add_Restriction (Typ, "enumeration", "unqualified");
      Register (G, Create_Type ("formChoice", Typ));

      --  The "derivationControl" type
      Typ := Clone (Get_Validator (Lookup (G, "NMTOKEN")));
      Add_Restriction (Typ, "enumeration", "substitution");
      Add_Restriction (Typ, "enumeration", "extension");
      Add_Restriction (Typ, "enumeration", "restriction");
      Register (G, Create_Type ("derivationControl", Typ));

      --  The "blockSet" type
      Typ := Clone (Get_Validator (Lookup (G, "token")));
      Add_Restriction (Typ, "enumeration", "#all");
      All_Validator := Create_Type ("#all", Typ);

      Typ := Clone (Get_Validator (Lookup (G, "anyType")));
      Add_Union (Typ, All_Validator);
      Add_Union (Typ, List_Of (Lookup (G, "derivationControl")));
      Register (G, Create_Type ("blockSet", Typ));

      --  The "reducedDerivationControl" type
      Typ := Clone (Get_Validator (Lookup (G, "derivationControl")));
      Add_Restriction (Typ, "enumeration", "extension");
      Add_Restriction (Typ, "enumeration", "restriction");
      Register (G, Create_Type ("reducedDerivationControl", Typ));

      --  The "derivationSet" type
      Typ := Clone (Get_Validator (Lookup (G, "anyType")));
      Add_Union (Typ, All_Validator);
      Add_Union (Typ, List_Of (Lookup (G, "reducedDerivationControl")));
      Register (G, Create_Type ("derivationSet", Typ));

      --  The "uriReference" type
      Typ := Clone (Get_Validator (Lookup (G, "anySimpleType")));
      Add_Restriction (Typ, "whiteSpace", "collapse");
      Register (G, Create_Type ("uriReference", Typ));

      --  The "annotated" type  ??? Invalid decl
      Register (G, Create_Type
                  ("annotated",
                   Clone (Get_Validator (Lookup (G, "anySimpleType")))));

      --  The "schemaTop" element  ??? Missing abstract
      Register (G, Create_Element ("schemaTop", Lookup (G, "annotated")));

      --  The "schema" element
      Choice1 := Create_Choice (Min_Occurs => 0, Max_Occurs => Unbounded);
      Add_Particle (Choice1, Create_Element ("include",    Unknown));
      Add_Particle (Choice1, Create_Element ("import",     Unknown));
      Add_Particle (Choice1, Create_Element ("redefine",   Unknown));
      Add_Particle (Choice1, Create_Element ("annotation", Unknown));
      Seq1    := Create_Sequence (Min_Occurs => 0, Max_Occurs => Unbounded);
      Set_Debug_Name (Seq1, "schema seq1");
      Add_Particle (Seq1, Lookup_Element (G, "schemaTop"));
      Add_Particle (Seq1, Create_Element ("annotation", Unknown),
                    Min_Occurs => 0, Max_Occurs => Unbounded);
      Seq2    := Create_Sequence;
      Set_Debug_Name (Seq2, "schema seq2");
      Add_Particle (Seq2, Choice1, Min_Occurs => 0, Max_Occurs => Unbounded);
      Add_Particle (Seq2, Seq1, Min_Occurs => 0, Max_Occurs => Unbounded);
      Add_Attribute
        (Seq2, Create_Attribute
           ("targetNameSpace", Lookup (G, "uriReference")));
      Add_Attribute
        (Seq2, Create_Attribute ("version", Lookup (G, "token")));
      Add_Attribute
        (Seq2, Create_Attribute
           ("finalDefault", Lookup (G, "derivationSet"),
            Attribute_Use     => Default,
            Value             => ""));
      Add_Attribute
        (Seq2, Create_Attribute
           ("blockDefault", Lookup (G, "blockSet"),
            Attribute_Use     => Default,
            Value             => ""));
      Add_Attribute
        (Seq2, Create_Attribute
           ("attributeFormDefault", Lookup (G, "formChoice"),
            Attribute_Use  => Default,
            Value          => "unqualified"));
      Add_Attribute
        (Seq2, Create_Attribute
           ("elementFormDefault", Lookup (G, "formChoice"),
            Attribute_Use     => Default,
            Value             => "unqualified"));
      Add_Attribute (Seq2, Create_Attribute ("id", Lookup (G, "ID")));
      Register (G,
                Create_Element ("schema", Create_Type ("schema type", Seq2)));

      --  The "localSimpleType" type  ??? Invalid
      Register (G, Create_Type
                  ("localSimpleType",
                   Clone (Get_Validator (Lookup (G, "anySimpleType")))));

      --  The "localComplexType" type ??? invalid
      Register (G, Create_Type ("localComplexType", Unknown_Valid));

      --  The "identityConstraint" element  ??? invalid
      Register (G, Create_Element ("identityConstraint", Unknown));

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
      Typ := Extension_Of (Lookup (G, "annotated"), Seq1);
      Add_Attribute (Typ, Create_Attribute ("type", Lookup (G, "QName")));
      Add_Attribute
        (Typ, Create_Attribute ("substitutionGroup", Lookup (G, "QName")));
      Add_Attribute (Typ, Create_Attribute ("default", Lookup (G, "string")));
      Add_Attribute (Typ, Create_Attribute ("fixed", Lookup (G, "string")));
      Add_Attribute (Typ, Create_Attribute ("name", Lookup (G, "NCName")));
      Add_Attribute (Typ, Create_Attribute ("ref", Lookup (G, "QName")));
      Register (G, Create_Type ("element", Typ));

      --  The "annotation" element  ??? invalid
      Register (G, Create_Element ("annotation", Unknown));

      --  The "topLevelElement" type
      Seq1 := Create_Sequence;
      Set_Debug_Name (Seq1, "topLevelElement seq");
      Add_Particle (Seq1, Lookup_Element (G, "annotation"));
      Choice1 := Create_Choice (Min_Occurs => 0);
      Add_Particle (Seq1, Choice1);
      Add_Particle (Choice1, Create_Element
                      ("simpleType", Lookup (G, "localSimpleType")));
      Add_Particle (Choice1, Create_Element
                      ("complexType", Lookup (G, "localComplexType")));
      Add_Particle (Seq1, Lookup_Element (G, "identityConstraint"),
                    Min_Occurs => 0, Max_Occurs => Unbounded);
      Typ := Restriction_Of (Lookup (G, "element"), Seq1);
      Register (G, Create_Type ("topLevelElement", Typ));

      --  The "element" element
      Elem := Create_Element ("element", Lookup (G, "topLevelElement"));
      Set_Substitution_Group (Elem, Lookup_Element (G, "schemaTop"));
      Register (G, Elem);

      --  The "redefinable" element  --  abstract=true
      Elem := Create_Element ("redefinable", Lookup (G, "anyType"));
      Set_Substitution_Group (Elem, Lookup_Element (G, "schemaTop"));
      Register (G, Elem);

      --  The "all" element
--        Seq1 := Create_Sequence;
--        Add_Particle (Seq1, Lookup (G, "annotation"), Min_Occurs => 0);
      Register (G, Create_Element ("all", Unknown));

      --  The "localElement" type   ??? invalid
      Register (G, Create_Type
                  ("localElement",
                   Clone (Get_Validator (Lookup (G, "element")))));

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

      --  "group" type   ??? Missing attribute groups
      Typ := Extension_Of
        (Lookup (G, "annotated"),
         Lookup_Group (G, "particle"),
         Min_Occurs => 0, Max_Occurs => Unbounded);
      Register (G, Create_Type ("group", Typ));

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
      Typ := Restriction_Of (Lookup (G, "group"), Seq1);
      Register (G, Create_Type ("explicitGroup", Typ));
      Add_Attribute
        (Typ, Create_Attribute
           ("name", Lookup (G, "NCName"), Attribute_Use => Prohibited));
      Add_Attribute
        (Typ, Create_Attribute
           ("ref", Lookup (G, "QName"), Attribute_Use => Prohibited));

      --  The "choice" element
      Register (G, Create_Element ("choice", Lookup (G, "explicitGroup")));

      --  The "sequence" element
      Register (G, Create_Element ("sequence", Lookup (G, "explicitGroup")));

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
      Typ := Restriction_Of (Lookup (G, "group"), Seq1);
      Register (G, Create_Type ("realGroup", Typ));

      --  The "groupRef" type
      Seq1 := Create_Sequence;
      Set_Debug_Name (Seq1, "groupRef seq");
      Add_Particle (Seq1, Lookup_Element (G, "annotation"), Min_Occurs => 0);
      Typ := Restriction_Of (Lookup (G, "realGroup"), Seq1);
      Register (G, Create_Type ("groupRef", Typ));
      Add_Attribute
        (Typ, Create_Attribute
           ("ref", Lookup (G, "QName"), Attribute_Use => Required));
      Add_Attribute
        (Typ, Create_Attribute ("name", Attribute_Use => Prohibited));

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

      --  The "attribute" type   ??? invalid
      Register (G, Create_Type ("attribute", Unknown_Valid));

      --  The "anyAttributes" element  ??? invalid
      Register (G, Create_Element ("anyAttributes", Unknown));

      --  The "attributeGroupRef"  ??? invalid
      Register (G, Create_Type ("attributeGroupRef", Unknown_Valid));

      --  The "attrDecls" group
      Gr := Create_Group ("attrDecls");
      Register (G, Gr);
      Seq1 := Create_Sequence;
      Set_Debug_Name (Seq1, "attrDecls seq");
      Add_Particle (Gr, Seq1);
      Choice1 := Create_Choice (Min_Occurs => 0, Max_Occurs => 1);
      Add_Particle (Seq1, Choice1);
      Add_Particle
        (Choice1, Create_Element ("attribute", Lookup (G, "attribute")));
      Add_Particle
        (Choice1, Create_Element ("attributeGroup",
                                  Lookup (G, "attributeGroupRef")));
      Add_Particle
        (Seq1, Lookup_Element (G, "anyAttributes"), Min_Occurs => 0);

      --  The "extensionType" type
      Seq1 := Create_Sequence;
      Set_Debug_Name (Seq1, "extensionType seq");
      Add_Particle
        (Seq1, Lookup_Group (G, "typeDefParticle"), Min_Occurs => 0);
      Add_Particle
        (Seq1, Lookup_Group (G, "attrDecls"));
      Typ := Extension_Of (Lookup (G, "annotated"), Seq1);
      Add_Attribute (Typ, Create_Attribute ("base", Lookup (G, "QName")));
      Register (G, Create_Type ("extensionType", Typ));

      --  The "restrictionType" type ???
      Register (G, Create_Type ("restrictionType", Unknown_Valid));

      --  The "simpleRestrictionModel" group
      Gr := Create_Group ("simpleRestrictionModel");
      Register (G, Gr);

      --  The "simpleExtensionType"
      Seq1 := Create_Sequence;
      Set_Debug_Name (Seq1, "simpleExtensionType seq");
      Add_Particle (Seq1, Lookup_Element (G, "annotation"), Min_Occurs => 0);
      Add_Particle (Seq1, Lookup_Group (G, "attrDecls"));
      Register (G, Create_Type
                  ("simpleExtensionType",
                   Restriction_Of (Lookup (G, "extensionType"), Seq1)));

      --  The "simpleRestrictionType"
      Seq1 := Create_Sequence;
      Set_Debug_Name (Seq1, "simpleRestrictionType seq");
      Add_Particle (Seq1, Lookup_Element (G, "annotation"), Min_Occurs => 0);
      Add_Particle (Seq1, Lookup_Group (G, "simpleRestrictionModel"),
                    Min_Occurs => 0);
      Add_Particle (Seq1, Lookup_Group (G, "attrDecls"));
      Register (G, Create_Type
                  ("simpleRestrictionType",
                   Restriction_Of (Lookup (G, "restrictionType"), Seq1)));

      --  The "simpleContent" element
      Choice1 := Create_Choice;
      Add_Particle (Choice1, Create_Element
                      ("retriction",
                       Lookup (G, "simpleRestrictionType")));
      Add_Particle (Choice1, Create_Element
                      ("extension",
                       Lookup (G, "simpleExtensionType")));
      Typ := Extension_Of (Lookup (G, "annotated"), Choice1);
      Register (G, Create_Element ("simpleContent",
                                   Create_Type ("simpleContent type", Typ)));

      --  The "complexContent" element
      Register (G, Create_Element ("complexContent", Unknown));

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
      Register (G, Create_Type ("complexType", Typ));
      Add_Attribute (Typ, Create_Attribute ("name", Lookup (G, "NCName")));
      Add_Attribute (Typ, Create_Attribute ("mixed", Lookup (G, "boolean"),
                                            Attribute_Use => Default,
                                            Value => "false"));
      Add_Attribute (Typ, Create_Attribute ("abstract", Lookup (G, "boolean"),
                                            Attribute_Use => Default,
                                            Value => "false"));
      Add_Attribute
        (Typ, Create_Attribute ("final", Lookup (G, "derivationSet")));
      Add_Attribute
        (Typ, Create_Attribute ("block", Lookup (G, "derivationSet"),
                                Attribute_Use => Default,
                                Value => ""));


      --  The "topLevelComplexType" type
      Seq1 := Create_Sequence;
      Set_Debug_Name (Seq1, "topLevelComplexType seq");
      Add_Particle (Seq1, Lookup_Element (G, "annotation"), Min_Occurs => 0);
      Add_Particle (Seq1, Lookup_Group (G, "complexTypeModel"));
      Typ := Restriction_Of (Lookup (G, "complexType"), Seq1);
      Add_Attribute (Typ, Create_Attribute ("name", Lookup (G, "NCName"),
                                            Attribute_Use => Required));
      Register (G, Create_Type ("topLevelComplexType", Typ));

      --  The "complexType" element
      Elem := Create_Element
        ("complexType", Lookup (G, "topLevelComplexType"));
      Set_Substitution_Group (Elem, Lookup_Element (G, "redefinable"));
      Register (G, Elem);



      return G;
   end Create_Schema_Grammar;

   --------------------
   -- Start_Document --
   --------------------

   procedure Start_Document (Handler : in out Schema_Reader) is
   begin
      Set_Grammar (Handler, Create_Schema_Grammar);

      Free (Handler.Grammar);
      Initialize (Handler.Grammar);
   end Start_Document;

   -------------------
   -- Start_Element --
   -------------------

   procedure Start_Element
     (Handler       : in out Schema_Reader;
      Namespace_URI : Unicode.CES.Byte_Sequence := "";
      Local_Name    : Unicode.CES.Byte_Sequence := "";
      Qname         : Unicode.CES.Byte_Sequence := "";
      Atts          : Sax.Attributes.Attributes'Class)
   is
--      Next_Element : XML_Type;
   begin
      --  Check the grammar
      Start_Element (Validating_Reader (Handler),
                     Namespace_URI,
                     Local_Name,
                     Qname,
                     Atts);

      --  Process the element


--        if Namespace_URI /= Schema_URI then
--           Raise_Exception
--             (XML_Validation_Error'Identity,
--              "All elements must be in the schema namespace");
--
--        elsif Handler.Current_Element = null then
--           if Local_Name /= "schema" then
--              Raise_Exception
--                (XML_Validation_Error'Identity,
--                 "Root element must be <schema>");
--           end if;
--
--           Push (Handler.Current_Element, new Schema_Node_Validator, null);
--
--        else
--           Validate_Start_Element
--             (Handler.Current_Element.Validator, Local_Name,
--              Handler.Current_Element.Data, Next_Element);
--
--           if Next_Element = null
--             and then Local_Name = Str_Simple_Type
--           then
--              Next_Element := new Simple_XML_Type;
--           end if;
--
--           Push (Handler.Current_Element, Next_Element,
--                 Create_Validator_Data (Next_Element));
--        end if;
--
--        Validate_Attributes (Handler.Current_Element.Validator, Atts,
--                             Handler.Current_Element.Data);
   end Start_Element;

   ----------------
   -- Characters --
   ----------------

   procedure Characters
     (Handler : in out Schema_Reader; Ch : Unicode.CES.Byte_Sequence)
   is
   begin
      Characters (Validating_Reader (Handler), Ch);
   end Characters;

end Schema.Schema_Readers;
