with Unicode.CES;       use Unicode.CES;
with Sax.Attributes;    use Sax.Attributes;
with Schema.Validators; use Schema.Validators;
with Schema.Readers;    use Schema.Readers;
with Schema.Validators; use Schema.Validators;
with GNAT.IO;           use GNAT.IO;
with Ada.Unchecked_Deallocation;
with Ada.Exceptions;    use Ada.Exceptions;

package body Schema.Schema_Readers is

   Debug : Boolean := False;

   function Create_Schema_Grammar return XML_Grammar;
   --  Create the grammar to validate an XML Schema document

   procedure Free (Mapping : in out Prefix_Mapping_Access);
   --  Free the memory occupied by Mapping

   procedure Free (C : in out Context_Access);
   --  Free the memory occupied by C

   function Get_Namespace
     (Handler : Schema_Reader'Class;
      Prefix  : Byte_Sequence) return Byte_Sequence;
   --  Return the namespace corresponding to Prefix

   function Split_Qname (Qname : Byte_Sequence) return Natural;
   --  Return the position of the ':' separate in Qname

   procedure Output (Str : String);
   --  Output a debug string

   procedure Lookup_With_NS
     (Handler : in out Schema_Reader'Class;
      QName   : Byte_Sequence;
      Result  : out XML_Type);
   procedure Lookup_With_NS
     (Handler : in out Schema_Reader'Class;
      QName   : Byte_Sequence;
      Result  : out XML_Element);
   --  Lookup a type or element  with a possible namespace specification

   function Ada_Name (Element : XML_Element) return String;
   function Ada_Name (Typ : XML_Type) return String;
   --  Return the name of an Ada variable suitable to represent Element

   --------------
   -- Ada_Name --
   --------------

   function Ada_Name (Element : XML_Element) return String is
   begin
      return "E_" & Get_Local_Name (Element);
   end Ada_Name;

   --------------
   -- Ada_Name --
   --------------

   function Ada_Name (Typ : XML_Type) return String is
   begin
      return "T_" & Get_Local_Name (Typ);
   end Ada_Name;

   -----------------
   -- Split_Qname --
   -----------------

   function Split_Qname (Qname : Byte_Sequence) return Natural is
   begin
      --  ??? This function assumes we are using UTF8 internally
      for Q in Qname'Range loop
         if Qname (Q) = ':' then
            return Q;
         end if;
      end loop;
      return Qname'First - 1;
   end Split_Qname;

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
      Grammar                    : XML_Grammar;
      G, XML_G                   : XML_Grammar_NS;
      Typ, Typ2                  : XML_Validator;
      Seq1, Seq2                 : Sequence;
      Choice1                    : Choice;
      All_Validator              : XML_Type;
      Unknown                    : XML_Type;
      Elem                       : XML_Element;
      Gr                         : XML_Group;
      Union                      : XML_Union;
      Attr                       : XML_Attribute_Group;
   begin
      Initialize (Grammar);
      Get_NS (Grammar, XML_Schema_URI, G);
      Get_NS (Grammar, XML_URI, XML_G);

      Unknown := Lookup (G, "debug");

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

      Union := Create_Union;
      Add_Union (Union, All_Validator);
      Add_Union (Union, List_Of (Lookup (G, "derivationControl")));
      Register (G, Create_Type ("blockSet", Union));

      --  The "reducedDerivationControl" type
      Typ := Clone (Get_Validator (Lookup (G, "derivationControl")));
      Add_Restriction (Typ, "enumeration", "extension");
      Add_Restriction (Typ, "enumeration", "restriction");
      Register (G, Create_Type ("reducedDerivationControl", Typ));

      --  The "derivationSet" type
      Union := Create_Union;
      Add_Union (Union, All_Validator);
      Add_Union (Union, List_Of (Lookup (G, "reducedDerivationControl")));
      Register (G, Create_Type ("derivationSet", Union));

      --  The "uriReference" type
      Typ := Clone (Get_Validator (Lookup (G, "anySimpleType")));
      Add_Restriction (Typ, "whiteSpace", "collapse");
      Register (G, Create_Type ("uriReference", Typ));

      --  The "openAttrs" type  --  ??? <anyAttribute>
      Typ := Clone (Get_Validator (Lookup (G, "anyType")));
      Register (G, Create_Type ("openAttrs", Typ));

      --  The "annotated" type
      Seq1 := Create_Sequence;
      Add_Particle (Seq1, Lookup_Element (G, "annotation"), Min_Occurs => 0);
      Typ := Extension_Of (Lookup (G, "openAttrs"), Seq1);
      Add_Attribute (Typ, Create_Attribute ("id", G, Lookup (G, "ID")));
      Register (G, Create_Type ("annotated", Typ));

      --  The "schemaTop" element  ??? Missing abstract
      Register (G, Create_Element ("schemaTop", Lookup (G, "annotated")));

      --  The "include" element
      Typ := Clone (Get_Validator (Lookup (G, "annotated")));
      Add_Attribute
        (Typ, Create_Attribute ("schemaLocation", G,
                                Lookup (G, "uriReference"),
                                Attribute_Use => Required));
      Register (G, Create_Element ("include", Create_Type ("", Typ)));

      --  The "import" element
      Typ := Clone (Get_Validator (Lookup (G, "annotated")));
      Add_Attribute
        (Typ, Create_Attribute ("namespace", G,
                                Lookup (G, "uriReference")));
      Add_Attribute
        (Typ, Create_Attribute ("schemaLocation", G,
                                Lookup (G, "uriReference")));
      Register (G, Create_Element ("import", Create_Type ("", Typ)));

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
      Add_Particle (Seq2, Choice1, Min_Occurs => 0, Max_Occurs => Unbounded);
      Add_Particle (Seq2, Seq1, Min_Occurs => 0, Max_Occurs => Unbounded);
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
      Add_Attribute (Seq2, Create_Attribute ("id", G, Lookup (G, "ID")));
      Register (G,
                Create_Element ("schema", Create_Type ("schema type", Seq2)));

      --  The "localComplexType" type
      Seq1 := Create_Sequence;
      Set_Debug_Name (Seq1, "localComplexType seq");
      Add_Particle (Seq1, Lookup_Element (G, "annotation"), Min_Occurs => 0);
      Add_Particle (Seq1, Lookup_Group (G, "complexTypeModel"));
      Typ := Restriction_Of (Lookup (G, "complexType"), Seq1);
      Add_Attribute
        (Typ, Create_Attribute ("name", G, Attribute_Use => Prohibited));
      Register (G, Create_Type ("localComplexType", Typ));

      --  The "keybase" type
      Seq1 := Create_Sequence;
      Add_Particle (Seq1, Lookup_Element (G, "selector"));
      Add_Particle (Seq1, Lookup_Element (G, "field"),
                    Min_Occurs => 1, Max_Occurs => Unbounded);
      Typ := Extension_Of (Lookup (G, "annotated"), Seq1);
      Add_Attribute
        (Typ, Create_Attribute ("name", G, Lookup (G, "NCName"),
                              Attribute_Use => Required));
      Register (G, Create_Type ("keybase", Typ));

      --  The "identityConstraint" element  ??? abstract=true
      Register
        (G, Create_Element ("identityConstraint", Lookup (G, "keybase")));

      --  The "key" element
      Elem := Create_Element
        ("key",
         Get_Type (Lookup_Element (G, "identityConstraint")));
      Set_Substitution_Group
        (Elem, Lookup_Element (G, "identityConstraint"));
      Register (G, Elem);

      --  The "XPathExprApprox" type  Incorrect pattern
      Typ := Clone (Get_Validator (Lookup (G, "string")));
--    Add_Restriction (Typ, "pattern", "(/|//|\.|\.\.|:|::|\||(\w-[.:/|])+)+");
      Register (G, Create_Type ("XPathExprApprox", Typ));

      --  The "XPathSpec" type"
      Typ := Clone (Get_Validator (Lookup (G, "annotated")));
      Add_Attribute (Typ, Create_Attribute ("xpath", G,
                                            Lookup (G, "XPathExprApprox")));
      Register (G, Create_Type ("XPathSpec", Typ));

      --  The "selector" element
      Register (G, Create_Element ("selector", Lookup (G, "XPathSpec")));

      --  The "field" element
      Register (G, Create_Element ("field", Lookup (G, "XPathSpec")));

      --  The "allNNI" type"
      Union := Create_Union;
      Add_Union (Union, Lookup (G, "nonNegativeInteger"));
      Typ   := Clone (Get_Validator (Lookup (G, "NMTOKEN")));
      Add_Restriction (Typ, "enumeration", "unbounded");
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
      Typ := Extension_Of (Lookup (G, "annotated"), Seq1);
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
        (Typ, Create_Attribute ("nullable", G, Lookup (G, "boolean"),
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
      Register
        (G, Create_Element ("appinfo", Create_Type ("", Seq1)));

      --  The "documentation" element
      Seq1 := Create_Sequence (Min_Occurs => 0, Max_Occurs => Unbounded);
      --   <any processContents="lax" />
      Add_Attribute
        (Seq1, Create_Attribute ("source", G, Lookup (G, "uriReference")));
      Add_Attribute (Seq1, Lookup_Attribute (XML_G, "lang"));
      Set_Mixed_Content (Seq1, True);
      Register
        (G, Create_Element ("documentation", Create_Type ("", Seq1)));

      --  The "annotation" element  ??? invalid
      Choice1 := Create_Choice (Min_Occurs => 0, Max_Occurs => Unbounded);
      Set_Debug_Name (Choice1, "annotation choice");
      Add_Particle (Choice1, Lookup_Element (G, "appinfo"));
      Add_Particle (Choice1, Lookup_Element (G, "documentation"));
      Register (G, Create_Element ("annotation", Create_Type ("", Choice1)));

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
--        Seq1 := Create_Sequence;
--        Add_Particle (Seq1, Lookup (G, "annotation"), Min_Occurs => 0);
      Register (G, Create_Element ("all", Unknown));

      --  The "localElement" type
      Seq1 := Create_Sequence;
      Add_Particle (Seq1, Lookup_Element (G, "annotation"), Min_Occurs => 0);
      Choice1 := Create_Choice;
      Add_Particle (Seq1, Choice1, Min_Occurs => 0);
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
      Typ := Restriction_Of (Lookup (G, "element"), Seq1);
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
      Typ := Restriction_Of (Lookup (G, "group"), Seq1);
      Register (G, Create_Type ("explicitGroup", Typ));
      Add_Attribute
        (Typ, Create_Attribute
           ("name", G, Lookup (G, "NCName"), Attribute_Use => Prohibited));
      Add_Attribute
        (Typ, Create_Attribute
           ("ref", G, Lookup (G, "QName"), Attribute_Use => Prohibited));

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
      Typ := Restriction_Of (Lookup (G, "realGroup"), Seq1);
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
      Typ := Extension_Of (Lookup (G, "annotated"), Seq1);
      Add_Attribute_Group (Typ, Lookup_Attribute_Group (G, "defRef"));
      Register (G, Create_Type ("attributeGroup", Typ));

      --  The "namedAttributeGroup" type
      Seq1 := Create_Sequence;
      Add_Particle (Seq1, Lookup_Element (G, "annotation"), Min_Occurs => 0);
      Add_Particle (Seq1, Lookup_Group (G, "attrDecls"));
      Typ := Restriction_Of (Lookup (G, "attributeGroup"), Seq1);
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
      Typ := Extension_Of (Lookup (G, "annotated"), Seq1);
      Set_Debug_Name (Typ, "attribute extension");
      Register (G, Create_Type ("attribute", Typ));
      Add_Attribute (Typ, Create_Attribute ("type", G, Lookup (G, "QName")));

      Typ2 := Clone (Get_Validator (Lookup (G, "NMTOKEN")));
      Add_Restriction (Typ2, "enumeration", "prohibited");
      Add_Restriction (Typ2, "enumeration", "optional");
      Add_Restriction (Typ2, "enumeration", "required");
      Add_Restriction (Typ2, "enumeration", "default");
      Add_Restriction (Typ2, "enumeration", "fixed");
      Add_Attribute_Group (Typ, Lookup_Attribute_Group (G, "defRef"));
      Add_Attribute (Typ, Create_Attribute
                       ("use", G, Create_Type ("use_type", Typ2),
                        Attribute_Use => Default,
                        Value => "optional"));
      Add_Attribute (Typ, Create_Attribute
                       ("value", G, Lookup (G, "string"),
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
      Typ := Restriction_Of (Lookup (G, "attribute"), Seq1);
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
      Register (G, Create_Element ("anyAttribute", Lookup (G, "wildcard")));

      --  The "namespaceList" type   ??? Incomplete
      Union := Create_Union;
      Typ := Clone (Get_Validator (Lookup (G, "token")));
      Add_Restriction (Typ, "enumeration", "##any");
      Add_Restriction (Typ, "enumeration", "##other");
      Add_Union (Union, Create_Type ("", Typ));
      Register (G, Create_Type ("namespaceList", Union));

      --  The "wildcard" type
      Typ := Clone (Get_Validator (Lookup (G, "annotated")));
      Add_Attribute (Typ, Create_Attribute ("namespace", G,
                                            Lookup (G, "namespaceList"),
                                            Attribute_Use => Default,
                                            Value => "##any"));
      Typ2 := Clone (Get_Validator (Lookup (G, "NMTOKEN")));
      Add_Restriction (Typ2, "enumeration", "skip");
      Add_Restriction (Typ2, "enumeration", "lax");
      Add_Restriction (Typ2, "enumeration", "strict");
      Add_Attribute (Typ, Create_Attribute ("processContents", G,
                                            Create_Type ("", Typ2),
                                            Attribute_Use => Default,
                                            Value => "strict"));
      Register (G, Create_Type ("wildcard", Typ));

      --  The "any" element   ??? Error if you put before "wildcard"
      Typ := Clone (Get_Validator (Lookup (G, "wildcard")));
      Add_Attribute_Group (Typ, Lookup_Attribute_Group (G, "occurs"));
      Register (G, Create_Element ("any", Create_Type ("", Typ)));

      --  The "attributeGroupRef"  ??? invalid
      Seq1 := Create_Sequence;
      Add_Particle (Seq1, Lookup_Element (G, "annotation"), Min_Occurs => 0);
      Typ := Restriction_Of (Lookup (G, "attributeGroup"), Seq1);
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
      Choice1 := Create_Choice;
      Set_Debug_Name (Choice1, "attrDecls choice");
      Add_Particle (Seq1, Choice1, Min_Occurs => 0, Max_Occurs => Unbounded);
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
      Typ := Extension_Of (Lookup (G, "annotated"), Seq1);
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
      Typ := Extension_Of (Lookup (G, "annotated"), Seq1);
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
      Set_Debug_Name (Choice1, "simpleContent choice");
      Add_Particle (Choice1, Create_Element
                      ("retriction",
                       Lookup (G, "simpleRestrictionType")));
      Add_Particle (Choice1, Create_Element
                      ("extension",
                       Lookup (G, "simpleExtensionType")));
      Typ := Extension_Of (Lookup (G, "annotated"), Choice1);
      Set_Debug_Name (Typ, "simpleContent extension");
      Register (G, Create_Element ("simpleContent",
                                   Create_Type ("simpleContent type", Typ)));

      --  The "complexRestrictionType" type
      Seq1 := Create_Sequence;
      Set_Debug_Name (Seq1, "complexRestrictionType seq");
      Add_Particle (Seq1, Lookup_Element (G, "annotation"), Min_Occurs => 0);
      Add_Particle
        (Seq1, Lookup_Group (G, "typeDefParticle"), Min_Occurs => 0);
      Add_Particle (Seq1, Lookup_Group (G, "attrDecls"));
      Typ := Restriction_Of (Lookup (G, "restrictionType"), Seq1);
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
      Typ := Extension_Of (Lookup (G, "annotated"), Choice1);
      Register (G, Create_Element ("complexContent", Create_Type ("", Typ)));

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
      Typ := Restriction_Of (Lookup (G, "complexType"), Seq1);
      Add_Attribute (Typ, Create_Attribute ("name", G, Lookup (G, "NCName"),
                                            Attribute_Use => Required));
      Register (G, Create_Type ("topLevelComplexType", Typ));

      --  The "complexType" element
      Elem := Create_Element
        ("complexType", Lookup (G, "topLevelComplexType"));
      Set_Substitution_Group (Elem, Lookup_Element (G, "redefinable"));
      Register (G, Elem);

      --  The "notation" element
      Typ := Clone (Get_Validator (Lookup (G, "annotated")));
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
      Typ := Restriction_Of (Lookup (G, "simpleType"), Seq1);
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
      Typ := Restriction_Of (Lookup (G, "simpleType"), Seq1);
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
      Typ := Extension_Of (Lookup (G, "annotated"), Seq1);
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
      Typ := Extension_Of (Lookup (G, "annotated"), Seq1);
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
      Typ := Extension_Of (Lookup (G, "annotated"), Seq1);
      Add_Attribute
        (Typ, Create_Attribute ("itemType", G, Lookup (G, "QName"),
                                Attribute_Use => Optional));
      Elem := Create_Element ("list", Create_Type ("", Typ));
      Set_Substitution_Group (Elem, Lookup_Element (G, "simpleDerivation"));
      Register (G, Elem);

      --  The "facet" type
      Typ := Clone (Get_Validator (Lookup (G, "annotated")));
      Add_Attribute
        (Typ, Create_Attribute ("value", G,
                                Lookup (G, "anySimpleType"),
                                Attribute_Use => Required));
      Add_Attribute
        (Typ, Create_Attribute ("fixed", G, Lookup (G, "boolean"),
                                Attribute_Use => Optional));
      Register (G, Create_Type ("facet", Typ));

      --  The "facet" element  ??? abstract=true
      Register (G, Create_Element ("facet", Lookup (G, "facet")));

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

      return Grammar;
   end Create_Schema_Grammar;

   --------------------
   -- Start_Document --
   --------------------

   procedure Start_Document (Handler : in out Schema_Reader) is
   begin
      Set_Grammar (Handler, Create_Schema_Grammar);

      Free (Handler.Grammar);
      Initialize (Handler.Grammar);

      Get_NS (Handler.Grammar, "", Handler.Target_NS);
   end Start_Document;

   ----------------------
   -- Set_Debug_Output --
   ----------------------

   procedure Set_Debug_Output (Output : Boolean) is
   begin
      Debug := Output;
   end Set_Debug_Output;

   ------------
   -- Output --
   ------------

   procedure Output (Str : String) is
   begin
      if Debug then
         Put_Line (ASCII.ESC & "[34m" & Str & ASCII.ESC & "[39m");
      end if;
   end Output;

   --------------------
   -- Lookup_With_NS --
   --------------------

   procedure Lookup_With_NS
     (Handler : in out Schema_Reader'Class;
      QName   : Byte_Sequence;
      Result  : out XML_Type)
   is
      Separator : constant Integer := Split_Qname (QName);
      G         : XML_Grammar_NS;
   begin
      if Separator < QName'First then
         Result := Lookup (Handler.Target_NS, QName);
         Output
           (Ada_Name (Result) & " := Lookup (Handler.Target_NS, """
            & QName & """);");
      else
         Get_NS
           (Handler.Grammar,
            Get_Namespace (Handler, QName (QName'First .. Separator - 1)),
            G);
         Output
           ("Get_NS (Handler.Grammar, """
            & Get_Namespace (Handler, QName (QName'First .. Separator - 1))
            & """, G);");
         Result := Lookup (G, QName (Separator + 1 .. QName'Last));
         Output
           (Ada_Name (Result) & " := Lookup (G, """
            & QName (Separator + 1 .. QName'Last) & """);");
      end if;
   end Lookup_With_NS;

   --------------------
   -- Lookup_With_NS --
   --------------------

   procedure Lookup_With_NS
     (Handler : in out Schema_Reader'Class;
      QName   : Byte_Sequence;
      Result  : out XML_Element)
   is
      Separator : constant Integer := Split_Qname (QName);
      G         : XML_Grammar_NS;
   begin
      if Separator < QName'First then
         Result := Lookup_Element (Handler.Target_NS, QName);
         Output
           (Ada_Name (Result)
            & " := Lookup_Element (Handler.Target_NS, """ & QName & """);");
      else
         Get_NS
           (Handler.Grammar,
            Get_Namespace (Handler, QName (QName'First .. Separator - 1)),
            G);
         Output
           ("Get_NS (Handler.Grammar, """
            & Get_Namespace (Handler, QName (QName'First .. Separator - 1))
            & """, G);");
         Result := Lookup_Element (G, QName (Separator + 1 .. QName'Last));
         Output
           (Ada_Name (Result)
            & " := Lookup_Element (G, """
            & QName (Separator + 1 .. QName'Last) & """);");
      end if;
   end Lookup_With_NS;

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
      procedure Push_In_Context (Element   : XML_Element);
      --  Insert a new item in the parent context, depending on the latter's
      --  type

      function Ada_Name (C : Context_Access) return String;
      --  Return the Ada_Image for that element

      --------------
      -- Ada_Name --
      --------------

      function Ada_Name (C : Context_Access) return String is
         L : constant String := Integer'Image (C.Level);
      begin
         case C.Typ is
            when Context_Schema =>
               return "";
            when Context_Choice =>
               return "Choice" & L (L'First + 1 .. L'Last);
            when Context_Sequence =>
               return "Seq" & L (L'First + 1 .. L'Last);
            when Context_Element =>
               return Ada_Name (C.Element);
            when Context_Complex_Type =>
               return "T_" & C.Complex_Type_Name.all;
            when Context_Attribute =>
               return "A_" & Get_Local_Name (C.Attribute);
         end case;
      end Ada_Name;

      ---------------------
      -- Push_In_Context --
      ---------------------

      procedure Push_In_Context (Element : XML_Element) is
      begin
         case Handler.Contexts.Typ is
            when Context_Schema =>
               Register (Handler.Target_NS, Element);
               Output ("Register (Handler.Target_NS, "
                       & Ada_Name (Element) & ");");
            when Context_Sequence =>
               Add_Particle (Handler.Contexts.Seq, Element);
               Output ("Add_Particle (" & Ada_Name (Handler.Contexts)
                       & ", " & Ada_Name (Element) & ");");
            when Context_Choice =>
               Add_Particle (Handler.Contexts.C, Element);
               Output ("Add_Particle (" & Ada_Name (Handler.Contexts)
                       & ", " & Ada_Name (Element) & ");");
            when others =>
               Output ("Can't handle nested element decl");
         end case;
      end Push_In_Context;


      Index  : Integer;
      Element, Group : XML_Element;
      Typ    : XML_Type;
      Min_Occurs, Max_Occurs : Integer;
--      Att    : Attribute_Validator;
   begin
      --  Check the grammar
      Start_Element (Validating_Reader (Handler),
                     Namespace_URI,
                     Local_Name,
                     Qname,
                     Atts);

      --  Process the element

      if Handler.Contexts = null then
         if Local_Name /= "schema" then
            Raise_Exception
              (XML_Validation_Error'Identity,
               "Root element must be <schema>");
         end if;

         Handler.Contexts := new Context'
           (Typ         => Context_Schema,
            Level       => 0,
            Next        => null);

      elsif Local_Name = "annotation" then
         null;

      elsif Local_Name = "element" then
         Typ := No_Type;

         Index := Get_Index (Atts, URI => "", Local_Name => "type");
         if Index /= -1 then
            Lookup_With_NS (Handler, Get_Value (Atts, Index), Result => Typ);
--            Set_Type (Element, Typ);
--              Output ("Set_Type (" & Ada_Name (Element) & ", "
--                      & Ada_Name (Typ) & ");");
         end if;

         Index   := Get_Index (Atts, URI => "", Local_Name => "name");
         if Index /= -1 then
            Element := Create_Element (Get_Value (Atts, Index), Typ);
            Output
              (Ada_Name (Element) & " := Create_Element ("""
               & Get_Value (Atts, Index) & """, " & Ada_Name (Typ) & ");");
            Push_In_Context (Element);
         end if;

         Index := Get_Index
           (Atts, URI => "", Local_Name => "substitutionGroup");
         if Index /= -1 then
            Lookup_With_NS (Handler, Get_Value (Atts, Index), Result => Group);
            Set_Substitution_Group (Element, Group);
            Output ("Set_Substitution_Group ("
                    & Ada_Name (Element) & ", " & Ada_Name (Group) & ");");
         end if;

         Handler.Contexts := new Context'
           (Typ         => Context_Element,
            Element     => Element,
            Level       => Handler.Contexts.Level + 1,
            Next        => Handler.Contexts);

      elsif Local_Name = "complexType" then
         null;
--           Index := Get_Index (Atts, URI => "", Local_Name => "name");
--
--           Handler.Contexts := new Context'
--             (Typ               => Context_Complex_Type,
--            Complex_Type_Name => new Byte_Sequence'(Get_Value (Atts, Index)),
--              Complex_Type_Validator => null,
--              Level                  => Handler.Contexts.Level + 1,
--              Next                   => Handler.Contexts);

      elsif Local_Name = "sequence" then
         --  ??? Should get the default from the attributes definition
--           Index := Get_Index (Atts, URI => "", Local_Name => "minOccurs");
--           if Index /= -1 then
--              Min_Occurs := Integer'Value (Get_Value (Atts, Index));
--           else
         Min_Occurs := 1;
--           end if;

         --  ??? Should get the default from the attributes definition
--           Index := Get_Index (Atts, URI => "", Local_Name => "maxOccurs");
--           if Index /= -1 then
--              Max_Occurs := Integer'Value (Get_Value (Atts, Index));
--           else
         Max_Occurs := 1;
--           end if;

         Handler.Contexts := new Context'
           (Typ      => Context_Sequence,
            Seq      => Create_Sequence (Min_Occurs, Max_Occurs),
            Level     => Handler.Contexts.Level + 1,
            Next     => Handler.Contexts);
         Output (Ada_Name (Handler.Contexts) & " := Create_Sequence ("
                 & Min_Occurs'Img & ',' & Max_Occurs'Img & ")");

         case Handler.Contexts.Next.Typ is
            when Context_Complex_Type =>
               Handler.Contexts.Next.Complex_Type_Validator :=
                 XML_Validator (Handler.Contexts.Seq);
               Output ("Validator := " & Ada_Name (Handler.Contexts) & ";");
            when Context_Sequence =>
               Add_Particle (Handler.Contexts.Next.Seq, Handler.Contexts.Seq);
               Output ("Add_Particle (" & Ada_Name (Handler.Contexts.Next)
                       & ", " & Ada_Name (Handler.Contexts) & ");");
            when Context_Choice =>
               Add_Particle (Handler.Contexts.Next.C, Handler.Contexts.Seq);
               Output ("Add_Particle (" & Ada_Name (Handler.Contexts.Next)
                       & ", " & Ada_Name (Handler.Contexts) & ");");
            when Context_Schema | Context_Attribute | Context_Element =>
               Output ("Can't handle nested sequence");
         end case;

      elsif Local_Name = "choice" then
         --  ??? Should get the default from the attributes definition
--           Index := Get_Index (Atts, URI => "", Local_Name => "minOccurs");
--           if Index /= -1 then
--              Min_Occurs := Integer'Value (Get_Value (Atts, Index));
--           else
         Min_Occurs := 1;
--           end if;

         --  ??? Should get the default from the attributes definition
--           Index := Get_Index (Atts, URI => "", Local_Name => "maxOccurs");
--           if Index /= -1 then
--              Max_Occurs := Integer'Value (Get_Value (Atts, Index));
--           else
         Max_Occurs := 1;
--           end if;

         Handler.Contexts := new Context'
           (Typ      => Context_Choice,
            C        => Create_Choice (Min_Occurs, Max_Occurs),
            Level    => Handler.Contexts.Level + 1,
            Next     => Handler.Contexts);
         Output (Ada_Name (Handler.Contexts) & " := Create_Choice ("
                 & Min_Occurs'Img & ',' & Max_Occurs'Img & ")");

         case Handler.Contexts.Next.Typ is
            when Context_Complex_Type =>
               Handler.Contexts.Next.Complex_Type_Validator :=
                 XML_Validator (Handler.Contexts.C);
               Output ("Validator := " & Ada_Name (Handler.Contexts) & ";");
            when Context_Sequence =>
               Add_Particle (Handler.Contexts.Next.Seq, Handler.Contexts.C);
               Output ("Add_Particle (" & Ada_Name (Handler.Contexts.Next)
                       & ", " & Ada_Name (Handler.Contexts) & ");");
            when Context_Choice =>
               Add_Particle (Handler.Contexts.Next.C, Handler.Contexts.C);
               Output ("Add_Particle (" & Ada_Name (Handler.Contexts.Next)
                       & ", " & Ada_Name (Handler.Contexts) & ");");
            when Context_Schema | Context_Attribute | Context_Element =>
               Output ("Can't handle nested sequence");
         end case;

      elsif Local_Name = "attribute" then
         null;
--           Att := Create_Attribute
--            (Local_Name => Get_Value (Atts, URI => "", Local_Name => "name"),
--              NS         => Handler.Target_NS,
--              Attribute_Type => No_Type,
--              Attribute_Use  => Optional,
--              Attribute_Form => Qualified,
--              Value          => "");
--
--           Handler.Contexts := new Context'
--             (Typ        => Context_Attribute,
--              Attribute  => Att,
--              Level      => Handler.Contexts.Level + 1,
--              Next       => Handler.Contexts);
--           Output (Ada_Name (Handler.Contexts) & " := Create_Attribute ("""
--                   & Get_Value (Atts, URI => "", Local_Name => "name")
--                   & """);");
--
--           case Handler.Contexts.Next.Typ is
--              when Context_Complex_Type =>
--                 Add_Attribute
--                   (Handler.Contexts.Next.Complex_Type_Validator, Att);
--                 Output ("Add_Attribute ("
--                         & Ada_Name (Handler.Contexts.Next) & ", "
--                         & Ada_Name (Handler.Contexts));
--
--              when Context_Schema =>
--                 Register (Att);
--                 Output ("Register (Handler.Target_NS, "
--                         & Ada_Name (Handler.Contexts) & ");");
--
--              when Context_Element | Context_Sequence | Context_Choice
--                 | Context_Attribute =>
--                 Output ("Can't handle attribute decl in this context");
--           end case;


      else
         Output ("Tag not handled yet: " & Local_Name);
      end if;
   end Start_Element;

   -----------------
   -- End_Element --
   -----------------

   procedure End_Element
     (Handler       : in out Schema_Reader;
      Namespace_URI : Unicode.CES.Byte_Sequence := "";
      Local_Name    : Unicode.CES.Byte_Sequence := "";
      Qname         : Unicode.CES.Byte_Sequence := "")
   is
      C : Context_Access := Handler.Contexts;
--      Typ : XML_Type;
      Handled : Boolean := True;
   begin
      --  Check the grammar
      End_Element (Validating_Reader (Handler),
                     Namespace_URI,
                     Local_Name,
                     Qname);

      --  Process the tag

      if Local_Name = "element" then
         null;

      elsif Local_Name = "schema" then
         --  ??? Check there remains no undefined forward declaration
         null;

      elsif Local_Name = "complexType" then
         Handled := False;
--           Typ := Create_Type
--             (C.Complex_Type_Name.all, C.Complex_Type_Validator);
--           Register (Handler.Target_NS, Typ);
--
--           Output ("Typ := Create_Type ("""
--                   & C.Complex_Type_Name.all & ", Validator);");
--           Output ("Register (Handler.Target_NS, Typ);");

      elsif Local_Name = "sequence"
        or else Local_Name = "choice"
      then
         null;

      elsif Local_Name = "attribute" then
         Handled := False;

      else
         Output ("Close tag not handled yet: " & Local_Name);
         Handled := False;
      end if;

      --  Free the context
      if Handled then
         Handler.Contexts := Handler.Contexts.Next;
         Free (C);
      end if;
   end End_Element;

   ----------
   -- Free --
   ----------

   procedure Free (C : in out Context_Access) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Context, Context_Access);
   begin
      if C /= null then
         case C.Typ is
         when Context_Complex_Type =>
            Free (C.Complex_Type_Name);
         when others =>
            null;
         end case;
         Unchecked_Free (C);
      end if;
   end Free;

   ----------------
   -- Characters --
   ----------------

   procedure Characters
     (Handler : in out Schema_Reader; Ch : Unicode.CES.Byte_Sequence)
   is
   begin
      Characters (Validating_Reader (Handler), Ch);
   end Characters;

   ----------
   -- Free --
   ----------

   procedure Free (Mapping : in out Prefix_Mapping_Access) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Prefix_Mapping, Prefix_Mapping_Access);
   begin
      if Mapping /= null then
         Free (Mapping.Prefix);
         Free (Mapping.Namespace);
         Unchecked_Free (Mapping);
      end if;
   end Free;

   --------------------------
   -- Start_Prefix_Mapping --
   --------------------------

   procedure Start_Prefix_Mapping
     (Handler : in out Schema_Reader;
      Prefix  : Unicode.CES.Byte_Sequence;
      URI     : Unicode.CES.Byte_Sequence) is
   begin
      Handler.Prefixes := new Prefix_Mapping'
        (Prefix    => new Byte_Sequence'(Prefix),
         Namespace => new Byte_Sequence'(URI),
         Next      => Handler.Prefixes);
   end Start_Prefix_Mapping;

   ------------------------
   -- End_Prefix_Mapping --
   ------------------------

   procedure End_Prefix_Mapping
     (Handler : in out Schema_Reader;
      Prefix  : Unicode.CES.Byte_Sequence)
   is
      Tmp  : Prefix_Mapping_Access := Handler.Prefixes;
      Tmp2 : Prefix_Mapping_Access;
   begin
      if Handler.Prefixes /= null then
         if Handler.Prefixes.Prefix.all = Prefix then
            Handler.Prefixes := Handler.Prefixes.Next;
            Free (Tmp);
         else
            while Tmp.Next /= null
              and then Tmp.Next.Prefix.all /= Prefix
            loop
               Tmp := Tmp.Next;
            end loop;

            if Tmp.Next /= null then
               Tmp2 := Tmp.Next;
               Tmp.Next := Tmp2.Next;
               Free (Tmp2);
            end if;
         end if;
      end if;
   end End_Prefix_Mapping;

   -------------------
   -- Get_Namespace --
   -------------------

   function Get_Namespace
     (Handler : Schema_Reader'Class;
      Prefix  : Byte_Sequence) return Byte_Sequence
   is
      Tmp : Prefix_Mapping_Access := Handler.Prefixes;
   begin
      while Tmp /= null loop
         if Tmp.Prefix.all = Prefix then
            return Tmp.Namespace.all;
         end if;
         Tmp := Tmp.Next;
      end loop;
      return "";
   end Get_Namespace;

end Schema.Schema_Readers;
