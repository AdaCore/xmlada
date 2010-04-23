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

      Annotation, Any          : XML_Element;
      Openattrs                : XML_Type;
      reducedDerivationControl : XML_Type;
      derivationControl        : XML_Type;
      NMTOKEN, NCNAME, QNAME   : XML_Type;
      Bool, nonNegativeInteger : XML_Type;
      uriReference             : XML_Type;
      Str                      : XML_Type;
      numFacet                 : XML_Type;
      Token                    : XML_Type;
      Annotated                : XML_Type;
      localSimpleType          : XML_Type;
      localComplexType         : XML_Type;
      derivationSet            : XML_Type;
      formChoice               : XML_Type;
      identityConstraint       : XML_Element;
      Facet, SimpleDerivation  : XML_Element;
      Sequence, Choice, All_E  : XML_Element;
      Facet_Type               : XML_Type;
      Redefinable, SchemaTop   : XML_Element;
      maxBound, minBound       : XML_Element;
      attrDecls                : XML_Group;
      typeDefParticle          : XML_Group;
      defRef, Occurs           : XML_Attribute_Group;
      complexTypeModel         : XML_Group;
      groupDefParticle         : XML_Group;
      simpleRestrictionModel   : XML_Group;

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

      NMTOKEN            := Lookup (G, R, "NMTOKEN");
      NCNAME             := Lookup (G, R, "NCName");
      QNAME              := Lookup (G, R, "QName");
      Str                := Lookup (G, R, "string");
      localSimpleType    := Lookup (G, R, "localSimpleType");
      Bool               := Lookup (G, R, "boolean");
      nonNegativeInteger := Lookup (G, R, "nonNegativeInteger");
      Annotation  := Create_Global_Element (G, R, "annotation",  Qualified);
      Facet       := Create_Global_Element (G, R, "facet",       Qualified);
      Sequence    := Create_Global_Element (G, R, "sequence",    Qualified);
      Choice      := Create_Global_Element (G, R, "choice",      Qualified);
      All_E       := Create_Global_Element (G, R, "all",         Qualified);
      Redefinable := Create_Global_Element (G, R, "redefinable", Qualified);
      SchemaTop   := Create_Global_Element (G, R, "schemaTop",   Qualified);
      Any         := Create_Global_Element (G, R, "any",         Qualified);
      maxBound    := Create_Global_Element (G, R, "maxBound",    Qualified);
      minBound    := Create_Global_Element (G, R, "minBound",    Qualified);
      SimpleDerivation :=
        Create_Global_Element (G, R, "simpleDerivation", Qualified);
      attrDecls        := Create_Global_Group (G, R, "attrDecls");
      complexTypeModel := Create_Global_Group (G, R, "complexTypeModel");
      groupDefParticle := Create_Global_Group (G, R, "groupDefParticle");
      simpleRestrictionModel :=
        Create_Global_Group (G, R, "simpleRestrictionModel");
      defRef      := Create_Global_Attribute_Group (G, R, "defRef");
      Occurs      := Create_Global_Attribute_Group (G, R, "occurs");

      --  The "formChoice" type of schema.xsd
      Typ := Restriction_Of (G, R, NMTOKEN);
      Add_Facet (Typ, R, "enumeration", "qualified");
      Add_Facet (Typ, R, "enumeration", "unqualified");
      formChoice := Create_Global_Type (G, R, "formChoice", Typ);

      --  The "derivationControl" type
      Typ := Restriction_Of (G, R, NMTOKEN);
      Add_Facet (Typ, R, "enumeration", "substitution");
      Add_Facet (Typ, R, "enumeration", "extension");
      Add_Facet (Typ, R, "enumeration", "restriction");
      derivationControl :=
        Create_Global_Type (G, R, "derivationControl", Typ);

      --  The "blockSet" type
      Token := Lookup (G, R, "token");
      Typ := Restriction_Of (G, R, Token);
      Add_Facet (Typ, R, "enumeration", "#all");
      All_Validator := Create_Local_Type (G, Typ);

      Union := Create_Union (G);
      Add_Union (Union, R, All_Validator);
      Add_Union (Union, R, List_Of (G, derivationControl));
      Create_Global_Type (G, R, "blockSet", Union);

      --  The "reducedDerivationControl" type
      Typ := Restriction_Of (G, R, derivationControl);
      Add_Facet (Typ, R, "enumeration", "extension");
      Add_Facet (Typ, R, "enumeration", "restriction");
      reducedDerivationControl :=
        Create_Global_Type (G, R, "reducedDerivationControl", Typ);

      --  The "derivationSet" type
      Union := Create_Union (G);
      Add_Union (Union, R, All_Validator);
      Add_Union (Union, R, List_Of (G, reducedDerivationControl));
      derivationSet := Create_Global_Type (G, R, "derivationSet", Union);

      --  The "openAttrs" type
      Typ := Restriction_Of (G, R, Lookup (G, R, "anyType"));
      Add_Attribute
        (Typ, Create_Any_Attribute (G, Process_Lax, Kind => Namespace_Other));
      Openattrs := Create_Global_Type (G, R, "openAttrs", Typ);

      --  The "annotated" type
      Seq1 := Create_Sequence (G);
      Add_Particle (Seq1, R, Annotation, Min_Occurs => 0);
      Typ := Extension_Of (G, Openattrs, XML_Validator (Seq1));
      Add_Attribute
        (Typ, Create_Local_Attribute ("id", G, Lookup (G, R, "ID")));
      Annotated := Create_Global_Type (G, R, "annotated", Typ);

      --  The "schemaTop" element  ??? Missing abstract
      Set_Type (SchemaTop, R, Annotated);

      --  The "include" element
      uriReference := Lookup (G, R, "uriReference");
      Typ := Restriction_Of (G, R, Annotated);
      Add_Attribute
        (Typ, Create_Local_Attribute
           ("schemaLocation", G, uriReference, Attribute_Use => Required));
      Set_Type (Create_Global_Element (G, R, "include", Qualified), R,
                Create_Local_Type (G, Typ));

      --  The "import" element
      Typ := Restriction_Of (G, R, Annotated);
      Add_Attribute
        (Typ, Create_Local_Attribute ("namespace", G, uriReference));
      Add_Attribute
        (Typ, Create_Local_Attribute ("schemaLocation", G, uriReference));
      Set_Type (Create_Global_Element (G, R, "import", Qualified), R,
                Create_Local_Type (G, Typ));

      --  The "schema" element
      Choice1 := Create_Choice (G);
      Add_Particle (Choice1, R, Lookup_Element (G, R, "include"));
      Add_Particle (Choice1, R, Lookup_Element (G, R, "import"));
      Add_Particle (Choice1, R, Lookup_Element (G, R, "redefine"));
      Add_Particle (Choice1, R, Annotation);
      Seq1    := Create_Sequence (G);
      Add_Particle (Seq1, R, SchemaTop);
      Add_Particle (Seq1, R, Annotation,
                    Min_Occurs => 0, Max_Occurs => Unbounded);
      Seq2    := Create_Sequence (G);
      Add_Particle
        (Seq2, R, Choice1, Min_Occurs => 0, Max_Occurs => Unbounded);
      Add_Particle
        (Seq2, R, Seq1, Min_Occurs => 0, Max_Occurs => Unbounded);
      Add_Attribute
        (Seq2, Create_Local_Attribute ("targetNamespace", G, uriReference));
      Add_Attribute (Seq2, Create_Local_Attribute ("version", G, Token));
      Add_Attribute
        (Seq2, Create_Local_Attribute
           ("finalDefault", G, derivationSet,
            Attribute_Use     => Default));
      Add_Attribute
        (Seq2, Create_Local_Attribute
           ("blockDefault", G, Lookup (G, R, "blockSet"),
            Attribute_Use     => Default));
      Add_Attribute
        (Seq2, Create_Local_Attribute
           ("attributeFormDefault", G, formChoice,
            Attribute_Use => Default,
            Default       => "unqualified"));
      Add_Attribute
        (Seq2, Create_Local_Attribute
           ("elementFormDefault", G, formChoice,
            Attribute_Use => Default,
            Default       => "unqualified"));
      Add_Attribute
        (Seq2, Create_Local_Attribute ("id", G, Lookup (G, R, "ID")));
      Add_Attribute (Seq2, Lookup_Attribute
         (XML_G, R, Local_Name => "lang"));
      Set_Type (Create_Global_Element (G, R, "schema", Qualified), R,
                Create_Local_Type (G, Seq2));

      --  The "localComplexType" type
      Seq1 := Create_Sequence (G);
      Add_Particle (Seq1, R, Annotation, Min_Occurs => 0);
      Add_Particle (Seq1, R, complexTypeModel);
      Typ := Restriction_Of
        (G, R, Lookup (G, R, "complexType"), XML_Validator (Seq1));
      Add_Attribute
        (Typ, Create_Local_Attribute ("name", G, Attribute_Use => Prohibited));
      localComplexType := Create_Global_Type (G, R, "localComplexType", Typ);

      --  The "keybase" type
      Seq1 := Create_Sequence (G);
      Add_Particle (Seq1, R, Lookup_Element (G, R, "selector"));
      Add_Particle (Seq1, R, Lookup_Element (G, R, "field"),
                    Min_Occurs => 1, Max_Occurs => Unbounded);
      Typ := Extension_Of (G, Annotated, XML_Validator (Seq1));
      Add_Attribute
        (Typ, Create_Local_Attribute ("name", G, NCNAME,
                                      Attribute_Use => Required));
      Create_Global_Type (G, R, "keybase", Typ);

      --  The "identityConstraint" element  ??? abstract=true
      identityConstraint :=
        Create_Global_Element (G, R, "identityConstraint", Qualified);
      Set_Type (identityConstraint, R, Lookup (G, R, "keybase"));

      --  The "unique" element
      Elem := Create_Global_Element (G, R, "unique", Qualified);
      Set_Type (Elem, R, Get_Type (identityConstraint));
      Set_Substitution_Group (Elem, R, identityConstraint);

      --  The "keyref" element
      Typ := Extension_Of (G, Lookup (G, R, "keybase"));
      Add_Attribute (Typ, Create_Local_Attribute
                       ("refer", G, QNAME, Attribute_Use => Required));
      Elem := Create_Global_Element (G, R, "keyref", Qualified);
      Set_Type (Elem, R, Create_Local_Type (G, Typ));
      Set_Substitution_Group (Elem, R, identityConstraint);

      --  The "key" element
      Elem := Create_Global_Element (G, R, "key", Qualified);
      Set_Type (Elem, R, Get_Type (identityConstraint));
      Set_Substitution_Group (Elem, R, identityConstraint);

      --  The "XPathExprApprox" type  Incorrect pattern
      Typ := Restriction_Of (G, R, Str);
--    Add_Facet (Typ, "pattern", "(/|//|\.|\.\.|:|::|\||(\w-[.:/|])+)+");
      Create_Global_Type (G, R, "XPathExprApprox", Typ);

      --  The "XPathSpec" type"
      Typ := Restriction_Of (G, R, Annotated);
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
      Add_Union (Union, R, nonNegativeInteger);
      Typ := Restriction_Of (G, R, NMTOKEN);
      Add_Facet (Typ, R, "enumeration", "unbounded");
      Add_Union (Union, R, Create_Local_Type (G, Typ));
      Create_Global_Type (G, R, "allNNI", Union);

      --  The "occurs" AttributeGroup
      Add_Attribute
        (Occurs,
         Create_Local_Attribute ("minOccurs", G,
           nonNegativeInteger, Attribute_Use => Default, Default => "1"));
      Add_Attribute
        (Occurs,
         Create_Local_Attribute ("maxOccurs", G,
           Lookup (G, R, "allNNI"),
           Attribute_Use => Default, Default => "1"));

      --  From AttributeGroup "defRef"
      Add_Attribute (defRef, Create_Local_Attribute ("name", G, NCNAME));
      Add_Attribute (defRef, Create_Local_Attribute ("ref", G, QNAME));

      --  The "element" type   ??? abstract=true
      Seq1 := Create_Sequence (G);
      Choice1 := Create_Choice (G);
      Add_Particle (Choice1, R, Create_Local_Element
                      ("simpleType", G, localSimpleType, Qualified));
      Add_Particle (Choice1, R, Create_Local_Element
                      ("complexType", G, localComplexType, Qualified));
      Add_Particle (Seq1, R, Choice1, Min_Occurs => 0);
      Add_Particle (Seq1, R, identityConstraint,
                    Min_Occurs => 0, Max_Occurs => Unbounded);
      Typ := Extension_Of (G, Annotated, XML_Validator (Seq1));
      Create_Global_Type (G, R, "element", Typ);
      Add_Attribute_Group (Typ, R, Occurs);
      Add_Attribute_Group (Typ, R, defRef);
      Add_Attribute (Typ, Create_Local_Attribute ("type", G, QNAME));
      Add_Attribute
        (Typ, Create_Local_Attribute ("substitutionGroup", G, QNAME));
      Add_Attribute (Typ, Create_Local_Attribute ("default", G, Str));
      Add_Attribute (Typ, Create_Local_Attribute ("fixed", G, Str));
      Add_Attribute
        (Typ, Create_Local_Attribute ("nillable", G, Bool,
                                Attribute_Use => Default, Default => "false"));
      Add_Attribute
        (Typ, Create_Local_Attribute ("abstract", G, Bool,
                                Attribute_Use => Default, Default => "false"));
      Add_Attribute
        (Typ,
         Create_Local_Attribute ("final", G, derivationSet,
           Attribute_Use => Default));
      Add_Attribute
        (Typ,
         Create_Local_Attribute ("block", G, Lookup (G, R, "blockSet"),
           Attribute_Use => Default));
      Add_Attribute (Typ, Create_Local_Attribute ("form", G, formChoice));

      --  The "appinfo" element"
      Seq1 := Create_Sequence (G);
      Seq2 := Create_Sequence (G);
      Add_Particle (Seq1, R, Seq2, Min_Occurs => 0, Max_Occurs => Unbounded);
      Add_Particle
        (Seq2, R,
         Create_Any (Process_Contents => Process_Lax,
                     Namespace        => "##any",
                     Target_NS        => XML_G));
      Add_Attribute
        (Seq1, Create_Local_Attribute ("source", G, uriReference));
      Set_Mixed_Content (Seq1, True);
      Set_Type (Create_Global_Element (G, R, "appinfo", Qualified), R,
                Create_Local_Type (G, Seq1));

      --  The "documentation" element
      Seq1 := Create_Sequence (G);
      Seq2 := Create_Sequence (G);
      Add_Particle (Seq1, R, Seq2, Min_Occurs => 0, Max_Occurs => Unbounded);
      Add_Particle
        (Seq2, R,
         Create_Any (Process_Contents => Process_Lax,
                     Namespace        => "##any",
                     Target_NS        => XML_G));
      Add_Attribute
        (Seq1, Create_Local_Attribute ("source", G, uriReference));
      Add_Attribute (Seq1, Lookup_Attribute (XML_G, R, "lang"));
      Set_Mixed_Content (Seq1, True);
      Set_Type (Create_Global_Element (G, R, "documentation", Qualified), R,
                Create_Local_Type (G, Seq1));

      --  The "annotation" element  ??? invalid
      Seq1 := Create_Sequence (G);
      Choice1 := Create_Choice (G);
      Add_Particle
        (Seq1, R, Choice1, Min_Occurs => 0, Max_Occurs => Unbounded);
      Add_Particle (Choice1, R, Lookup_Element (G, R, "appinfo"));
      Add_Particle (Choice1, R, Lookup_Element (G, R, "documentation"));
      Typ := Extension_Of (G, Openattrs, XML_Validator (Seq1));
      Add_Attribute
        (Typ, Create_Local_Attribute ("id", G, Lookup (G, R, "ID")));
      Set_Type (Annotation, R, Create_Local_Type (G, Typ));

      --  The "topLevelElement" type
      Seq1 := Create_Sequence (G);
      Add_Particle (Seq1, R, Annotation, Min_Occurs => 0);
      Choice1 := Create_Choice (G);
      Add_Particle (Seq1, R, Choice1, Min_Occurs => 0);
      Add_Particle (Choice1, R, Create_Local_Element
                      ("simpleType", G, localSimpleType, Qualified));
      Add_Particle (Choice1, R, Create_Local_Element
                      ("complexType", G, localComplexType, Qualified));
      Add_Particle (Seq1, R, identityConstraint,
                    Min_Occurs => 0, Max_Occurs => Unbounded);
      Typ := Restriction_Of
        (G, R, Lookup (G, R, "element"), XML_Validator (Seq1));
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
        (Typ, Create_Local_Attribute
           ("name", G, NCNAME, Attribute_Use => Required));
      Create_Global_Type (G, R, "topLevelElement", Typ);

      --  The "element" element
      Elem := Create_Global_Element (G, R, "element", Qualified);
      Set_Type (Elem, R, Lookup (G, R, "topLevelElement"));
      Set_Substitution_Group (Elem, R, SchemaTop);

      --  The "attribute" element
      Elem := Create_Global_Element (G, R, "attribute", Qualified);
      Set_Type (Elem, R, Lookup (G, R, "topLevelAttribute"));
      Set_Substitution_Group (Elem, R, SchemaTop);

      --  The "redefinable" element  --  abstract=true
      Set_Type (Redefinable, R, Get_Type (SchemaTop));
      Set_Substitution_Group (Redefinable, R, SchemaTop);

      --  The "all" element
      Seq1 := Create_Sequence (G);
      Add_Particle (Seq1, R, Annotation, Min_Occurs => 0);

      Seq2 := Create_Sequence (G);
      Add_Particle (Seq2, R, Annotation, Min_Occurs => 0);
      Choice1 := Create_Choice (G);
      Add_Particle (Seq2, R, Choice1, Min_Occurs => 0);
      Add_Particle (Choice1, R, Create_Local_Element
                      ("simpleType", G, localSimpleType, Qualified));
      Add_Particle (Choice1, R, Create_Local_Element
                      ("complexType", G, localComplexType, Qualified));
      Add_Particle (Seq2, R, identityConstraint,
                    Min_Occurs => 0, Max_Occurs => Unbounded);
      Typ2 := Restriction_Of
        (G, R, Lookup (G, R, "localElement"), XML_Validator (Seq2));

      Typ := Restriction_Of (G, R, nonNegativeInteger);
      Add_Facet (Typ, R, "enumeration", "0");
      Add_Facet (Typ, R, "enumeration", "1");
      Add_Attribute
        (Typ2, Create_Local_Attribute
           ("minOccurs", G, Create_Local_Type (G, Typ),
            Attribute_Use => Default, Default => "1"));

      Typ := Restriction_Of (G, R, Lookup (G, R, "allNNI"));
      Add_Facet (Typ, R, "enumeration", "0");
      Add_Facet (Typ, R, "enumeration", "1");
      Add_Attribute
        (Typ2, Create_Local_Attribute
           ("maxOccurs", G, Create_Local_Type (G, Typ),
            Attribute_Use => Default, Default => "1"));

      Add_Particle (Seq1, R,
                    Create_Local_Element
                      ("element", G, Create_Local_Type (G, Typ2), Qualified),
                    Min_Occurs => 0, Max_Occurs => Unbounded);

      Typ := Restriction_Of
        (G, R, Lookup (G, R, "explicitGroup"), XML_Validator (Seq1));

      Typ2 := Restriction_Of (G, R, nonNegativeInteger);
      Add_Facet (Typ2, R, "enumeration", "1");
      Add_Attribute
        (Typ, Create_Local_Attribute
           ("minOccurs", G, Create_Local_Type (G, Typ2),
            Attribute_Use => Default, Default => "1"));

      Typ2 := Restriction_Of (G, R, Lookup (G, R, "allNNI"));
      Add_Facet (Typ2, R, "enumeration", "1");
      Add_Attribute
        (Typ, Create_Local_Attribute
           ("maxOccurs", G, Create_Local_Type (G, Typ2),
            Attribute_Use => Default, Default => "1"));

      Set_Type (All_E, R, Create_Local_Type (G, Typ));

      --  The "localElement" type
      Seq1 := Create_Sequence (G);
      Add_Particle (Seq1, R, Annotation, Min_Occurs => 0);
      Choice1 := Create_Choice (G);
      Add_Particle (Seq1, R, Choice1, Min_Occurs => 0);
      Add_Particle
        (Choice1, R, Create_Local_Element
           ("simpleType", G, localSimpleType, Qualified));
      Add_Particle
        (Choice1, R, Create_Local_Element
           ("complexType", G, localComplexType, Qualified));
      Add_Particle (Seq1, R, identityConstraint,
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
      Add_Particle (Gr, R, Choice1);
      Add_Particle
        (Choice1, R, Create_Local_Element
           ("element", G, Lookup (G, R, "localElement"), Qualified));
      Add_Particle
        (Choice1, R, Create_Local_Element
           ("group", G, Lookup (G, R, "groupRef"), Qualified));
      Add_Particle (Choice1, R, All_E);
      Add_Particle (Choice1, R, Choice);
      Add_Particle (Choice1, R, Sequence);
      Add_Particle (Choice1, R, Any);

      --  "group" type
      Typ := Extension_Of
        (G, R, Annotated,
         Lookup_Group (G, R, "particle"),
         Min_Occurs => 0, Max_Occurs => Unbounded);
      Create_Global_Type (G, R, "group", Typ);
      Add_Attribute_Group (Typ, R, defRef);
      Add_Attribute_Group (Typ, R, Occurs);

      --  The "nestedParticle" element
      Gr := Create_Global_Group (G, R, "nestedParticle");
      Choice1 := Create_Choice (G);
      Add_Particle (Gr, R, Choice1);
      Add_Particle
        (Choice1, R, Create_Local_Element
           ("element", G, Lookup (G, R, "localElement"), Qualified));
      Add_Particle
        (Choice1, R, Create_Local_Element
           ("group", G, Lookup (G, R, "groupRef"), Qualified));
      Add_Particle (Choice1, R, Choice);
      Add_Particle (Choice1, R, Sequence);
      Add_Particle (Choice1, R, Any);

      --  "explicitGroup" type
      Seq1 := Create_Sequence (G);
      Add_Particle (Seq1, R, Annotation, Min_Occurs => 0);
      Add_Particle (Seq1, R, Lookup_Group (G, R, "nestedParticle"),
                    Min_Occurs => 0, Max_Occurs => Unbounded);
      Typ := Restriction_Of
        (G, R, Lookup (G, R, "group"), XML_Validator (Seq1));
      Create_Global_Type (G, R, "explicitGroup", Typ);
      Add_Attribute
        (Typ, Create_Local_Attribute
           ("name", G, NCNAME, Attribute_Use => Prohibited));
      Add_Attribute
        (Typ, Create_Local_Attribute
           ("ref", G, QNAME, Attribute_Use => Prohibited));

      --  The "choice" element
      Set_Type (Choice, R, Lookup (G, R, "explicitGroup"));

      --  The "sequence" element
      Set_Type (Sequence, R, Lookup (G, R, "explicitGroup"));

      --  "groupDefParticle" group
      Choice1 := Create_Choice (G);
      Add_Particle (groupDefParticle, R, Choice1);
      Add_Particle (Choice1, R, All_E);
      Add_Particle (Choice1, R, Choice);
      Add_Particle (Choice1, R, Sequence);

      --  The "realGroup" type
      Seq1 := Create_Sequence (G);
      Add_Particle (Seq1, R, Annotation, Min_Occurs => 0);
      Add_Particle (Seq1, R, groupDefParticle,
                    Min_Occurs => 0, Max_Occurs => 1);
      Typ := Restriction_Of
        (G, R, Lookup (G, R, "group"), XML_Validator (Seq1));
      Create_Global_Type (G, R, "realGroup", Typ);

      --  The "groupRef" type
      Seq1 := Create_Sequence (G);
      Add_Particle (Seq1, R, Annotation, Min_Occurs => 0);
      Typ := Restriction_Of
        (G, R, Lookup (G, R, "realGroup"), XML_Validator (Seq1));
      Create_Global_Type (G, R, "groupRef", Typ);
      Add_Attribute
        (Typ, Create_Local_Attribute
           ("ref", G, QNAME, Attribute_Use => Required));
      Add_Attribute
        (Typ, Create_Local_Attribute ("name", G, Attribute_Use => Prohibited));

      --  The "group" element
      Elem := Create_Global_Element (G, R, "group", Qualified);
      Set_Type (Elem, R, Lookup (G, R, "namedGroup"));
      Set_Substitution_Group (Elem, R, Redefinable);

      --  The "namedGroup" type
      Seq1 := Create_Sequence (G);
      Add_Particle (Seq1, R, Annotation, Min_Occurs => 0);
      Add_Particle (Seq1, R, groupDefParticle);
      Typ := Restriction_Of
        (G, R, Lookup (G, R, "realGroup"), XML_Validator (Seq1));
      Add_Attribute (Typ, Create_Local_Attribute
                       ("name", G, NCNAME, Attribute_Use => Required));
      Add_Attribute (Typ, Create_Local_Attribute
                       ("ref", G, Attribute_Use => Prohibited));
      Add_Attribute (Typ, Create_Local_Attribute
                       ("minOccurs", G, Attribute_Use => Prohibited));
      Add_Attribute (Typ, Create_Local_Attribute
                       ("maxOccurs", G, Attribute_Use => Prohibited));
      Create_Global_Type (G, R, "namedGroup", Typ);

      --  The "attributeGroup" type
      Seq1 := Create_Sequence (G);
      Add_Particle (Seq1, R, attrDecls);
      Typ := Extension_Of (G, Annotated, XML_Validator (Seq1));
      Add_Attribute_Group (Typ, R, defRef);
      Create_Global_Type (G, R, "attributeGroup", Typ);

      --  The "namedAttributeGroup" type
      Seq1 := Create_Sequence (G);
      Add_Particle (Seq1, R, Annotation, Min_Occurs => 0);
      Add_Particle (Seq1, R, attrDecls);
      Typ := Restriction_Of
        (G, R, Lookup (G, R, "attributeGroup"), XML_Validator (Seq1));
      Add_Attribute
        (Typ, Create_Local_Attribute ("name", G, NCNAME,
                                Attribute_Use => Required));
      Add_Attribute
        (Typ, Create_Local_Attribute ("ref", G, Attribute_Use => Prohibited));
      Create_Global_Type (G, R, "namedAttributeGroup", Typ);

      --  The "attributeGroup" element
      Elem := Create_Global_Element (G, R, "attributeGroup", Qualified);
      Set_Type (Elem, R, Lookup (G, R, "namedAttributeGroup"));
      Set_Substitution_Group (Elem, R, Redefinable);

      --  The "typeDefParticle" group
      typeDefParticle := Create_Global_Group (G, R, "typeDefParticle");
      Choice1 := Create_Choice (G);
      Add_Particle (typeDefParticle, R, Choice1);
      Add_Particle (Choice1, R, Create_Local_Element
                      ("group", G, Lookup (G, R, "groupRef"), Qualified));
      Add_Particle (Choice1, R, All_E);
      Add_Particle (Choice1, R, Choice);
      Add_Particle (Choice1, R, Sequence);

      --  The "attribute" type
      Seq1 := Create_Sequence (G);
      Add_Particle (Seq1, R, Create_Local_Element
                      ("simpleType", G, localSimpleType, Qualified),
                    Min_Occurs => 0);
      Typ := Extension_Of (G, Annotated, XML_Validator (Seq1));
      Create_Global_Type (G, R, "attribute", Typ);
      Add_Attribute (Typ, Create_Local_Attribute ("type", G, QNAME));

      Typ2 := Restriction_Of (G, R, NMTOKEN);
      Add_Facet (Typ2, R, "enumeration", "prohibited");
      Add_Facet (Typ2, R, "enumeration", "optional");
      Add_Facet (Typ2, R, "enumeration", "required");
      Add_Attribute_Group (Typ, R, defRef);
      Add_Attribute (Typ, Create_Local_Attribute
                       ("use", G, Create_Local_Type (G, Typ2),
                        Attribute_Use => Default,
                        Default => "optional"));
      Add_Attribute (Typ, Create_Local_Attribute
                       ("default", G, Str, Attribute_Use => Optional));
      Add_Attribute (Typ, Create_Local_Attribute
                       ("fixed", G, Str, Attribute_Use => Optional));
      Add_Attribute (Typ, Create_Local_Attribute ("form", G, formChoice));

      --  The "topLevelAttribute" type
      Seq1 := Create_Sequence (G);
      Add_Particle (Seq1, R, Annotation, Min_Occurs => 0);
      Add_Particle (Seq1, R, Create_Local_Element
                      ("simpleType", G, localSimpleType, Qualified),
                    Min_Occurs => 0);
      Typ := Restriction_Of
        (G, R, Lookup (G, R, "attribute"), XML_Validator (Seq1));
      Create_Global_Type (G, R, "topLevelAttribute", Typ);
      Add_Attribute
        (Typ, Create_Local_Attribute ("ref", G, Attribute_Use => Prohibited));
      Add_Attribute
        (Typ, Create_Local_Attribute ("form", G, Attribute_Use => Prohibited));
      Add_Attribute
        (Typ, Create_Local_Attribute ("use", G, Attribute_Use => Prohibited));
      Add_Attribute
        (Typ, Create_Local_Attribute
           ("name", G, NCNAME, Attribute_Use => Required));

      --  The "anyAttributes" element
      Set_Type (Create_Global_Element (G, R, "anyAttribute", Qualified), R,
                Lookup (G, R, "wildcard"));

      --  The "namespaceList" type   ??? Incomplete
      Union := Create_Union (G);
      Typ := Restriction_Of (G, R, Token);
      Add_Facet (Typ, R, "enumeration", "##any");
      Add_Facet (Typ, R, "enumeration", "##other");
      Add_Union (Union, R, Create_Local_Type (G, Typ));

      Union2 := Create_Union (G);
      Add_Union (Union, R, Create_Local_Type (G, Union2));
      Add_Union (Union2, R, uriReference);
      Typ := Restriction_Of (G, R, Token);
      Add_Facet (Typ, R, "enumeration", "##targetNamespace");
      Add_Facet (Typ, R, "enumeration", "##local");
      Add_Union (Union2, R, Create_Local_Type (G, Typ));

      Create_Global_Type (G, R, "namespaceList", Union);

      --  The "wildcard" type
      Typ := Extension_Of (G, Annotated);
      Add_Attribute (Typ, Create_Local_Attribute ("namespace", G,
                                            Lookup (G, R, "namespaceList"),
                                            Attribute_Use => Default,
                                            Default => "##any"));
      Typ2 := Restriction_Of (G, R, NMTOKEN);
      Add_Facet (Typ2, R, "enumeration", "skip");
      Add_Facet (Typ2, R, "enumeration", "lax");
      Add_Facet (Typ2, R, "enumeration", "strict");
      Add_Attribute (Typ, Create_Local_Attribute ("processContents", G,
                                            Create_Local_Type (G, Typ2),
                                            Attribute_Use => Default,
                                            Default => "strict"));
      Create_Global_Type (G, R, "wildcard", Typ);

      --  The "any" element   ??? Error if you put before "wildcard"
      Typ := Extension_Of (G, Lookup (G, R, "wildcard"));
      Add_Attribute_Group (Typ, R, Occurs);
      Set_Type (Any, R, Create_Local_Type (G, Typ));

      --  The "attributeGroupRef"  ??? invalid
      Seq1 := Create_Sequence (G);
      Add_Particle (Seq1, R, Annotation, Min_Occurs => 0);
      Typ := Restriction_Of
        (G, R, Lookup (G, R, "attributeGroup"), XML_Validator (Seq1));
      Add_Attribute
        (Typ, Create_Local_Attribute
           ("ref", G, QNAME, Attribute_Use => Required));
      Add_Attribute
        (Typ, Create_Local_Attribute ("name", G, Attribute_Use => Prohibited));
      Create_Global_Type (G, R, "attributeGroupRef", Typ);

      --  The "attrDecls" group
      Seq1 := Create_Sequence (G);
      Add_Particle (attrDecls, R, Seq1);
      Choice1 := Create_Choice (G);
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
      Add_Particle (Seq1, R, typeDefParticle, Min_Occurs => 0);
      Add_Particle (Seq1, R, attrDecls);
      Typ := Extension_Of (G, Annotated, XML_Validator (Seq1));
      Add_Attribute (Typ, Create_Local_Attribute ("base", G, QNAME));
      Create_Global_Type (G, R, "extensionType", Typ);

      --  The "restrictionType" type
      Seq1 := Create_Sequence (G);
      Choice1 := Create_Choice (G);
      Add_Particle (Choice1, R, typeDefParticle, Min_Occurs => 0);
      Add_Particle (Choice1, R, simpleRestrictionModel, Min_Occurs => 0);
      Add_Particle (Seq1, R, Choice1);
      Add_Particle (Seq1, R, attrDecls);
      Typ := Extension_Of (G, Annotated, XML_Validator (Seq1));
      Add_Attribute (Typ, Create_Local_Attribute
                       ("base", G, QNAME, Attribute_Use => Required));
      Create_Global_Type (G, R, "restrictionType", Typ);

      --  The "simpleRestrictionModel" group
      Seq1 := Create_Sequence (G);
      Add_Particle (Seq1, R, Create_Local_Element
                    ("simpleType", G, localSimpleType, Qualified),
                    Min_Occurs => 0);
      Add_Particle (Seq1, R, Facet, Min_Occurs => 0, Max_Occurs => Unbounded);
      Add_Particle (simpleRestrictionModel, R, Seq1);

      --  The "simpleExtensionType"
      Seq1 := Create_Sequence (G);
      Add_Particle (Seq1, R, Annotation, Min_Occurs => 0);
      Add_Particle (Seq1, R, attrDecls);
      Create_Global_Type (G, R, "simpleExtensionType",
                Restriction_Of (G, R, Lookup (G, R, "extensionType"),
                                XML_Validator (Seq1)));

      --  The "simpleRestrictionType"
      Seq1 := Create_Sequence (G);
      Add_Particle (Seq1, R, Annotation, Min_Occurs => 0);
      Add_Particle (Seq1, R, simpleRestrictionModel, Min_Occurs => 0);
      Add_Particle (Seq1, R, attrDecls);
      Create_Global_Type (G, R, "simpleRestrictionType",
                Restriction_Of (G, R, Lookup (G, R, "restrictionType"),
                                XML_Validator (Seq1)));

      --  The "simpleContent" element
      Choice1 := Create_Choice (G);
      Add_Particle (Choice1, R, Create_Local_Element
                      ("restriction", G,
                       Lookup (G, R, "simpleRestrictionType"),
                       Qualified));
      Add_Particle (Choice1, R, Create_Local_Element
                      ("extension", G,
                       Lookup (G, R, "simpleExtensionType"),
                       Qualified));
      Typ := Extension_Of (G, Annotated, XML_Validator (Choice1));
      Set_Type (Create_Global_Element (G, R, "simpleContent", Qualified), R,
                Create_Local_Type (G, Typ));

      --  The "complexRestrictionType" type
      Seq1 := Create_Sequence (G);
      Add_Particle (Seq1, R, Annotation, Min_Occurs => 0);
      Add_Particle (Seq1, R, typeDefParticle, Min_Occurs => 0);
      Add_Particle (Seq1, R, attrDecls);
      Typ := Restriction_Of (G, R, Lookup (G, R, "restrictionType"),
                             XML_Validator (Seq1));
      Create_Global_Type (G, R, "complexRestrictionType", Typ);

      --  The "complexContent" element
      Choice1 := Create_Choice (G);
      Add_Particle
        (Choice1, R, Create_Local_Element
           ("restriction", G, Lookup (G, R, "complexRestrictionType"),
            Qualified));
      Add_Particle
        (Choice1, R, Create_Local_Element
           ("extension", G, Lookup (G, R, "extensionType"), Qualified));
      Add_Attribute (Choice1, Create_Local_Attribute ("mixed", G, Bool));
      Typ := Extension_Of (G, Annotated, XML_Validator (Choice1));
      Set_Type (Create_Global_Element (G, R, "complexContent", Qualified), R,
                Create_Local_Type (G, Typ));

      --  The "complexTypeModel" group
      Choice1 := Create_Choice (G);
      Add_Particle (complexTypeModel, R, Choice1);
      Add_Particle (Choice1, R, Lookup_Element (G, R, "simpleContent"));
      Add_Particle (Choice1, R, Lookup_Element (G, R, "complexContent"));
      Seq1 := Create_Sequence (G);
      Add_Particle (Choice1, R, Seq1);
      Add_Particle (Seq1, R, typeDefParticle, Min_Occurs => 0);
      Add_Particle (Seq1, R, attrDecls);

      --  The "complexType" type  ??? abstract=true
      Typ := Extension_Of (G, R, Annotated, complexTypeModel);
      Create_Global_Type (G, R, "complexType", Typ);
      Add_Attribute (Typ, Create_Local_Attribute ("name", G, NCNAME));
      Add_Attribute (Typ, Create_Local_Attribute
                       ("mixed", G, Bool, Attribute_Use => Default,
                        Default => "false"));
      Add_Attribute (Typ, Create_Local_Attribute
                       ("abstract", G, Bool, Attribute_Use => Default,
                        Default => "false"));
      Add_Attribute (Typ, Create_Local_Attribute ("final", G, derivationSet));
      Add_Attribute
        (Typ,
         Create_Local_Attribute ("block", G, derivationSet,
                                 Attribute_Use => Default));

      --  The "topLevelComplexType" type
      Seq1 := Create_Sequence (G);
      Add_Particle (Seq1, R, Annotation, Min_Occurs => 0);
      Add_Particle (Seq1, R, complexTypeModel);
      Typ := Restriction_Of (G, R, Lookup (G, R, "complexType"),
                             XML_Validator (Seq1));
      Add_Attribute (Typ, Create_Local_Attribute
                       ("name", G, NCNAME, Attribute_Use => Required));
      Create_Global_Type (G, R, "topLevelComplexType", Typ);

      --  The "complexType" element
      Elem := Create_Global_Element (G, R, "complexType", Qualified);
      Set_Type (Elem, R, Lookup (G, R, "topLevelComplexType"));
      Set_Substitution_Group (Elem, R, Redefinable);

      --  The "notation" element
      Typ := Restriction_Of (G, R, Annotated);
      Add_Attribute (Typ, Create_Local_Attribute
                       ("name", G, NCNAME, Attribute_Use => Required));
      Add_Attribute (Typ, Create_Local_Attribute
                       ("public", G, Lookup (G, R, "public"),
                        Attribute_Use => Required));
      Add_Attribute (Typ, Create_Local_Attribute
                       ("system", G, uriReference));
      Elem := Create_Global_Element (G, R, "notation", Qualified);
      Set_Type (Elem, R, Create_Local_Type (G, Typ));
      Set_Substitution_Group (Elem, R, SchemaTop);

      --  The "public" type
      Create_Global_Type (G, R, "public", Get_Validator (Token));

      --  The "redefine" element
      Seq1 := Create_Sequence (G);
      Choice1 := Create_Choice (G);
      Add_Particle
        (Seq1, R, Choice1, Min_Occurs => 0, Max_Occurs => Unbounded);
      Add_Particle (Choice1, R, Annotation);
      Add_Particle (Choice1, R, Redefinable);
      Add_Attribute
        (Seq1, Create_Local_Attribute
           ("schemaLocation", G, uriReference, Attribute_Use => Required));
      Typ := Extension_Of (G, Openattrs, XML_Validator (Seq1));
      Set_Type (Create_Global_Element (G, R, "redefine", Qualified), R,
                Create_Local_Type (G, Typ));

      --  From datatypes.xsd

      --  The "localSimpleType" type
      Seq1 := Create_Sequence (G);
      Add_Particle (Seq1, R, Annotation, Min_Occurs => 0);
      Add_Particle (Seq1, R, SimpleDerivation);
      Typ := Restriction_Of (G, R, Lookup (G, R, "simpleType"),
                             XML_Validator (Seq1));
      Add_Attribute (Typ, Create_Local_Attribute
                       ("name", G, Attribute_Use => Prohibited));
      localSimpleType := Create_Global_Type (G, R, "localSimpleType", Typ);

      --  The "simpleDerivation" element  ??? abstract=true
      Set_Type (SimpleDerivation, R, Annotated);

      --  The "simpleDerivationSet" type
      Union := Create_Union (G);
      Typ := Restriction_Of (G, R, Token);
      Add_Facet (Typ, R, "enumeration", "#all");
      Add_Union (Union, R, Create_Local_Type (G, Typ));
      Typ := Restriction_Of (G, R, derivationControl);
      Add_Facet (Typ, R, "enumeration", "list");
      Add_Facet (Typ, R, "enumeration", "union");
      Add_Facet (Typ, R, "enumeration", "restriction");
      Add_Facet (Typ, R, "enumeration", "extension");
      Add_Union (Union, R, List_Of (G, Create_Local_Type (G, Typ)));
      Create_Global_Type (G, R, "simpleDerivationSet", Union);

      --  The "simpleType" type  ??? abstract=true
      Seq1 := Create_Sequence (G);
      Add_Particle (Seq1, R, SimpleDerivation);
      Typ := Extension_Of (G, Annotated, XML_Validator (Seq1));
      Add_Attribute (Typ, Create_Local_Attribute ("name", G, NCNAME));
      Add_Attribute
        (Typ, Create_Local_Attribute
           ("final", G, Lookup (G, R, "simpleDerivationSet")));
      Create_Global_Type (G, R, "simpleType", Typ);

      --  The "topLevelSimpleType" type
      Seq1 := Create_Sequence (G);
      Add_Particle (Seq1, R, Annotation, Min_Occurs => 0);
      Add_Particle (Seq1, R, SimpleDerivation);
      Typ := Restriction_Of (G, R, Lookup (G, R, "simpleType"),
                             XML_Validator (Seq1));
      Create_Global_Type (G, R, "topLevelSimpleType", Typ);
      Add_Attribute
        (Typ, Create_Local_Attribute
           ("name", G, NCNAME, Attribute_Use => Required));
      Add_Attribute
        (Typ, Create_Any_Attribute (G, Process_Lax, Kind => Namespace_Other));

      --  The "simpleType" element
      Elem := Create_Global_Element (G, R, "simpleType", Qualified);
      Set_Type (Elem, R, Lookup (G, R, "topLevelSimpleType"));
      Set_Substitution_Group (Elem, R, Redefinable);

      --  The "restriction" element
      Typ := Extension_Of (G, R, Annotated, simpleRestrictionModel);
      Add_Attribute
        (Typ, Create_Local_Attribute ("base", G, QNAME,
                                      Attribute_Use => Optional));
      Elem := Create_Global_Element (G, R, "restriction", Qualified);
      Set_Type (Elem, R, Create_Local_Type (G, Typ));
      Set_Substitution_Group (Elem, R, SimpleDerivation);

      --  The "union" element
      Seq1 := Create_Sequence (G);
      Add_Particle (Seq1, R,
                    Create_Local_Element
                      ("simpleType", G, localSimpleType, Qualified),
                    Min_Occurs => 0, Max_Occurs => Unbounded);
      Typ := Extension_Of (G, Annotated, XML_Validator (Seq1));
      Add_Attribute
        (Typ, Create_Local_Attribute ("memberTypes", G,
                                List_Of (G, QNAME),
                                Attribute_Use => Optional));
      Elem := Create_Global_Element (G, R, "union", Qualified);
      Set_Type (Elem, R, Create_Local_Type (G, Typ));
      Set_Substitution_Group (Elem, R, SimpleDerivation);

      --  The "list" element
      Seq1 := Create_Sequence (G);
      Add_Particle (Seq1, R, Create_Local_Element
                      ("simpleType", G, localSimpleType, Qualified),
                    Min_Occurs => 0);
      Typ := Extension_Of (G, Annotated, XML_Validator (Seq1));
      Add_Attribute
        (Typ, Create_Local_Attribute ("itemType", G, QNAME,
         Attribute_Use => Optional));
      Elem := Create_Global_Element (G, R, "list", Qualified);
      Set_Type (Elem, R, Create_Local_Type (G, Typ));
      Set_Substitution_Group (Elem, R, SimpleDerivation);

      --  The "facet" type
      Typ := Restriction_Of (G, R, Annotated);
      Add_Attribute
        (Typ, Create_Local_Attribute ("value", G,
                                Lookup (G, R, "anySimpleType"),
                                Attribute_Use => Required));
      Add_Attribute
        (Typ, Create_Local_Attribute ("fixed", G, Bool,
                                Attribute_Use => Optional));
      Facet_Type := Create_Global_Type (G, R, "facet", Typ);

      --  The "numFacet" type
      Seq1 := Create_Sequence (G);
      Add_Particle (Seq1, R, Annotation, Min_Occurs => 0);
      Typ := Restriction_Of (G, R, Facet_Type, XML_Validator (Seq1));
      Add_Attribute
        (Typ, Create_Local_Attribute ("value", G, nonNegativeInteger));
      numFacet := Create_Global_Type (G, R, "numFacet", Typ);

      --  The "facet" element  ??? abstract=true
      Set_Type (Facet, R, Facet_Type);

      --  The "enumeration" element
      Elem := Create_Global_Element (G, R, "enumeration", Qualified);
      Set_Type (Elem, R, Get_Type (Facet));
      Set_Substitution_Group (Elem, R, Facet);

      --  The "pattern" element
      Elem := Create_Global_Element (G, R, "pattern", Qualified);
      Set_Type (Elem, R, Get_Type (Facet));
      Set_Substitution_Group (Elem, R, Facet);

      --  The "maxLength" element
      Elem := Create_Global_Element (G, R, "maxLength", Qualified);
      Set_Type (Elem, R, numFacet);
      Set_Substitution_Group (Elem, R, Facet);

      --  The "minLength" element
      Elem := Create_Global_Element (G, R, "minLength", Qualified);
      Set_Type (Elem, R, numFacet);
      Set_Substitution_Group (Elem, R, Facet);

      --  The "length" element
      Elem := Create_Global_Element (G, R, "length", Qualified);
      Set_Type (Elem, R, numFacet);
      Set_Substitution_Group (Elem, R, Facet);

      --  The "minBound" element
      Set_Type (minBound, R, Facet_Type);
      Set_Abstract (minBound, True);
      Set_Substitution_Group (minBound, R, Facet);

      --  The "minExclusive" element
      Elem := Create_Global_Element (G, R, "minExclusive", Qualified);
      Set_Type (Elem, R, Facet_Type);
      Set_Substitution_Group (Elem, R, minBound);

      --  The "minInclusive" element
      Elem := Create_Global_Element (G, R, "minInclusive", Qualified);
      Set_Type (Elem, R, Facet_Type);
      Set_Substitution_Group (Elem, R, minBound);

      --  The "maxBound" element
      Set_Type (maxBound, R, Facet_Type);
      Set_Abstract (maxBound, True);
      Set_Substitution_Group (maxBound, R, Facet);

      --  The "maxExclusive" element
      Elem := Create_Global_Element (G, R, "maxExclusive", Qualified);
      Set_Type (Elem, R, Facet_Type);
      Set_Substitution_Group (Elem, R, maxBound);

      --  The "maxInclusive" element
      Elem := Create_Global_Element (G, R, "maxInclusive", Qualified);
      Set_Type (Elem, R, Facet_Type);
      Set_Substitution_Group (Elem, R, maxBound);

      --  The "whiteSpace" element
      Elem := Create_Global_Element (G, R, "whiteSpace", Qualified);
      Set_Substitution_Group (Elem, R, Facet);
      Seq1 := Create_Sequence (G);
      Add_Particle (Seq1, R, Annotation, Min_Occurs => 0);
      Typ := Restriction_Of (G, R, Facet_Type, XML_Validator (Seq1));
      Typ2 := Restriction_Of (G, R, NMTOKEN);
      Add_Facet (Typ2, R, "enumeration", "preserve");
      Add_Facet (Typ2, R, "enumeration", "replace");
      Add_Facet (Typ2, R, "enumeration", "collapse");
      Add_Attribute
        (Typ, Create_Local_Attribute ("value", G,
                                      Create_Local_Type (G, Typ2)));
      Set_Type (Elem, R, Create_Local_Type (G, Typ));

      --  The "totalDigits" element
      Elem := Create_Global_Element (G, R, "totalDigits", Qualified);
      Set_Substitution_Group (Elem, R, Facet);
      Seq1 := Create_Sequence (G);
      Add_Particle (Seq1, R, Annotation, Min_Occurs => 0);
      Typ := Restriction_Of (G, R, numFacet, XML_Validator (Seq1));
      Add_Attribute
        (Typ, Create_Local_Attribute
           ("value", G, Lookup (G, R, "positiveInteger"),
           Attribute_Use => Required));
      Add_Attribute
        (Typ, Create_Any_Attribute (G, Process_Lax, Kind => Namespace_Other));
      Set_Type (Elem, R, Create_Local_Type (G, Typ));

      --  The "fractionDigits" element
      Elem := Create_Global_Element (G, R, "fractionDigits", Qualified);
      Set_Type (Elem, R, numFacet);
      Set_Substitution_Group (Elem, R, Facet);

      --  The namespace schema

      Create_Global_Attribute (XML_G, R, "base", Str);

      --  The schema instance namespace

      Create_Global_Attribute (XML_IG, R, "nil", Bool);
      Create_Global_Attribute (XML_IG, R, "type", QNAME);
      Create_Global_Attribute
        (XML_IG, R, "schemaLocation", List_Of (XML_IG, uriReference));
      Create_Global_Attribute
        (XML_IG, R, "noNamespaceSchemaLocation", uriReference);

      Global_Check (R, G);
   end Add_Schema_For_Schema;

end Schema.Validators.XSD_Grammar;
