with Unicode.CES;       use Unicode.CES;
with Sax.Attributes;    use Sax.Attributes;
with Schema.Validators; use Schema.Validators;
with Schema.Readers;    use Schema.Readers;
with Schema.Validators; use Schema.Validators;
with Schema.Schema_Grammar; use Schema.Schema_Grammar;
with GNAT.IO;           use GNAT.IO;
with Ada.Unchecked_Deallocation;
with Ada.Exceptions;    use Ada.Exceptions;

package body Schema.Schema_Readers is

   Debug : Boolean := False;

   procedure Free (Mapping : in out Prefix_Mapping_Access);
   --  Free the memory occupied by Mapping

   procedure Free (C : in out Context_Access);
   --  Free the memory occupied by C

   procedure Get_Grammar_For_Namespace
     (Handler : in out Schema_Reader'Class;
      Prefix  : Byte_Sequence;
      Grammar : out XML_Grammar_NS);
   --  Return the grammar matching a given prefix

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
   function Ada_Name (Typ : XML_Type)        return String;
   function Ada_Name (C : Context_Access)    return String;
   --  Return the name of an Ada variable suitable to represent Element

   procedure Create_Element
     (Handler : in out Schema_Reader; Atts : Sax.Attributes.Attributes'Class);
   procedure Create_Complex_Type
     (Handler : in out Schema_Reader; Atts : Sax.Attributes.Attributes'Class);
   procedure Create_Restriction
     (Handler : in out Schema_Reader; Atts : Sax.Attributes.Attributes'Class);
   procedure Create_All
     (Handler : in out Schema_Reader; Atts : Sax.Attributes.Attributes'Class);
   procedure Create_Sequence
     (Handler : in out Schema_Reader; Atts : Sax.Attributes.Attributes'Class);
   procedure Create_Attribute
     (Handler : in out Schema_Reader; Atts : Sax.Attributes.Attributes'Class);
   procedure Create_Schema
     (Handler : in out Schema_Reader; Atts : Sax.Attributes.Attributes'Class);
   procedure Create_Extension
     (Handler : in out Schema_Reader; Atts : Sax.Attributes.Attributes'Class);
   procedure Create_List
     (Handler : in out Schema_Reader; Atts : Sax.Attributes.Attributes'Class);
   --  Create a new context for a specific tag:
   --  resp. <element>, <complexType>, <restriction>, <all>, <sequence>,
   --  <attribute>, <schema>, <extension>, <list>

   procedure Finish_Element (Handler : in out Schema_Reader);
   procedure Finish_Complex_Type (Handler : in out Schema_Reader);
   procedure Finish_Restriction (Handler : in out Schema_Reader);
   procedure Finish_All (Handler : in out Schema_Reader);
   procedure Finish_Sequence (Handler : in out Schema_Reader);
   procedure Finish_Attribute (Handler : in out Schema_Reader);
   procedure Finish_Extension (Handler : in out Schema_Reader);
   --  Finish the handling of various tags:
   --  resp. <element>, <complexType>, <restriction>, <all>, <sequence>,
   --  <extension>

   function Max_Occurs_From_Value (Value : Byte_Sequence) return Integer;
   --  Return the value of maxOccurs from the attributes'value. This properly
   --  takes into account the "unbounded" case

   ---------------------------
   -- Max_Occurs_From_Value --
   ---------------------------

   function Max_Occurs_From_Value (Value : Byte_Sequence) return Integer is
   begin
      if Value = "unbounded" then
         return Unbounded;
      else
         return Integer'Value (Value);
      end if;
   end Max_Occurs_From_Value;

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

   -------------------------
   -- Get_Created_Grammar --
   -------------------------

   function Get_Created_Grammar
     (Reader : Schema_Reader) return Schema.Validators.XML_Grammar is
   begin
      return Reader.Grammar;
   end Get_Created_Grammar;

   -------------------------
   -- Set_Created_Grammar --
   -------------------------

   procedure Set_Created_Grammar
     (Reader  : in out Schema_Reader;
      Grammar : Schema.Validators.XML_Grammar) is
   begin
      Reader.Grammar := Grammar;
   end Set_Created_Grammar;

   --------------------
   -- Start_Document --
   --------------------

   procedure Start_Document (Handler : in out Schema_Reader) is
   begin
      if Handler.Grammar = No_Grammar then
         Handler.Grammar := Create_Schema_For_Schema;
      end if;

      Handler.Target_NS := null;
      Get_NS (Handler.Grammar, XML_Schema_URI, Handler.Schema_NS);
   end Start_Document;

   ------------------
   -- End_Document --
   ------------------

   procedure End_Document (Handler : in out Schema_Reader) is
   begin
      Global_Check (Handler.Target_NS);
   end End_Document;

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
         Get_Grammar_For_Namespace (Handler, "", G);
      else
         Get_Grammar_For_Namespace
           (Handler, QName (QName'First .. Separator - 1), G);
      end if;

      Result := Lookup (G, QName (Separator + 1 .. QName'Last));
      Output
        (Ada_Name (Result) & " := Lookup (G, """
         & QName (Separator + 1 .. QName'Last) & """);");
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
         Get_Grammar_For_Namespace (Handler, "", G);
      else
         Get_Grammar_For_Namespace
           (Handler, QName (QName'First .. Separator - 1), G);
      end if;

      Result := Lookup_Element (G, QName (Separator + 1 .. QName'Last));
      Output
        (Ada_Name (Result)
         & " := Lookup_Element (G, """
         & QName (Separator + 1 .. QName'Last) & """);");
   end Lookup_With_NS;

   --------------------
   -- Create_Element --
   --------------------

   procedure Create_Element
     (Handler  : in out Schema_Reader;
      Atts     : Sax.Attributes.Attributes'Class)
   is
      Type_Index : constant Integer :=
        Get_Index (Atts, URI => "", Local_Name => "type");
      Name_Index : constant Integer :=
        Get_Index (Atts, URI => "", Local_Name => "name");
      Ref_Index : constant Integer :=
        Get_Index (Atts, URI => "", Local_Name => "ref");
      Subst_Index : constant Integer :=
        Get_Index (Atts, URI => "", Local_Name => "substitutionGroup");
      Default_Index : constant Integer :=
        Get_Index (Atts, URI => "", Local_Name => "default");
      Fixed_Index : constant Integer :=
        Get_Index (Atts, URI => "", Local_Name => "fixed");
      Min_Occurs_Index : constant Integer :=
        Get_Index (Atts, URI => "", Local_Name => "minOccurs");
      Max_Occurs_Index : constant Integer :=
        Get_Index (Atts, URI => "", Local_Name => "maxOccurs");
      Abstract_Index : constant Integer :=
        Get_Index (Atts, URI => "", Local_Name => "abstract");
      Nillable_Index : constant Integer :=
        Get_Index (Atts, URI => "", Local_Name => "nillable");

      Min_Occurs, Max_Occurs : Integer := 1;
      Element : XML_Element;
      Typ     : XML_Type := No_Type;
      Group   : XML_Element;

   begin
      if Name_Index /= -1 then
         if Type_Index /= -1 then
            Lookup_With_NS
              (Handler, Get_Value (Atts, Type_Index), Result => Typ);
         end if;

         Element := Create_Element (Get_Value (Atts, Name_Index), Typ);
         Output
           (Ada_Name (Element) & " := Create_Element ("""
            & Get_Value (Atts, Name_Index) & """, " & Ada_Name (Typ) & ");");

         if Ref_Index /= -1
           and then Get_Value (Atts, Name_Index) =
             Get_Value (Atts, Ref_Index)
         then
            Raise_Exception
              (XML_Validation_Error'Identity,
               """ref"" attribute cannot be self-referencing");
         end if;

      elsif Ref_Index = -1 then
         Raise_Exception
           (XML_Validation_Error'Identity,
            "Either ""name"" or ""ref"" attribute must be present");

      else
         Lookup_With_NS
           (Handler, Get_Value (Atts, Ref_Index), Result => Element);

         --  Section 3.3.2, validity constraints 3.3.3
         if Type_Index /= -1 then
            Raise_Exception
              (XML_Validation_Error'Identity,
               """type"" attribute cannot be specified along with ""ref""");
         end if;
      end if;

      if Subst_Index /= -1 then
         Lookup_With_NS
           (Handler, Get_Value (Atts, Subst_Index), Result => Group);
         Set_Substitution_Group (Element, Group);
         Output ("Set_Substitution_Group ("
                 & Ada_Name (Element) & ", " & Ada_Name (Group) & ");");
      end if;

      if Default_Index /= -1 then
         Set_Default (Element, Get_Value (Atts, Default_Index));
         Output ("Set_Default ("
                 & Ada_Name (Element) & ", """
                 & Get_Value (Atts, Default_Index) & """);");
      end if;

      if Fixed_Index /= -1 then
         Set_Fixed (Element, Get_Value (Atts, Fixed_Index));
         Output ("Set_Fixed ("
                 & Ada_Name (Element) & ", """
                 & Get_Value (Atts, Fixed_Index) & """);");
      end if;

      if Abstract_Index /= -1 then
         Set_Abstract (Element, Get_Value_As_Boolean (Atts, Abstract_Index));
         Output ("Set_Abstract ("
                 & Ada_Name (Element) & ", "
                 & Boolean'Image
                   (Get_Value_As_Boolean (Atts, Abstract_Index)) & ");");
      end if;

      if Nillable_Index /= -1 then
         Set_Nillable (Element, Get_Value_As_Boolean (Atts, Nillable_Index));
         Output ("Set_Nillable ("
                 & Ada_Name (Element) & ", "
                 & Boolean'Image
                   (Get_Value_As_Boolean (Atts, Nillable_Index)) & ");");
      end if;

      if Min_Occurs_Index /= -1 then
         Min_Occurs := Integer'Value (Get_Value (Atts, Min_Occurs_Index));
      end if;

      if Max_Occurs_Index /= -1 then
         Max_Occurs := Max_Occurs_From_Value
           (Get_Value (Atts, Max_Occurs_Index));

         --  Imposed by test elemJ001.xsd, but not sure why
         if Max_Occurs = 0 then
            Min_Occurs := 0;
         end if;
      end if;

      case Handler.Contexts.Typ is
         when Context_Schema =>
            Register (Handler.Target_NS, Element);
            Output ("Register (Handler.Target_NS, "
                    & Ada_Name (Element) & ");");
         when Context_Sequence =>
            Add_Particle
              (Handler.Contexts.Seq, Element, Min_Occurs, Max_Occurs);
            Output ("Add_Particle (" & Ada_Name (Handler.Contexts)
                    & ", " & Ada_Name (Element) & ','
                    & Min_Occurs'Img & ',' & Max_Occurs'Img & ");");
         when Context_Choice =>
            Add_Particle (Handler.Contexts.C, Element, Min_Occurs, Max_Occurs);
            Output ("Add_Particle (" & Ada_Name (Handler.Contexts)
                    & ", " & Ada_Name (Element) & ','
                    & Min_Occurs'Img & ',' & Max_Occurs'Img & ");");
         when Context_All =>
            Add_Particle (Handler.Contexts.All_Validator, Element,
                          Min_Occurs, Max_Occurs);
            Output ("Add_Particle (" & Ada_Name (Handler.Contexts)
                    & ", " & Ada_Name (Element) & ','
                    & Min_Occurs'Img & ',' & Max_Occurs'Img & ");");
         when others =>
            Output ("Can't handle nested element decl");
      end case;


      Handler.Contexts := new Context'
        (Typ            => Context_Element,
         Element        => Element,
         Level          => Handler.Contexts.Level + 1,
         Next           => Handler.Contexts);
   end Create_Element;

   --------------------
   -- Finish_Element --
   --------------------

   procedure Finish_Element (Handler : in out Schema_Reader) is
   begin
      if Get_Type (Handler.Contexts.Element) = No_Type then
         Set_Type (Handler.Contexts.Element,
                   Lookup (Handler.Schema_NS, "anyType"));
         Output ("Set_Type (" & Ada_Name (Handler.Contexts)
                 & ", Lookup (Handler.Schema_NS, ""anyType"");");
      end if;
   end Finish_Element;

   -------------------------
   -- Create_Complex_Type --
   -------------------------

   procedure Create_Complex_Type
     (Handler  : in out Schema_Reader;
      Atts     : Sax.Attributes.Attributes'Class)
   is
      Name_Index : constant Integer :=
        Get_Index (Atts, URI => "", Local_Name => "name");
      Name       : Byte_Sequence_Access;
   begin
      if Name_Index /= -1 then
         Name := new Byte_Sequence'(Get_Value (Atts, Name_Index));
      end if;

      Handler.Contexts := new Context'
        (Typ            => Context_Type_Def,
         Type_Name      => Name,
         Type_Validator => null,
         Level          => Handler.Contexts.Level + 1,
         Next           => Handler.Contexts);
   end Create_Complex_Type;

   -------------------------
   -- Finish_Complex_Type --
   -------------------------

   procedure Finish_Complex_Type (Handler : in out Schema_Reader) is
      C   : constant Context_Access := Handler.Contexts;
      Typ : XML_Type;
   begin
      if C.Type_Name = null then
         Typ := Create_Type ("", C.Type_Validator);
         Output (Ada_Name (C) & " := Create_Type ("""", Validator);");
      else
         Typ := Create_Type (C.Type_Name.all, C.Type_Validator);
         Output (Ada_Name (C)
                 & " := Create_Type (""" & C.Type_Name.all
                 & """, Validator);");
      end if;

      case Handler.Contexts.Next.Typ is
         when Context_Schema =>
            Register (Handler.Target_NS, Typ);
            Output ("Register (Handler.Target_NS, " & Ada_Name (C) & ");");
         when Context_Element =>
            Set_Type (Handler.Contexts.Next.Element, Typ);
            Output ("Set_Type (" & Ada_Name (Handler.Contexts.Next)
                    & ", " & Ada_Name (C) & ");");
         when Context_Attribute =>
            Set_Type (Handler.Contexts.Next.Attribute, Typ);
            Output ("Set_Type (" & Ada_Name (Handler.Contexts.Next)
                    & ", " & Ada_Name (Typ) & ");");
         when others =>
            Output ("Can't handle nested type decl");
      end case;
   end Finish_Complex_Type;

   ------------------------
   -- Create_Restriction --
   ------------------------

   procedure Create_Restriction
     (Handler  : in out Schema_Reader;
      Atts     : Sax.Attributes.Attributes'Class)
   is
      Base_Index : constant Integer :=
        Get_Index (Atts, URI => "", Local_Name => "base");
      Base : XML_Type;
   begin
      if Handler.Contexts.Type_Name /= null
        and then Get_Value (Atts, Base_Index) = Handler.Contexts.Type_Name.all
      then
         Raise_Exception
           (XML_Validation_Error'Identity,
            "Self-referencing restriction not allowed");
      end if;

      Lookup_With_NS (Handler, Get_Value (Atts, Base_Index), Result => Base);

      Handler.Contexts := new Context'
        (Typ            => Context_Restriction,
         Restriction    => Restriction_Of (Base, null),
         Level          => Handler.Contexts.Level + 1,
         Next           => Handler.Contexts);
      Output (Ada_Name (Handler.Contexts)
              & " := Restriction_Of (" & Ada_Name (Base) & ", null);");
   end Create_Restriction;

   ------------------------
   -- Finish_Restriction --
   ------------------------

   procedure Finish_Restriction (Handler : in out Schema_Reader) is
   begin
      case Handler.Contexts.Next.Typ is
         when Context_Type_Def =>
            Handler.Contexts.Next.Type_Validator :=
              Handler.Contexts.Restriction;

         when others =>
            Output ("Can't handler nested restrictions");
      end case;
   end Finish_Restriction;

   ----------------------
   -- Create_Extension --
   ----------------------

   procedure Create_Extension
     (Handler  : in out Schema_Reader;
      Atts     : Sax.Attributes.Attributes'Class)
   is
      Base_Index : constant Integer :=
        Get_Index (Atts, URI => "", Local_Name => "base");
      Base : XML_Type;
   begin
      if Handler.Contexts.Type_Name /= null
        and then Get_Value (Atts, Base_Index) = Handler.Contexts.Type_Name.all
      then
         Raise_Exception
           (XML_Validation_Error'Identity,
            "Self-referencing restriction not allowed");
      end if;

      Lookup_With_NS (Handler, Get_Value (Atts, Base_Index), Result => Base);

      Handler.Contexts := new Context'
        (Typ            => Context_Extension,
         Extension      => Extension_Of (Base, null),
         Level          => Handler.Contexts.Level + 1,
         Next           => Handler.Contexts);
      Output (Ada_Name (Handler.Contexts)
              & " := Extension_Of (" & Ada_Name (Base) & ", null);");
   end Create_Extension;

   ----------------------
   -- Finish_Extension --
   ----------------------

   procedure Finish_Extension (Handler : in out Schema_Reader) is
   begin
      case Handler.Contexts.Next.Typ is
         when Context_Type_Def =>
            Handler.Contexts.Next.Type_Validator :=
              Handler.Contexts.Extension;
            Output ("Validator := " & Ada_Name (Handler.Contexts) & ";");

         when others =>
            Output ("Can't handle nested extensions");
      end case;
   end Finish_Extension;

   -----------------
   -- Create_List --
   -----------------

   procedure Create_List
     (Handler : in out Schema_Reader;
      Atts    : Sax.Attributes.Attributes'Class)
   is
      Item_Type_Index : constant Integer :=
        Get_Index (Atts, URI => "", Local_Name => "itemType");
      Items : XML_Type;
   begin
      case Handler.Contexts.Typ is
         when Context_Type_Def =>
            Lookup_With_NS
              (Handler, Get_Value (Atts, Item_Type_Index), Result => Items);
            Handler.Contexts.Type_Validator :=
              Get_Validator (List_Of (Items));
         when others =>
            Output ("Can't handle nested list");
      end case;
   end Create_List;

   ---------------------
   -- Create_Sequence --
   ---------------------

   procedure Create_Sequence
     (Handler  : in out Schema_Reader;
      Atts     : Sax.Attributes.Attributes'Class)
   is
      Min_Occurs_Index : constant Integer :=
        Get_Index (Atts, URI => "", Local_Name => "minOccurs");
      Max_Occurs_Index : constant Integer :=
        Get_Index (Atts, URI => "", Local_Name => "maxOccurs");
      Min_Occurs, Max_Occurs : Integer := 1;
   begin
      if Min_Occurs_Index /= -1 then
         Min_Occurs := Integer'Value (Get_Value (Atts, Min_Occurs_Index));
      end if;

      if Max_Occurs_Index /= -1 then
         Max_Occurs := Max_Occurs_From_Value
           (Get_Value (Atts, Max_Occurs_Index));
      end if;

      Handler.Contexts := new Context'
        (Typ      => Context_Sequence,
         Seq      => Create_Sequence (Min_Occurs, Max_Occurs),
         Level    => Handler.Contexts.Level + 1,
         Next     => Handler.Contexts);
      Output (Ada_Name (Handler.Contexts) & " := Create_Sequence ("
              & Min_Occurs'Img & ',' & Max_Occurs'Img & ")");
   end Create_Sequence;

   ---------------------
   -- Finish_Sequence --
   ---------------------

   procedure Finish_Sequence (Handler : in out Schema_Reader) is
   begin
      case Handler.Contexts.Next.Typ is
         when Context_Type_Def =>
            Handler.Contexts.Next.Type_Validator :=
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
         when Context_Schema | Context_Attribute | Context_Element
            | Context_Restriction | Context_Extension | Context_All =>
            Output ("Can't handle nested sequence");
      end case;
   end Finish_Sequence;

   ----------------------
   -- Create_Attribute --
   ----------------------

   procedure Create_Attribute
     (Handler  : in out Schema_Reader;
      Atts     : Sax.Attributes.Attributes'Class)
   is
      Name_Index : constant Integer :=
        Get_Index (Atts, URI => "", Local_Name => "name");
      Type_Index : constant Integer :=
        Get_Index (Atts, URI => "", Local_Name => "type");
      Use_Index : constant Integer :=
        Get_Index (Atts, URI => "", Local_Name => "use");

      Att : Attribute_Validator;
      Typ : XML_Type := No_Type;
      Use_Type : Attribute_Use_Type := Optional;
   begin
      if Type_Index /= -1 then
         Lookup_With_NS
           (Handler, Get_Value (Atts, Type_Index), Result => Typ);
      end if;

      if Use_Index /= -1 then
         Use_Type := Optional;
      end if;

      Att := Create_Attribute
        (Local_Name     => Get_Value (Atts, Name_Index),
         NS             => Handler.Target_NS,
         Attribute_Type => Typ,
         Attribute_Use  => Use_Type,
         Attribute_Form => Qualified,
         Value          => "");

      Handler.Contexts := new Context'
        (Typ        => Context_Attribute,
         Attribute  => Att,
         Level      => Handler.Contexts.Level + 1,
         Next       => Handler.Contexts);
      Output (Ada_Name (Handler.Contexts) & " := Create_Attribute ("""
              & Get_Value (Atts, Name_Index) & """);");
   end Create_Attribute;

   ----------------------
   -- Finish_Attribute --
   ----------------------

   procedure Finish_Attribute (Handler : in out Schema_Reader) is
   begin
      case Handler.Contexts.Next.Typ is
         when Context_Type_Def =>
            Add_Attribute (Handler.Contexts.Next.Type_Validator,
                           Handler.Contexts.Attribute);
            Output ("Add_Attribute (Validator, "
                    & Ada_Name (Handler.Contexts) & ");");

         when Context_Schema =>
            Register (Handler.Contexts.Attribute);
            Output ("Register (Handler.Target_NS, "
                    & Ada_Name (Handler.Contexts) & ");");

         when Context_Extension =>
            Add_Attribute (Handler.Contexts.Next.Extension,
                           Handler.Contexts.Attribute);
            Output ("Add_Attribute (" & Ada_Name (Handler.Contexts.Next) & ", "
                    & Ada_Name (Handler.Contexts) & ");");

         when Context_Element | Context_Sequence | Context_Choice
            | Context_Attribute | Context_Restriction | Context_All =>
            Output ("Can't handle attribute decl in this context");
      end case;
   end Finish_Attribute;

   -------------------
   -- Create_Schema --
   -------------------

   procedure Create_Schema
     (Handler  : in out Schema_Reader;
      Atts     : Sax.Attributes.Attributes'Class)
   is
      Target_NS_Index : constant Integer :=
        Get_Index (Atts, URI => "", Local_Name => "targetNamespace");
   begin
      if Target_NS_Index /= -1 then
         Get_NS (Handler.Grammar, Get_Value (Atts, Target_NS_Index),
                 Handler.Target_NS);
         if Debug then
            Output ("Get_NS (Handler.Grammar, """
                    & Get_Value (Atts, Target_NS_Index)
                    & """, Handler.Target_NS)");
         end if;
      else
         Get_NS (Handler.Grammar, "", Handler.Target_NS);
         if Debug then
            Output ("Get_NS (Handler.Grammar, """", Handler.Target_NS)");
         end if;
      end if;

      Handler.Contexts := new Context'
        (Typ         => Context_Schema,
         Level       => 0,
         Next        => null);
   end Create_Schema;

   ----------------
   -- Create_All --
   ----------------

   procedure Create_All
     (Handler  : in out Schema_Reader;
      Atts     : Sax.Attributes.Attributes'Class)
   is
      pragma Unreferenced (Atts);
   begin
      Handler.Contexts := new Context'
        (Typ          => Context_All,
         All_Validator => Create_All,
         Level         => Handler.Contexts.Level + 1,
         Next          => Handler.Contexts);
      Output (Ada_Name (Handler.Contexts) & " := Create_All;");
   end Create_All;

   ----------------
   -- Finish_All --
   ----------------

   procedure Finish_All (Handler : in out Schema_Reader) is
   begin
      case Handler.Contexts.Next.Typ is
         when Context_Type_Def =>
            Handler.Contexts.Next.Type_Validator :=
              XML_Validator (Handler.Contexts.All_Validator);
            Output ("Validator := XML_Validator ("
                    & Ada_Name (Handler.Contexts) & ");");

         when others =>
            Output ("Can't handled nested all");
      end case;
   end Finish_All;

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
         when Context_All =>
            return "All" & L (L'First + 1 .. L'Last);
         when Context_Element =>
            return Ada_Name (C.Element);
         when Context_Type_Def =>
            if C.Type_Name = null then
               return "T_" & L (L'First + 1 .. L'Last);
            else
               return "T_" & C.Type_Name.all;
            end if;
         when Context_Attribute =>
            return "A_" & L (L'First + 1 .. L'Last);
         when Context_Restriction =>
            return "Valid" & L (L'First + 1 .. L'Last);
         when Context_Extension =>
            return "E_" & L (L'First + 1 .. L'Last);
      end case;
   end Ada_Name;

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
      Min_Occurs, Max_Occurs : Integer;
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

         Create_Schema (Handler, Atts);

      elsif Local_Name = "annotation" then
         null;

      elsif Local_Name = "element" then
         Create_Element (Handler, Atts);

      elsif Local_Name = "complexType"
        or else Local_Name = "simpleType"
      then
         Create_Complex_Type (Handler, Atts);

      elsif Local_Name = "restriction" then
         Create_Restriction (Handler, Atts);

      elsif Local_Name = "extension" then
         Create_Extension (Handler, Atts);

      elsif Local_Name = "maxLength"
        or else Local_Name = "pattern"
        or else Local_Name = "minLength"
        or else Local_Name = "enumeration"
        or else Local_Name = "whiteSpace"
        or else Local_Name = "totalDigits"
        or else Local_Name = "fractionDigits"
        or else Local_Name = "maxInclusive"
        or else Local_Name = "maxExclusive"
        or else Local_Name = "minInclusive"
        or else Local_Name = "minExclusive"
      then
         Add_Facet (Handler.Contexts.Restriction, Local_Name,
                    Get_Value (Atts, URI => "", Local_Name => "value"));

      elsif Local_Name = "all" then
         Create_All (Handler, Atts);

      elsif Local_Name = "sequence" then
         Create_Sequence (Handler, Atts);

      elsif Local_Name = "list" then
         Create_List (Handler, Atts);

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
            when Context_Type_Def =>
               Handler.Contexts.Next.Type_Validator :=
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
            when Context_Schema | Context_Attribute | Context_Element
               | Context_Restriction | Context_Extension | Context_All =>
               Output ("Can't handle nested sequence");
         end case;

      elsif Local_Name = "attribute" then
         Create_Attribute (Handler, Atts);

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
      Handled : Boolean := True;
   begin
      --  Check the grammar
      End_Element (Validating_Reader (Handler),
                     Namespace_URI,
                     Local_Name,
                     Qname);

      --  Process the tag
      if Local_Name = "element" then
         Finish_Element (Handler);

      elsif Local_Name = "schema" then
         --  ??? Check there remains no undefined forward declaration
         null;

      elsif Local_Name = "complexType"
        or else Local_Name = "simpleType"
      then
         Finish_Complex_Type (Handler);

      elsif Local_Name = "all" then
         Finish_All (Handler);

      elsif Local_Name = "sequence" then
         Finish_Sequence (Handler);

      elsif Local_Name = "choice" then
         null;

      elsif Local_Name = "restriction" then
         Finish_Restriction (Handler);

      elsif Local_Name = "extension" then
         Finish_Extension (Handler);

      elsif Local_Name = "attribute" then
         Finish_Attribute (Handler);

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
            when Context_Type_Def =>
               Free (C.Type_Name);
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

   -------------------------------
   -- Get_Grammar_For_Namespace --
   -------------------------------

   procedure Get_Grammar_For_Namespace
     (Handler : in out Schema_Reader'Class;
      Prefix  : Byte_Sequence;
      Grammar : out XML_Grammar_NS)
   is
      Tmp : Prefix_Mapping_Access := Handler.Prefixes;
   begin
      while Tmp /= null and then Tmp.Prefix.all /= Prefix loop
         Tmp := Tmp.Next;
      end loop;

      if Tmp = null then
         Output ("G := Handler.Target_NS;");
         Grammar := Handler.Target_NS;
      else
         Output
           ("Get_NS (Handler.Grammar, """ & Tmp.Namespace.all & """, G);");
         Get_NS (Handler.Grammar, Tmp.Namespace.all, Grammar);
      end if;
   end Get_Grammar_For_Namespace;

end Schema.Schema_Readers;
