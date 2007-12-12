-----------------------------------------------------------------------
--                XML/Ada - An XML suite for Ada95                   --
--                                                                   --
--                Copyright (C) 2004-2007, AdaCore                   --
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

with Unicode;           use Unicode;
with Unicode.CES;       use Unicode.CES;
with Sax.Attributes;    use Sax.Attributes;
with Sax.Encodings;     use Sax.Encodings;
with Sax.Readers;
with Sax.Utils;         use Sax.Utils;
with Schema.Validators; use Schema.Validators;
with Schema.Readers;    use Schema.Readers;
with Schema.Schema_Grammar; use Schema.Schema_Grammar;
with GNAT.IO;           use GNAT.IO;
with Ada.Unchecked_Deallocation;

package body Schema.Schema_Readers is

   Debug : Boolean := False;

   procedure Free (C : in out Context_Access; Recurse : Boolean);
   --  Free the memory occupied by C

   procedure Get_Grammar_For_Namespace
     (Handler : in out Schema_Reader'Class;
      Prefix  : Byte_Sequence;
      Grammar : out XML_Grammar_NS);
   --  Return the grammar matching a given prefix

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
   procedure Lookup_With_NS
     (Handler : in out Schema_Reader'Class;
      QName   : Byte_Sequence;
      Result  : out XML_Group);
   procedure Lookup_With_NS
     (Handler : in out Schema_Reader'Class;
      QName   : Byte_Sequence;
      Result  : out XML_Attribute_Group);
   procedure Lookup_With_NS
     (Handler : in out Schema_Reader'Class;
      QName   : Byte_Sequence;
      Result  : out Attribute_Validator);
   --  Lookup a type or element  with a possible namespace specification

   function Create_Repeat
     (Handler   : Schema_Reader;
      Validator : access XML_Validator_Record'Class;
      Min_Occurs, Max_Occurs : Integer) return XML_Validator;
   --  Repeat Validator a number of times, by including it in a sequence if
   --  needed

   procedure Ensure_Type (Handler : in out Schema_Reader; C : Context_Access);
   --  Make sure the context (of type Context_Type_Def) has a proper
   --  type validator defined

   function Ada_Name (Element : XML_Element) return String;
   function Ada_Name (Typ : XML_Type)        return String;
   function Ada_Name (C : Context_Access)    return String;
   --  Return the name of an Ada variable suitable to represent Element

   function XML_To_Ada (Str : Byte_Sequence) return Byte_Sequence;
   --  Return a string suitable as an Ada identifier

   function In_Redefine_Context (Handler : Schema_Reader) return Boolean;
   --  Whether we are currently processing a <redefine> tag

   procedure Insert_Attribute
     (Handler    : in out Schema_Reader;
      In_Context : Context_Access;
      Attribute  : Attribute_Validator;
      Attribute_Name : Byte_Sequence);
   --  Insert attribute at the right location in In_Context.
   --  Attribute_Name is only for debugging purposes

   function Process_Contents_From_Atts
     (Atts : Sax.Attributes.Attributes'Class) return Process_Contents_Type;
   --  Get the value of processContents from the attributes

   procedure Create_Element
     (Handler : in out Schema_Reader; Atts : Sax.Attributes.Attributes'Class);
   procedure Create_Complex_Type
     (Handler : in out Schema_Reader; Atts : Sax.Attributes.Attributes'Class);
   procedure Create_Simple_Type
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
   procedure Create_Union
     (Handler : in out Schema_Reader; Atts : Sax.Attributes.Attributes'Class);
   procedure Create_Choice
     (Handler : in out Schema_Reader; Atts : Sax.Attributes.Attributes'Class);
   procedure Create_Redefine
     (Handler : in out Schema_Reader; Atts : Sax.Attributes.Attributes'Class);
   procedure Create_Include
     (Handler : in out Schema_Reader; Atts : Sax.Attributes.Attributes'Class);
   procedure Create_Group
     (Handler : in out Schema_Reader; Atts : Sax.Attributes.Attributes'Class);
   procedure Create_Attribute_Group
     (Handler : in out Schema_Reader; Atts : Sax.Attributes.Attributes'Class);
   procedure Create_Any
     (Handler : in out Schema_Reader; Atts : Sax.Attributes.Attributes'Class);
   procedure Create_Import
     (Handler : in out Schema_Reader; Atts : Sax.Attributes.Attributes'Class);
   procedure Create_Any_Attribute
     (Handler : in out Schema_Reader; Atts : Sax.Attributes.Attributes'Class);
   --  Create a new context for a specific tag:
   --  resp. <element>, <complexType>, <restriction>, <all>, <sequence>,
   --  <attribute>, <schema>, <extension>, <list>, <union>, <choice>,
   --  <redefine>, <group>, <attributeGroup>, <any>, <import>, <anyAttribute>

   procedure Finish_Element (Handler : in out Schema_Reader);
   procedure Finish_Complex_Type (Handler : in out Schema_Reader);
   procedure Finish_Simple_Type (Handler : in out Schema_Reader);
   procedure Finish_Restriction (Handler : in out Schema_Reader);
   procedure Finish_All (Handler : in out Schema_Reader);
   procedure Finish_Sequence (Handler : in out Schema_Reader);
   procedure Finish_Attribute (Handler : in out Schema_Reader);
   procedure Finish_Extension (Handler : in out Schema_Reader);
   procedure Finish_Union (Handler : in out Schema_Reader);
   procedure Finish_List (Handler : in out Schema_Reader);
   procedure Finish_Choice (Handler : in out Schema_Reader);
   procedure Finish_Group (Handler : in out Schema_Reader);
   --  Finish the handling of various tags:
   --  resp. <element>, <complexType>, <restriction>, <all>, <sequence>,
   --  <extension>, <union>, <list>, <choice>, <group>

   function Max_Occurs_From_Value (Value : Byte_Sequence) return Integer;
   --  Return the value of maxOccurs from the attributes'value. This properly
   --  takes into account the "unbounded" case

   procedure Create_Restricted
     (Handler : in out Schema_Reader;
      Ctx     : Context_Access);
   --  Applies to a Context_Restriction, ensures that the restriction has been
   --  created appropriately.

   -------------------------
   -- In_Redefine_Context --
   -------------------------

   function In_Redefine_Context (Handler : Schema_Reader) return Boolean is
      Tmp : Context_Access := Handler.Contexts;
   begin
      while Tmp /= null loop
         if Tmp.Typ = Context_Redefine then
            return True;
         end if;
         Tmp := Tmp.Next;
      end loop;
      return False;
   end In_Redefine_Context;

   ---------------------------
   -- Max_Occurs_From_Value --
   ---------------------------

   function Max_Occurs_From_Value (Value : Byte_Sequence) return Integer is
      Index : Integer;
      C     : Unicode_Char;
   begin
      if Value = "unbounded" then
         return Unbounded;
      else
         begin
            return Integer'Value (Value);
         exception
            when Constraint_Error =>
               --  Either we have an integer too big to fit in Integer, or we
               --  do not have an integer at all
               Index := Value'First;
               while Index <= Value'Last loop
                  Encoding.Read (Value, Index, C);
                  if not Is_Digit (C) then
                     Validation_Error
                       ("Value for ""maxOccurs"" must"
                        & " be an integer or ""unbounded""");
                  end if;
               end loop;

               return Integer'Last;
         end;
      end if;
   end Max_Occurs_From_Value;

   ----------------
   -- XML_To_Ada --
   ----------------

   function XML_To_Ada (Str : Byte_Sequence) return Byte_Sequence is
      Str2 : Byte_Sequence (Str'Range);
   begin
      for S in Str'Range loop
         if Str (S) = '-' then
            Str2 (S) := '_';
         else
            Str2 (S) := Str (S);
         end if;
      end loop;
      return Str2;
   end XML_To_Ada;

   --------------
   -- Ada_Name --
   --------------

   function Ada_Name (Element : XML_Element) return String is
   begin
      return "E_" & XML_To_Ada (Get_Local_Name (Element));
   end Ada_Name;

   --------------
   -- Ada_Name --
   --------------

   function Ada_Name (Typ : XML_Type) return String is
   begin
      return "T_" & XML_To_Ada (Get_Local_Name (Typ));
   end Ada_Name;

   -------------------------
   -- Get_Created_Grammar --
   -------------------------

   function Get_Created_Grammar
     (Reader : Schema_Reader) return Schema.Validators.XML_Grammar is
   begin
      return Reader.Created_Grammar;
   end Get_Created_Grammar;

   -------------------------
   -- Set_Created_Grammar --
   -------------------------

   procedure Set_Created_Grammar
     (Reader  : in out Schema_Reader;
      Grammar : Schema.Validators.XML_Grammar := No_Grammar) is
   begin
      Reader.Created_Grammar := Grammar;
      Reader.Check_Undefined := False;
   end Set_Created_Grammar;

   --------------------
   -- Start_Document --
   --------------------

   procedure Start_Document (Handler : in out Schema_Reader) is
      G : XML_Grammar;
   begin
      --  Add the definition of all predefined types to the created grammar
      --  Not needed, since that is already done by the call to Set_Parsed_URI
      --  in Parse.
      --  Initialize (Handler.Created_Grammar);

      --  Make sure the grammar used to validate the XSD file is correct. This
      --  won't do anything if the schema-for-schema was already added.
      G := Get_Validating_Grammar (Handler);
      Add_Schema_For_Schema (G);
      Set_Validating_Grammar (Handler, G);

      Handler.Target_NS := Get_Target_NS (Handler.Created_Grammar);
      if Handler.Target_NS = null then
         Get_NS (Handler.Created_Grammar, "", Handler.Target_NS);
         if Debug then
            Output
              ("Get_NS (Handler.Created_Grammar, """", Handler.Target_NS)");
         end if;
      end if;

      Get_NS (Handler.Created_Grammar, XML_Schema_URI, Handler.Schema_NS);
   end Start_Document;

   ------------------
   -- End_Document --
   ------------------

   procedure End_Document (Handler : in out Schema_Reader) is
      pragma Unmodified (Handler);
   begin
      if Handler.Check_Undefined then
         Global_Check (Handler.Created_Grammar);
      end if;
   end End_Document;

   -----------
   -- Parse --
   -----------

   procedure Parse
     (Parser : in out Schema_Reader;
      Input  : in out Input_Sources.Input_Source'Class) is
   begin
      if not URI_Was_Parsed
        (Parser.Created_Grammar, Input_Sources.Get_System_Id (Input))
      then
         Set_Feature (Parser, Sax.Readers.Schema_Validation_Feature, True);
         Set_Parsed_URI
           (Parser.Created_Grammar, Input_Sources.Get_System_Id (Input));
         Parse (Validating_Reader (Parser), Input);
      end if;

   exception
      when others =>
         Free (Parser.Contexts, Recurse => True);
         raise;
   end Parse;

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
      Get_Grammar_For_Namespace
        (Handler, QName (QName'First .. Separator - 1), G);
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
      Get_Grammar_For_Namespace
        (Handler, QName (QName'First .. Separator - 1), G);

      Result := Lookup_Element (G, QName (Separator + 1 .. QName'Last));
      Output
        (Ada_Name (Result)
         & " := Lookup_Element (G, """
         & QName (Separator + 1 .. QName'Last) & """);");
   end Lookup_With_NS;

   --------------------
   -- Lookup_With_NS --
   --------------------

   procedure Lookup_With_NS
     (Handler : in out Schema_Reader'Class;
      QName   : Byte_Sequence;
      Result  : out XML_Group)
   is
      Separator : constant Integer := Split_Qname (QName);
      G         : XML_Grammar_NS;
   begin
      Get_Grammar_For_Namespace
        (Handler, QName (QName'First .. Separator - 1), G);

      Result := Lookup_Group (G, QName (Separator + 1 .. QName'Last));
      Output
        ("Group := Lookup_Group (G, """
         & QName (Separator + 1 .. QName'Last) & """);");
   end Lookup_With_NS;

   --------------------
   -- Lookup_With_NS --
   --------------------

   procedure Lookup_With_NS
     (Handler : in out Schema_Reader'Class;
      QName   : Byte_Sequence;
      Result  : out XML_Attribute_Group)
   is
      Separator : constant Integer := Split_Qname (QName);
      G         : XML_Grammar_NS;
   begin
      Get_Grammar_For_Namespace
        (Handler, QName (QName'First .. Separator - 1), G);

      Result := Lookup_Attribute_Group
        (G, QName (Separator + 1 .. QName'Last));
      Output
        ("Attr_Group := Lookup_Attribute_Group (G, """
         & QName (Separator + 1 .. QName'Last) & """);");
   end Lookup_With_NS;

   --------------------
   -- Lookup_With_NS --
   --------------------

   procedure Lookup_With_NS
     (Handler : in out Schema_Reader'Class;
      QName   : Byte_Sequence;
      Result  : out Attribute_Validator)
   is
      Separator : constant Integer := Split_Qname (QName);
      G         : XML_Grammar_NS;
   begin
      Get_Grammar_For_Namespace
        (Handler, QName (QName'First .. Separator - 1), G);

      Result := Lookup_Attribute
        (G, QName (Separator + 1 .. QName'Last));
      Output
        ("Attr := Lookup_Attribute_NS (G, """
         & QName (Separator + 1 .. QName'Last) & """);");
   end Lookup_With_NS;

   ------------------
   -- Create_Group --
   ------------------

   procedure Create_Group
     (Handler : in out Schema_Reader;
      Atts    : Sax.Attributes.Attributes'Class)
   is
      Name_Index : constant Integer :=
        Get_Index (Atts, URI => "", Local_Name => "name");
      Ref_Index : constant Integer :=
        Get_Index (Atts, URI => "", Local_Name => "ref");
      Min_Occurs_Index : constant Integer :=
        Get_Index (Atts, URI => "", Local_Name => "minOccurs");
      Max_Occurs_Index : constant Integer :=
        Get_Index (Atts, URI => "", Local_Name => "maxOccurs");
      Tmp  : Context_Access;
      Min_Occurs, Max_Occurs : Integer := 1;
      Seq : Sequence;
   begin
      if Min_Occurs_Index /= -1 then
         Min_Occurs := Max_Occurs_From_Value
           (Get_Value (Atts, Min_Occurs_Index));
         if Min_Occurs = Unbounded then
            Validation_Error ("minOccurs cannot be ""unbounded""");
         end if;
      end if;

      if Max_Occurs_Index /= -1 then
         Max_Occurs := Max_Occurs_From_Value
           (Get_Value (Atts, Max_Occurs_Index));
      end if;

      Handler.Contexts := new Context'
        (Typ             => Context_Group,
         Group           => No_XML_Group,
         Redefined_Group => No_XML_Group,
         Level           => Handler.Contexts.Level + 1,
         Next            => Handler.Contexts);

      --  Do not use In_Redefine_Context, since this only applies for types
      --  that are redefined
      if Handler.Contexts.Next.Typ = Context_Redefine then
         Handler.Contexts.Redefined_Group := Redefine_Group
           (Handler.Target_NS, Get_Value (Atts, Name_Index));
         Output (Ada_Name (Handler.Contexts)
                 & " := Redefine_Group (Handler.Target_NS, """
                 & Get_Value (Atts, Name_Index) & """);");
      end if;

      if Name_Index /= -1 then
         Handler.Contexts.Group := Create_Global_Group
           (Handler.Target_NS, Get_Value (Atts, Name_Index));
         Output (Ada_Name (Handler.Contexts)
                 & " := Create_Global_Group (Handler.Target_NS, """
                 & Get_Value (Atts, Name_Index) & """);");

      elsif Ref_Index /= -1 then
         if In_Redefine_Context (Handler) then
            Tmp := Handler.Contexts;
            while Tmp /= null loop
               if Tmp.Typ = Context_Group
                 and then Tmp.Next.Typ = Context_Redefine
                 and then Get_Local_Name (Tmp.Group) =
                 Get_Value (Atts, Ref_Index)
               then
                  Handler.Contexts.Group := Tmp.Redefined_Group;
                  Output
                    (Ada_Name (Handler.Contexts)
                     & " := <old definition of group>;");
                  exit;
               end if;
               Tmp := Tmp.Next;
            end loop;
         end if;

         if Handler.Contexts.Group = No_XML_Group then
            Lookup_With_NS
              (Handler, Get_Value (Atts, Ref_Index), Handler.Contexts.Group);
            Output (Ada_Name (Handler.Contexts) & " := Group;");
         end if;
      end if;

      case Handler.Contexts.Next.Typ is
         when Context_Schema | Context_Redefine =>
            null;

         when Context_Type_Def =>
            Handler.Contexts.Next.Type_Validator := Extension_Of
              (Handler.Target_NS,
               Lookup (Handler.Schema_NS, "anyType"),
               Handler.Contexts.Group, Min_Occurs, Max_Occurs);
            Output ("Validator := Extension_Of (Lookup (Handler.Schema.NS,"
                    & """anytype""), " & Ada_Name (Handler.Contexts)
                    & "," & Min_Occurs'Img & "," & Max_Occurs'Img& ");");

         when Context_Sequence =>
            Add_Particle (Handler.Contexts.Next.Seq, Handler.Contexts.Group,
                          Min_Occurs, Max_Occurs);
            Output ("Add_Particle (" & Ada_Name (Handler.Contexts.Next)
                    & ", " & Ada_Name (Handler.Contexts)
                    & Min_Occurs'Img & "," & Max_Occurs'Img& ");");

         when Context_Choice =>
            Add_Particle (Handler.Contexts.Next.C, Handler.Contexts.Group,
                          Min_Occurs, Max_Occurs);
            Output ("Add_Particle (" & Ada_Name (Handler.Contexts.Next)
                    & ", " & Ada_Name (Handler.Contexts)
                    & Min_Occurs'Img & "," & Max_Occurs'Img& ");");

         when Context_Extension =>
            Seq := Create_Sequence (Handler.Target_NS);
            Output ("Validator := Create_Sequence;");
            Add_Particle (Seq, Handler.Contexts.Group);
            Output ("Add_Particle (Validator, " & Ada_Name (Handler.Contexts)
                    & ");");

            Handler.Contexts.Next.Extension := XML_Validator (Seq);

         when others =>
            Output ("Can't handle nested group decl");
      end case;
   end Create_Group;

   ------------------
   -- Finish_Group --
   ------------------

   procedure Finish_Group (Handler : in out Schema_Reader) is
      pragma Unreferenced (Handler);
   begin
      null;
   end Finish_Group;

   ----------------------------
   -- Create_Attribute_Group --
   ----------------------------

   procedure Create_Attribute_Group
     (Handler : in out Schema_Reader;
      Atts    : Sax.Attributes.Attributes'Class)
   is
      Name_Index : constant Integer :=
        Get_Index (Atts, URI => "", Local_Name => "name");
      Ref_Index : constant Integer :=
        Get_Index (Atts, URI => "", Local_Name => "ref");
      In_Redefine : constant Boolean := In_Redefine_Context (Handler);
   begin
      Handler.Contexts := new Context'
        (Typ            => Context_Attribute_Group,
         Attr_Group     => Empty_Attribute_Group,
         Level          => Handler.Contexts.Level + 1,
         Next           => Handler.Contexts);

      if In_Redefine then
         --  <redefine><attributeGroup>
         --     <attributeGroup ref="foo" />
         --     <attribute name="bar" />
         --  </attributeGroup></redefine>    <!--  xsd003b.xsd test -->

         if Handler.Contexts.Next.Typ = Context_Attribute_Group then
            --  Ignore, this is just to indicate which group we are redefining,
            --  but this was already taken into account for the enclosing tag
            return;
         end if;

         Handler.Contexts.Attr_Group := Lookup_Attribute_Group
           (Handler.Target_NS, Get_Value (Atts, Name_Index));
         Output (Ada_Name (Handler.Contexts)
                 & " := Lookup_Attribute_Group (Handler.Target_NS, """
                 & Get_Value (Atts, Name_Index) & """);");

      elsif Name_Index /= -1 then
         Handler.Contexts.Attr_Group := Create_Global_Attribute_Group
           (Handler.Target_NS, Get_Value (Atts, Name_Index));
         Output (Ada_Name (Handler.Contexts)
                 & " := Create_Global_Attribute_Group (Handler.Target_NS, """
                 & Get_Value (Atts, Name_Index) & """);");

      elsif Ref_Index /= -1 then
         Lookup_With_NS
           (Handler, Get_Value (Atts, Ref_Index), Handler.Contexts.Attr_Group);
         Output (Ada_Name (Handler.Contexts) & " := Attr_Group");
      end if;

      if not In_Redefine then
         case Handler.Contexts.Next.Typ is
            when Context_Schema | Context_Redefine =>
               null;

            when Context_Type_Def =>
               Ensure_Type (Handler, Handler.Contexts.Next);
               Add_Attribute_Group
                 (Handler.Contexts.Next.Type_Validator,
                  Handler.Contexts.Attr_Group);
               Output ("Add_Attribute_Group ("
                       & Ada_Name (Handler.Contexts.Next)
                       & ", " & Ada_Name (Handler.Contexts) & ");");

            when Context_Extension =>
               if Handler.Contexts.Next.Extension = null then
                  Handler.Contexts.Next.Extension := Extension_Of
                    (Handler.Target_NS,
                     Handler.Contexts.Next.Extension_Base, null);
                  Output (Ada_Name (Handler.Contexts.Next)
                          & " := Extension_Of ("
                          & Ada_Name (Handler.Contexts.Next.Extension_Base)
                          & ", null);");
                  Handler.Contexts.Next.Extension_Base := No_Type;
               end if;

               Add_Attribute_Group
                 (Handler.Contexts.Next.Extension,
                  Handler.Contexts.Attr_Group);
               Output ("Add_Attribute_Group ("
                       & Ada_Name (Handler.Contexts.Next)
                       & ", " & Ada_Name (Handler.Contexts) & ");");

            when Context_Attribute_Group =>
               Add_Attribute_Group
                 (Handler.Contexts.Next.Attr_Group,
                  Handler.Contexts.Attr_Group);
               Output ("Add_Attribute_Group ("
                       & Ada_Name (Handler.Contexts.Next)
                       & ", " & Ada_Name (Handler.Contexts) & ");");

            when others =>
               Output ("Context is " & Handler.Contexts.Next.Typ'Img);
               Validation_Error
                 ("Can't handle attributeGroup in this context");
         end case;
      end if;
   end Create_Attribute_Group;

   --------------------
   -- Create_Include --
   --------------------

   procedure Create_Include
     (Handler : in out Schema_Reader;
      Atts    : Sax.Attributes.Attributes'Class)
   is
      Schema_Location_Index : constant Integer :=
        Get_Index (Atts, URI => "", Local_Name => "schemaLocation");
   begin
      Parse_Grammar
        (Handler, Get_Value (Atts, Schema_Location_Index),
         Handler.Created_Grammar);
   end Create_Include;

   ---------------------
   -- Create_Redefine --
   ---------------------

   procedure Create_Redefine
     (Handler : in out Schema_Reader;
      Atts    : Sax.Attributes.Attributes'Class)
   is
      Location_Index : constant Integer :=
        Get_Index (Atts, URI => "", Local_Name => "schemaLocation");
   begin
      Parse_Grammar
        (Handler, Get_Value (Atts, Location_Index), Handler.Created_Grammar);

      Handler.Contexts := new Context'
        (Typ            => Context_Redefine,
         Level          => Handler.Contexts.Level + 1,
         Next           => Handler.Contexts);
   end Create_Redefine;

   -------------------
   -- Create_Import --
   -------------------

   procedure Create_Import
     (Handler : in out Schema_Reader;
      Atts    : Sax.Attributes.Attributes'Class)
   is
      Location_Index : constant Integer :=
        Get_Index (Atts, URI => "", Local_Name => "schemaLocation");
      Namespace_Index : constant Integer :=
        Get_Index (Atts, URI => "", Local_Name => "namespace");
   begin
      if Location_Index = -1 or else Namespace_Index = -1 then
         Validation_Error
           ("schemaLocation and namespace attributes mandatory for <import>");
      end if;

      declare
         Location : constant Byte_Sequence := Get_Value (Atts, Location_Index);
         Absolute : constant Byte_Sequence := To_Absolute_URI
           (Handler, Location);
      begin
         if Debug then
            Put_Line ("Import: " & Absolute);
            Put_Line ("Adding new grammar to Handler.Created_Grammar");
         end if;

         if not URI_Was_Parsed (Handler.Created_Grammar, Absolute) then
            Parse_Grammar (Handler, Location, Handler.Created_Grammar);
         elsif Debug then
            Put_Line ("Already imported");
         end if;
      end;
   end Create_Import;

   --------------------------
   -- Create_Any_Attribute --
   --------------------------

   procedure Create_Any_Attribute
     (Handler : in out Schema_Reader;
      Atts    : Sax.Attributes.Attributes'Class)
   is
      Namespace_Index : constant Integer :=
        Get_Index (Atts, URI => "", Local_Name => "namespace");
      Process_Contents : constant Process_Contents_Type :=
        Process_Contents_From_Atts (Atts);
      Kind             : Namespace_Kind;
      NS               : XML_Grammar_NS := Handler.Target_NS;
   begin
      if Namespace_Index = -1 then
         Kind := Namespace_Any;
      elsif Get_Value (Atts, Namespace_Index) = "##other" then
         Kind := Namespace_Other;
      elsif Get_Value (Atts, Namespace_Index) = "##any" then
         Kind := Namespace_Any;
      elsif Get_Value (Atts, Namespace_Index) = "##local" then
         Kind := Namespace_Local;
      elsif Get_Value (Atts, Namespace_Index) = "##targetNamespace" then
         Kind := Namespace_List;
      else
         Kind := Namespace_List;
         Get_NS (Handler.Created_Grammar,
                 Get_Value (Atts, Namespace_Index), NS);
      end if;

      Insert_Attribute
        (Handler,
         Handler.Contexts,
         Create_Any_Attribute (Process_Contents, Kind, NS),
         "Create_Any_Attribute (" & Process_Contents'Img
         & ", " & Kind'Img & ", """
         & Get_Namespace_URI (NS) & """);");
   end Create_Any_Attribute;

   --------------------
   -- Create_Element --
   --------------------

   procedure Create_Element
     (Handler  : in out Schema_Reader;
      Atts     : Sax.Attributes.Attributes'Class)
   is
      --  ??? Could be more efficient by traversing the list of attributes
      --  only once
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
      Final_Index : constant Integer :=
        Get_Index (Atts, URI => "", Local_Name => "final");
      Block_Index : constant Integer :=
        Get_Index (Atts, URI => "", Local_Name => "block");
      Form_Index : constant Integer :=
        Get_Index (Atts, URI => "", Local_Name => "form");

      Min_Occurs, Max_Occurs : Integer := 1;
      Element : XML_Element;
      Typ     : XML_Type := No_Type;
      Group   : XML_Element;
      Form    : Form_Type;
      Is_Ref  : Boolean;

   begin
      if Form_Index /= -1 then
         if Get_Value (Atts, Form_Index) = "qualified" then
            Form := Qualified;
         else
            Form := Unqualified;
         end if;
      else
         Form := Handler.Element_Form_Default;
      end if;

      if Name_Index /= -1 then
         if Type_Index /= -1 then
            Lookup_With_NS
              (Handler, Get_Value (Atts, Type_Index), Result => Typ);
         end if;

         case Handler.Contexts.Typ is
            when Context_Schema | Context_Redefine =>
               Element := Create_Global_Element
                 (Handler.Target_NS,
                  Get_Value (Atts, Name_Index),
                  Form => Form);
               Is_Ref := False;
               Output (Ada_Name (Element)
                       & " := Create_Global_Element (Handler.Target_NS, """
                       & Get_Value (Atts, Name_Index) & """, " & Form'Img
                       & ");");

               if Typ /= No_Type then
                  Set_Type (Element, Typ);
                  Output ("Set_Type (" & Ada_Name (Element) & ", "
                          & Ada_Name (Typ) & ");");
               end if;
            when others =>
               Element := Create_Local_Element
                 (Get_Value (Atts, Name_Index),
                  Handler.Target_NS, Typ, Form => Form);
               Is_Ref := False;
               Output
                 (Ada_Name (Element) & " := Create_Local_Element ("""
                  & Get_Value (Atts, Name_Index) & """, Handler.Target_NS, "
                  & Ada_Name (Typ) & ", " & Form'Img & ");");
         end case;

         if Ref_Index /= -1
           and then Get_Value (Atts, Name_Index) = Get_Value (Atts, Ref_Index)
           and then not In_Redefine_Context (Handler)
         then
            Validation_Error
              ("""ref"" attribute cannot be self-referencing");
         end if;

      elsif Ref_Index = -1 then
         Validation_Error
           ("Either ""name"" or ""ref"" attribute must be present");

      else
         Lookup_With_NS
           (Handler, Get_Value (Atts, Ref_Index), Result => Element);
         Is_Ref := True;

         --  Section 3.3.2, validity constraints 3.3.3
         if Type_Index /= -1 then
            Validation_Error
              ("""type"" attribute cannot be specified along with ""ref""");
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

      if Final_Index /= -1 then
         declare
            Final : constant Byte_Sequence := Get_Value (Atts, Final_Index);
            On_Restriction : constant Boolean :=
              Final = "restriction" or else Final = "#all";
            On_Extension : constant Boolean :=
              Final = "extension" or else Final = "#all";
         begin
            Set_Final (Element,
                       On_Restriction => On_Restriction,
                       On_Extension   => On_Extension);
            Output ("Set_Final ("
                    & Ada_Name (Element) & ", "
                    & Boolean'Image (On_Restriction) & ", "
                    & Boolean'Image (On_Extension) & ");");
         end;
      end if;

      if Block_Index /= -1 then
         declare
            Block : constant Byte_Sequence := Get_Value (Atts, Block_Index);
            On_Restriction : constant Boolean :=
              Block = "restriction" or else Block = "#all";
            On_Extension : constant Boolean :=
              Block = "extension" or else Block = "#all";
         begin
            Set_Block (Element,
                       On_Restriction => On_Restriction,
                       On_Extension   => On_Extension);
            Output ("Set_Block ("
                    & Ada_Name (Element) & ", "
                    & Boolean'Image (On_Restriction) & ", "
                    & Boolean'Image (On_Extension) & ");");
         end;
      end if;

      if Min_Occurs_Index /= -1 then
         Min_Occurs := Max_Occurs_From_Value
           (Get_Value (Atts, Min_Occurs_Index));
         if Min_Occurs = Unbounded then
            Validation_Error ("minOccurs can not be set to ""unbounded""");
         end if;
      end if;

      if Max_Occurs_Index /= -1 then
         Max_Occurs := Max_Occurs_From_Value
           (Get_Value (Atts, Max_Occurs_Index));
      end if;

      case Handler.Contexts.Typ is
         when Context_Schema | Context_Redefine =>
            null;
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
         Is_Ref         => Is_Ref,
         Level          => Handler.Contexts.Level + 1,
         Next           => Handler.Contexts);
   end Create_Element;

   --------------------
   -- Finish_Element --
   --------------------

   procedure Finish_Element (Handler : in out Schema_Reader) is
      pragma Unmodified (Handler);
   begin
      if not Handler.Contexts.Is_Ref
        and then Get_Type (Handler.Contexts.Element) = No_Type
      then
         Set_Type (Handler.Contexts.Element,
                   Lookup (Handler.Schema_NS, "ur-Type"));
         Output ("Set_Type (" & Ada_Name (Handler.Contexts)
                 & ", Lookup (Handler.Schema_NS, ""ur-Type"");");
      end if;
   end Finish_Element;

   ------------------------
   -- Create_Simple_Type --
   ------------------------

   procedure Create_Simple_Type
     (Handler  : in out Schema_Reader;
      Atts     : Sax.Attributes.Attributes'Class)
   is
   begin
      if Handler.Contexts.Typ = Context_Restriction
        and then Handler.Contexts.Restriction_Base = No_Type
      then
         Handler.Contexts := new Context'
           (Typ            => Context_Type_Def,
            Type_Name      => null,
            Type_Validator => null,
            Redefined_Type => No_Type,
            Mixed_Content  => False,
            Simple_Content => False,
            Block_Restriction => False,
            Block_Extension   => False,
            Level             => Handler.Contexts.Level + 1,
            Next              => Handler.Contexts);

      else
         Create_Complex_Type (Handler, Atts);
      end if;
   end Create_Simple_Type;

   ------------------------
   -- Finish_Simple_Type --
   ------------------------

   procedure Finish_Simple_Type (Handler : in out Schema_Reader) is
      C   : constant Context_Access := Handler.Contexts;
      Typ : XML_Type;
   begin
      if C.Next.Typ = Context_Restriction
        and then C.Next.Restriction_Base = No_Type
      then
         Ensure_Type (Handler, C);

         Typ := Create_Local_Type (Handler.Target_NS, C.Type_Validator);
         Output (Ada_Name (C) & " := Create_Local_Type (Validator);");

         C.Next.Restriction_Base := Typ;
         Output ("Setting base type for restriction");
      else
         Handler.Contexts.Mixed_Content := True;
         Finish_Complex_Type (Handler);
      end if;
   end Finish_Simple_Type;

   -------------------------
   -- Create_Complex_Type --
   -------------------------

   procedure Create_Complex_Type
     (Handler  : in out Schema_Reader;
      Atts     : Sax.Attributes.Attributes'Class)
   is
      Name_Index : constant Integer :=
        Get_Index (Atts, URI => "", Local_Name => "name");
      Mixed_Index : constant Integer :=
        Get_Index (Atts, URI => "", Local_Name => "mixed");
      Block_Index : constant Integer :=
        Get_Index (Atts, URI => "", Local_Name => "block");
      Name       : Byte_Sequence_Access;
      Mixed   : Boolean;

   begin
      if Name_Index /= -1 then
         Name := new Byte_Sequence'(Get_Value (Atts, Name_Index));
      end if;

      Mixed := Mixed_Index /= -1
        and then Get_Value_As_Boolean (Atts, Mixed_Index);

      Handler.Contexts := new Context'
        (Typ            => Context_Type_Def,
         Type_Name      => Name,
         Type_Validator => null,
         Redefined_Type => No_Type,
         Mixed_Content  => Mixed,
         Simple_Content => False,
         Block_Restriction => False,
         Block_Extension => False,
         Level          => Handler.Contexts.Level + 1,
         Next           => Handler.Contexts);

      if Block_Index /= -1 then
         declare
            Block : constant Byte_Sequence := Get_Value (Atts, Block_Index);
         begin
            Handler.Contexts.Block_Restriction :=
              Block = "restriction" or else Block = "#all";
            Handler.Contexts.Block_Extension :=
              Block = "extension" or else Block = "#all";
         end;
      end if;

      --  Do not use In_Redefine_Context, since this only applies for types
      --  that are redefined
      if Handler.Contexts.Next.Typ = Context_Redefine then
         Handler.Contexts.Redefined_Type := Redefine_Type
           (Handler.Target_NS, Name.all);
         Output ("Validator := Redefine_Type (Handler.Target_NS, """
                 & Name.all & """);");
      end if;
   end Create_Complex_Type;

   -----------------
   -- Ensure_Type --
   -----------------

   procedure Ensure_Type
     (Handler : in out Schema_Reader; C : Context_Access)
   is
      XML_G : XML_Grammar_NS;
   begin
      if C.Type_Validator = null then
         --  Create an extension, instead of a simple ur-Type, so that we can
         --  add attributes to it without impacting ur-Type itself
         Get_NS (Handler.Created_Grammar, XML_Schema_URI, XML_G);
         C.Type_Validator := Extension_Of (XML_G, Lookup (XML_G, "anyType"));
         Output ("Validator := Extension_Of (Lookup (G, ""anyType""));");
      end if;
   end Ensure_Type;

   -------------------------
   -- Finish_Complex_Type --
   -------------------------

   procedure Finish_Complex_Type (Handler : in out Schema_Reader) is
      C   : constant Context_Access := Handler.Contexts;
      Typ : XML_Type;
   begin
      Ensure_Type (Handler, C);
      if C.Type_Name = null then
         Typ := Create_Local_Type (Handler.Target_NS, C.Type_Validator);
         Output (Ada_Name (C) & " := Create_Local_Type (Validator);");
      else
         Typ := Create_Global_Type
           (Handler.Target_NS, C.Type_Name.all, C.Type_Validator);
         Set_Debug_Name (C.Type_Validator, "for_type_" & C.Type_Name.all);
         Output (Ada_Name (C)
                 & " := Create_Global_Type (Handler.Target_NS, """
                 & C.Type_Name.all
                 & """, Validator);");
      end if;

      if Handler.Contexts.Block_Restriction
        or else Handler.Contexts.Block_Extension
      then
         Set_Block (Typ,
                    On_Restriction => Handler.Contexts.Block_Restriction,
                    On_Extension   => Handler.Contexts.Block_Extension);
         Output ("Set_Block ("
                 & Ada_Name (Typ) & ", "
                 & Boolean'Image (Handler.Contexts.Block_Restriction) & ", "
                 & Boolean'Image (Handler.Contexts.Block_Extension) & ");");
      end if;

      Set_Mixed_Content (Get_Validator (Typ), Handler.Contexts.Mixed_Content);
      Output ("Set_Mixed_Content ("
              & Ada_Name (C) & ", "
              & Boolean'Image (Handler.Contexts.Mixed_Content) & ");");

      case Handler.Contexts.Next.Typ is
         when Context_Schema | Context_Redefine =>
            null;
         when Context_Element =>
            Set_Type (Handler.Contexts.Next.Element, Typ);
            Output ("Set_Type (" & Ada_Name (Handler.Contexts.Next)
                    & ", " & Ada_Name (C) & ");");
         when Context_Attribute =>
            Set_Type (Handler.Contexts.Next.Attribute, Typ);
            Output ("Set_Type (" & Ada_Name (Handler.Contexts.Next)
                    & ", " & Ada_Name (Typ) & ");");
         when Context_List =>
            Handler.Contexts.Next.List_Items := Typ;
            Output ("Validator := " & Ada_Name (C) & ";");
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
        and then Base_Index /= -1
        and then Get_Value (Atts, Base_Index) = Handler.Contexts.Type_Name.all
      then
         if In_Redefine_Context (Handler) then
            Base := Handler.Contexts.Redefined_Type;
         else
            Validation_Error
              ("Self-referencing restriction not allowed");
         end if;

      elsif Base_Index /= -1 then
         Lookup_With_NS
           (Handler, Get_Value (Atts, Base_Index), Result => Base);
      else
         Base := No_Type;
      end if;

      if Handler.Contexts.Simple_Content then
         Check_Content_Type (Base, Should_Be_Simple => True);
      end if;

      Handler.Contexts := new Context'
        (Typ              => Context_Restriction,
         Restriction_Base => Base,
         Restricted       => null,
         Restriction      => null,
         Level            => Handler.Contexts.Level + 1,
         Next             => Handler.Contexts);
   end Create_Restriction;

   -----------------------
   -- Create_Restricted --
   -----------------------

   procedure Create_Restricted
     (Handler : in out Schema_Reader;
      Ctx     : Context_Access)
   is
      G : XML_Grammar_NS;
   begin
      if Ctx.Restricted = null then
         if Ctx.Restriction_Base = No_Type then
            Get_NS (Handler.Created_Grammar, XML_Schema_URI, G);
            Ctx.Restriction_Base := Lookup (G, "ur-Type");
            Output ("Restriction has no base type set");
         end if;

         Ctx.Restricted := Restriction_Of
           (Handler.Target_NS, Ctx.Restriction_Base, Ctx.Restriction);
         Output (Ada_Name (Ctx)
                 & " := Restriction_Of ("
                 & Ada_Name (Ctx.Restriction_Base) & ", "
                 & "Validator" & ");");
      end if;
   end Create_Restricted;

   ------------------------
   -- Finish_Restriction --
   ------------------------

   procedure Finish_Restriction (Handler : in out Schema_Reader) is
   begin
      Create_Restricted (Handler, Handler.Contexts);

      case Handler.Contexts.Next.Typ is
         when Context_Type_Def =>
            Handler.Contexts.Next.Type_Validator :=
              Handler.Contexts.Restricted;
            Set_Debug_Name (Handler.Contexts.Next.Type_Validator,
                            Ada_Name (Handler.Contexts));
            Output ("Validator := " & Ada_Name (Handler.Contexts) & ";");
         when others =>
            Output ("Can't handler nested restrictions");
      end case;
   end Finish_Restriction;

   ------------------
   -- Create_Union --
   ------------------

   procedure Create_Union
     (Handler  : in out Schema_Reader;
      Atts     : Sax.Attributes.Attributes'Class)
   is
      Member_Index : constant Integer :=
        Get_Index (Atts, URI => "", Local_Name => "memberTypes");
   begin
      Handler.Contexts := new Context'
        (Typ   => Context_Union,
         Union => Create_Union (Handler.Target_NS),
         Level => Handler.Contexts.Level + 1,
         Next  => Handler.Contexts);
      Output (Ada_Name (Handler.Contexts) & " := Create_Union;");

      if Member_Index /= -1 then
         declare
            Members : constant Byte_Sequence := Get_Value (Atts, Member_Index);
            Index   : Integer := Members'First;
            Start, Last : Integer;
            C       : Unicode_Char;
            Typ     : XML_Type;
         begin
            while Index <= Members'Last loop
               while Index <= Members'Last loop
                  Start := Index;
                  Encoding.Read (Members, Index, C);
                  exit when not Is_White_Space (C);
               end loop;

               while Index <= Members'Last loop
                  Last := Index;
                  Encoding.Read (Members, Index, C);
                  exit when Is_White_Space (C);
               end loop;

               if Index > Members'Last then
                  Last := Members'Last + 1;
               end if;

               Lookup_With_NS (Handler, Members (Start .. Last - 1), Typ);
               Add_Union (Handler.Contexts.Union, Typ);
               Output ("Add_Union ("
                       & Ada_Name (Handler.Contexts)
                       & ", """ & Members (Start .. Last - 1) & """)");
            end loop;
         end;
      end if;
   end Create_Union;

   ------------------
   -- Finish_Union --
   ------------------

   procedure Finish_Union (Handler : in out Schema_Reader) is
      pragma Unmodified (Handler);
   begin
      case Handler.Contexts.Next.Typ is
         when Context_Type_Def =>
            Handler.Contexts.Next.Type_Validator :=
              XML_Validator (Handler.Contexts.Union);
            Output ("Validator := " & Ada_Name (Handler.Contexts) & ";");

         when others =>
            Output ("Can't handle nested unions");
      end case;
   end Finish_Union;

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
      if Base_Index = -1 then
         Validation_Error
           ("Attribute ""base"" required for <extensionType>");
      end if;

      if Handler.Contexts.Type_Name /= null
        and then Get_Value (Atts, Base_Index) = Handler.Contexts.Type_Name.all
      then
         if In_Redefine_Context (Handler) then
            Base := Handler.Contexts.Redefined_Type;
         else
            Validation_Error
              ("Self-referencing extension not allowed");
         end if;
      else
         Lookup_With_NS
           (Handler, Get_Value (Atts, Base_Index), Result => Base);
      end if;

      Handler.Contexts := new Context'
        (Typ            => Context_Extension,
         Extension_Base => Base,
         Extension      => null,
         Level          => Handler.Contexts.Level + 1,
         Next           => Handler.Contexts);
   end Create_Extension;

   ----------------------
   -- Finish_Extension --
   ----------------------

   procedure Finish_Extension (Handler : in out Schema_Reader) is
      pragma Unmodified (Handler);
   begin
      case Handler.Contexts.Next.Typ is
         when Context_Type_Def =>
            if Handler.Contexts.Extension_Base /= No_Type then
               Handler.Contexts.Next.Type_Validator := Extension_Of
                 (Handler.Target_NS,
                  Handler.Contexts.Extension_Base,
                  Handler.Contexts.Extension);
               Set_Debug_Name (Handler.Contexts.Next.Type_Validator,
                               Ada_Name (Handler.Contexts));

               if Handler.Contexts.Extension /= null then
                  Set_Debug_Name
                    (Handler.Contexts.Extension,
                     "extension_of_"
                     & Get_Local_Name (Handler.Contexts.Extension_Base));
               end if;

               Output (Ada_Name (Handler.Contexts) & " := Extension_Of ("
                       & Ada_Name (Handler.Contexts.Extension_Base)
                       & ", Validator);");
            else
               Handler.Contexts.Next.Type_Validator :=
                 Handler.Contexts.Extension;
               Output (Ada_Name (Handler.Contexts) & " := Validator;");
            end if;

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
      if Item_Type_Index /= -1 then
         Lookup_With_NS
           (Handler, Get_Value (Atts, Item_Type_Index), Result => Items);
      end if;

      Handler.Contexts := new Context'
        (Typ            => Context_List,
         List_Items     => Items,
         Level          => Handler.Contexts.Level + 1,
         Next           => Handler.Contexts);
   end Create_List;

   -----------------
   -- Finish_List --
   -----------------

   procedure Finish_List (Handler : in out Schema_Reader) is
      pragma Unmodified (Handler);
   begin
      case Handler.Contexts.Next.Typ is
         when Context_Type_Def =>
            Handler.Contexts.Next.Type_Validator :=
              Get_Validator
                (List_Of (Handler.Target_NS, Handler.Contexts.List_Items));
            Output ("Validator := List_Of (Validator);");
         when others =>
            Output ("Can't handle nested list");
      end case;
   end Finish_List;

   -------------------
   -- Create_Repeat --
   -------------------

   function Create_Repeat
     (Handler   : Schema_Reader;
      Validator : access XML_Validator_Record'Class;
      Min_Occurs, Max_Occurs : Integer)
      return XML_Validator
   is
      Seq : Sequence;
   begin
      if Min_Occurs = 1 and then Max_Occurs = 1 then
         return XML_Validator (Validator);
      else
         Seq := Create_Sequence (Handler.Target_NS);
         Set_Debug_Name (Seq, "repeat_seq");
         if Validator.all in Sequence_Record'Class then
            Add_Particle (Seq, Sequence (Validator),
                          Min_Occurs => Min_Occurs,
                          Max_Occurs => Max_Occurs);
         else
            Add_Particle (Seq, Choice (Validator),
                          Min_Occurs => Min_Occurs,
                          Max_Occurs => Max_Occurs);
         end if;
         Output ("Seq := Create_Sequence;");
         Output ("Add_Particle (Seq, Validator,"
                 & Min_Occurs'Img & Max_Occurs'Img & ")");
         return XML_Validator (Seq);
      end if;
   end Create_Repeat;

   -------------------
   -- Create_Choice --
   -------------------

   procedure Create_Choice
     (Handler : in out Schema_Reader; Atts : Sax.Attributes.Attributes'Class)
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
        (Typ      => Context_Choice,
         C        => Create_Choice (Handler.Target_NS),
         Level    => Handler.Contexts.Level + 1,
         Next     => Handler.Contexts);
      Output (Ada_Name (Handler.Contexts) & " := Create_Choice;");

      case Handler.Contexts.Next.Typ is
         when Context_Type_Def =>
            Output ("Validator := " & Ada_Name (Handler.Contexts) & ";");
            Handler.Contexts.Next.Type_Validator := Create_Repeat
              (Handler, Handler.Contexts.C, Min_Occurs, Max_Occurs);
         when Context_Sequence =>
            Add_Particle (Handler.Contexts.Next.Seq, Handler.Contexts.C,
                          Min_Occurs, Max_Occurs);
            Output ("Add_Particle (" & Ada_Name (Handler.Contexts.Next)
                    & ", " & Ada_Name (Handler.Contexts) & ");");
         when Context_Choice =>
            Add_Particle (Handler.Contexts.Next.C, Handler.Contexts.C,
                          Min_Occurs, Max_Occurs);
            Output ("Add_Particle (" & Ada_Name (Handler.Contexts.Next)
                    & ", " & Ada_Name (Handler.Contexts) & ");");
         when Context_Extension =>
            Output ("Validator := " & Ada_Name (Handler.Contexts));
            Handler.Contexts.Next.Extension := Create_Repeat
              (Handler, Handler.Contexts.C, Min_Occurs, Max_Occurs);

         when Context_Group =>
            Add_Particle (Handler.Contexts.Next.Group, Handler.Contexts.C,
                          Min_Occurs, Max_Occurs);
            Output ("Add_Particle ("
                    & Ada_Name (Handler.Contexts.Next)
                    & ", " & Ada_Name (Handler.Contexts) & ");");

         when Context_Schema | Context_Attribute | Context_Element
            | Context_Restriction | Context_All | Context_Union
            | Context_List | Context_Redefine | Context_Attribute_Group =>
            Output ("Can't handle nested sequence");
      end case;
   end Create_Choice;

   -------------------
   -- Finish_Choice --
   -------------------

   procedure Finish_Choice (Handler : in out Schema_Reader) is
      pragma Unreferenced (Handler);
   begin
      null;
   end Finish_Choice;

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
         Seq      => Create_Sequence (Handler.Target_NS),
         Level    => Handler.Contexts.Level + 1,
         Next     => Handler.Contexts);
      Output (Ada_Name (Handler.Contexts) & " := Create_Sequence;");

      case Handler.Contexts.Next.Typ is
         when Context_Type_Def =>
            Output ("Validator := " & Ada_Name (Handler.Contexts) & ";");
            Handler.Contexts.Next.Type_Validator := Create_Repeat
              (Handler, Handler.Contexts.Seq, Min_Occurs, Max_Occurs);
         when Context_Sequence =>
            Add_Particle (Handler.Contexts.Next.Seq, Handler.Contexts.Seq,
                          Min_Occurs, Max_Occurs);
            Output ("Add_Particle (" & Ada_Name (Handler.Contexts.Next)
                    & ", " & Ada_Name (Handler.Contexts)
                    & "," & Min_Occurs'Img & "," & Max_Occurs'Img & ");");
         when Context_Choice =>
            Add_Particle (Handler.Contexts.Next.C, Handler.Contexts.Seq,
                          Min_Occurs, Max_Occurs);
            Output ("Add_Particle (" & Ada_Name (Handler.Contexts.Next)
                    & ", " & Ada_Name (Handler.Contexts)
                    & "," & Min_Occurs'Img & "," & Max_Occurs'Img & ");");
         when Context_Extension =>
            Output ("Validator := " & Ada_Name (Handler.Contexts) & ";");
            Handler.Contexts.Next.Extension := Create_Repeat
              (Handler, Handler.Contexts.Seq, Min_Occurs, Max_Occurs);
         when Context_Restriction =>
            Output ("Validator := " & Ada_Name (Handler.Contexts) & ";");
            Handler.Contexts.Next.Restriction := Create_Repeat
              (Handler, Handler.Contexts.Seq, Min_Occurs, Max_Occurs);
         when Context_Group =>
            Add_Particle (Handler.Contexts.Next.Group, Handler.Contexts.Seq,
                          Min_Occurs, Max_Occurs);
            Output ("Add_Particle ("
                    & Ada_Name (Handler.Contexts.Next)
                    & ", " & Ada_Name (Handler.Contexts) & ");");
            Set_Debug_Name
              (Handler.Contexts.Seq,
               "seq_in_group__"
               & Get_Local_Name (Handler.Contexts.Next.Group));
         when Context_Schema | Context_Attribute | Context_Element
            | Context_All | Context_Union
            | Context_List | Context_Redefine | Context_Attribute_Group =>
            Output ("Can't handle nested sequence");
      end case;
   end Create_Sequence;

   ---------------------
   -- Finish_Sequence --
   ---------------------

   procedure Finish_Sequence (Handler : in out Schema_Reader) is
      pragma Unreferenced (Handler);
   begin
      null;
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
      Ref_Index : constant Integer :=
        Get_Index (Atts, URI => "", Local_Name => "ref");

      Att : Attribute_Validator;
      Typ : XML_Type := No_Type;
      Use_Type : Attribute_Use_Type := Optional;
   begin
      if Type_Index /= -1 then
         Lookup_With_NS
           (Handler, Get_Value (Atts, Type_Index), Result => Typ);
      end if;

      if Use_Index = -1 then
         Use_Type := Optional;
      else
         declare
            Val : constant String := Get_Value (Atts, Use_Index);
         begin
            if Val = "required" then
               Use_Type := Required;
            elsif Val = "prohibited" then
               Use_Type := Prohibited;
            elsif Val = "default" then
               Use_Type := Default;
            elsif Val = "fixed" then
               Use_Type := Fixed;
            else
               Use_Type := Optional;
            end if;
         end;
      end if;

      Handler.Contexts := new Context'
        (Typ              => Context_Attribute,
         Attribute        => null,
         Attribute_Is_Ref => Name_Index = -1,
         Level            => Handler.Contexts.Level + 1,
         Next             => Handler.Contexts);

      if Name_Index /= -1 then
         case Handler.Contexts.Next.Typ is
            when Context_Schema | Context_Redefine =>
               Att := Create_Global_Attribute
                 (Local_Name     => Get_Value (Atts, Name_Index),
                  NS             => Handler.Target_NS,
                  Attribute_Type => Typ);
               Output (Ada_Name (Handler.Contexts)
                       & " := Create_Global_Attribute ("""
                       & Get_Value (Atts, Name_Index)
                       & """, Handler.Target_NS, "
                       & Ada_Name (Typ) & ");");

            when others =>
               Att := Create_Local_Attribute
                 (Local_Name     => Get_Value (Atts, Name_Index),
                  NS             => Handler.Target_NS,
                  Attribute_Type => Typ,
                  Attribute_Use  => Use_Type,
                  Attribute_Form => Qualified,
                  Value          => "");
               Output (Ada_Name (Handler.Contexts)
                       & " := Create_Local_Attribute ("""
                       & Get_Value (Atts, Name_Index)
                       & """, Handler.Target_NS, "
                       & Ada_Name (Typ)
                       & ", " & Use_Type'Img & ", Qualified);");
         end case;
      else
         Lookup_With_NS (Handler, Get_Value (Atts, Ref_Index), Att);
      end if;

      Handler.Contexts.Attribute := Att;

      Insert_Attribute
        (Handler, Handler.Contexts.Next, Handler.Contexts.Attribute,
         Ada_Name (Handler.Contexts));
   end Create_Attribute;

   ----------------------
   -- Insert_Attribute --
   ----------------------

   procedure Insert_Attribute
     (Handler    : in out Schema_Reader;
      In_Context : Context_Access;
      Attribute  : Attribute_Validator;
      Attribute_Name : Byte_Sequence) is
   begin
      case In_Context.Typ is
         when Context_Type_Def =>
            Ensure_Type (Handler, In_Context);
            Add_Attribute (In_Context.Type_Validator, Attribute);
            Output ("Add_Attribute (Validator, " & Attribute_Name & ");");

         when Context_Schema | Context_Redefine =>
            null;

         when Context_Extension =>
            --  If there is no extension at this point, there won't be any as
            --  per the XML schema, since the attributes come last
            if In_Context.Extension = null then
               In_Context.Extension := Extension_Of
                 (Handler.Target_NS, In_Context.Extension_Base, null);
               Output (Ada_Name (In_Context) & " := Extension_Of ("
                       & Ada_Name (In_Context.Extension_Base)
                       & ", null);");
               In_Context.Extension_Base := No_Type;
            end if;

            Add_Attribute (In_Context.Extension, Attribute);
            Output ("Add_Attribute (" & Ada_Name (In_Context) & ", "
                    & Attribute_Name & ");");

         when Context_Restriction =>
            Create_Restricted (Handler, In_Context);
            Add_Attribute (In_Context.Restricted, Attribute);
            Output ("Add_Attribute (" & Ada_Name (In_Context) & ", "
                    & Attribute_Name & ");");

         when Context_Attribute_Group =>
            Add_Attribute (In_Context.Attr_Group, Attribute);
            Output ("Add_Attribute (" & Ada_Name (In_Context) & ", "
                    & Attribute_Name & ");");

         when Context_Element | Context_Sequence | Context_Choice
            | Context_Attribute | Context_All
            | Context_Union | Context_List | Context_Group =>
            Output ("Can't handle attribute decl in this context");
      end case;
   end Insert_Attribute;

   ----------------------
   -- Finish_Attribute --
   ----------------------

   procedure Finish_Attribute (Handler : in out Schema_Reader) is
      pragma Unmodified (Handler);
   begin
      if not Handler.Contexts.Attribute_Is_Ref
        and then Get_Type (Handler.Contexts.Attribute) = No_Type
      then
         Set_Type (Handler.Contexts.Attribute,
                   Lookup (Handler.Schema_NS, "ur-Type"));
         Output ("Set_Type (" & Ada_Name (Handler.Contexts)
                 & ", Lookup (Handler.Schema_NS, ""ur-Type"");");
      end if;
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
      Form_Default_Index : constant Integer :=
        Get_Index (Atts, URI => "", Local_Name => "elementFormDefault");
      Block_Index : constant Integer :=
        Get_Index (Atts, URI => "", Local_Name => "blockDefault");
   begin
      if Target_NS_Index /= -1 then
         Get_NS (Handler.Created_Grammar, Get_Value (Atts, Target_NS_Index),
                 Handler.Target_NS);
         if Debug then
            Output ("Get_NS (Handler.Created_Grammar, """
                    & Get_Value (Atts, Target_NS_Index)
                    & """, Handler.Target_NS)");
         end if;
         Set_Target_NS (Handler.Created_Grammar, Handler.Target_NS);
      end if;

      if Form_Default_Index /= -1 then
         if Get_Value (Atts, Form_Default_Index) = "qualified" then
            Handler.Element_Form_Default := Qualified;
            Output
              ("Set_Element_Form_Default (Handler.Target_NS, Qualified);");
         else
            Handler.Element_Form_Default := Unqualified;
            Output
              ("Set_Element_Form_Default (Handler.Target_NS, Unqualified);");
         end if;
      end if;

      if Block_Index /= -1 then
         declare
            Block : constant Byte_Sequence := Get_Value (Atts, Block_Index);
         begin
            Set_Block_Default
              (Handler.Target_NS,
               On_Restriction => Block = "restriction" or else Block = "#all",
               On_Extension   => Block = "extension" or else Block = "#all");
            Output ("Set_Block (Handler.Target_NS, "
                    & Boolean'Image
                      (Block = "restriction" or else Block = "#all")
                    & ", " & Boolean'Image
                      (Block = "extension" or else Block = "#all"));
         end;
      end if;

      Handler.Contexts := new Context'
        (Typ         => Context_Schema,
         Level       => 0,
         Next        => null);
   end Create_Schema;

   --------------------------------
   -- Process_Contents_From_Atts --
   --------------------------------

   function Process_Contents_From_Atts
     (Atts : Sax.Attributes.Attributes'Class) return Process_Contents_Type
   is
      Process_Contents_Index : constant Integer :=
        Get_Index (Atts, "processContents");
   begin
      if Process_Contents_Index = -1 then
         return Process_Strict;
      elsif Get_Value (Atts, Process_Contents_Index) = "lax" then
         return Process_Lax;
      elsif Get_Value (Atts, Process_Contents_Index) = "strict" then
         return Process_Strict;
      else
         return Process_Skip;
      end if;
   end Process_Contents_From_Atts;

   ----------------
   -- Create_Any --
   ----------------

   procedure Create_Any
     (Handler : in out Schema_Reader;
      Atts    : Sax.Attributes.Attributes'Class)
   is
      pragma Unmodified (Handler);
      Namespace_Index        : constant Integer := Get_Index
        (Atts, "namespace");
      Min_Occurs_Index       : constant Integer := Get_Index
        (Atts, URI => "", Local_Name => "minOccurs");
      Max_Occurs_Index       : constant Integer := Get_Index
        (Atts, URI => "", Local_Name => "maxOccurs");
      Min_Occurs, Max_Occurs : Integer := 1;
      Process_Contents       : Process_Contents_Type;
      Any                    : XML_Any;
   begin
      if Min_Occurs_Index /= -1 then
         Min_Occurs := Integer'Value (Get_Value (Atts, Min_Occurs_Index));
      end if;

      if Max_Occurs_Index /= -1 then
         Max_Occurs := Max_Occurs_From_Value
           (Get_Value (Atts, Max_Occurs_Index));
      end if;

      Process_Contents := Process_Contents_From_Atts (Atts);

      if Namespace_Index /= -1 then
         Any := Create_Any
           (Process_Contents => Process_Contents,
            Namespace        => Get_Value (Atts, Namespace_Index),
            Target_NS        => Handler.Target_NS);
         Output
           ("Validator := Create_Any (" & Process_Contents'Img & ", "
            & Get_Value (Atts, Namespace_Index) & ", Handler.Target_NS);");
      else
         Any := Create_Any
           (Process_Contents => Process_Contents,
            Namespace        => "##any",
            Target_NS        => Handler.Target_NS);
         Output
           ("Validator := Create_Any (" & Process_Contents'Img
            & ", ""##any"", Handler.Target_NS);");
      end if;

      case Handler.Contexts.Typ is
         when Context_Sequence =>
            Add_Particle (Handler.Contexts.Seq, Any, Min_Occurs, Max_Occurs);
            Output ("Add_Particle ("
                    & Ada_Name (Handler.Contexts)
                    & ", Validator," & Min_Occurs'Img & ","
                    & Max_Occurs'Img & ");");

         when Context_Choice =>
            Add_Particle (Handler.Contexts.C, Any, Min_Occurs, Max_Occurs);
            Output ("Add_Particle ("
                    & Ada_Name (Handler.Contexts)
                    & ", Validator," & Min_Occurs'Img & ","
                    & Max_Occurs'Img & ");");

         when others =>
            Output ("Can't handled nested <any>");
      end case;
   end Create_Any;

   ----------------
   -- Create_All --
   ----------------

   procedure Create_All
     (Handler  : in out Schema_Reader;
      Atts     : Sax.Attributes.Attributes'Class)
   is
      Min_Occurs_Index       : constant Integer := Get_Index
        (Atts, URI => "", Local_Name => "minOccurs");
      Max_Occurs_Index       : constant Integer := Get_Index
        (Atts, URI => "", Local_Name => "maxOccurs");
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
        (Typ          => Context_All,
         All_Validator =>
           Create_All (Handler.Target_NS, Min_Occurs, Max_Occurs),
         Level         => Handler.Contexts.Level + 1,
         Next          => Handler.Contexts);
      Output (Ada_Name (Handler.Contexts) & " := Create_All ("
              & Min_Occurs'Img & "," & Max_Occurs'Img & ");");
   end Create_All;

   ----------------
   -- Finish_All --
   ----------------

   procedure Finish_All (Handler : in out Schema_Reader) is
      pragma Unmodified (Handler);
   begin
      case Handler.Contexts.Next.Typ is
         when Context_Type_Def =>
            Handler.Contexts.Next.Type_Validator :=
              XML_Validator (Handler.Contexts.All_Validator);
            Output ("Validator := XML_Validator ("
                    & Ada_Name (Handler.Contexts) & ");");

         when Context_Group =>
            Add_Particle
              (Handler.Contexts.Next.Group,
               Handler.Contexts.All_Validator);
            Output ("Add_Particle (" & Ada_Name (Handler.Contexts.Next)
                    & ", " & Ada_Name (Handler.Contexts) & ");");

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
         when Context_Schema | Context_Redefine =>
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
               return "T_" & XML_To_Ada (C.Type_Name.all);
            end if;
         when Context_Attribute =>
            return "A_" & L (L'First + 1 .. L'Last);
         when Context_Restriction =>
            return "R_" & L (L'First + 1 .. L'Last);
         when Context_Extension =>
            return "E_" & L (L'First + 1 .. L'Last);
         when Context_Union =>
            return "U_" & L (L'First + 1 .. L'Last);
         when Context_List =>
            return "L_" & L (L'First + 1 .. L'Last);
         when Context_Group =>
            return "G_" & L (L'First + 1 .. L'Last);
         when Context_Attribute_Group =>
            return "AG_" & L (L'First + 1 .. L'Last);
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
      Atts          : Sax.Attributes.Attributes'Class) is
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
            Validation_Error ("Root element must be <schema>");
         end if;

         Create_Schema (Handler, Atts);

      elsif Local_Name = "annotation" then
         null;

      elsif Local_Name = "element" then
         Create_Element (Handler, Atts);

      elsif Local_Name = "complexType" then
         Create_Complex_Type (Handler, Atts);

      elsif Local_Name = "simpleType" then
         Create_Simple_Type (Handler, Atts);

      elsif Local_Name = "restriction" then
         Create_Restriction (Handler, Atts);

      elsif Local_Name = "extension" then
         Create_Extension (Handler, Atts);

      elsif Local_Name = "anyAttribute" then
         Create_Any_Attribute (Handler, Atts);

      elsif Local_Name = "maxLength"
        or else Local_Name = "pattern"
        or else Local_Name = "minLength"
        or else Local_Name = "length"
        or else Local_Name = "enumeration"
        or else Local_Name = "whiteSpace"
        or else Local_Name = "totalDigits"
        or else Local_Name = "fractionDigits"
        or else Local_Name = "maxInclusive"
        or else Local_Name = "maxExclusive"
        or else Local_Name = "minInclusive"
        or else Local_Name = "minExclusive"
      then
         Create_Restricted (Handler, Handler.Contexts);
         Add_Facet (Handler.Contexts.Restricted, Local_Name,
                    Get_Value (Atts, URI => "", Local_Name => "value"));
         Output ("Add_Facet ("
                 & Ada_Name (Handler.Contexts) & ", """ & Local_Name
                 & """, """
                 & Get_Value (Atts, URI => "", Local_Name => "value")
                 & """);");

      elsif Local_Name = "all" then
         Create_All (Handler, Atts);

      elsif Local_Name = "sequence" then
         Create_Sequence (Handler, Atts);

      elsif Local_Name = "choice" then
         Create_Choice (Handler, Atts);

      elsif Local_Name = "list" then
         Create_List (Handler, Atts);

      elsif Local_Name = "union" then
         Create_Union (Handler, Atts);

      elsif Local_Name = "attribute" then
         Create_Attribute (Handler, Atts);

      elsif Local_Name = "group" then
         Create_Group (Handler, Atts);

      elsif Local_Name = "simpleContent" then
         Handler.Contexts.Simple_Content := True;

      elsif Local_Name = "complexContent" then
         Handler.Contexts.Simple_Content := False;

      elsif Local_Name = "attributeGroup" then
         Create_Attribute_Group (Handler, Atts);

      elsif Local_Name = "any" then
         Create_Any (Handler, Atts);

      elsif Local_Name = "redefine" then
         Create_Redefine (Handler, Atts);

      elsif Local_Name = "include" then
         Create_Include (Handler, Atts);

      elsif Local_Name = "import" then
         Create_Import (Handler, Atts);

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

      elsif Local_Name = "complexType" then
         Finish_Complex_Type (Handler);

      elsif Local_Name = "simpleType" then
         Finish_Simple_Type (Handler);

      elsif Local_Name = "all" then
         Finish_All (Handler);

      elsif Local_Name = "sequence" then
         Finish_Sequence (Handler);

      elsif Local_Name = "anyAttribute" then
         Handled := False;

      elsif Local_Name = "choice" then
         Finish_Choice (Handler);

      elsif Local_Name = "restriction" then
         Finish_Restriction (Handler);

      elsif Local_Name = "extension" then
         Finish_Extension (Handler);

      elsif Local_Name = "attribute" then
         Finish_Attribute (Handler);

      elsif Local_Name = "union" then
         Finish_Union (Handler);

      elsif Local_Name = "list" then
         Finish_List (Handler);

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
         Handled := False;

      elsif Local_Name = "redefine" then
         null;

      elsif Local_Name = "include" then
         Handled := False;

      elsif Local_Name = "group" then
         Finish_Group (Handler);

      elsif Local_Name = "attributeGroup" then
         null;

      elsif Local_Name = "any" then
         Handled := False;

      elsif Local_Name = "import"
        or else Local_Name = "simpleContent"
        or else Local_Name = "complexContent"
      then
         Handled := False;

      else
         Output ("Close tag not handled yet: " & Local_Name);
         Handled := False;
      end if;

      --  Free the context
      if Handled then
         Handler.Contexts := Handler.Contexts.Next;
         Free (C, Recurse => False);
      end if;
   end End_Element;

   ----------
   -- Free --
   ----------

   procedure Free (C : in out Context_Access; Recurse : Boolean) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Context, Context_Access);
      Tmp : Context_Access;
   begin
      while C /= null loop
         case C.Typ is
            when Context_Type_Def =>
               Free (C.Type_Name);
            when others =>
               null;
         end case;
         Tmp := C.Next;
         Unchecked_Free (C);

         exit when not Recurse;
         C := Tmp;
      end loop;
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

   -------------------------------
   -- Get_Grammar_For_Namespace --
   -------------------------------

   procedure Get_Grammar_For_Namespace
     (Handler : in out Schema_Reader'Class;
      Prefix  : Byte_Sequence;
      Grammar : out XML_Grammar_NS)
   is
      Namespace : constant Byte_Sequence_Access := Get_Namespace_From_Prefix
        (Handler, Prefix);
   begin
      if Namespace = null then
         Output ("G := Handler.Target_NS;");
         Grammar := Handler.Target_NS;
      else
         Output
           ("Get_NS (Handler.Created_Grammar, """
            & Namespace.all & """, G);");
         Get_NS (Handler.Created_Grammar, Namespace.all, Grammar);
      end if;
   end Get_Grammar_For_Namespace;

end Schema.Schema_Readers;
