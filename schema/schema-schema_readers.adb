-----------------------------------------------------------------------
--                XML/Ada - An XML suite for Ada95                   --
--                                                                   --
--                Copyright (C) 2004-2010, AdaCore                   --
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

with Ada.Exceptions;    use Ada.Exceptions;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Unicode;           use Unicode;
with Unicode.CES;       use Unicode.CES;
with Sax.Attributes;    use Sax.Attributes;
with Sax.Encodings;     use Sax.Encodings;
with Sax.Readers;       use Sax.Readers;
with Sax.Utils;         use Sax.Utils;
with Schema.Validators; use Schema.Validators;
with Schema.Validators.Lists; use Schema.Validators.Lists;
with Schema.Readers;    use Schema.Readers;
with GNAT.IO;           use GNAT.IO;
with Ada.Unchecked_Deallocation;

package body Schema.Schema_Readers is

   Debug : Boolean := False;

   Max_Namespaces_In_Any_Attribute : constant := 50;
   --  Maximum number of namespaces for a <anyAttribute>
   --  This only impacts the parsing of the grammar, so can easily be raised if
   --  need be.

   procedure Free (C : in out Context_Access; Recurse : Boolean);
   --  Free the memory occupied by C

   procedure Get_Grammar_For_Namespace
     (Handler : in out Schema_Reader'Class;
      Prefix  : Byte_Sequence;
      Grammar : out XML_Grammar_NS);
   --  Return the grammar matching a given prefix

   procedure Output (Str : String);
   --  Output a debug string

   function To_String (Blocks : Block_Status) return String;

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
     (Handler        : in out Schema_Reader;
      In_Context     : Context_Access;
      Attribute      : Attribute_Validator;
      Attribute_Name : Byte_Sequence;
      Is_Local       : Boolean);
   --  Insert attribute at the right location in In_Context.
   --  Attribute_Name is only for debugging purposes

   function Process_Contents_From_Atts
     (Atts : Sax.Attributes.Attributes'Class) return Process_Contents_Type;
   --  Get the value of processContents from the attributes

   procedure Create_Element
     (Handler : access Schema_Reader; Atts : Sax.Attributes.Attributes'Class);
   procedure Create_Complex_Type
     (Handler : access Schema_Reader; Atts : Sax.Attributes.Attributes'Class);
   procedure Create_Simple_Type
     (Handler : access Schema_Reader; Atts : Sax.Attributes.Attributes'Class);
   procedure Create_Restriction
     (Handler : access Schema_Reader; Atts : Sax.Attributes.Attributes'Class);
   procedure Create_All
     (Handler : access Schema_Reader; Atts : Sax.Attributes.Attributes'Class);
   procedure Create_Sequence
     (Handler : access Schema_Reader; Atts : Sax.Attributes.Attributes'Class);
   procedure Create_Attribute
     (Handler : access Schema_Reader; Atts : Sax.Attributes.Attributes'Class);
   procedure Create_Schema
     (Handler : access Schema_Reader; Atts : Sax.Attributes.Attributes'Class);
   procedure Create_Extension
     (Handler : access Schema_Reader; Atts : Sax.Attributes.Attributes'Class);
   procedure Create_List
     (Handler : access Schema_Reader; Atts : Sax.Attributes.Attributes'Class);
   procedure Create_Union
     (Handler : access Schema_Reader; Atts : Sax.Attributes.Attributes'Class);
   procedure Create_Choice
     (Handler : access Schema_Reader; Atts : Sax.Attributes.Attributes'Class);
   procedure Create_Redefine
     (Handler : access Schema_Reader; Atts : Sax.Attributes.Attributes'Class);
   procedure Create_Include
     (Handler : access Schema_Reader; Atts : Sax.Attributes.Attributes'Class);
   procedure Create_Group
     (Handler : access Schema_Reader; Atts : Sax.Attributes.Attributes'Class);
   procedure Create_Attribute_Group
     (Handler : access Schema_Reader; Atts : Sax.Attributes.Attributes'Class);
   procedure Create_Any
     (Handler : access Schema_Reader; Atts : Sax.Attributes.Attributes'Class);
   procedure Create_Import
     (Handler : access Schema_Reader; Atts : Sax.Attributes.Attributes'Class);
   procedure Create_Any_Attribute
     (Handler : access Schema_Reader; Atts : Sax.Attributes.Attributes'Class);
   --  Create a new context for a specific tag:
   --  resp. <element>, <complexType>, <restriction>, <all>, <sequence>,
   --  <attribute>, <schema>, <extension>, <list>, <union>, <choice>,
   --  <redefine>, <group>, <attributeGroup>, <any>, <import>, <anyAttribute>

   procedure Finish_Element      (Handler : access Schema_Reader);
   procedure Finish_Complex_Type (Handler : access Schema_Reader);
   procedure Finish_Simple_Type  (Handler : access Schema_Reader);
   procedure Finish_Restriction  (Handler : access Schema_Reader);
   procedure Finish_All          (Handler : access Schema_Reader);
   procedure Finish_Sequence     (Handler : access Schema_Reader);
   procedure Finish_Attribute    (Handler : access Schema_Reader);
   procedure Finish_Extension    (Handler : access Schema_Reader);
   procedure Finish_Union        (Handler : access Schema_Reader);
   procedure Finish_List         (Handler : access Schema_Reader);
   procedure Finish_Choice       (Handler : access Schema_Reader);
   procedure Finish_Group        (Handler : access Schema_Reader);
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

   procedure Debug_Dump_Contexts (Handler : Schema_Reader; Prefix : String);
   --  List the current contexts

   -------------------------
   -- Debug_Dump_Contexts --
   -------------------------

   procedure Debug_Dump_Contexts (Handler : Schema_Reader; Prefix : String) is
      C : Context_Access := Handler.Contexts;
   begin
      if Debug then
         while C /= null loop
            case C.Typ is
               when Context_Type_Def =>
                  Put_Line (Prefix & "=" & C.Typ'Img & C.Level'Img
                            & " mixed=" & C.Mixed_Content'Img
                            & " simple=" & C.Simple_Content'Img);

               when others =>
                  Put_Line (Prefix & "=" & C.Typ'Img & C.Level'Img);
            end case;
            C := C.Next;
         end loop;
      end if;
   end Debug_Dump_Contexts;

   ---------------
   -- To_String --
   ---------------

   function To_String (Blocks : Block_Status) return String is
   begin
      return "restr=" & Blocks (Block_Restriction)'Img
        & " ext=" & Blocks (Block_Extension)'Img
        & " sub=" & Blocks (Block_Substitution)'Img;
   end To_String;

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
      return "E_" & XML_To_Ada (To_QName (Element));
   end Ada_Name;

   --------------
   -- Ada_Name --
   --------------

   function Ada_Name (Typ : XML_Type) return String is
   begin
      return "T_" & XML_To_Ada (To_QName (Typ));
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
      Reader.Supported.XSD_Version := Get_XSD_Version (Grammar);
      Reader.Created_Grammar := Grammar;
      Reader.Check_Undefined := False;
   end Set_Created_Grammar;

   --------------------
   -- Start_Document --
   --------------------

   procedure Start_Document (Handler : in out Schema_Reader) is
   begin
      Set_XSD_Version (Handler.Created_Grammar,
                       Handler.Supported.XSD_Version);
      Get_NS (Handler.Created_Grammar, XML_Schema_URI, Handler.Schema_NS);

      Set_Validating_Grammar (Handler, Handler.Created_Grammar);
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
      Input  : in out Input_Sources.Input_Source'Class;
      Default_Namespace : Unicode.CES.Byte_Sequence) is
   begin
      if not URI_Was_Parsed
        (Parser.Created_Grammar, Input_Sources.Get_System_Id (Input))
      then
         Get_NS
           (Parser.Created_Grammar, Default_Namespace, Parser.Target_NS);
         if Debug then
            Output ("Get_NS (Handler.Created_Grammar, {"
                    & Get_Namespace_URI (Parser.Target_NS)
                    & "}, Handler.Target_NS)");
         end if;

         Set_Feature (Parser, Sax.Readers.Schema_Validation_Feature, True);
         Set_Parsed_URI
           (Parser.Created_Grammar, Input_Sources.Get_System_Id (Input));
         Parse (Validating_Reader (Parser), Input);

         Set_System_Id (Parser.Target_NS, Input_Sources.Get_System_Id (Input));
      end if;

   exception
      when others =>
         Free (Parser.Contexts, Recurse => True);
         raise;
   end Parse;

   -----------
   -- Parse --
   -----------

   procedure Parse
     (Parser : in out Schema_Reader;
      Input  : in out Input_Sources.Input_Source'Class) is
   begin
      Parse (Parser, Input, Default_Namespace => "");
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

   ------------------
   -- Create_Group --
   ------------------

   procedure Create_Group
     (Handler : access Schema_Reader;
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
         Group_Min       => Min_Occurs,
         Group_Max       => Max_Occurs,
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
         if In_Redefine_Context (Handler.all) then
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
              (Handler.all, Get_Value (Atts, Ref_Index),
               Handler.Contexts.Group);
            Output (Ada_Name (Handler.Contexts) & " := Group;");
         end if;
      end if;

      case Handler.Contexts.Next.Typ is
         when Context_Schema | Context_Redefine =>
            null;

         when Context_Type_Def =>
            null;  --  See Finish_Group

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
            Add_Particle (Seq, Handler.Contexts.Group,
                          Min_Occurs, Max_Occurs);
            Output ("Add_Particle (Validator, " & Ada_Name (Handler.Contexts)
                    & Min_Occurs'Img & "," & Max_Occurs'Img& ");");

            Handler.Contexts.Next.Extension := XML_Validator (Seq);

         when Context_Restriction =>
            Seq := Create_Sequence (Handler.Target_NS);
            Output ("Validator := Create_Sequence;");
            Add_Particle (Seq, Handler.Contexts.Group,
                          Min_Occurs, Max_Occurs);
            Output ("Add_Particle (Validator, " & Ada_Name (Handler.Contexts)
                    & Min_Occurs'Img & "," & Max_Occurs'Img& ");");
            Handler.Contexts.Next.Restriction := XML_Validator (Seq);

         when others =>
            Output ("Can't handle nested group decl");
            Raise_Exception
              (XML_Not_Implemented'Identity,
               "Unsupported: ""group"" in this context");
      end case;
   end Create_Group;

   ------------------
   -- Finish_Group --
   ------------------

   procedure Finish_Group (Handler : access Schema_Reader) is
      Seq : Sequence;
   begin
      case Handler.Contexts.Next.Typ is
         when Context_Type_Def =>
            Seq := Create_Sequence (Handler.Target_NS);
            Add_Particle (Seq, Handler.Contexts.Group,
                          Handler.Contexts.Group_Min,
                          Handler.Contexts.Group_Max);

            Handler.Contexts.Next.Type_Validator := Restriction_Of
              (Handler.Target_NS,
               Lookup (Handler.Schema_NS, "anyType"), XML_Validator (Seq));
            Output ("Validator := Restriction_Of (Lookup (Handler.Schema.NS,"
                    & """anytype""), " & Ada_Name (Handler.Contexts));

         when others =>
            null;
      end case;
   end Finish_Group;

   ----------------------------
   -- Create_Attribute_Group --
   ----------------------------

   procedure Create_Attribute_Group
     (Handler : access Schema_Reader;
      Atts    : Sax.Attributes.Attributes'Class)
   is
      Name_Index : constant Integer :=
        Get_Index (Atts, URI => "", Local_Name => "name");
      Ref_Index : constant Integer :=
        Get_Index (Atts, URI => "", Local_Name => "ref");
      In_Redefine : constant Boolean := In_Redefine_Context (Handler.all);
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
           (Handler.all, Get_Value (Atts, Ref_Index),
            Handler.Contexts.Attr_Group);
         Output (Ada_Name (Handler.Contexts) & " := Attr_Group");
      end if;

      if not In_Redefine then
         case Handler.Contexts.Next.Typ is
            when Context_Schema | Context_Redefine =>
               null;

            when Context_Type_Def =>
               Ensure_Type (Handler.all, Handler.Contexts.Next);
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
               Raise_Exception
                 (XML_Not_Implemented'Identity,
                  "Unsupported: ""attributeGroup"" in this context");
         end case;
      end if;
   end Create_Attribute_Group;

   --------------------
   -- Create_Include --
   --------------------

   procedure Create_Include
     (Handler : access Schema_Reader;
      Atts    : Sax.Attributes.Attributes'Class)
   is
      Schema_Location_Index : constant Integer :=
        Get_Index (Atts, URI => "", Local_Name => "schemaLocation");
   begin
      Parse_Grammar
        (Handler.all,
         URI      => Get_Namespace_URI (Handler.Target_NS),
         Xsd_File => Get_Value (Atts, Schema_Location_Index),
         Add_To   => Handler.Created_Grammar);
   end Create_Include;

   ---------------------
   -- Create_Redefine --
   ---------------------

   procedure Create_Redefine
     (Handler : access Schema_Reader;
      Atts    : Sax.Attributes.Attributes'Class)
   is
      Location_Index : constant Integer :=
        Get_Index (Atts, URI => "", Local_Name => "schemaLocation");
   begin
      --  Disable for now.
      --  On the test./testschema -xsd boeingData/ipo4/ipo.xsd
      --    -xsd boeingData/ipo4/address.xsd
      --    -xsd boeingData/ipo4/itematt.xsd
      --    boeingData/ipo4/ipo_1.xml
      --  we redefine an extension whose base type comes from the redefined
      --  grammar, and whose name is the same. As a result, the extension and
      --  its base type end up being the same XML_Type, and thus we get
      --  infinite loops. We should really merge the models when the grammar is
      --  parsed.

      Raise_Exception
        (XML_Not_Implemented'Identity,
         "<redefine> not supported");
      Parse_Grammar
        (Handler.all,
         URI      => Get_Namespace_URI (Handler.Target_NS),
         Xsd_File => Get_Value (Atts, Location_Index),
         Add_To   => Handler.Created_Grammar);

      Handler.Contexts := new Context'
        (Typ            => Context_Redefine,
         Level          => Handler.Contexts.Level + 1,
         Next           => Handler.Contexts);
   end Create_Redefine;

   -------------------
   -- Create_Import --
   -------------------

   procedure Create_Import
     (Handler : access Schema_Reader;
      Atts    : Sax.Attributes.Attributes'Class)
   is
      Location_Index : constant Integer :=
        Get_Index (Atts, URI => "", Local_Name => "schemaLocation");
      Namespace_Index : constant Integer :=
        Get_Index (Atts, URI => "", Local_Name => "namespace");
      NS : XML_Grammar_NS;
   begin
      if Location_Index = -1 then
         if Namespace_Index = -1 then
            Validation_Error ("Missing ""namespace"" attribute");
         end if;

         declare
            N : constant Byte_Sequence := Get_Value (Atts, Namespace_Index);
         begin
            Get_NS
              (Handler.Created_Grammar, N,
               Result => NS, Create_If_Needed => False);
            if NS = null then
               Validation_Error ("Cannot resolve namespace " & N);
            end if;
         end;
      else
         declare
            Location : constant Byte_Sequence :=
              Get_Value (Atts, Location_Index);
            Absolute : constant Byte_Sequence := To_Absolute_URI
              (Handler.all, Location);
         begin
            if Debug then
               Put_Line ("Import: " & Absolute);
               Put_Line ("Adding new grammar to Handler.Created_Grammar");
            end if;

            if not URI_Was_Parsed (Handler.Created_Grammar, Absolute) then
               --  The namespace attribute indicates that the XSD may contain
               --  qualified references to schema components in that namespace.
               --  (4.2.6.1). It does not give the default targetNamespace
               Parse_Grammar
                 (Handler.all,
                  URI      => "",
                  Xsd_File => Location,
                  Add_To   => Handler.Created_Grammar);
            elsif Debug then
               Put_Line ("Already imported");
            end if;
         end;
      end if;
   end Create_Import;

   --------------------------
   -- Create_Any_Attribute --
   --------------------------

   procedure Create_Any_Attribute
     (Handler : access Schema_Reader;
      Atts    : Sax.Attributes.Attributes'Class)
   is
      Namespace_Index : constant Integer :=
        Get_Index (Atts, URI => "", Local_Name => "namespace");
      Process_Contents : constant Process_Contents_Type :=
        Process_Contents_From_Atts (Atts);
      Fixed_Index : constant Integer :=
        Get_Index (Atts, URI => "", Local_Name => "fixed");
      Kind  : Namespace_Kind;

      List  : NS_List (1 .. Max_Namespaces_In_Any_Attribute);
      Last  : Integer := List'First;

      procedure Cb_Item (Str : Byte_Sequence);
      procedure Cb_Item (Str : Byte_Sequence) is
      begin
         List (Last) := new Byte_Sequence'(Str);
         Last := Last + 1;
      end Cb_Item;

      procedure For_Each is new For_Each_Item (Cb_Item);
   begin
      if Fixed_Index /= -1 then
         Raise_Exception
           (XML_Not_Implemented'Identity,
            """fixed"" not supported for <anyAttribute>");
      end if;

      if Namespace_Index = -1 then
         Kind := Namespace_Any;
      else
         declare
            Val : constant Byte_Sequence := Get_Value (Atts, Namespace_Index);
         begin
            if Val = "##other" then
               Kind := Namespace_Other;
            elsif Val = "##any" then
               Kind := Namespace_Any;
            else
               Kind := Namespace_List;
               For_Each (Val);
            end if;
         end;
      end if;

      Insert_Attribute
        (Handler.all,
         Handler.Contexts,
         Create_Any_Attribute
           (Handler.Target_NS, Process_Contents, Kind, List (1 .. Last - 1)),
         "Create_Any_Attribute (" & Process_Contents'Img
         & ", " & Kind'Img & """);",
         Is_Local => False);
   end Create_Any_Attribute;

   --------------------
   -- Create_Element --
   --------------------

   procedure Create_Element
     (Handler : access Schema_Reader;
      Atts    : Sax.Attributes.Attributes'Class)
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
              (Handler.all, Get_Value (Atts, Type_Index), Result => Typ);

            if To_QName (Typ) = "IDREF"
              or else To_QName (Typ) = "IDREFS"
            then
               Raise_Exception
                 (XML_Not_Implemented'Identity,
                  "Unsupported type IDREF and IDREFS");
            end if;
         end if;

         case Handler.Contexts.Typ is
            when Context_Schema | Context_Redefine =>
               Element := Create_Global_Element
                 (Handler.Target_NS,
                  Get_Value (Atts, Name_Index),
                  Form => Form);
               Is_Ref := False;
               Output (Ada_Name (Element)
                       & " := Create_Global_Element ({"
                       & Get_Namespace_URI (Handler.Target_NS)
                       & "}, """
                       & Get_Value (Atts, Name_Index) & """, " & Form'Img
                       & ");");

               if Typ /= No_Type then
                  Output ("Set_Type (" & Ada_Name (Element) & ", "
                          & Ada_Name (Typ) & ");");
                  Set_Type (Element, Typ, Get_Context (Handler).all);
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
           and then not In_Redefine_Context (Handler.all)
         then
            Validation_Error
              ("""ref"" attribute cannot be self-referencing");
         end if;

      elsif Ref_Index = -1 then
         Validation_Error
           ("Either ""name"" or ""ref"" attribute must be present");

      else
         Lookup_With_NS
           (Handler.all, Get_Value (Atts, Ref_Index), Result => Element);
         Is_Ref := True;

         --  Section 3.3.2, validity constraints 3.3.3
         if Type_Index /= -1 then
            Validation_Error
              ("""type"" attribute cannot be specified along with ""ref""");
         end if;
      end if;

      if Subst_Index /= -1 then
         Lookup_With_NS
           (Handler.all, Get_Value (Atts, Subst_Index), Result => Group);
         Set_Substitution_Group (Element, Group);
         Output ("Set_Substitution_Group ("
                 & Ada_Name (Element) & ", " & Ada_Name (Group) & ");");
      end if;

      if Default_Index /= -1 then
         Set_Default
           (Element, Get_Value (Atts, Default_Index),
            Get_Context (Handler).all);
         Output ("Set_Default ("
                 & Ada_Name (Element) & ", """
                 & Get_Value (Atts, Default_Index) & """);");
      end if;

      if Fixed_Index /= -1 then
         Set_Fixed
           (Element, Get_Value (Atts, Fixed_Index),
            Get_Context (Handler).all);
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
            Restrictions, Extensions, Unions, Lists : Boolean := False;
         begin
            Compute_Final
              (Get_Value (Atts, Final_Index),
               Restrictions => Restrictions,
               Extensions   => Extensions,
               Unions       => Unions,
               Lists        => Lists);
            Set_Final (Element,
                       On_Restriction => Restrictions,
                       On_Extension   => Extensions,
                       On_Unions      => Unions,
                       On_Lists       => Lists);
            Output ("Set_Final ("
                    & Ada_Name (Element) & ", "
                    & Boolean'Image (Restrictions) & ", "
                    & Boolean'Image (Extensions) & ");");
         end;
      end if;

      if Block_Index /= -1 then
         declare
            Blocks : Block_Status;
         begin
            Compute_Blocks (Get_Value (Atts, Block_Index), Blocks);
            Set_Block (Element, Blocks);
            Output ("Set_Block (" & Ada_Name (Element) & ", "
                    & To_String (Blocks) & ")");
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
              (Handler.Contexts.Seq, Element,
               Min_Occurs       => Min_Occurs,
               Max_Occurs       => Max_Occurs);
            Output ("Add_Particle (" & Ada_Name (Handler.Contexts)
                    & ", " & Ada_Name (Element) & ", is_ref="
                    & Boolean'Image (Ref_Index /= -1) & ','
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
            Raise_Exception
              (XML_Not_Implemented'Identity,
               "Unsupported: ""element"" in this context");
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

   procedure Finish_Element (Handler : access Schema_Reader) is
   begin
      if not Handler.Contexts.Is_Ref
        and then Get_Type (Handler.Contexts.Element) = No_Type
      then
         --  From 3.3.2.1, the type should be that of the substitutionGroup
         --  attribute if there is any

         if Get_Substitution_Group (Handler.Contexts.Element) /=
           No_Element
         then
            Output ("Set_Type (" & Ada_Name (Handler.Contexts)
                    & " from substitution group");

            if Get_Type (Get_Substitution_Group (Handler.Contexts.Element)) =
               No_Type
            then
               Raise_Exception
                 (XML_Not_Implemented'Identity,
                  "Not supported: type computed from substitutionGroup when"
                  & " the group has not been fully defined yet");
            end if;

            Set_Type
              (Handler.Contexts.Element,
               Get_Type (Get_Substitution_Group (Handler.Contexts.Element)),
               Get_Context (Handler).all);

         else
            --  Otherwise the type is anyType
            Output ("Set_Type (" & Ada_Name (Handler.Contexts)
                    & ", Lookup (Handler.Schema_NS, ""ur-Type"");");
            Set_Type (Handler.Contexts.Element,
                      Lookup (Handler.Schema_NS, "ur-Type"),
                      Get_Context (Handler).all);
         end if;
      end if;
   end Finish_Element;

   ------------------------
   -- Create_Simple_Type --
   ------------------------

   procedure Create_Simple_Type
     (Handler  : access Schema_Reader;
      Atts     : Sax.Attributes.Attributes'Class)
   is
   begin
      if Handler.Contexts.Typ = Context_Restriction
        and then Handler.Contexts.Restriction_Base = No_Type
      then
         Handler.Contexts := new Context'
           (Typ               => Context_Type_Def,
            Type_Name         => null,
            Type_Validator    => null,
            Redefined_Type    => No_Type,
            Mixed_Content     => False,
            Simple_Content    => True,
            Blocks            => (others => False),
            Final_Restriction => False,
            Final_Extension   => False,
            Final_Unions      => False,
            Final_Lists       => False,
            Level             => Handler.Contexts.Level + 1,
            Next              => Handler.Contexts);

      else
         Create_Complex_Type (Handler, Atts);
      end if;
   end Create_Simple_Type;

   ------------------------
   -- Finish_Simple_Type --
   ------------------------

   procedure Finish_Simple_Type (Handler : access Schema_Reader) is
      C   : constant Context_Access := Handler.Contexts;
      Typ : XML_Type;
   begin
      if C.Next.Typ = Context_Restriction
        and then C.Next.Restriction_Base = No_Type
      then
         Ensure_Type (Handler.all, C);

         Typ := Create_Local_Type (Handler.Target_NS, C.Type_Validator);
         Output (Ada_Name (C) & " := Create_Local_Type (Validator);");

         Set_Final (Typ,
                    On_Restriction => Handler.Contexts.Final_Restriction,
                    On_Extension   => Handler.Contexts.Final_Extension,
                    On_Unions      => Handler.Contexts.Final_Unions,
                    On_Lists       => Handler.Contexts.Final_Lists);
         Output ("Set_Final ("
                 & Ada_Name (Typ) & ", "
                 & Boolean'Image (Handler.Contexts.Final_Restriction) & ", "
                 & Boolean'Image (Handler.Contexts.Final_Extension) & ");");

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
     (Handler  : access Schema_Reader;
      Atts     : Sax.Attributes.Attributes'Class)
   is
      Name_Index : constant Integer :=
        Get_Index (Atts, URI => "", Local_Name => "name");
      Mixed_Index : constant Integer :=
        Get_Index (Atts, URI => "", Local_Name => "mixed");
      Block_Index : constant Integer :=
        Get_Index (Atts, URI => "", Local_Name => "block");
      Final_Index : constant Integer :=
        Get_Index (Atts, URI => "", Local_Name => "final");
      Abstract_Index : constant Integer :=
        Get_Index (Atts, URI => "", Local_Name => "abstract");
      Name    : Byte_Sequence_Access;
      Mixed   : Boolean;

   begin
      if Abstract_Index /= -1 then
         --  Not supported yet anyway
         Raise_Exception
           (XML_Not_Implemented'Identity,
            "Unsupported ""abstract"" attribute for complexType");
      end if;

      if Name_Index /= -1 then
         Name := new Byte_Sequence'(Get_Value (Atts, Name_Index));
      end if;

      Mixed := Mixed_Index /= -1
        and then Get_Value_As_Boolean (Atts, Mixed_Index);

      Handler.Contexts := new Context'
        (Typ               => Context_Type_Def,
         Type_Name         => Name,
         Type_Validator    => null,
         Redefined_Type    => No_Type,
         Mixed_Content     => Mixed,
         Simple_Content    => False,
         Blocks            => Get_Block_Default (Handler.Target_NS),
         Final_Restriction => False,
         Final_Extension   => False,
         Final_Unions      => False,
         Final_Lists       => False,
         Level             => Handler.Contexts.Level + 1,
         Next              => Handler.Contexts);

      if Block_Index /= -1 then
         Compute_Blocks
           (Get_Value (Atts, Block_Index), Blocks => Handler.Contexts.Blocks);
      end if;

      if Final_Index /= -1 then
         Compute_Final
           (Get_Value (Atts, Final_Index),
            Restrictions  => Handler.Contexts.Final_Restriction,
            Extensions    => Handler.Contexts.Final_Extension,
            Unions        => Handler.Contexts.Final_Unions,
            Lists         => Handler.Contexts.Final_Lists);
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
      Base  : XML_Type;
   begin
      if C.Type_Validator = null then
         --  Create a restriction, instead of a simple ur-Type, so that we can
         --  add attributes to it without impacting ur-Type itself
         Get_NS (Handler.Created_Grammar, XML_Schema_URI, XML_G);

         if C.Simple_Content then
            Output
              ("Validator := Restriction_Of (Lookup (G, ""anySimpleType""));");
            Base := Lookup (XML_G, "anySimpleType");

         else
            Output ("Validator := Restriction_Of (Lookup (G, ""anyType""));");
            Base := Lookup (XML_G, "anyType");
         end if;

         C.Type_Validator := Restriction_Of (XML_G, Base);
      end if;
   end Ensure_Type;

   -------------------------
   -- Finish_Complex_Type --
   -------------------------

   procedure Finish_Complex_Type (Handler  : access Schema_Reader) is
      C   : constant Context_Access := Handler.Contexts;
      Typ : XML_Type;
   begin
      Ensure_Type (Handler.all, C);
      if C.Type_Name = null then
         Typ := Create_Local_Type (Handler.Target_NS, C.Type_Validator);
         Output (Ada_Name (C) & " := Create_Local_Type (Validator);");
      else
         Typ := Create_Global_Type
           (Handler.Target_NS, C.Type_Name.all, C.Type_Validator);
         Set_Debug_Name (C.Type_Validator, "for_type_" & C.Type_Name.all);
         Output (Ada_Name (C)
                 & " := Create_Global_Type ({"
                 & Get_Namespace_URI (Handler.Target_NS)
                 & "}, """ & C.Type_Name.all & """, Validator);");
      end if;

      Set_Block (Typ, Handler.Contexts.Blocks);
      Output ("Set_Block (" & Ada_Name (Typ) & ", "
              & To_String (Handler.Contexts.Blocks) & ")");

      Set_Final (Typ,
                 On_Restriction => Handler.Contexts.Final_Restriction,
                 On_Extension   => Handler.Contexts.Final_Extension,
                 On_Unions      => Handler.Contexts.Final_Unions,
                 On_Lists       => Handler.Contexts.Final_Lists);
      Output ("Set_Final ("
              & Ada_Name (Typ) & ", "
              & Boolean'Image (Handler.Contexts.Final_Restriction) & ", "
              & Boolean'Image (Handler.Contexts.Final_Extension) & ");");

      Set_Mixed_Content
        (Get_Validator (Typ), Handler.Contexts.Mixed_Content
                              or Handler.Contexts.Simple_Content);
      Output ("Set_Mixed_Content ("
              & Ada_Name (C) & ", "
        & Boolean'Image (Handler.Contexts.Mixed_Content
                         or Handler.Contexts.Simple_Content) & ");");

      case Handler.Contexts.Next.Typ is
         when Context_Schema | Context_Redefine =>
            null;
         when Context_Element =>
            Output ("Set_Type (" & Ada_Name (Handler.Contexts.Next)
                    & ", " & Ada_Name (C) & ");");
            Set_Type (Handler.Contexts.Next.Element, Typ,
                      Get_Context (Handler).all);
         when Context_Attribute =>
            Set_Type (Handler.Contexts.Next.Attribute, Typ);
            Output ("Set_Type (" & Ada_Name (Handler.Contexts.Next)
                    & ", " & Ada_Name (Typ) & ");");
         when Context_List =>
            Handler.Contexts.Next.List_Items := Typ;
            Output ("Validator := " & Ada_Name (C) & ";");
         when others =>
            Output ("Can't handle nested type decl");
            Raise_Exception
              (XML_Not_Implemented'Identity,
               "Unsupported: ""complexType"" in this context");
      end case;
   end Finish_Complex_Type;

   ------------------------
   -- Create_Restriction --
   ------------------------

   procedure Create_Restriction
     (Handler  : access Schema_Reader;
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
         if In_Redefine_Context (Handler.all) then
            Base := Handler.Contexts.Redefined_Type;
         else
            Validation_Error
              ("Self-referencing restriction not allowed");
         end if;

      elsif Base_Index /= -1 then
         Lookup_With_NS
           (Handler.all, Get_Value (Atts, Base_Index), Result => Base);

         if To_QName (Base) = "IDREF"
           or else To_QName (Base) = "IDREFS"
         then
            Raise_Exception
              (XML_Not_Implemented'Identity,
               "Unsupported type IDREF and IDREFS");
         end if;

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

   procedure Finish_Restriction (Handler : access Schema_Reader) is
   begin
      Create_Restricted (Handler.all, Handler.Contexts);

      case Handler.Contexts.Next.Typ is
         when Context_Type_Def =>
            Handler.Contexts.Next.Type_Validator :=
              Handler.Contexts.Restricted;
            Set_Debug_Name (Handler.Contexts.Next.Type_Validator,
                            Ada_Name (Handler.Contexts));
            Output ("Validator := " & Ada_Name (Handler.Contexts) & ";");
         when others =>
            Output ("Can't handler nested restrictions");
            Raise_Exception
              (XML_Not_Implemented'Identity,
               "Unsupported: ""restriction"" in this context");
      end case;
   end Finish_Restriction;

   ------------------
   -- Create_Union --
   ------------------

   procedure Create_Union
     (Handler  : access Schema_Reader;
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
            procedure Cb_Item (Str : Byte_Sequence);

            procedure Cb_Item (Str : Byte_Sequence) is
               Typ : XML_Type;
            begin
               Lookup_With_NS (Handler.all, Str, Typ);
               Add_Union (Handler.Contexts.Union, Typ);
               Output ("Add_Union ("
                       & Ada_Name (Handler.Contexts) & ", """ & Str & """)");
            end Cb_Item;

            procedure For_Each is new For_Each_Item (Cb_Item);
         begin
            For_Each (Get_Value (Atts, Member_Index));
         end;
      end if;
   end Create_Union;

   ------------------
   -- Finish_Union --
   ------------------

   procedure Finish_Union (Handler : access Schema_Reader) is
   begin
      case Handler.Contexts.Next.Typ is
         when Context_Type_Def =>
            Handler.Contexts.Next.Type_Validator :=
              XML_Validator (Handler.Contexts.Union);
            Output ("Validator := " & Ada_Name (Handler.Contexts) & ";");

         when others =>
            Output ("Can't handle nested unions");
            Raise_Exception
              (XML_Not_Implemented'Identity,
               "Unsupported: ""union"" in this context");
      end case;
   end Finish_Union;

   ----------------------
   -- Create_Extension --
   ----------------------

   procedure Create_Extension
     (Handler  : access Schema_Reader;
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
         if In_Redefine_Context (Handler.all) then
            Base := Handler.Contexts.Redefined_Type;
         else
            Validation_Error
              ("Self-referencing extension not allowed");
         end if;
      else
         Lookup_With_NS
           (Handler.all, Get_Value (Atts, Base_Index), Result => Base);
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

   procedure Finish_Extension (Handler : access Schema_Reader) is
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
                     & To_QName (Handler.Contexts.Extension_Base));
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
            Raise_Exception
              (XML_Not_Implemented'Identity,
               "Unsupported: ""extension"" in this context");
      end case;
   end Finish_Extension;

   -----------------
   -- Create_List --
   -----------------

   procedure Create_List
     (Handler : access Schema_Reader;
      Atts    : Sax.Attributes.Attributes'Class)
   is
      Item_Type_Index : constant Integer :=
        Get_Index (Atts, URI => "", Local_Name => "itemType");
      Items : XML_Type;
   begin
      if Item_Type_Index /= -1 then
         Lookup_With_NS
           (Handler.all, Get_Value (Atts, Item_Type_Index), Result => Items);
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

   procedure Finish_List (Handler : access Schema_Reader) is
   begin
      case Handler.Contexts.Next.Typ is
         when Context_Type_Def =>
            Handler.Contexts.Next.Type_Validator :=
              Get_Validator
                (List_Of (Handler.Target_NS, Handler.Contexts.List_Items));
            Output ("Validator := List_Of (Validator);");
         when others =>
            Output ("Can't handle nested list");
            Raise_Exception
              (XML_Not_Implemented'Identity,
               "Unsupported: ""list"" in this context");
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
     (Handler : access Schema_Reader; Atts : Sax.Attributes.Attributes'Class)
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
              (Handler.all, Handler.Contexts.C, Min_Occurs, Max_Occurs);
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
              (Handler.all, Handler.Contexts.C, Min_Occurs, Max_Occurs);

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
            Raise_Exception
              (XML_Not_Implemented'Identity,
               "Unsupported: ""choice"" in this context");
      end case;
   end Create_Choice;

   -------------------
   -- Finish_Choice --
   -------------------

   procedure Finish_Choice (Handler : access Schema_Reader) is
      pragma Unreferenced (Handler);
   begin
      null;
   end Finish_Choice;

   ---------------------
   -- Create_Sequence --
   ---------------------

   procedure Create_Sequence
     (Handler  : access Schema_Reader;
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
              (Handler.all, Handler.Contexts.Seq, Min_Occurs, Max_Occurs);
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
              (Handler.all, Handler.Contexts.Seq, Min_Occurs, Max_Occurs);
         when Context_Restriction =>
            Output ("Validator := " & Ada_Name (Handler.Contexts) & ";");
            Handler.Contexts.Next.Restriction := Create_Repeat
              (Handler.all, Handler.Contexts.Seq, Min_Occurs, Max_Occurs);
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
            Raise_Exception
              (XML_Not_Implemented'Identity,
               "Unsupported: ""sequence"" in this context");
      end case;
   end Create_Sequence;

   ---------------------
   -- Finish_Sequence --
   ---------------------

   procedure Finish_Sequence (Handler : access Schema_Reader) is
      pragma Unreferenced (Handler);
   begin
      null;
   end Finish_Sequence;

   ----------------------
   -- Create_Attribute --
   ----------------------

   procedure Create_Attribute
     (Handler  : access Schema_Reader;
      Atts     : Sax.Attributes.Attributes'Class)
   is
      Name_Index : constant Integer :=
        Get_Index (Atts, URI => "", Local_Name => "name");
      Type_Index : constant Integer :=
        Get_Index (Atts, URI => "", Local_Name => "type");
      Use_Index : constant Integer :=
        Get_Index (Atts, URI => "", Local_Name => "use");
      Fixed_Index : constant Integer :=
        Get_Index (Atts, URI => "", Local_Name => "fixed");
      Ref_Index : constant Integer :=
        Get_Index (Atts, URI => "", Local_Name => "ref");
      Form_Index : constant Integer :=
        Get_Index (Atts, URI => "", Local_Name => "form");
      Default_Index : constant Integer :=
        Get_Index (Atts, URI => "", Local_Name => "default");

      Att : Attribute_Validator;
      Typ : XML_Type := No_Type;
      Use_Type : Attribute_Use_Type := Optional;
      Form : Form_Type;

      function Get_Fixed return String;
      --  Return the "fixed" value for the attribute

      function Get_Fixed return String is
      begin
         if Fixed_Index /= -1 then
            if Typ /= No_Type then
               Normalize_Whitespace  --  Depending on the type of the attribute
                 (Typ   => Typ,
                  Atts  => Atts,
                  Index => Fixed_Index);
            end if;
            return Get_Value (Atts, Fixed_Index);
         else
            return "";
         end if;
      end Get_Fixed;

   begin
      if Form_Index /= -1 then
         Form := Form_Type'Value (Get_Value (Atts, Form_Index));
      else
         Form := Handler.Attribute_Form_Default;
      end if;

      if Type_Index /= -1 then
         if Ref_Index /= -1 then
            Validation_Error
              ("Attributes ""type"" and ""ref"" cannot be both specified");
         end if;

         Lookup_With_NS
           (Handler.all, Get_Value (Atts, Type_Index), Result => Typ);

         if To_QName (Typ) = "IDREF"
           or else To_QName (Typ) = "IDREFS"
         then
            Raise_Exception
              (XML_Not_Implemented'Identity,
               "Unsupported type IDREF and IDREFS");
         end if;
      end if;

      if Fixed_Index /= -1 and then Default_Index /= -1 then
         Validation_Error
           ("Attributes ""fixed"" and ""default"" cannot be both specified");
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
            else
               Use_Type := Optional;
            end if;
         end;
      end if;

      Handler.Contexts := new Context'
        (Typ              => Context_Attribute,
         Attribute        => null,
         Attribute_Is_Ref => Ref_Index /= -1,
         Level            => Handler.Contexts.Level + 1,
         Next             => Handler.Contexts);

      if Name_Index /= -1 then
         case Handler.Contexts.Next.Typ is
            when Context_Schema | Context_Redefine =>
               Att := Create_Global_Attribute
                 (Local_Name     => Get_Value (Atts, Name_Index),
                  NS             => Handler.Target_NS,
                  Attribute_Type => Typ,
                  Attribute_Use  => Use_Type,
                  Attribute_Form => Form,
                  Has_Fixed      => Fixed_Index /= -1,
                  Fixed          => Get_Fixed);

               if Debug then
                  Output (Ada_Name (Handler.Contexts)
                          & " := Create_Global_Attribute ("""
                          & Get_Value (Atts, Name_Index)
                          & """, Handler.Target_NS, "
                          & Ada_Name (Typ)
                          & ", " & Use_Type'Img & ", Qualified, Has_Fixed="
                          & Boolean'Image (Fixed_Index /= -1)
                          & ", " & Form'Img
                          & " """ & Get_Fixed & """);");
               end if;

            when others =>
               Att := Create_Local_Attribute
                 (Local_Name     => Get_Value (Atts, Name_Index),
                  NS             => Handler.Target_NS,
                  Attribute_Type => Typ,
                  Attribute_Use  => Use_Type,
                  Attribute_Form => Form,
                  Has_Fixed      => Fixed_Index /= -1,
                  Fixed          => Get_Fixed,
                  Value          => "");

               if Debug then
                  Output (Ada_Name (Handler.Contexts)
                          & " := Create_Local_Attribute ("""
                          & Get_Value (Atts, Name_Index)
                          & """, Handler.Target_NS, "
                          & Ada_Name (Typ)
                          & ", " & Use_Type'Img & ", Qualified, Has_Fixed="
                          & Boolean'Image (Fixed_Index /= -1)
                          & ", " & Form'Img
                          & " """ & Get_Fixed & """);");
               end if;
         end case;
      else
         declare
            QName     : constant Byte_Sequence := Get_Value (Atts, Ref_Index);
            Separator : constant Integer := Split_Qname (QName);
            G         : XML_Grammar_NS;
         begin
            Get_Grammar_For_Namespace
              (Handler.all, QName (QName'First .. Separator - 1), G);
            Att := Lookup_Attribute (G, QName (Separator + 1 .. QName'Last));

            --  ??? We haven't normalized the value for fixed here
            Att := Create_Local_Attribute
              (Based_On       => Att,
               Attribute_Use  => Use_Type,
               Attribute_Form => Form,
               Has_Fixed      => Fixed_Index /= -1,
               Fixed          => Get_Fixed,
               Value          => "");

            if Debug then
               Output
                 ("Attr := Lookup_Attribute_NS (G, """
                  & QName (Separator + 1 .. QName'Last) & """);");
               Output
                 (Ada_Name (Handler.Contexts)
                  & " := Create_Local_Attribute (Attr, Handler.Target_NS, "
                  & Use_Type'Img & ", Qualified, Has_Fixed="
                  & Boolean'Image (Fixed_Index /= -1)
                  & ", " & Form'Img
                  & " """ & Get_Fixed & """);");
            end if;
         end;
      end if;

      Handler.Contexts.Attribute := Att;
   end Create_Attribute;

   ----------------------
   -- Insert_Attribute --
   ----------------------

   procedure Insert_Attribute
     (Handler        : in out Schema_Reader;
      In_Context     : Context_Access;
      Attribute      : Attribute_Validator;
      Attribute_Name : Byte_Sequence;
      Is_Local       : Boolean) is
   begin
      case In_Context.Typ is
         when Context_Type_Def =>
            Ensure_Type (Handler, In_Context);
            Add_Attribute (In_Context.Type_Validator, Attribute,
                           Is_Local => Is_Local);
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

            Add_Attribute (In_Context.Extension, Attribute,
                           Is_Local => Is_Local);
            Output ("Add_Attribute (" & Ada_Name (In_Context) & ", "
                    & Attribute_Name & ");");

         when Context_Restriction =>
            Create_Restricted (Handler, In_Context);
            Add_Attribute (In_Context.Restricted, Attribute,
                           Is_Local => Is_Local);
            Output ("Add_Attribute (" & Ada_Name (In_Context) & ", "
                    & Attribute_Name & ");");

         when Context_Attribute_Group =>
            Add_Attribute (In_Context.Attr_Group, Attribute,
                           Is_Local => Is_Local);
            Output ("Add_Attribute (" & Ada_Name (In_Context) & ", "
                    & Attribute_Name & ");");

         when Context_Element | Context_Sequence | Context_Choice
            | Context_Attribute | Context_All
            | Context_Union | Context_List | Context_Group =>
            Output ("Can't handle attribute decl in this context");
            Raise_Exception
              (XML_Not_Implemented'Identity,
               "Unsupported: ""attribute"" in this context");
      end case;
   end Insert_Attribute;

   ----------------------
   -- Finish_Attribute --
   ----------------------

   procedure Finish_Attribute (Handler : access Schema_Reader) is
   begin
      if not Handler.Contexts.Attribute_Is_Ref
        and then Get_Type (Handler.Contexts.Attribute.all) = No_Type
      then
         Set_Type (Handler.Contexts.Attribute,
                   Lookup (Handler.Schema_NS, "ur-Type"));
         Output ("Set_Type (" & Ada_Name (Handler.Contexts)
                 & ", Lookup (Handler.Schema_NS, ""ur-Type"");");
      end if;

      Insert_Attribute
        (Handler.all, Handler.Contexts.Next, Handler.Contexts.Attribute,
         Ada_Name (Handler.Contexts),
         Is_Local => not Handler.Contexts.Attribute_Is_Ref);
   end Finish_Attribute;

   -------------------
   -- Create_Schema --
   -------------------

   procedure Create_Schema
     (Handler  : access Schema_Reader;
      Atts     : Sax.Attributes.Attributes'Class)
   is
      Target_NS_Index : constant Integer :=
        Get_Index (Atts, URI => "", Local_Name => "targetNamespace");
      Form_Default_Index : constant Integer :=
        Get_Index (Atts, URI => "", Local_Name => "elementFormDefault");
      Attr_Form_Default_Index : constant Integer :=
        Get_Index (Atts, URI => "", Local_Name => "attributeFormDefault");
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

      if Attr_Form_Default_Index /= -1
        and then Get_Value (Atts, Attr_Form_Default_Index) = "qualified"
      then
         Handler.Attribute_Form_Default := Qualified;
         Output
           ("Set_Attribute_Form_Default (Handler.Target_NS, Qualified);");
      else
         Handler.Attribute_Form_Default := Unqualified;
         Output
           ("Set_Attribute_Form_Default (Handler.Target_NS, Unqualified);");
      end if;

      if Block_Index /= -1 then
         declare
            Blocks : Block_Status;
         begin
            Compute_Blocks (Get_Value (Atts, Block_Index), Blocks);
            Set_Block_Default (Handler.Target_NS, Blocks);
            Output ("Set_Block (Handler.Target_NS, "
                    & To_String (Blocks) & ")");
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
     (Handler : access Schema_Reader;
      Atts    : Sax.Attributes.Attributes'Class)
   is
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
            Raise_Exception
              (XML_Not_Implemented'Identity,
               "Unsupported: ""any"" in this context");
      end case;
   end Create_Any;

   ----------------
   -- Create_All --
   ----------------

   procedure Create_All
     (Handler  : access Schema_Reader;
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

   procedure Finish_All (Handler : access Schema_Reader) is
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
            Raise_Exception
              (XML_Not_Implemented'Identity,
               "Unsupported: ""all"" in this context");
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
      if False and Debug then
         Debug_Dump_Contexts (Handler, "Start");
      end if;

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

         Create_Schema (Handler'Access, Atts);

      elsif Local_Name = "annotation" then
         Handler.In_Annotation := True;

      elsif Local_Name = "element" then
         Create_Element (Handler'Access, Atts);

      elsif Local_Name = "complexType" then
         Create_Complex_Type (Handler'Access, Atts);

      elsif Local_Name = "simpleType" then
         Create_Simple_Type (Handler'Access, Atts);

      elsif Local_Name = "restriction" then
         Create_Restriction (Handler'Access, Atts);

      elsif Local_Name = "extension" then
         Create_Extension (Handler'Access, Atts);

      elsif Local_Name = "anyAttribute" then
         Create_Any_Attribute (Handler'Access, Atts);

      elsif Local_Name = "pattern" then
         declare
            Val2 : constant Byte_Sequence :=
              Get_Non_Normalized_Value (Atts, "", "value");
         begin
            Create_Restricted (Handler, Handler.Contexts);
            Add_Facet (Handler.Contexts.Restricted, Local_Name, Val2);
            Output ("Add_Facet ("
                    & Ada_Name (Handler.Contexts) & ", """ & Local_Name
                    & """, unnormalized=""" & Val2 & """);");
         end;

      elsif Local_Name = "maxLength"
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
         case Handler.Contexts.Typ is
            when Context_Restriction =>
               Create_Restricted (Handler, Handler.Contexts);
               Add_Facet
                 (Handler.Contexts.Restricted, Local_Name,
                  Trim (Get_Value (Atts, URI => "", Local_Name => "value"),
                        Ada.Strings.Both));

            when Context_Extension =>
               Validation_Error
                 ("Invalid restriction in an extension: """
                  & Local_Name & """");

            when others =>
               Raise_Exception
                 (XML_Not_Implemented'Identity,
                  '"' & Local_Name
                  & """ not supported outside of restriction or extension");
         end case;

         Output ("Add_Facet ("
                 & Ada_Name (Handler.Contexts) & ", """ & Local_Name
                 & """, """
                 & Get_Value (Atts, URI => "", Local_Name => "value")
                 & """);");

      elsif Local_Name = "all" then
         Create_All (Handler'Access, Atts);

      elsif Local_Name = "sequence" then
         Create_Sequence (Handler'Access, Atts);

      elsif Local_Name = "choice" then
         Create_Choice (Handler'Access, Atts);

      elsif Local_Name = "list" then
         Create_List (Handler'Access, Atts);

      elsif Local_Name = "union" then
         Create_Union (Handler'Access, Atts);

      elsif Local_Name = "attribute" then
         Create_Attribute (Handler'Access, Atts);

      elsif Local_Name = "group" then
         Create_Group (Handler'Access, Atts);

      elsif Local_Name = "simpleContent" then
         Handler.Contexts.Simple_Content := True;

      elsif Local_Name = "complexContent" then
         Handler.Contexts.Simple_Content := False;

      elsif Local_Name = "attributeGroup" then
         Create_Attribute_Group (Handler'Access, Atts);

      elsif Local_Name = "any" then
         Create_Any (Handler'Access, Atts);

      elsif Local_Name = "redefine" then
         Create_Redefine (Handler'Access, Atts);

      elsif Local_Name = "include" then
         Create_Include (Handler'Access, Atts);

      elsif Local_Name = "import" then
         Create_Import (Handler'Access, Atts);

      elsif Handler.In_Annotation then
         null;   --  ignore all tags

      else
         Output ("Tag not handled yet: " & Local_Name);
         Raise_Exception
           (XML_Not_Implemented'Identity,
            "Unsupported element in the schema: " & Local_Name);
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
      if False and Debug then
         Debug_Dump_Contexts (Handler, "End");
      end if;

      --  Check the grammar
      End_Element (Validating_Reader (Handler),
                   Namespace_URI,
                   Local_Name,
                   Qname);

      --  Process the tag
      if Local_Name = "element" then
         Finish_Element (Handler'Access);

      elsif Local_Name = "schema" then
         --  ??? Check there remains no undefined forward declaration
         null;

      elsif Local_Name = "complexType" then
         Finish_Complex_Type (Handler'Access);

      elsif Local_Name = "simpleType" then
         Finish_Simple_Type (Handler'Access);

      elsif Local_Name = "all" then
         Finish_All (Handler'Access);

      elsif Local_Name = "sequence" then
         Finish_Sequence (Handler'Access);

      elsif Local_Name = "anyAttribute" then
         Handled := False;

      elsif Local_Name = "choice" then
         Finish_Choice (Handler'Access);

      elsif Local_Name = "restriction" then
         Finish_Restriction (Handler'Access);

      elsif Local_Name = "extension" then
         Finish_Extension (Handler'Access);

      elsif Local_Name = "attribute" then
         Finish_Attribute (Handler'Access);

      elsif Local_Name = "union" then
         Finish_Union (Handler'Access);

      elsif Local_Name = "list" then
         Finish_List (Handler'Access);

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
         Finish_Group (Handler'Access);

      elsif Local_Name = "attributeGroup" then
         null;

      elsif Local_Name = "any" then
         Handled := False;

      elsif Local_Name = "import"
        or else Local_Name = "simpleContent"
        or else Local_Name = "complexContent"
      then
         Handled := False;

      elsif Local_Name = "annotation" then
         Handler.In_Annotation := False;
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
      NS : XML_NS;
   begin
      Get_Namespace_From_Prefix (Handler, Prefix, NS);

      if NS = No_XML_NS then
         Output ("G := Handler.Target_NS;");
         Grammar := Handler.Target_NS;

      else
         if Debug then
            Output
              ("Get_NS (Handler.Created_Grammar, """
               & Get_URI (NS) & """, G);");
         end if;

         Get_NS (Handler.Created_Grammar, Get_URI (NS), Grammar);
      end if;
   end Get_Grammar_For_Namespace;

   ----------------------------
   -- Set_Supported_Features --
   ----------------------------

   procedure Set_Supported_Features
     (Reader   : in out Schema_Reader;
      Features : Supported_Features) is
   begin
      Reader.Supported := Features;
   end Set_Supported_Features;

end Schema.Schema_Readers;
