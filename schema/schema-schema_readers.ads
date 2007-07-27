-----------------------------------------------------------------------
--                XML/Ada - An XML suite for Ada95                   --
--                                                                   --
--                       Copyright (C) 2004-2007, AdaCore            --
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

--  This package provides a SAX Reader that parses an XML Schema file, and
--  creates the appropriate data structure

with Sax.Attributes;
with Schema.Readers;
with Unicode.CES;
with Schema.Validators;
with Input_Sources;

package Schema.Schema_Readers is

   type Schema_Reader is new Schema.Readers.Validating_Reader with private;
   --  An XML reader that parses an XML schema, and store the information in
   --  a grammar

   procedure Set_Created_Grammar
     (Reader  : in out Schema_Reader;
      Grammar : Schema.Validators.XML_Grammar := Schema.Validators.No_Grammar);
   --  Start the parsing of multiple schema files.
   --  When a schema file is parsed, a new XML_Grammar_NS will be created
   --  based on the value of "targetNamespace" attribute. This should be used
   --  to cumulate several schema files into one grammar.
   --  If this isn't call, a new grammar will be created from scratch.
   --
   --  Calling this subprogram will also disable the checks, at the end of the
   --  parsing, that ensure that any entity (element, type, attribute,...) that
   --  has been referenced was correctly declared.
   --
   --  The Grammar should initially be No_Grammar. There are also some checks
   --  that can only be done when all schemas have been parsed and therefore
   --  you need to call Global_Check after parsing all of them. This isn't
   --  necessary if you are not calling Set_Created_Grammar.
   --
   --  For instance, if you need to parse several XSD files:
   --       Schema  : Schema_Reader;
   --       Grammar : XML_Grammar := No_Grammar;
   --
   --       Set_Created_Grammar (Schema, No_Grammar);  --  Required
   --       Parse (Schema, My_Input_Source1);
   --       Parse (Schema, My_Input_Source2);
   --       Global_Check (Grammar);
   --       Grammar := Get_Created_Grammar (Schema);
   --
   --  If you are only parsing one schema, this could be changed to:
   --       Parse (Schema, My_Input_Source1);
   --       Grammar := Get_Created_Grammar (Schema);
   --
   --  Another small difference is in the error messages that are output in
   --  both cases: in the first case above, errors raised by Global_Check are
   --  not associated with a specific location in an XSD file (these error
   --  typically indicate that a referenced element has not been defined),
   --  whereas in the second case it will reference a location in the single
   --  XSD file.

   function Get_Created_Grammar
     (Reader : Schema_Reader) return Schema.Validators.XML_Grammar;
   --  Return the grammar parsed

   procedure Set_Debug_Output (Output : Boolean);
   --  Whether extra debug output should be displayed

private
   type Context_Type is (Context_Type_Def,
                         Context_Element,
                         Context_Sequence,
                         Context_Choice,
                         Context_Schema,
                         Context_Restriction,
                         Context_Extension,
                         Context_All,
                         Context_List,
                         Context_Union,
                         Context_Redefine,
                         Context_Group,
                         Context_Attribute_Group,
                         Context_Attribute);

   type Context (Typ : Context_Type);
   type Context_Access is access all Context;
   type Context (Typ : Context_Type) is record
      Next  : Context_Access;
      Level : Integer;
      case Typ is
         when Context_Type_Def =>
            Type_Name      : Unicode.CES.Byte_Sequence_Access;
            Type_Validator : Schema.Validators.XML_Validator;
            Redefined_Type : Schema.Validators.XML_Type;
            --  Handling of <redefine>
            Mixed_Content  : Boolean;
            Simple_Content : Boolean;
            Block_Restriction : Boolean;
            Block_Extension   : Boolean;
         when Context_Element =>
            Element : Schema.Validators.XML_Element;
            Is_Ref  : Boolean;
         when Context_Sequence =>
            Seq       : Schema.Validators.Sequence;
         when Context_Choice =>
            C       : Schema.Validators.Choice;
         when Context_Schema | Context_Redefine =>
            null;
         when Context_All =>
            All_Validator : Schema.Validators.XML_All;
         when Context_Restriction =>
            Restriction : Schema.Validators.XML_Validator;
            Restricted  : Schema.Validators.XML_Validator; --  result
            Restriction_Base : Schema.Validators.XML_Type;
         when Context_Extension =>
            Extension_Base : Schema.Validators.XML_Type;
            Extension      : Schema.Validators.XML_Validator;
            --  Extension_Base set to null if Extension is the result of the
            --  call to Extension_Of already
         when Context_Union =>
            Union : Schema.Validators.XML_Validator;
         when Context_List =>
            List_Items : Schema.Validators.XML_Type;
         when Context_Attribute =>
            Attribute : Schema.Validators.Attribute_Validator;
            Attribute_Is_Ref : Boolean;
         when Context_Group =>
            Group     : Schema.Validators.XML_Group;
            Redefined_Group : Schema.Validators.XML_Group;
            --  Handling of <redefine>
         when Context_Attribute_Group =>
            Attr_Group : Schema.Validators.XML_Attribute_Group;
      end case;
   end record;

   type Schema_Reader is new Schema.Readers.Validating_Reader with record
      Created_Grammar : Schema.Validators.XML_Grammar :=
        Schema.Validators.No_Grammar;
      --  This is the grammar created by the Schema file. Do not mix up with
      --  Schema.Readers.Validating_Reader.Grammar, which is in this case the
      --  grammar used to validate the schema itself.

      Check_Undefined : Boolean := True;

      Element_Form_Default : Schema.Validators.Form_Type :=
        Schema.Validators.Unqualified;
      --  The value of elementFormDefault for the current file

      Target_NS       : Schema.Validators.XML_Grammar_NS;
      --  The namespace for which we are currently parsing. This might be
      --  different from Get_Target_NS (Created_Grammar) when processing
      --  <import> for instance.

      Schema_NS       : Schema.Validators.XML_Grammar_NS;
      Contexts        : Context_Access;
   end record;

   procedure Start_Document (Handler : in out Schema_Reader);
   procedure End_Document (Handler : in out Schema_Reader);
   procedure Start_Element
     (Handler       : in out Schema_Reader;
      Namespace_URI : Unicode.CES.Byte_Sequence := "";
      Local_Name    : Unicode.CES.Byte_Sequence := "";
      Qname         : Unicode.CES.Byte_Sequence := "";
      Atts          : Sax.Attributes.Attributes'Class);
   procedure End_Element
     (Handler       : in out Schema_Reader;
      Namespace_URI : Unicode.CES.Byte_Sequence := "";
      Local_Name    : Unicode.CES.Byte_Sequence := "";
      Qname         : Unicode.CES.Byte_Sequence := "");
   procedure Characters
     (Handler : in out Schema_Reader; Ch : Unicode.CES.Byte_Sequence);
   procedure Parse
     (Parser : in out Schema_Reader;
      Input  : in out Input_Sources.Input_Source'Class);

end Schema.Schema_Readers;
