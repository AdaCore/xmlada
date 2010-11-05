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

--  This package provides a SAX Reader that parses an XML Schema file, and
--  creates the appropriate data structure

pragma Ada_05;

with Input_Sources;
with Sax.Readers;
with Sax.Symbols;
with Sax.Utils;
with Schema.Readers;
with Schema.Validators;
with Unicode.CES;

package Schema.Schema_Readers is

   type Schema_Reader is new Schema.Readers.Validating_Reader with private;
   type Schema_Reader_Access is access all Schema_Reader'Class;
   --  An XML reader that parses an XML schema, and store the information in
   --  a grammar

   procedure Parse
     (Parser            : in out Schema_Reader;
      Input             : in out Input_Sources.Input_Source'Class;
      Default_Namespace : Sax.Symbols.Symbol;
      Do_Global_Check   : Boolean);
   --  Same as inherited parse, but you can indicate the default value for
   --  targetNamespace. In practice, this is useful when processing <include>
   --  or <redefine> elements from another schema, but should not be used in
   --  other contexts.
   --  Do_Global_Check should be True if the parser needs to check for missing
   --  declarations when the parsing is done.

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
      Next        : Context_Access;
      Level       : Integer;
      Start_State : Schema.Validators.Schema_State_Machines.State;
      Last_State  : Schema.Validators.Schema_State_Machines.State;

      case Typ is
         when Context_Type_Def =>
            Type_Name      : Sax.Symbols.Symbol;
            Type_Validator : Schema.Validators.XML_Validator;
            Redefined_Type : Schema.Validators.XML_Type;
            --  Handling of <redefine>
            Mixed_Content  : Boolean;
            Simple_Content : Boolean;
            Blocks         : Schema.Validators.Block_Status;
            Final          : Schema.Validators.Final_Status;
            NFA           : Schema.Validators.Schema_State_Machines.Nested_NFA;

         when Context_Element =>
            Element : Schema.Validators.XML_Element;
            Is_Ref  : Boolean;
            Element_Min, Element_Max : Integer;
         when Context_Sequence =>
            Seq        : Schema.Validators.Sequence;
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
            Group_Min, Group_Max : Integer;
            --  Handling of <redefine>
         when Context_Attribute_Group =>
            Attr_Group : Schema.Validators.XML_Attribute_Group;
      end case;
   end record;

   type Schema_Reader is new Schema.Readers.Validating_Reader with record
      Attribute_Form_Default : Schema.Validators.Form_Type :=
        Schema.Validators.Unqualified;
      Element_Form_Default : Schema.Validators.Form_Type :=
        Schema.Validators.Unqualified;
      --  The value of elementFormDefault for the current file

      Target_NS       : Schema.Validators.XML_Grammar_NS;
      --  The namespace for which we are currently parsing. This might be
      --  different from Get_Target_NS (Created_Grammar) when processing
      --  <import> for instance.

      In_Annotation   : Boolean := False;
      --  Whether we are processing an <annotation> node, in which case we
      --  need to ignore all children

      NFA : Schema.Validators.Schema_State_Machines.NFA_Access;
      --  Pointer to the state machine of [Target_NS]. This pointer is really
      --  still owned by [Target_NS], and should not be freed explicitly.

      Schema_NS       : Schema.Validators.XML_Grammar_NS;
      Contexts        : Context_Access;
   end record;

   overriding procedure Start_Element
     (Handler       : in out Schema_Reader;
      NS            : Sax.Utils.XML_NS;
      Local_Name    : Sax.Symbols.Symbol;
      Atts          : Sax.Readers.Sax_Attribute_List);
   overriding procedure End_Element
     (Handler       : in out Schema_Reader;
      NS            : Sax.Utils.XML_NS;
      Local_Name    : Sax.Symbols.Symbol);
   overriding procedure Characters
     (Handler : in out Schema_Reader; Ch : Unicode.CES.Byte_Sequence);
   overriding procedure Parse
     (Parser : in out Schema_Reader;
      Input  : in out Input_Sources.Input_Source'Class);

end Schema.Schema_Readers;
