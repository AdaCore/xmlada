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
with Interfaces;
with Sax.Readers;
with Sax.Symbols;
with Sax.Utils;
with Schema.Readers;
with Schema.Validators;
with Unicode.CES;
with GNAT.Dynamic_Tables;
with GNAT.Dynamic_HTables;

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
   use Schema.Validators;

   type Type_Index is new Integer;
   No_Type_Index : constant Type_Index := -1;

   type Type_Kind is (Type_Empty, Type_Sequence, Type_Choice, Type_Element,
                      Type_Any, Type_Group, Type_Extension);

   type Type_Details;
   type Type_Details_Access is access all Type_Details;

   type Element_Descr is record
      Name               : Qualified_Name     := No_Qualified_Name;
      Typ                : Qualified_Name     := No_Qualified_Name;
      Local_Type         : Type_Index         := No_Type_Index;
      Ref                : Qualified_Name     := No_Qualified_Name;
      Form               : Form_Type          := Unqualified;
      Default            : Sax.Symbols.Symbol := Sax.Symbols.No_Symbol;
      Fixed              : Sax.Symbols.Symbol := Sax.Symbols.No_Symbol;
      Substitution_Group : Sax.Symbols.Symbol := Sax.Symbols.No_Symbol;
      Final              : Final_Status       := (others => False);
      Block              : Block_Status       := (others => False);
      Is_Abstract        : Boolean            := False;
      Nillable           : Boolean            := False;
      Has_Block          : Boolean            := False;
   end record;
   No_Element_Descr : constant Element_Descr := (others => <>);

   type Group_Descr is record
      Name           : Qualified_Name := No_Qualified_Name;
      Ref            : Qualified_Name := No_Qualified_Name;
      Details        : Type_Details_Access;
   end record;
   No_Group_Descr : constant Group_Descr := (others => <>);

   type Attr_Descr_Kind is (Kind_Group, Kind_Attribute, Kind_Unset);
   type Attr_Descr (Kind : Attr_Descr_Kind := Kind_Unset) is record
      case Kind is
         when Kind_Unset     => null;
         when Kind_Group     => Group_Ref : Qualified_Name;
         when Kind_Attribute =>
            Attr      : Attribute_Validator;
            Is_Local  : Boolean;
      end case;
   end record;
   type Attr_Array is array (Natural range <>) of Attr_Descr;
   type Attr_Array_Access is access all Attr_Array;

   type AttrGroup_Descr is record
      Name           : Qualified_Name := No_Qualified_Name;
      Ref            : Qualified_Name := No_Qualified_Name;
      Attributes     : Attr_Array_Access;
   end record;
   No_AttrGroup_Descr : constant AttrGroup_Descr := (others => <>);

   type Extension_Descr is record
      Base           : Qualified_Name := No_Qualified_Name;
      Details        : Type_Details_Access;
      Attributes     : Attr_Array_Access;
   end record;

   type Type_Details (Kind : Type_Kind := Type_Empty) is record
      Min_Occurs, Max_Occurs : Integer;
      Next : Type_Details_Access;
      case Kind is
         when Type_Empty     => null;
         when Type_Sequence  => First_In_Seq    : Type_Details_Access;
         when Type_Choice    => First_In_Choice : Type_Details_Access;
         when Type_Element   => Element         : Element_Descr;
         when Type_Any       => Any             : Any_Descr;
         when Type_Group     => Group           : Group_Descr;
         when Type_Extension => Extension       : Extension_Descr;
      end case;
   end record;

   type Type_Descr is record
      Name           : Qualified_Name := No_Qualified_Name;
      Block          : Block_Status := No_Block;
      Final          : Final_Status := (others => False);
      Mixed          : Boolean := False;
      Is_Abstract    : Boolean := False;
      Simple_Content : Boolean := False;
      Details        : Type_Details_Access;
      NFA            : Schema.Validators.Schema_State_Machines.Nested_NFA;
      Attributes     : Attr_Array_Access;
   end record;

   type Schema_Descr is record
      Target_NS              : Sax.Symbols.Symbol := Sax.Symbols.No_Symbol;
      Block                  : Block_Status       := No_Block;
      Element_Form_Default   : Form_Type          := Unqualified;
      Attribute_Form_Default : Form_Type          := Unqualified;
   end record;

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

      case Typ is
         when Context_Type_Def        =>
            Type_Info      : Type_Index;
            Type_Validator : Schema.Validators.XML_Validator;
            Redefined_Type : Schema.Validators.XML_Type; --  <redefine>
         when Context_Element         => Element    : Element_Descr;
         when Context_Sequence        => Seq        : Type_Details_Access;
         when Context_Choice          => Choice     : Type_Details_Access;
         when Context_Attribute_Group => Attr_Group : AttrGroup_Descr;
         when Context_Schema          => null;
         when Context_Redefine        => null;
         when Context_Group           => Group      : Group_Descr;
         when Context_Extension       => Extension  : Type_Details_Access;

         when Context_All =>
            null;
            --  All_Validator : Schema.Validators.XML_All;
         when Context_Restriction =>
            Restriction : Schema.Validators.XML_Validator;
            Restricted  : Schema.Validators.XML_Validator; --  result
            Restriction_Base : Schema.Validators.XML_Type;
         when Context_Union =>
            Union : Schema.Validators.XML_Validator;
         when Context_List =>
            List_Items : Schema.Validators.XML_Type;
         when Context_Attribute =>
            Attribute : Schema.Validators.Attribute_Validator;
            Attribute_Is_Ref : Boolean;
      end case;
   end record;

   type Header_Num is new Interfaces.Integer_32 range 0 .. 1023;
   function Hash (Name : Qualified_Name) return Header_Num;
   function Hash (Name : Sax.Symbols.Symbol) return Header_Num;

   package Type_Tables is new GNAT.Dynamic_Tables
     (Table_Component_Type => Type_Descr,
      Table_Index_Type     => Type_Index,
      Table_Low_Bound      => 1,
      Table_Initial        => 100,
      Table_Increment      => 100);
   package Type_HTables is new GNAT.Dynamic_HTables.Simple_HTable
     (Header_Num => Header_Num,
      Element    => Type_Index,
      No_Element => No_Type_Index,
      Key        => Qualified_Name,
      Hash       => Hash,
      Equal      => "=");
   package Element_HTables is new GNAT.Dynamic_HTables.Simple_HTable
     (Header_Num => Header_Num,
      Element    => Element_Descr,
      No_Element => No_Element_Descr,
      Key        => Qualified_Name,
      Hash       => Hash,
      Equal      => "=");
   package Group_HTables is new GNAT.Dynamic_HTables.Simple_HTable
     (Header_Num => Header_Num,
      Element    => Group_Descr,
      No_Element => No_Group_Descr,
      Key        => Qualified_Name,
      Hash       => Hash,
      Equal      => "=");
   package AttrGroup_HTables is new GNAT.Dynamic_HTables.Simple_HTable
     (Header_Num => Header_Num,
      Element    => AttrGroup_Descr,
      No_Element => No_AttrGroup_Descr,
      Key        => Qualified_Name,
      Hash       => Hash,
      Equal      => "=");

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

      Schema_NS       : Schema.Validators.XML_Grammar_NS;
      Contexts        : Context_Access;

      --  The following data should be shared among all readers that parse a
      --  given XSD and all its namespaces. In fact, it might be better to have
      --  a single reader for this, and pass around the above fields for each
      --  of the namespaces.

      Types             : Type_Tables.Instance;
      Global_Elements   : Element_HTables.Instance;
      Global_Types      : Type_HTables.Instance;
      Global_Groups     : Group_HTables.Instance;
      Global_AttrGroups : AttrGroup_HTables.Instance;
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
