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
with Sax.Locators;
with Sax.Readers;
with Sax.Symbols;
with Sax.Utils;
with Schema.Readers;
with Schema.Simple_Types;
with Schema.Validators;
with Unicode.CES;
with GNAT.Dynamic_Tables;
with GNAT.Dynamic_HTables;

package Schema.Schema_Readers is

   type Schema_Reader is new Schema.Readers.Validating_Reader with private;
   type Schema_Reader_Access is access all Schema_Reader'Class;
   --  An XML reader that parses an XML schema, and store the information in
   --  a grammar

   procedure Parse_Grammar
     (Handler  : access Schema.Readers.Validating_Reader'Class;
      URI      : Sax.Symbols.Symbol;
      Xsd_File : Sax.Symbols.Symbol;
      Do_Create_NFA : Boolean);
   --  Parse (if not done already) the specified [Xsd_File], and associate it
   --  with the given namespace [URI].
   --  [Handler] is used to convert [Xsd_File] to an absolute URI, and find
   --  the grammar.

private
   use Schema.Validators;

   type Type_Index is new Integer;
   No_Type_Index : constant Type_Index := -1;

   type Type_Kind is (Type_Empty, Type_Sequence, Type_Choice, Type_Element,
                      Type_Any, Type_Group, Type_Extension, Type_Restriction,
                      Type_All);

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
      Substitution_Group : Qualified_Name     := No_Qualified_Name;
      Final              : Final_Status       := (others => False);
      Block              : Block_Status       := (others => False);
      Is_Abstract        : Boolean            := False;
      Nillable           : Boolean            := False;
      Has_Block          : Boolean            := False;
      Loc                : Sax.Locators.Location := Sax.Locators.No_Location;
   end record;
   No_Element_Descr : constant Element_Descr := (others => <>);

   type Group_Descr is record
      Name           : Qualified_Name := No_Qualified_Name;
      Ref            : Qualified_Name := No_Qualified_Name;
      Details        : Type_Details_Access;
   end record;
   No_Group_Descr : constant Group_Descr := (others => <>);

   type Internal_Attribute_Descr is record
      Descr        : Attribute_Descr    := No_Attribute_Descr;
      Typ          : Qualified_Name     := No_Qualified_Name;
      Local_Type   : Type_Index         := No_Type_Index;
      Ref          : Qualified_Name     := No_Qualified_Name;
      Loc          : Sax.Locators.Location := Sax.Locators.No_Location;
   end record;
   No_Internal_Attribute : constant Internal_Attribute_Descr := (others => <>);

   type Attr_Descr_Kind is (Kind_Group, Kind_Attribute, Kind_Unset);
   type Attr_Descr (Kind : Attr_Descr_Kind := Kind_Unset) is record
      case Kind is
         when Kind_Unset     => null;
         when Kind_Group     => Group_Ref : Qualified_Name;
         when Kind_Attribute => Attr      : Internal_Attribute_Descr;
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

   type Restriction_Descr is record
      Base           : Qualified_Name := No_Qualified_Name;
      Details        : Type_Details_Access;
      Attributes     : Attr_Array_Access;
   end record;

   type Type_Details (Kind : Type_Kind := Type_Empty) is record
      Min_Occurs, Max_Occurs : Integer;
      Next : Type_Details_Access;
      case Kind is
         when Type_Empty       => null;
         when Type_Sequence    => First_In_Seq    : Type_Details_Access;
         when Type_Choice      => First_In_Choice : Type_Details_Access;
         when Type_Element     => Element         : Element_Descr;
         when Type_Any         => Any             : Any_Descr;
         when Type_Group       => Group           : Group_Descr;
         when Type_Extension   => Extension       : Extension_Descr;
         when Type_Restriction => Restriction     : Restriction_Descr;
         when Type_All         => First_In_All    : Type_Details_Access;
      end case;
   end record;

   type Type_Member is record
      Name  : Qualified_Name := No_Qualified_Name;
      Local : Type_Index := No_Type_Index;
   end record;
   No_Type_Member : constant Type_Member := (No_Qualified_Name, No_Type_Index);
   --  Only one of the two fields is set. These are the possible members of a
   --  union or list.

   type Type_Member_Array is array (Natural range <>) of Type_Member;

   type Simple_Type_Kind is (Simple_Type,
                             Simple_Type_Restriction,
                             Simple_Type_Union,
                             Simple_Type_List);
   type Internal_Simple_Type_Descr (Kind : Simple_Type_Kind := Simple_Type)
   is record
      Loc : Sax.Locators.Location := Sax.Locators.No_Location;
      case Kind is
         when Simple_Type             => null;
         when Simple_Type_Union       =>
            Union_Items      : Type_Member_Array
              (1 .. Schema.Simple_Types.Max_Types_In_Union) :=
              (others => No_Type_Member);
         when Simple_Type_List        =>
            List_Items      : Type_Member_Array
              (1 .. 1) := (others => No_Type_Member);
         when Simple_Type_Restriction =>
            Restriction_Base : Qualified_Name;
            Facets           : Schema.Simple_Types.All_Facets :=
              Schema.Simple_Types.No_Facets;
      end case;
   end record;
   subtype Union_Type_Descr is Internal_Simple_Type_Descr (Simple_Type_Union);
   subtype List_Type_Descr  is Internal_Simple_Type_Descr (Simple_Type_List);

   type Internal_Type_Descr (Is_Simple : Boolean := False) is record
      Descr      : Type_Descr;
      S          : Schema_State_Machines.State;
      Loc        : Sax.Locators.Location := Sax.Locators.No_Location;

      case Is_Simple is
         when False =>
            Attributes : Attr_Array_Access;
            Details    : Type_Details_Access;
         when True =>
            Simple     : Internal_Simple_Type_Descr;
      end case;
   end record;
   --  Temporary structure while parsing a XSD file. Only [Descr] will be
   --  stored in the NFA for reuse while validating (or while parsing other
   --  XSD).

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
                         Context_Simple_Restriction, --  simpleType
                         Context_Extension,
                         Context_All,
                         Context_List,
                         Context_Union,
                         Context_Redefine,
                         Context_Group,
                         Context_Attribute_Group,
                         Context_Attribute);

   type Context (Typ : Context_Type := Context_Schema) is record
      case Typ is
         when Context_Type_Def        => Type_Info   : Type_Index;
         when Context_Element         =>
            Element      : Element_Descr;
            Elem_Details : Type_Details_Access;
         when Context_Sequence        => Seq         : Type_Details_Access;
         when Context_Choice          => Choice      : Type_Details_Access;
         when Context_All             => All_Detail  : Type_Details_Access;
         when Context_Attribute_Group => Attr_Group  : AttrGroup_Descr;
         when Context_Schema          => null;
         when Context_Redefine        => null;
         when Context_Group           => Group       : Group_Descr;
         when Context_Extension       => Extension   : Type_Details_Access;
         when Context_List            => List        : List_Type_Descr;
         when Context_Restriction     => Restriction : Type_Details_Access;
         when Context_Simple_Restriction =>
            Simple   : Internal_Simple_Type_Descr;
         when Context_Union           => Union       : Union_Type_Descr;
         when Context_Attribute      => Attribute   : Internal_Attribute_Descr;
      end case;
   end record;
   type Context_Access is access all Context;
   type Context_Array is array (Natural range <>) of aliased Context;
   type Context_Array_Access is access all Context_Array;

   package Type_Tables is new GNAT.Dynamic_Tables
     (Table_Component_Type => Internal_Type_Descr,
      Table_Index_Type     => Type_Index,
      Table_Low_Bound      => 1,
      Table_Initial        => 200,
      Table_Increment      => 100);
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
   package Attribute_HTables is new GNAT.Dynamic_HTables.Simple_HTable
     (Header_Num => Header_Num,
      Element    => Internal_Attribute_Descr,
      No_Element => No_Internal_Attribute,
      Key        => Qualified_Name,
      Hash       => Hash,
      Equal      => "=");

   type XSD_Data is record
      Types             : Type_Tables.Instance;
      Global_Elements   : Element_HTables.Instance;
      Global_Groups     : Group_HTables.Instance;
      Global_AttrGroups : AttrGroup_HTables.Instance;
      Global_Attributes : Attribute_HTables.Instance;
   end record;
   type XSD_Data_Access is access all XSD_Data;
   --  Data modified while loading XSD, and needed while loading nested (input
   --  or redefine) XSD, until we can create the NFA

   type Schema_Reader is new Schema.Readers.Validating_Reader with record
      Locator          : Sax.Locators.Locator;

      Attribute_Form_Default : Schema.Validators.Form_Type :=
        Schema.Validators.Unqualified;
      Element_Form_Default : Schema.Validators.Form_Type :=
        Schema.Validators.Unqualified;
      --  The value of elementFormDefault for the current file

      Target_NS            : Sax.Symbols.Symbol;
      Target_Block_Default : Block_Status := No_Block;
      --  The namespace for which we are currently parsing. This might be
      --  different from Get_Target_NS (Created_Grammar) when processing
      --  <import> for instance.

      In_Annotation   : Boolean := False;
      --  Whether we are processing an <annotation> node, in which case we
      --  need to ignore all children

      Contexts        : Context_Array_Access;
      Contexts_Last   : Natural := 0;

      Shared          : XSD_Data_Access;
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
   overriding procedure Set_Document_Locator
     (Handler : in out Schema_Reader; Loc : in out Sax.Locators.Locator);
   overriding function Get_Locator
     (Reader : Schema_Reader) return Sax.Locators.Locator;
   --  See inherited documentation

end Schema.Schema_Readers;
