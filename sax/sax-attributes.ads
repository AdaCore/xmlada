-----------------------------------------------------------------------
--                XML/Ada - An XML suite for Ada95                   --
--                                                                   --
--                       Copyright (C) 2001-2002                     --
--                            ACT-Europe                             --
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

--  In addition to the SAX standard, we have added an extra field to
--  Attributes to memorize the default declaration for the attribute
--  (REQUIRED, IMPLIED, FIXED).
--  Likewise, enumerations are represented in a full structure, rather than
--  a simple string.
--  We have also merged the interfaces Attributes and Attributes_Impl, for
--  ease of use.

with Unicode.CES;
with Sax.Models;

package Sax.Attributes is

   type Attributes is tagged private;

   type Default_Declaration is (Required, Implied, Fixed, Default);
   --  See 3.3.2 in XML specifications

   type Attribute_Type is
     (Cdata, Id, Idref, Idrefs, Entity, Entities, Nmtoken, Nmtokens,
      Notation, Enumeration);
   --  See 3.3.1 in XML specifications. The last value "Enumeration"
   --  corresponds to a model like "(a|b)*",...

   --------------------------
   -- Attributes interface --
   --------------------------
   --  In the following functions, an empty string is returned when the
   --  index is out of bounds.
   --  Indexes are zero-based.

   function Get_Index (Attr : Attributes; Qname : Unicode.CES.Byte_Sequence)
      return Integer;
   --  Look up the index of an attribute by XML 1.0 qualified name.
   --  (-1) is returned if there is no match

   function Get_Index
     (Attr       : Attributes;
      URI        : Unicode.CES.Byte_Sequence;
      Local_Name : Unicode.CES.Byte_Sequence)
      return Integer;
   --  Look up the index of an attribute by Namespace name
   --  (-1) is returned if there is no match

   function Get_Length (Attr : Attributes) return Natural;
   --  Return the number of attributes in the list

   function Get_Local_Name (Attr : Attributes; Index : Natural)
      return Unicode.CES.Byte_Sequence;
   --  Return an attribute's local name by index

   function Get_Qname (Attr : Attributes; Index : Natural)
      return Unicode.CES.Byte_Sequence;
   --  Return an attribute's qualified name by index

   function Get_Type (Attr : Attributes; Index : Natural)
      return Attribute_Type;
   --  Return an attribute's type by index

   function Get_Type (Attr : Attributes; Qname : Unicode.CES.Byte_Sequence)
      return Attribute_Type;
   --  Return an attribute's type by XML 1.0 qualified name

   function Get_Type
     (Attr       : Attributes;
      URI        : Unicode.CES.Byte_Sequence;
      Local_Name : Unicode.CES.Byte_Sequence)
      return Attribute_Type;
   --  Return an attribute's type by Namespace name, or "CDATA" if the type
   --  is unknown.

   function Get_URI (Attr : Attributes; Index : Natural)
      return Unicode.CES.Byte_Sequence;
   --  Return an attribute's Namespace URI by index

   function Get_Value (Attr : Attributes; Index : Natural)
      return Unicode.CES.Byte_Sequence;
   --  Return an attribute's value by index

   function Get_Value (Attr : Attributes; Qname : Unicode.CES.Byte_Sequence)
      return Unicode.CES.Byte_Sequence;
   --  Return an attribute's value by XML 1.0 qualified name

   function Get_Value
     (Attr       : Attributes;
      URI        : Unicode.CES.Byte_Sequence;
      Local_Name : Unicode.CES.Byte_Sequence)
      return Unicode.CES.Byte_Sequence;
   --  Return an attribute's value by Namespace name

   function Get_Content
     (Attr : Attributes; Index : Natural) return Sax.Models.Element_Model_Ptr;
   --  Return the content model for the attribute.
   --  This function doesn't exist in the SAX 2.0 standard.
   --  Do not modify the pointer returned, since this is directly the
   --  internal pointer.

   procedure Set_Content
     (Attr    : Attributes;
      Index   : Natural;
      Content : Sax.Models.Element_Model_Ptr);
   --  Set the content model for the attribute. No copy of content is made, and
   --  you shouldn't free it until the attribute itself is destroyed.

   function Get_Default_Declaration
     (Attr : Attributes; Index : Natural) return Default_Declaration;
   --  Return the specification used for the default value of the attribute.
   --  This function is not part of the SAX 2.0 standard.

   procedure Add_Attribute
     (Attr       : in out Attributes;
      URI        : Unicode.CES.Byte_Sequence;
      Local_Name : Unicode.CES.Byte_Sequence;
      Qname      : Unicode.CES.Byte_Sequence;
      Att_Type   : Attribute_Type;
      Content    : Sax.Models.Element_Model_Ptr;
      Value      : Unicode.CES.Byte_Sequence;
      Default_Decl : Default_Declaration := Default);
   --  Add an attribute to the end of the list.
   --  For the sake of speed, this function doesn't check if the attribute is
   --  already in the list, this is the responsability of the application.
   --  Content should be null unless Att_Type is Notation or Enumeration.
   --
   --  No copy of Content is made, so you shouldn't free it until the attribute
   --  itself is destroyed (this is also not done automatically)

   procedure Clear (Attr : in out Attributes);
   --  Clear the list of attributes for reuse (or to free the memory allocated
   --  for it). You should always call this procedure when you are done with
   --  the attribute list.

   procedure Remove_Attribute (Attr : in out Attributes; Index : Natural);
   --  Remove an attribute from the list, by index.

   procedure Set_Attribute
     (Attr       : in out Attributes;
      Index      : Natural;
      URI        : Unicode.CES.Byte_Sequence;
      Local_Name : Unicode.CES.Byte_Sequence;
      Qname      : Unicode.CES.Byte_Sequence;
      Att_Type   : Attribute_Type;
      Content    : Sax.Models.Element_Model_Ptr;
      Value      : Unicode.CES.Byte_Sequence;
      Default_Decl : Default_Declaration := Default);
   --  Set an attribute in the list.
   --  For the sake of speed, this function doesn't check if the attribute is
   --  already in the list, this is the responsability of the application.

   procedure Set_Attributes
     (Attr : in out Attributes; From : Attributes'Class);
   --  Copy an entire attribute object

   procedure Set_Local_Name
     (Attr       : in out Attributes;
      Index      : Natural;
      Local_Name : Unicode.CES.Byte_Sequence);
   --   Set the local name of a specific attribute in the list

   procedure Set_Qname
     (Attr  : in out Attributes;
      Index : Natural;
      Qname : Unicode.CES.Byte_Sequence);
   --   Set the XML 1.0 qualified name of a specific attribute in the list

   procedure Set_Type
     (Attr     : in out Attributes;
      Index    : Natural;
      Att_Type : Attribute_Type);
   --   Set the type of a specific attribute in the list

   procedure Set_URI
     (Attr  : in out Attributes;
      Index : Natural;
      URI   : Unicode.CES.Byte_Sequence);
   --   Set the Namespace URI of a specific attribute in the list

   procedure Set_Value
     (Attr  : in out Attributes;
      Index : Natural;
      Value : Unicode.CES.Byte_Sequence);
   --   Set the value of a specific attribute in the list

   Out_Of_Bounds : exception;
   --  Raised when Index is out of bounds in all the Set_* subprograms.

private

   type Attribute;
   type Attribute_Access is access Attribute;
   type Attribute is record
      URI          : Unicode.CES.Byte_Sequence_Access;
      Local_Name   : Unicode.CES.Byte_Sequence_Access;
      Value        : Unicode.CES.Byte_Sequence_Access;
      Att_Type     : Attribute_Type;
      Qname        : Unicode.CES.Byte_Sequence_Access;
      Default_Decl : Default_Declaration;
      Content      : Sax.Models.Element_Model_Ptr;
      Next         : Attribute_Access;
   end record;

   type Attributes is tagged record
      Length : Natural := 0;
      First  : Attribute_Access;
      Last   : Attribute_Access;
   end record;
end Sax.Attributes;
