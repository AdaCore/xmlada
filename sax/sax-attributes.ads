-----------------------------------------------------------------------
--                XML/Ada - An XML suite for Ada95                   --
--                                                                   --
--                       Copyright (C) 2001                          --
--                            ACT-Europe                             --
--                       Author: Emmanuel Briot                      --
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
-----------------------------------------------------------------------

with Unicode.CES;

package Sax.Attributes is

   type Attributes is abstract tagged private;

   --------------------------
   -- Attributes interface --
   --------------------------
   --  In the following functions, an empty string is returned when the
   --  index is out of bounds.
   --  Indexes are zero-based.

   function Get_Index (Attr : Attributes; Qname : Unicode.CES.Byte_Sequence)
      return Integer is abstract;
   --  Look up the index of an attribute by XML 1.0 qualified name.
   --  (-1) is returned if there is no match

   function Get_Index
     (Attr       : Attributes;
      URI        : Unicode.CES.Byte_Sequence;
      Local_Name : Unicode.CES.Byte_Sequence)
      return Integer is abstract;
   --  Look up the index of an attribute by Namespace name
   --  (-1) is returned if there is no match

   function Get_Length (Attr : Attributes) return Natural is abstract;
   --  Return the number of attributes in the list

   function Get_Local_Name (Attr : Attributes; Index : Natural)
      return Unicode.CES.Byte_Sequence is abstract;
   --  Return an attribute's local name by index

   function Get_Qname (Attr : Attributes; Index : Natural)
      return Unicode.CES.Byte_Sequence is abstract;
   --  Return an attribute's qualified name by index

   function Get_Type (Attr : Attributes; Index : Natural)
      return Unicode.CES.Byte_Sequence is abstract;
   --  Return an attribute's type by index

   function Get_Type (Attr : Attributes; Qname : Unicode.CES.Byte_Sequence)
      return Unicode.CES.Byte_Sequence is abstract;
   --  Return an attribute's type by XML 1.0 qualified name

   function Get_Type
     (Attr       : Attributes;
      URI        : Unicode.CES.Byte_Sequence;
      Local_Name : Unicode.CES.Byte_Sequence)
      return Unicode.CES.Byte_Sequence is abstract;
   --  Return an attribute's type by Namespace name, or "CDATA" if the type
   --  is unknown.

   function Get_URI (Attr : Attributes; Index : Natural)
      return Unicode.CES.Byte_Sequence is abstract;
   --  Return an attribute's Namespace URI by index

   function Get_Value (Attr : Attributes; Index : Natural)
      return Unicode.CES.Byte_Sequence is abstract;
   --  Return an attribute's value by index

   function Get_Value (Attr : Attributes; Qname : Unicode.CES.Byte_Sequence)
      return Unicode.CES.Byte_Sequence is abstract;
   --  Return an attribute's value by XML 1.0 qualified name

   function Get_Value
     (Attr       : Attributes;
      URI        : Unicode.CES.Byte_Sequence;
      Local_Name : Unicode.CES.Byte_Sequence)
      return Unicode.CES.Byte_Sequence is abstract;
   --  Return an attribute's value by Namespace name

   ------------------------------------
   -- Attributes_Impl implementation --
   ------------------------------------

   type Attributes_Impl is new Attributes with private;

   procedure Add_Attribute
     (Attr       : in out Attributes_Impl;
      URI        : Unicode.CES.Byte_Sequence;
      Local_Name : Unicode.CES.Byte_Sequence;
      Qname      : Unicode.CES.Byte_Sequence;
      Att_Type   : Unicode.CES.Byte_Sequence;
      Value      : Unicode.CES.Byte_Sequence);
   --  Add an attribute to the end of the list.
   --  For the sake of speed, this function doesn't check if the attribute is
   --  already in the list, this is the responsability of the application.

   procedure Clear (Attr : in out Attributes_Impl);
   --  Clear the list of attributes for reuse (or to free the memory allocated
   --  for it). You should always call this procedure when you are done with
   --  the attribute list.

   procedure Remove_Attribute
     (Attr : in out Attributes_Impl;
      Index : Natural);
   --  Remove an attribute from the list, by index.

   procedure Set_Attribute
     (Attr       : in out Attributes_Impl;
      Index      : Natural;
      URI        : Unicode.CES.Byte_Sequence;
      Local_Name : Unicode.CES.Byte_Sequence;
      Qname      : Unicode.CES.Byte_Sequence;
      Att_Type   : Unicode.CES.Byte_Sequence;
      Value      : Unicode.CES.Byte_Sequence);
   --  Set an attribute in the list.
   --  For the sake of speed, this function doesn't check if the attribute is
   --  already in the list, this is the responsability of the application.

   procedure Set_Attributes
     (Attr : in out Attributes_Impl;
      From : Attributes'Class);
   --  Copy an entire attribute object

   procedure Set_Local_Name
     (Attr       : in out Attributes_Impl;
      Index      : Natural;
      Local_Name : Unicode.CES.Byte_Sequence);
   --   Set the local name of a specific attribute in the list

   procedure Set_Qname
     (Attr  : in out Attributes_Impl;
      Index : Natural;
      Qname : Unicode.CES.Byte_Sequence);
   --   Set the XML 1.0 qualified name of a specific attribute in the list

   procedure Set_Type
     (Attr     : in out Attributes_Impl;
      Index    : Natural;
      Att_Type : Unicode.CES.Byte_Sequence);
   --   Set the type of a specific attribute in the list

   procedure Set_URI
     (Attr  : in out Attributes_Impl;
      Index : Natural;
      URI   : Unicode.CES.Byte_Sequence);
   --   Set the Namespace URI of a specific attribute in the list

   procedure Set_Value
     (Attr  : in out Attributes_Impl;
      Index : Natural;
      Value : Unicode.CES.Byte_Sequence);
   --   Set the value of a specific attribute in the list

   function Get_Index
     (Attr  : Attributes_Impl;
      Qname : Unicode.CES.Byte_Sequence) return Integer;

   function Get_Index
     (Attr       : Attributes_Impl;
      URI        : Unicode.CES.Byte_Sequence;
      Local_Name : Unicode.CES.Byte_Sequence)
      return Integer;

   function Get_Length (Attr : Attributes_Impl) return Natural;

   function Get_Local_Name (Attr : Attributes_Impl; Index : Natural)
      return Unicode.CES.Byte_Sequence;

   function Get_Qname (Attr : Attributes_Impl; Index : Natural)
      return Unicode.CES.Byte_Sequence;

   function Get_Type (Attr : Attributes_Impl; Index : Natural)
      return Unicode.CES.Byte_Sequence;

   function Get_Type
     (Attr : Attributes_Impl;
      Qname : Unicode.CES.Byte_Sequence)
      return Unicode.CES.Byte_Sequence;

   function Get_Type
     (Attr       : Attributes_Impl;
      URI        : Unicode.CES.Byte_Sequence;
      Local_Name : Unicode.CES.Byte_Sequence)
      return Unicode.CES.Byte_Sequence;

   function Get_URI (Attr : Attributes_Impl; Index : Natural)
      return Unicode.CES.Byte_Sequence;

   function Get_Value (Attr : Attributes_Impl; Index : Natural)
      return Unicode.CES.Byte_Sequence;

   function Get_Value
     (Attr : Attributes_Impl;
      Qname : Unicode.CES.Byte_Sequence)
      return Unicode.CES.Byte_Sequence;

   function Get_Value
     (Attr       : Attributes_Impl;
      URI        : Unicode.CES.Byte_Sequence;
      Local_Name : Unicode.CES.Byte_Sequence)
      return Unicode.CES.Byte_Sequence;

   Out_Of_Bounds : exception;
   --  Raised when Index is out of bounds in all the Set_* subprograms.

private

   type Attribute;
   type Attribute_Access is access Attribute;
   type Attribute is record
      URI        : Unicode.CES.Byte_Sequence_Access;
      Local_Name : Unicode.CES.Byte_Sequence_Access;
      Value      : Unicode.CES.Byte_Sequence_Access;
      Att_Type   : Unicode.CES.Byte_Sequence_Access;
      Qname      : Unicode.CES.Byte_Sequence_Access;
      Next       : Attribute_Access;
   end record;

   type Attributes is abstract tagged null record;
   type Attributes_Impl is new Attributes with record
      Length : Natural := 0;
      First  : Attribute_Access;
      Last   : Attribute_Access;
   end record;
end Sax.Attributes;
