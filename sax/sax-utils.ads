-----------------------------------------------------------------------
--                XML/Ada - An XML suite for Ada95                   --
--                                                                   --
--                       Copyright (C) 2005-2007, AdaCore            --
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

--  This package contains various subprograms not described in the SAX
--  standard, but which are used by the various components of XML/Ada

with Unicode.CES;
with Interfaces;

package Sax.Utils is

   function Is_Valid_Language_Name
     (Lang : Unicode.CES.Byte_Sequence) return Boolean;
   --  Whether Lang is a valid language, as per 2.12 in the XML specifications.
   --  Lang is encoded with Sax.Encodings.Encoding

   function Is_Valid_Name_Char (Char : Unicode.Unicode_Char) return Boolean;
   --  Whether Char is a valid NameChar, as per 2.3 in the XML specifications

   function Is_Valid_NCname_Char (Char : Unicode.Unicode_Char) return Boolean;
   --  Whether Char is a valid NCnameChar, as per 2 in the XML specifications

   function Is_Valid_Nmtoken
     (Nmtoken     : Unicode.CES.Byte_Sequence) return Boolean;
   --  Whether Nmtoken is valid NMTOKEN as per 2.3 in the XML specifications

   function Is_Valid_Nmtokens
     (Nmtokens    : Unicode.CES.Byte_Sequence) return Boolean;
   --  Whether Nmtokens is valid NMTOKENS as per 2.3

   function Is_Valid_Name
     (Name        : Unicode.CES.Byte_Sequence) return Boolean;
   --  Whether Name is valid name as per 2.3 in the XML specifications.

   function Is_Valid_Names
     (Name        : Unicode.CES.Byte_Sequence) return Boolean;
   --  Whether Name contains one or more valid Name, separated by a single
   --  space character.

   function Is_Valid_NCname
     (Name : Unicode.CES.Byte_Sequence) return Boolean;
   --  Whether Name is valid NCname as per 2 in the XML namespaces
   --  specifications
   --  Colon should not be allowed when namespaces are supported, since names
   --  must then match NCName, as per 6 in XML Namespaces specifications

   function Is_Valid_NCnames
     (Name        : Unicode.CES.Byte_Sequence) return Boolean;
   --  Whether Name contains one or more valid NCname, separated by a single
   --  space character.

   function Is_Valid_QName
     (Name : Unicode.CES.Byte_Sequence) return Boolean;
   --  Whether Name is valid QName as per 3 in the XML specifications

   type URI_Type is (URI_Absolute, URI_Relative_Ref, URI_None);

   function Check_URI
     (Name : Unicode.CES.Byte_Sequence) return URI_Type;
   --  Check whether Name is a URI, and its type if it is. This is RFC 3986,
   --  see http://www.ietf.org/rfc/rfc3986.txt.

   function Is_Valid_URI
     (Name : Unicode.CES.Byte_Sequence) return Boolean;
   --  Check whether URI is a valid absolute or relative URI

   function Is_Valid_URN
     (Name : Unicode.CES.Byte_Sequence) return Boolean;
   --  True if Name is a valid URN (Uniform Ressource Name) identification, as
   --  per RFC 2141.
   --  See http://www.faqs.org/rfcs/rfc2141.html

   function Is_Valid_IRI
     (Name : Unicode.CES.Byte_Sequence) return Boolean;
   --  Whether Name is a valid IRI (Internationalized Resource Identifier), as
   --  per Namespaces in XML 1.1 definition
   --  See http://www.w3.org/TR/xml-names11/#dt-IRI

   function Contains_URI_Fragment
     (Name : Unicode.CES.Byte_Sequence) return Boolean;
   --  True if Name contains a URI fragment (starting with #)

   function Is_Valid_HexBinary
     (Str  : Unicode.CES.Byte_Sequence) return Boolean;
   --  Whether Str only contains valid hexadecimal digits

   function Hash
     (Key : Unicode.CES.Byte_Sequence) return Interfaces.Unsigned_32;
   --  Hash-code used for all htables indexed on strings

   function Split_Qname (Qname : Unicode.CES.Byte_Sequence) return Integer;
   --  Return an index so that:
   --     Qname (Qname'First .. Result - 1) = <prefix>
   --     Qname (Result + 1 .. Qname'Last) = <local_name>

end Sax.Utils;
