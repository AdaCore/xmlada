
--  This package contains various subprograms not described in the SAX
--  standard, but which are used by the various components of XML/Ada

with Unicode.CES;

package Sax.Utils is

   function Is_Valid_Language_Name
     (Lang : Unicode.CES.Byte_Sequence) return Boolean;
   --  Whether Lang is a valid language, as per 2.12 in the XML specifications.
   --  Lang is encoded with Sax.Encodings.Encoding

   function Is_Valid_Name_Char (Char : Unicode.Unicode_Char) return Boolean;
   --  Whether Char is a valid NameChar, as per 2.3 in the XML specifications

   function Is_Valid_Nmtoken
     (Nmtoken : Unicode.CES.Byte_Sequence) return Boolean;
   --  Whether Nmtoken is valid NMTOKEN as per 2.3 in the XML specifications

   function Is_Valid_Name
     (Name : Unicode.CES.Byte_Sequence) return Boolean;
   --  Whether Name is valid name as per 2.3 in the XML specifications

   function Is_Valid_NCname
     (Name : Unicode.CES.Byte_Sequence) return Boolean;
   --  Whether Name is valid NCname as per 2 in the XML namespaces
   --  specifications

   function Is_Valid_QName
     (Name : Unicode.CES.Byte_Sequence) return Boolean;
   --  Whether Name is valid QName as per 3 in the XML specifications

end Sax.Utils;
