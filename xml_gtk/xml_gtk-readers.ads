-----------------------------------------------------------------------
--                XML/Ada - An XML suite for Ada95                   --
--                                                                   --
--                       Copyright (C) 2004                          --
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

with Sax.Readers;          use Sax.Readers;
with Sax.Attributes;
with Sax.Exceptions;
with Unicode.CES;
with Glib.XML;

generic
   type XML_Specific_Data is private;
   No_Specific_Data : XML_Specific_Data;
   with package Glib_XML is new Glib.XML (XML_Specific_Data);
package XML_Gtk.Readers is

   type Gtk_Reader is new Reader with private;
   type Gtk_Reader_Access is access all Gtk_Reader'Class;
   --  Special SAX Reader that creates a tree compatible with Glib.XML, found
   --  in the GtkAda distribution. This allows replacing the parser in that
   --  package with the better one in XML/Ada.

   function Get_Tree (Read : Gtk_Reader) return Glib_XML.Node_Ptr;

   procedure Set_Warnings_As_Errors
     (Read : in out Gtk_Reader; Warnings_As_Error : Boolean);
   --  iF Warnings_As_Error is True, then all warnings will raise a fatal error
   --  exception, just like a fatal error. Otherwise, warnings are ignored.

private

   type Gtk_Reader is new Reader with record
      Tree                       : Glib_XML.Node_Ptr;
      Current_Node               : Glib_XML.Node_Ptr;
      Internal_Encoding          : Unicode.CES.Encoding_Scheme;
      Warnings_As_Error          : Boolean := False;
   end record;

   procedure Start_Document (Handler : in out Gtk_Reader);
   procedure Start_Element
     (Handler       : in out Gtk_Reader;
      Namespace_URI : Unicode.CES.Byte_Sequence := "";
      Local_Name    : Unicode.CES.Byte_Sequence := "";
      Qname         : Unicode.CES.Byte_Sequence := "";
      Atts          : Sax.Attributes.Attributes'Class);
   procedure End_Element
     (Handler : in out Gtk_Reader;
      Namespace_URI : Unicode.CES.Byte_Sequence := "";
      Local_Name    : Unicode.CES.Byte_Sequence := "";
      Qname         : Unicode.CES.Byte_Sequence := "");
   procedure Characters
     (Handler : in out Gtk_Reader;
      Ch      : Unicode.CES.Byte_Sequence);
   procedure Ignorable_Whitespace
     (Handler : in out Gtk_Reader;
      Ch      : Unicode.CES.Byte_Sequence);
   procedure Error
     (Handler : in out Gtk_Reader;
      Except  : Sax.Exceptions.Sax_Parse_Exception'Class);
   procedure Warning
     (Handler : in out Gtk_Reader;
      Except : Sax.Exceptions.Sax_Parse_Exception'Class);
end XML_Gtk.Readers;