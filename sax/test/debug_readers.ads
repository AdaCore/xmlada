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

with Sax.Exceptions;
with Sax.Locators;
with Sax.Readers;
with Sax.Attributes;
with Sax.Models;
with Unicode.CES;
with Input_Sources;

package Debug_Readers is
   type Debug_Reader is new Sax.Readers.Reader with private;

   procedure Set_Silent
     (Handler : in out Debug_Reader; Silent : Boolean);
   --  If Silent is True, then nothing will be output on the console, except
   --  error messages

   procedure Set_Color
     (Handler : in out Debug_Reader; Color : Boolean);
   --  Whether the output should use color for locations

   procedure Warning
     (Handler : in out Debug_Reader;
      Except : Sax.Exceptions.Sax_Parse_Exception'Class);
   procedure Error
     (Handler : in out Debug_Reader;
      Except  : Sax.Exceptions.Sax_Parse_Exception'Class);
   procedure Fatal_Error
     (Handler : in out Debug_Reader;
      Except  : Sax.Exceptions.Sax_Parse_Exception'Class);
   procedure Set_Document_Locator
     (Handler : in out Debug_Reader;
      Loc     : in out Sax.Locators.Locator);
   procedure Start_Document (Handler : in out Debug_Reader);
   procedure End_Document (Handler : in out Debug_Reader);
   procedure Start_Prefix_Mapping
     (Handler : in out Debug_Reader;
      Prefix  : Unicode.CES.Byte_Sequence;
      URI     : Unicode.CES.Byte_Sequence);
   procedure End_Prefix_Mapping
     (Handler : in out Debug_Reader;
      Prefix  : Unicode.CES.Byte_Sequence);
   procedure Start_Element
     (Handler       : in out Debug_Reader;
      Namespace_URI : Unicode.CES.Byte_Sequence := "";
      Local_Name    : Unicode.CES.Byte_Sequence := "";
      Qname         : Unicode.CES.Byte_Sequence := "";
      Atts          : Sax.Attributes.Attributes'Class);
   procedure End_Element
     (Handler : in out Debug_Reader;
      Namespace_URI : Unicode.CES.Byte_Sequence := "";
      Local_Name    : Unicode.CES.Byte_Sequence := "";
      Qname         : Unicode.CES.Byte_Sequence := "");
   procedure Characters
     (Handler : in out Debug_Reader; Ch : Unicode.CES.Byte_Sequence);
   procedure Ignorable_Whitespace
     (Handler : in out Debug_Reader; Ch : Unicode.CES.Byte_Sequence);
   procedure Processing_Instruction
     (Handler : in out Debug_Reader;
      Target  : Unicode.CES.Byte_Sequence;
      Data    : Unicode.CES.Byte_Sequence);
   procedure Skipped_Entity
     (Handler : in out Debug_Reader; Name : Unicode.CES.Byte_Sequence);
   procedure Comment
     (Handler : in out Debug_Reader; Ch : Unicode.CES.Byte_Sequence);
   procedure Start_Cdata (Handler : in out Debug_Reader);
   procedure End_Cdata (Handler : in out Debug_Reader);
   procedure Start_Entity
     (Handler : in out Debug_Reader; Name : Unicode.CES.Byte_Sequence);
   procedure End_Entity
     (Handler : in out Debug_Reader; Name : Unicode.CES.Byte_Sequence);
   procedure Start_DTD
     (Handler   : in out Debug_Reader;
      Name      : Unicode.CES.Byte_Sequence;
      Public_Id : Unicode.CES.Byte_Sequence := "";
      System_Id : Unicode.CES.Byte_Sequence := "");
   procedure End_DTD (Handler : in out Debug_Reader);
   procedure Internal_Entity_Decl
     (Handler : in out Debug_Reader;
      Name    : Unicode.CES.Byte_Sequence;
      Value   : Unicode.CES.Byte_Sequence);
   procedure External_Entity_Decl
     (Handler   : in out Debug_Reader;
      Name      : Unicode.CES.Byte_Sequence;
      Public_Id : Unicode.CES.Byte_Sequence;
      System_Id : Unicode.CES.Byte_Sequence);
   procedure Unparsed_Entity_Decl
     (Handler       : in out Debug_Reader;
      Name          : Unicode.CES.Byte_Sequence;
      System_Id     : Unicode.CES.Byte_Sequence;
      Notation_Name : Unicode.CES.Byte_Sequence);
   procedure Element_Decl
     (Handler : in out Debug_Reader;
      Name    : Unicode.CES.Byte_Sequence;
      Model   : Sax.Models.Content_Model);
   procedure Notation_Decl
     (Handler       : in out Debug_Reader;
      Name          : Unicode.CES.Byte_Sequence;
      Public_Id     : Unicode.CES.Byte_Sequence;
      System_Id     : Unicode.CES.Byte_Sequence);
   procedure Attribute_Decl
     (Handler : in out Debug_Reader;
      Ename   : Unicode.CES.Byte_Sequence;
      Aname   : Unicode.CES.Byte_Sequence;
      Typ     : Sax.Attributes.Attribute_Type;
      Content : Sax.Models.Content_Model;
      Value_Default : Sax.Attributes.Default_Declaration;
      Value   : Unicode.CES.Byte_Sequence);
   function Resolve_Entity
     (Handler   : Debug_Reader;
      Public_Id : Unicode.CES.Byte_Sequence;
      System_Id : Unicode.CES.Byte_Sequence)
      return Input_Sources.Input_Source_Access;

private
   type String_Access is access String;
   type String_List is array (Natural range <>) of String_Access;
   type String_List_Access is access String_List;

   type Debug_Reader is new Sax.Readers.Reader with record
      Locator : Sax.Locators.Locator;
      Silent  : Boolean := False;
      Saved_Locs : String_List_Access;
      Color   : Boolean := True;
   end record;
end Debug_Readers;
