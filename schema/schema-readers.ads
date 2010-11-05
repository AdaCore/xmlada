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

pragma Ada_05;

with Sax.Exceptions;
with Sax.Locators;
with Input_Sources;
with Sax.Symbols;
with Sax.Utils;
with Unicode.CES;
with Schema.Validators;

package Schema.Readers is

   type Validating_Reader is new Schema.Validators.Abstract_Validation_Reader
      with private;
   type Validating_Reader_Access is access all Validating_Reader'Class;
   --  To get full validation of an XML document, you must derive from this
   --  type. You must also enable the Validation_Feature feature, through a
   --  call to Set_Feature.
   --  If you override the Parse method in your code, you must call
   --     Parse (Validating_Reader (Your_Reader), Input);
   --  and not  Parse (Reader (Your_Reader), Input) to get validation.
   --
   --  In case of validation error, the exception XML_Validation_Error is
   --  raised, and you can get the error message by calling Get_Error_Message.
   --
   --  In most cases, the reader will find by itself what variable should be
   --  used, from the contents of the XML file:
   --  It uses the attribute of the nodes to find out what grammar, for
   --  instance from the following XML extract:
   --     <root xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
   --           xsi:noNamespaceSchemaLocation="my_file.xsd" />
   --  or
   --     <root xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
   --           xsi:schemaLocation="ns1 my_file.xsd  ns2 my_file2.xsd" />
   --
   --  The second variant associates a specific grammar with each of the
   --  namespaces found in the document.

   procedure Set_Grammar
     (Reader  : in out Validating_Reader;
      Grammar : Schema.Validators.XML_Grammar);
   function Get_Grammar
     (Reader  : Validating_Reader) return Schema.Validators.XML_Grammar;
   --  Sets the grammar to use to validate the document.
   --  When parsing a XSD files, the grammar should contain the schema for XSD
   --  as defined by the W3C norm (although this will be automatically
   --  initialized in this case, so calling Set_Grammar is optional). Multiple
   --  XSD files can be parsed, and the result will be added to the same
   --  Grammar. Get_Grammar can be used to retrieve the resulting grammar after
   --  parsing all the XSD files.
   --
   --  On the other hand, when parsing XML files, Grammar must have been
   --  initialized (in general through a call to Schema.Schema_Readers.Parse).
   --  If Set_Grammar is not called, no validation takes place.
   --
   --  If a symbol table was set for this reader, the grammar must have been
   --  created with the same symbol table.

   overriding procedure Set_Symbol_Table
     (Parser  : in out Validating_Reader;
      Symbols : Sax.Utils.Symbol_Table);
   --  Override the symbol table. If a grammar was already set for this parser,
   --  the symbol table must be the same as in the grammar.

   overriding function Get_Locator
     (Reader : Validating_Reader) return Sax.Locators.Locator;
   --  Return the current location

   procedure Validation_Error
     (Reader : in out Validating_Reader;
      Except : Sax.Exceptions.Sax_Parse_Exception'Class);
   --  Called when a validation error occurs.
   --  By default, this raises XML_Validation_Error

   function To_Absolute_URI
     (Handler : Validating_Reader;
      URI     : Sax.Symbols.Symbol) return Sax.Symbols.Symbol;
   --  Convert a URI read in the input stream of Handler to an absolute URI.
   --  This is used for instance to find the location of a schema file,...

   procedure Parse_Grammars
     (Handler         : access Validating_Reader'Class;
      Schema_Location : Sax.Symbols.Symbol;
      Do_Create_NFA : Boolean);
   --  Parse multiple grammars, as defined by the "schemaLocation" attribute

   procedure Get_Namespace_From_Prefix
     (Handler  : in out Validating_Reader;
      Prefix   : Sax.Symbols.Symbol;
      NS       : out Sax.Utils.XML_NS);
   --  Get the namespace associated with a given prefix, in the current
   --  context.
   --  The caller must not modify the return value.
   --  Returns No_XML_NS if the prefix is not defined

   procedure Free (Reader : in out Validating_Reader_Access);
   --  Free the memory used by Reader

   overriding procedure Parse
     (Parser : in out Validating_Reader;
      Input  : in out Input_Sources.Input_Source'Class);
   --  Override inherited method.

   function Locator (Parser : Validating_Reader) return Sax.Locators.Locator;
   procedure Set_Locator
     (Parser : in out Validating_Reader; Loc : Sax.Locators.Locator);
   --  The locator used to get information on the current location of the
   --  parser.

private

   type Validating_Reader is new Schema.Validators.Abstract_Validation_Reader
   with record
      Locator          : Sax.Locators.Locator;
      Matcher          : Schema.Validators.Schema_State_Machines.NFA_Matcher;

      Characters       : Unicode.CES.Byte_Sequence_Access;
      Characters_Count : Natural := 0;
      --  The current stream of characters we have seen. We need to collapse
      --  adjacent characters, so that we can validate the full contents of a
      --  tag at once, and not by parts.
   end record;

end Schema.Readers;
