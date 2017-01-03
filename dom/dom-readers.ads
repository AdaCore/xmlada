------------------------------------------------------------------------------
--                     XML/Ada - An XML suite for Ada95                     --
--                                                                          --
--                     Copyright (C) 2001-2017, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

pragma Ada_05;

with Sax.Readers;          use Sax.Readers;
with Sax.Exceptions;
with Sax.Symbols;
with Sax.Utils;
with Unicode.CES;
with DOM.Core;             use DOM.Core;

package DOM.Readers is

   type Tree_Reader is new Sax_Reader with private;
   type Tree_Reader_Access is access all Tree_Reader'Class;
   --  Special SAX Reader that creates a DOM tree in its callbacks.
   --  Note that in case of a fatal error, it is your responsability to
   --  free the tree, since it is left in the state it was when the error
   --  was raised (for post-death analysis, if required).

   function Get_Tree (Read : Tree_Reader) return Document;
   --  The document created on the fly by the reader

   function Current_Node (Read : Tree_Reader) return Node;
   --  The last node created by Start_Element. This function is useful when
   --  you want to override Start_Element or Characters to do your own
   --  specific changes.

   procedure Free (Read : in out Tree_Reader);
   --  Free the memory associated with the reader, in particular the tree

   procedure Set_Warnings_As_Errors
     (Read : in out Tree_Reader; Warnings_As_Error : Boolean);
   --  iF Warnings_As_Error is True, then all warnings will raise a fatal error
   --  exception, just like a fatal error. Otherwise, warnings are ignored.

   overriding procedure Start_Document (Handler : in out Tree_Reader);
   overriding procedure Start_Element
     (Handler    : in out Tree_Reader;
      NS         : Sax.Utils.XML_NS;
      Local_Name : Sax.Symbols.Symbol;
      Atts       : Sax.Readers.Sax_Attribute_List);
   overriding procedure End_Element
     (Handler    : in out Tree_Reader;
      NS         : Sax.Utils.XML_NS;
      Local_Name : Sax.Symbols.Symbol);
   overriding procedure Characters
     (Handler : in out Tree_Reader;
      Ch      : Unicode.CES.Byte_Sequence);
   overriding procedure Ignorable_Whitespace
     (Handler : in out Tree_Reader;
      Ch      : Unicode.CES.Byte_Sequence);
   overriding procedure Processing_Instruction
     (Handler : in out Tree_Reader;
      Target  : Unicode.CES.Byte_Sequence;
      Data    : Unicode.CES.Byte_Sequence);
   overriding procedure Start_DTD
     (Handler   : in out Tree_Reader;
      Name      : Unicode.CES.Byte_Sequence;
      Public_Id : Unicode.CES.Byte_Sequence := "";
      System_Id : Unicode.CES.Byte_Sequence := "");
   overriding procedure End_DTD (Handler : in out Tree_Reader);
   overriding procedure Comment
     (Handler : in out Tree_Reader;
      Comment : Unicode.CES.Byte_Sequence);
   overriding procedure Error
     (Handler : in out Tree_Reader;
      Except  : Sax.Exceptions.Sax_Parse_Exception'Class);
   overriding procedure Warning
     (Handler : in out Tree_Reader;
      Except  : Sax.Exceptions.Sax_Parse_Exception'Class);
   --  See inherited documentation

private

   type Tree_Reader is new Reader with record
      Tree              : Document;
      Current_Node      : Node;
      Internal_Encoding : Unicode.CES.Encoding_Scheme;
      In_DTD            : Boolean := False;
      Warnings_As_Error : Boolean := False;
   end record;

end DOM.Readers;
