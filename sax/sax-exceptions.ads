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

with Ada.Exceptions;
with Sax.Locators;
with Unicode.CES;

package Sax.Exceptions is

   -------------------
   -- Sax_Exception --
   -------------------

   type Sax_Exception (<>) is tagged private;
   --  General type that encapsulates a general SAX error or warning.
   --  It does not contain source location information (see Sax_Parse_Exception
   --  instead)

   function Create (Ada_Exception : Ada.Exceptions.Exception_Id)
      return Sax_Exception'Class;
   --  Create a new SAX exception wrapping an existing exception

   function Create (Message : Unicode.CES.Byte_Sequence)
      return Sax_Exception'Class;
   --  Create a new SAX exception

   function Create
     (Message       : Unicode.CES.Byte_Sequence;
      Ada_Exception : Ada.Exceptions.Exception_Id)
      return Sax_Exception'Class;
   --  Create a new SAX exception from an existing exception

   function Get_Exception (Except : Sax_Exception)
      return Ada.Exceptions.Exception_Id;
   --  Return the embedded exception

   function Get_Message (Except : Sax_Exception)
      return Unicode.CES.Byte_Sequence;
   --  Return the message

   -------------------------
   -- Sax_Parse_Exception --
   -------------------------

   type Sax_Parse_Exception (<>) is new Sax_Exception with private;

   function Create (Message : Unicode.CES.Byte_Sequence;
                    Loc     : access Sax.Locators.Locator_Impl'Class)
      return Sax_Parse_Exception'Class;

   function Create
     (Message       : Unicode.CES.Byte_Sequence;
      Ada_Exception : Ada.Exceptions.Exception_Id;
      Loc           : access Sax.Locators.Locator_Impl'Class)
      return Sax_Exception'Class;
   --  Create a new Sax_Parse_Exception. Note: no copy of Loc is made.

   function Get_Locator (Except : Sax_Parse_Exception)
      return Locators.Locator'Class;
   --  return the location where the exception was raised.

private
   type Sax_Exception (Length : Natural) is tagged record
      Message : Unicode.CES.Byte_Sequence (1 .. Length);
      Except  : Ada.Exceptions.Exception_Id;
   end record;

   type Sax_Parse_Exception is new Sax_Exception with record
      Loc : Sax.Locators.Locator_Impl_Access;
   end record;
end Sax.Exceptions;
