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

with Ada.Exceptions;  use Ada.Exceptions;
with Unicode.CES;     use Unicode.CES;
with Sax.Locators;    use Sax.Locators;

package body Sax.Exceptions is

   ------------
   -- Create --
   ------------

   function Create (Ada_Exception : Ada.Exceptions.Exception_Id)
      return Sax_Exception'Class is
   begin
      return Sax_Exception'
        (Length => 0, Message => "", Except => Ada_Exception);
   end Create;

   ------------
   -- Create --
   ------------

   function Create (Message : Byte_Sequence) return Sax_Exception'Class is
   begin
      return Sax_Exception'(Length  => Message'Length,
                            Message => Message,
                            Except  => Null_Id);
   end Create;

   ------------
   -- Create --
   ------------

   function Create
     (Message : Byte_Sequence;
      Ada_Exception : Ada.Exceptions.Exception_Id) return Sax_Exception'Class
   is
   begin
      return Sax_Exception'(Length  => Message'Length,
                            Message => Message,
                            Except  => Ada_Exception);
   end Create;

   -------------------
   -- Get_Exception --
   -------------------

   function Get_Exception (Except : Sax_Exception)
      return Ada.Exceptions.Exception_Id is
   begin
      return Except.Except;
   end Get_Exception;

   -----------------
   -- Get_Message --
   -----------------

   function Get_Message (Except : Sax_Exception) return Byte_Sequence is
   begin
      return Except.Message;
   end Get_Message;

   ------------
   -- Create --
   ------------

   function Create (Message : Unicode.CES.Byte_Sequence;
                    Loc     : access Locators.Locator'Class)
      return Sax_Parse_Exception'Class is
   begin
      return Sax_Parse_Exception'
        (Length  => Message'Length,
         Message => Message,
         Loc     => Locator_Access (Loc),
         Except  => Null_Id);
   end Create;

   ------------
   -- Create --
   ------------

   function Create
     (Message       : Unicode.CES.Byte_Sequence;
      Ada_Exception : Ada.Exceptions.Exception_Id;
      Loc           : access Locators.Locator'Class)
      return Sax_Exception'Class is
   begin
      return Sax_Parse_Exception'
        (Length  => Message'Length,
         Message => Message,
         Loc     => Locator_Access (Loc),
         Except  => Ada_Exception);
   end Create;

   -----------------
   -- Get_Locator --
   -----------------

   function Get_Locator (Except : Sax_Parse_Exception)
      return Locators.Locator'Class is
   begin
      return Except.Loc.all;
   end Get_Locator;

end Sax.Exceptions;
