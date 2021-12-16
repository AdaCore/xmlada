------------------------------------------------------------------------------
--                     XML/Ada - An XML suite for Ada95                     --
--                                                                          --
--                     Copyright (C) 2001-2021, AdaCore                     --
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

with GNAT.OS_Lib;      use GNAT.OS_Lib;
with Sax.Exceptions;   use Sax.Exceptions;

package body Testxml_Support is

   -----------
   -- Error --
   -----------

   procedure Error
     (Handler : in out My_Tree_Reader;
      Except  : Sax.Exceptions.Sax_Parse_Exception'Class)
   is
      Tmp : String_Access := Handler.Error_Msg;
   begin
      if Tmp = null then
         Handler.Error_Msg := new String'(Get_Message (Except));
      else
         Handler.Error_Msg := new String'
           (Tmp.all & ASCII.LF & Get_Message (Except));
         Free (Tmp);
      end if;
      Handler.Had_Error := True;
   end Error;

end Testxml_Support;
