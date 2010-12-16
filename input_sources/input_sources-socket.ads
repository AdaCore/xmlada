-----------------------------------------------------------------------
--                XML/Ada - An XML suite for Ada95                   --
--                                                                   --
--                       Copyright (C) 2001-2010, AdaCore            --
--                       Copyright (C) 2010, Jehan Pages             --
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

with Unicode;
with Unicode.CES;
with GNAT.Sockets;

package Input_Sources.Socket is

   type Socket_Input is new Input_Source with private;
   type Socket_Input_Access is access all Socket_Input'Class;
   --  A special implementation of a reader, that reads from a
   --  streaming socket.
   --  Compared to Input_Sources.Html, this package does not expect to read
   --  the whole stream when calling Open. It is in fact an example on how to
   --  detect incomplete input (as opposed to invalid input).

   procedure Open
     (Socket : GNAT.Sockets.Socket_Type; Input : out Socket_Input);
   --  Open a new input reading from the socket.

   procedure Close (Input : in out Socket_Input);
   --  Free the memory

   procedure Next_Char
     (From : in out Socket_Input;
      C    : out Unicode.Unicode_Char);
   --  Return the next character in the buffer.
   --  This is a blocking procedure until some character becomes available on
   --  the socket.

   function Eof (From : Socket_Input) return Boolean;
   --  True if the socket is closed and all data received from it has been
   --  read.

private
   type Socket_Input is new Input_Source with record
      Socket : GNAT.Sockets.Socket_Type;
      Index  : Natural;
      Buffer_Last : Natural;
      Buffer : Unicode.CES.Byte_Sequence_Access;
      End_Of_File : Boolean;
   end record;
end Input_Sources.Socket;
