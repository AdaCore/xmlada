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
with Unicode.CES;        use Unicode.CES;
with Unicode.CES.Utf8;   use Unicode.CES.Utf8;

with GNAT.Sockets;            use GNAT.Sockets;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Streams; use Ada.Streams;

package body Input_Sources.Socket is

   Debug  : constant Boolean := False;
   BUFSIZ : constant := 2048;

   ----------
   -- Open --
   ----------

   procedure Open (Socket : Socket_Type; Input : out Socket_Input) is
   begin
      Input.Socket := Socket;
      Input.Buffer := new String (1 .. BUFSIZ);
      Input.Index := Input.Buffer'First;
      Input.Buffer_Last := 0;
      Input.End_Of_File := False;
      Set_Encoding (Input, Utf8_Encoding);
   end Open;

   -----------
   -- Close --
   -----------

   procedure Close (Input : in out Socket_Input) is
   begin
      Close_Socket (Input.Socket);
      Input_Sources.Close (Input_Source (Input));
      Input.Index := 0;
      Input.Buffer_Last := 0;
      Free (Input.Buffer);
   end Close;

   ---------------
   -- Next_Char --
   ---------------

   procedure Next_Char
     (From : in out Socket_Input; C : out Unicode.Unicode_Char)
   is
      procedure Update_Buffer;
      --  Read the next stream of bytes from the socket

      -------------------
      -- Update_Buffer --
      -------------------

      procedure Update_Buffer is
         --  There can be at most 3 bytes not processed (unfinished UTF-8 code)
         Len : constant Stream_Element_Count :=
           Stream_Element_Count (BUFSIZ - From.Buffer_Last + From.Index + 1);
         Buffer  : Stream_Element_Array (1 .. Len);
         Buffer_Last : Stream_Element_Count := 0;

      begin
         GNAT.Sockets.Receive_Socket (From.Socket, Buffer, Buffer_Last);

         if Buffer_Last = Buffer'Last - 1 then
            From.End_Of_File := True;
            return;
         end if;

         if From.Index <= From.Buffer_Last then
            for A in From.Index .. From.Buffer_Last loop
               From.Buffer (A - From.Index + 1) := From.Buffer (A);
            end loop;
            From.Buffer_Last := From.Buffer_Last - From.Index + 1;
         else
            From.Buffer_Last := 0;
         end if;

         From.Index := 1;

         for A in 1 .. Buffer_Last loop
            From.Buffer (From.Buffer_Last + Natural (A)) :=
              Character'Val (Buffer (A));
         end loop;

         From.Buffer_Last := From.Buffer_Last + Natural (Buffer_Last);

         if Debug then
            Put ("< ");
            for B in Buffer'First .. Buffer_Last loop
               Put (Character'Val (Buffer (B)));
            end loop;
            New_Line;
         end if;
      end Update_Buffer;

   begin
      --  loop until there is something in the buffer.
      --  This is a blocking procedure.

      loop
         begin
            if From.Index > From.Buffer_Last then
               Update_Buffer;
            end if;

            if From.Index <= From.Buffer_Last then
               From.Es.Read (From.Buffer.all, From.Index, C);
               C := From.Cs.To_Unicode (C);
               return;
            end if;

         exception
            when Incomplete_Encoding =>
               --  Incomplete byte sequence at end of the buffer, is not an
               --  error.
               --  Loop until buffer is upated with enough data to find out
               --  whether we have a fully invalid sequence or a complete one.
               null;
         end;
      end loop;
   end Next_Char;

   ---------
   -- Eof --
   ---------

   function Eof (From : Socket_Input) return Boolean is
   begin
      --  Even with no data in the buffer, the input must never be considered
      --  end of file except when the socket was closed and there is no more
      --  data to process.

      return From.End_Of_File and then From.Index > From.Buffer_Last;
   end Eof;

end Input_Sources.Socket;
