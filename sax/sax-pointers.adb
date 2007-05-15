-----------------------------------------------------------------------
--                XML/Ada - An XML suite for Ada95                   --
--                                                                   --
--                       Copyright (C) 2007, AdaCore                 --
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

--  with Ada.Text_IO; use Ada.Text_IO;
--  with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
--  with GNAT.Traceback; use GNAT.Traceback;
--  with System.Address_Image;

package body Sax.Pointers is

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Root_Encapsulated'Class, Root_Encapsulated_Access);
--     function UC is new Ada.Unchecked_Conversion
--       (Root_Encapsulated_Access, System.Address);

   --------------
   -- Get_Name --
   --------------

   function Get_Name (Data : access Root_Encapsulated) return String is
      pragma Unreferenced (Data);
   begin
      return "<unknown>";
   end Get_Name;

   ----------
   -- Free --
   ----------

   procedure Free (Data : in out Root_Encapsulated) is
      pragma Unreferenced (Data);
   begin
      null;
   end Free;

   --------------------
   -- Smart_Pointers --
   --------------------

   package body Smart_Pointers is

      --------------
      -- Allocate --
      --------------

      function Allocate (Data : Encapsulated'Class) return Pointer is
      begin
         return (Ada.Finalization.Controlled with
                 Data => new Encapsulated'Class'(Data));
      end Allocate;

      function Allocate (Data : access Encapsulated'Class) return Pointer is
      begin
         return (Ada.Finalization.Controlled with
                 Root_Encapsulated_Access (Data));
      end Allocate;

      ---------
      -- Get --
      ---------

      function Get (P : Pointer) return Encapsulated_Access is
      begin
         return Encapsulated_Access (P.Data);
      end Get;

      --------------
      -- Finalize --
      --------------

--        Indent : Natural := 0;
--
--        procedure Put_Stack_Trace;
--        procedure Put_Stack_Trace is
--           Tracebacks : Tracebacks_Array (1 .. 50);
--           Len        : Natural;
--        begin
--           Call_Chain (Tracebacks, Len);
--           Put ("(callstack: ");
--           for J in Tracebacks'First .. Len loop
--              Put (System.Address_Image (Tracebacks (J)) & ' ');
--           end loop;
--           Put_Line (")");
--        end Put_Stack_Trace;

      procedure Finalize (P : in out Pointer) is
      begin
         if P.Data /= null then
--              Put_Line ((1 .. Indent => ' ')
--            & "About to finalize "    & System.Address_Image (UC (P.Data)));
--              Put_Stack_Trace;
            P.Data.Refcount := P.Data.Refcount - 1;
--              Put_Line ((1 .. Indent => ' ')
--                & "Finalize "    & System.Address_Image (UC (P.Data))
--                & " " & Get_Name (P.Data)
--                & " Refcount=" & P.Data.Refcount'Img);
--              Indent := Indent + 1;
            if P.Data.Refcount = 0 then
               Free (P.Data.all);
               Unchecked_Free (P.Data);
            end if;
--              Indent := Indent - 1;
            P.Data := null;
         end if;
      end Finalize;

      ------------
      -- Adjust --
      ------------

      procedure Adjust (P : in out Pointer) is
      begin
         if P.Data /= null then
            P.Data.Refcount := P.Data.Refcount + 1;
--              Put_Line
--                ((1 .. Indent => ' ')
--                 & "Adjust "
--                 & System.Address_Image (UC (P.Data))
--                 & P.Data.Refcount'Img);
--              Put_Stack_Trace;
         end if;
      end Adjust;
   end Smart_Pointers;

end Sax.Pointers;
