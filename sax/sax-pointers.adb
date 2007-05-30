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

with Ada.Unchecked_Deallocation;

package body Sax.Pointers is

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Root_Encapsulated'Class, Root_Encapsulated_Access);

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
         Tmp : constant Encapsulated_Access :=
           new Encapsulated'Class'(Data);
      begin
         return Allocate (Tmp);
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

      procedure Finalize (P : in out Pointer) is
      begin
         --  Test if refcount is > 0, in case we are already freeing this
         --  element. That shouldn't happen, though, since we are not in a
         --  multi-tasking environment.

         if P.Data /= null and then P.Data.Refcount > 0 then
            P.Data.Refcount := P.Data.Refcount - 1;
            if P.Data.Refcount = 0 then
               Free (P.Data.all);
               Unchecked_Free (P.Data);
            end if;
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
         end if;
      end Adjust;
   end Smart_Pointers;

end Sax.Pointers;
