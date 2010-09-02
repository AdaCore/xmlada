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

with Ada.Text_IO;  use Ada.Text_IO;

package body Schema is

   Debug_Prefixes_Level : Natural := 0;

   ------------------
   -- Debug_Output --
   ------------------

   procedure Debug_Output
     (Str  : String;
      Mode : Debug_Output_Mode := Debug_Default) is
   begin
      Put ((1 .. Debug_Prefixes_Level * 2 => ' '));

      case Mode is
         when Debug_Default =>
            null;
         when Debug_Seen =>
            Put (ASCII.ESC & "[33m");
         when Debug_Action =>
            Put (ASCII.ESC & "[34m");
      end case;

      Put (Str);

      if Mode /= Debug_Default then
         Put (ASCII.ESC & "[39m");
      end if;

      New_Line;
   end Debug_Output;

   -------------------
   -- Output_Action --
   -------------------

   procedure Output_Action (Str : String) is
   begin
      Debug_Output (Str, Mode => Debug_Action);
   end Output_Action;

   -----------------
   -- Output_Seen --
   -----------------

   procedure Output_Seen (Str : String) is
   begin
      Debug_Output (Str, Mode => Debug_Seen);
   end Output_Seen;

   ----------------------
   -- Set_Debug_Output --
   ----------------------

   procedure Set_Debug_Output (Output : Boolean) is
   begin
      Debug := Output;
   end Set_Debug_Output;

   -----------------------
   -- Debug_Push_Prefix --
   -----------------------

   procedure Debug_Push_Prefix
     (Append : String; Mode : Debug_Output_Mode := Debug_Default)
   is
   begin
      if Debug then
         Debug_Output (Append, Mode);
         Debug_Prefixes_Level := Debug_Prefixes_Level + 1;
      end if;
   end Debug_Push_Prefix;

   ----------------------
   -- Debug_Pop_Prefix --
   ----------------------

   procedure Debug_Pop_Prefix is
   begin
      if Debug then
         Debug_Prefixes_Level := Debug_Prefixes_Level - 1;
      end if;
   end Debug_Pop_Prefix;

   --------------------
   -- Debug_Tag_Name --
   --------------------

   function Debug_Tag_Name (Self : Ada.Tags.Tag) return String is
      E : constant String := Ada.Tags.External_Tag (Self);
   begin
      for P in reverse E'Range loop
         if E (P) = '.' then
            return E (P + 1 .. E'Last);
         end if;
      end loop;

      return E;
   end Debug_Tag_Name;

end Schema;
