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
private with Ada.Tags;

package Schema is

   procedure Set_Debug_Output (Output : Boolean);
   --  Whether we should output debug traces

private

   -----------
   -- Debug --
   -----------
   --  The following subprograms are used to print debug traces for XML/Ada
   --  itself, and should not be used in user applications

   type Debug_Output_Mode is
     (Debug_Default,
      Debug_Seen,    --  to show elements seen in XML stream
      Debug_Action); --  to show actions performed on the grammars

   procedure Debug_Push_Prefix
     (Append : String; Mode : Debug_Output_Mode := Debug_Default);
   procedure Debug_Pop_Prefix;
   --  Append a prefix to the current output

   function Debug_Tag_Name (Self : Ada.Tags.Tag) return String;
   --  Return the external name for Self

   procedure Debug_Output
     (Str : String; Mode : Debug_Output_Mode := Debug_Default);
   pragma Inline (Debug_Output);
   --  Display a string for debugging purposes

   procedure Output_Action (Str : String);
   procedure Output_Seen (Str : String);
   pragma Inline (Output_Action, Output_Seen);
   --  Same as Debug_Output (Str, Debug_Action);
   --  or Debug_Output (Debug_Seen);

   Debug : Boolean := False;
   --  Whether we are in debug mode.
   --  The above subprograms do nothing if not in debug mode, but this
   --  variable can be used to avoid preparing strings for display if we are
   --  not going to display them afterward.

end Schema;
