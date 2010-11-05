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

with Sax.Readers;           use Sax.Readers;
with Schema.Validators;     use Schema.Validators;
with Schema.Schema_Readers; use Schema.Schema_Readers;
with Input_Sources.File;    use Input_Sources.File;

package body Schema.Validators.XSD_Grammar is

   procedure Add_Schema_For_Schema
     (R : in out Schema.Validators.Abstract_Validation_Reader'Class)
   is
      Schema : Schema_Reader;
      Input  : File_Input;
   begin
      Set_Grammar (Schema, R.Grammar);
      Set_Feature (Schema, Schema_Validation_Feature, False);
      Open ("/home/briot/svn/trunk/xmlada/schema/test/schema.xsd", Input);
      Parse (Schema, Input);
      Close (Input);
   end Add_Schema_For_Schema;

end Schema.Validators.XSD_Grammar;
