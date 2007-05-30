-----------------------------------------------------------------------
--                XML/Ada - An XML suite for Ada95                   --
--                                                                   --
--                       Copyright (C) 2004-2007, AdaCore            --
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

--  This file contains a function that manually creates a schema-for-schema
--  grammar, ie a grammar that is used to validate a schema file.
--
--  This is intended both as an example on how to manually write grammars,
--  and as a way to check schema.xsd.

with Schema.Validators;

package Schema.Schema_Grammar is

   procedure Add_Schema_For_Schema
     (Grammar : in out Schema.Validators.XML_Grammar);
   --  Adds the definition for the standard XML namespaces (schemas,...) to
   --  Grammar. The resulting grammar can thus be used, among other things, to
   --  validate schema files. This doesn't add the predefined types, which
   --  must be added separately.

end Schema.Schema_Grammar;
