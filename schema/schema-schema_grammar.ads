
--  This file contains a function that manually creates a schema-for-schema
--  grammar, ie a grammar that is used to validate a schema file.
--
--  This is intended both as an example on how to manually write grammars,
--  and as a way to check schema.xsd. The latter should be used, however, since
--  the grammar created here is partically incomplete and will not accept any
--  kind of schema.

with Schema.Validators;

package Schema.Schema_Grammar is

   function Create_Schema_For_Schema return Schema.Validators.XML_Grammar;
   --  Return a grammar that can be used to validate XML Schema files (.xsd)

end Schema.Schema_Grammar;
