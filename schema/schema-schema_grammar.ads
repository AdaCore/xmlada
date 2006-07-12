
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
