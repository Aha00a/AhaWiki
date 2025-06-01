# Default Schema

# --- !Ups

rename table SchemaOrg to CalculatedSchemaOrg;

# --- !Downs

rename table CalculatedSchemaOrg to SchemaOrg;
