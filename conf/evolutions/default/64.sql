# Default Schema

# --- !Ups

CREATE INDEX CalculatedLink_site_dst_src_alias_index
    ON CalculatedLink (site, dst, src, alias);

CREATE INDEX CalculatedSchemaOrg_site_value_page_index
    ON CalculatedSchemaOrg (site, value, page);

# --- !Downs

DROP INDEX CalculatedSchemaOrg_site_value_page_index ON CalculatedSchemaOrg;

DROP INDEX CalculatedLink_site_dst_src_alias_index ON CalculatedLink;
