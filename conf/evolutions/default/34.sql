# Default Schema

# --- !Ups

rename table Link to CalculatedLink;

# --- !Downs

rename table CalculatedLink to Link;
