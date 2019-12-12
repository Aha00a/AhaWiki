# GeocodeCache

# --- !Ups

create table GeocodeCache
(
	address VARCHAR(255) not null,
	lat double not null,
	lng double not null,
	created DATETIME default NOW() not null,
	constraint GeocodeCache_pk
		primary key (address)
);

# --- !Downs

DROP TABLE GeocodeCache;
