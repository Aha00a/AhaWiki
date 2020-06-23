# GeocodeCache

# --- !Ups

create table DistanceCache
(
	src VARCHAR(255) not null,
	dst double not null,
	created DATETIME default NOW() not null,
	constraint DistanceCache_pk
		primary key (src, dst)
);

# --- !Downs

DROP TABLE DistanceCache;
