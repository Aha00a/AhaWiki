# GeocodeCache

# --- !Ups

DROP TABLE DistanceCache;

# --- !Downs

create table DistanceCache
(
	src VARCHAR(255) not null,
	dst VARCHAR(255) not null,
	meters int not null,
	seconds int not null,
	created DATETIME default NOW() not null,
	constraint DistanceCache_pk
		primary key (src, dst)
);
