# GeocodeCache

# --- !Ups

create table SchemaOrg
(
	page varchar(255) not null,
	cls varchar(50) not null,
	prop varchar(50) not null,
	value varchar(255) not null,
	primary key (page, cls, prop, value),
	constraint SchemaOrg_Page_name_fk
		foreign key (page) references Page (name)
			on update cascade on delete cascade
);


# --- !Downs

drop table SchemaOrg;

