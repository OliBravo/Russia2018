
--drop table users;

create table users (
	id bigint identity(1,1) not null,
	login varchar(50) not null,
	password varbinary(128) not null,
	primary key(id)
);



insert into users (login, password)
values ('test', pwdencrypt('test123'));

insert into users (login, password)
values ('kuba', pwdencrypt('kuba123'));

insert into users (login, password)
values ('bartek', pwdencrypt('bartek123'));

insert into users (login, password)
values ('robert', pwdencrypt('robert123'));



create table Bets(
	user_id bigint not null,
	match_id bigint not null,
	team1 int not null,
	team2 int not null
);


alter table Bets
add constraint unq_bet UNIQUE (match_id, user_id);

alter table Bets
add winner_after_penalties integer;

/*
insert into Bets
values (1, 1, 0, 2);
insert into Bets
values (1, 2, 1, 0);

insert into Bets
values (2, 1, 2, 2);

insert into Bets
values (3, 1, 1, 0);

insert into Bets
values (4, 1, 3, 1);
*/


create table Results (
	match_id bigint not null,
	team1_regular int,
	team2_regular int,
	winner_ater_penalties integer,
	primary key (match_id)
);