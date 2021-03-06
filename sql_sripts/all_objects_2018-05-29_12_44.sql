USE [master]
GO
/****** Object:  Database [Russia2018]    Script Date: 2018-05-29 12:45:21 ******/
CREATE DATABASE [Russia2018] ON  PRIMARY 
( NAME = N'Russia2018', FILENAME = N'H:\SQL\Russia2018.mdf' , SIZE = 4096KB , MAXSIZE = UNLIMITED, FILEGROWTH = 1024KB )
 LOG ON 
( NAME = N'Russia2018_log', FILENAME = N'H:\SQL\Russia2018.ldf' , SIZE = 1024KB , MAXSIZE = 2048GB , FILEGROWTH = 10%)
GO
ALTER DATABASE [Russia2018] SET COMPATIBILITY_LEVEL = 100
GO
IF (1 = FULLTEXTSERVICEPROPERTY('IsFullTextInstalled'))
begin
EXEC [Russia2018].[dbo].[sp_fulltext_database] @action = 'enable'
end
GO
ALTER DATABASE [Russia2018] SET ANSI_NULL_DEFAULT OFF 
GO
ALTER DATABASE [Russia2018] SET ANSI_NULLS OFF 
GO
ALTER DATABASE [Russia2018] SET ANSI_PADDING OFF 
GO
ALTER DATABASE [Russia2018] SET ANSI_WARNINGS OFF 
GO
ALTER DATABASE [Russia2018] SET ARITHABORT OFF 
GO
ALTER DATABASE [Russia2018] SET AUTO_CLOSE OFF 
GO
ALTER DATABASE [Russia2018] SET AUTO_SHRINK OFF 
GO
ALTER DATABASE [Russia2018] SET AUTO_UPDATE_STATISTICS ON 
GO
ALTER DATABASE [Russia2018] SET CURSOR_CLOSE_ON_COMMIT OFF 
GO
ALTER DATABASE [Russia2018] SET CURSOR_DEFAULT  GLOBAL 
GO
ALTER DATABASE [Russia2018] SET CONCAT_NULL_YIELDS_NULL OFF 
GO
ALTER DATABASE [Russia2018] SET NUMERIC_ROUNDABORT OFF 
GO
ALTER DATABASE [Russia2018] SET QUOTED_IDENTIFIER OFF 
GO
ALTER DATABASE [Russia2018] SET RECURSIVE_TRIGGERS OFF 
GO
ALTER DATABASE [Russia2018] SET  DISABLE_BROKER 
GO
ALTER DATABASE [Russia2018] SET AUTO_UPDATE_STATISTICS_ASYNC OFF 
GO
ALTER DATABASE [Russia2018] SET DATE_CORRELATION_OPTIMIZATION OFF 
GO
ALTER DATABASE [Russia2018] SET TRUSTWORTHY OFF 
GO
ALTER DATABASE [Russia2018] SET ALLOW_SNAPSHOT_ISOLATION OFF 
GO
ALTER DATABASE [Russia2018] SET PARAMETERIZATION SIMPLE 
GO
ALTER DATABASE [Russia2018] SET READ_COMMITTED_SNAPSHOT OFF 
GO
ALTER DATABASE [Russia2018] SET HONOR_BROKER_PRIORITY OFF 
GO
ALTER DATABASE [Russia2018] SET RECOVERY SIMPLE 
GO
ALTER DATABASE [Russia2018] SET  MULTI_USER 
GO
ALTER DATABASE [Russia2018] SET PAGE_VERIFY CHECKSUM  
GO
ALTER DATABASE [Russia2018] SET DB_CHAINING OFF 
GO
EXEC sys.sp_db_vardecimal_storage_format N'Russia2018', N'ON'
GO
USE [Russia2018]
GO
/****** Object:  User [Russia2018]    Script Date: 2018-05-29 12:45:21 ******/
CREATE USER [Russia2018] FOR LOGIN [Russia2018] WITH DEFAULT_SCHEMA=[dbo]
GO
/****** Object:  User [OFFICE\jakub.malecki]    Script Date: 2018-05-29 12:45:21 ******/
CREATE USER [OFFICE\jakub.malecki] FOR LOGIN [OFFICE\jakub.malecki] WITH DEFAULT_SCHEMA=[dbo]
GO
ALTER ROLE [db_owner] ADD MEMBER [Russia2018]
GO
ALTER ROLE [db_owner] ADD MEMBER [OFFICE\jakub.malecki]
GO
/****** Object:  Table [dbo].[Bets]    Script Date: 2018-05-29 12:45:21 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[Bets](
	[user_id] [bigint] NOT NULL,
	[match_id] [bigint] NOT NULL,
	[team1] [int] NOT NULL,
	[team2] [int] NOT NULL,
	[winner_after_penalties] [int] NULL,
 CONSTRAINT [unq_bet] UNIQUE NONCLUSTERED 
(
	[match_id] ASC,
	[user_id] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY]

GO
/****** Object:  Table [dbo].[matches_logo2]    Script Date: 2018-05-29 12:45:22 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [dbo].[matches_logo2](
	[match_id] [bigint] NULL,
	[team1] [varchar](12) NULL,
	[team2] [varchar](12) NULL,
	[logo1] [varchar](111) NULL,
	[logo2] [varchar](111) NULL,
	[group] [varchar](13) NULL,
	[date] [date] NULL,
	[time] [time](7) NULL,
	[venue] [varchar](24) NULL,
	[group_phase] [int] NULL
) ON [PRIMARY]

GO
SET ANSI_PADDING OFF
GO
/****** Object:  Table [dbo].[Results]    Script Date: 2018-05-29 12:45:22 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[Results](
	[match_id] [bigint] NOT NULL,
	[team1_regular] [int] NULL,
	[team2_regular] [int] NULL,
	[winner_after_penalties] [int] NULL,
PRIMARY KEY CLUSTERED 
(
	[match_id] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY]

GO
/****** Object:  Table [dbo].[users]    Script Date: 2018-05-29 12:45:22 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [dbo].[users](
	[id] [bigint] IDENTITY(1,1) NOT NULL,
	[login] [varchar](50) NOT NULL,
	[password] [varbinary](128) NOT NULL,
	[role] [varchar](10) NULL,
PRIMARY KEY CLUSTERED 
(
	[id] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY]

GO
SET ANSI_PADDING OFF
GO
/****** Object:  UserDefinedFunction [dbo].[enter_results]    Script Date: 2018-05-29 12:45:22 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE function [dbo].[enter_results] (@mytime varchar(50))
returns table
as
return
	with ms as (
	select m.*,
		cast([date] as datetime) + cast([time] as datetime) as time2,
		r.team1_regular,
		r.team2_regular,
		r.winner_after_penalties
	from matches_logo2 m
	left join results r
	on m.match_id = r.match_id
	)
	select *
	from ms
	where time2 <= @mytime;


GO
/****** Object:  UserDefinedFunction [dbo].[login_check]    Script Date: 2018-05-29 12:45:22 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

create function [dbo].[login_check](@login varchar(255), @pass varchar(255))
returns table
as
return
(
	select *
	from users
	where login=@login and PWDCOMPARE(@pass, password) = 1
);


GO
/****** Object:  UserDefinedFunction [dbo].[matches_to_bet]    Script Date: 2018-05-29 12:45:22 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO


CREATE function [dbo].[matches_to_bet](@mytime varchar(50), @login varchar(50))
returns table
as return
	with ms as (
	select *,
		cast([date] as datetime) + cast([time] as datetime) as time2,
		0 as bet1,
		0 as bet2,
		NULL as winner_after_penalties
	from matches_logo2 m
	where match_id not in (
		select match_id
		from Bets
		where user_id = (
			select id
			from users
			where login = @login
			)
		)
	)
	select *
	from ms
	where time2 >= @mytime
;



GO
/****** Object:  UserDefinedFunction [dbo].[matches_to_bet_already_bet]    Script Date: 2018-05-29 12:45:22 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE function [dbo].[matches_to_bet_already_bet](@mytime varchar(50), @login varchar(50))
/*
list of matches a user can still bet, but she has already bet them
*/
returns table
as return
	with ms as (
	select m.*,
		cast([date] as datetime) + cast([time] as datetime) as time2,
		b.team1 as 'bet1',
		b.team2 as 'bet2',
		b.winner_after_penalties
	from matches_logo2 m, Bets b
	where m.match_id in (
		select match_id
		from Bets
		where user_id = (
			select id
			from users
			where login = @login
			)
		) and m.match_id = b.match_id and b.user_id = (
			select id
			from users
			where login = @login
			)
	)
	select *
	from ms
	where time2 >= @mytime
;
GO
/****** Object:  View [dbo].[vw_Bets]    Script Date: 2018-05-29 12:45:22 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO



CREATE view [dbo].[vw_Bets] as
select 
	u.login,
	m.match_id,
	m.team1,
	b.team1 as bet1,
	b.team2 as bet2,
	m.team2,
	b.winner_after_penalties,
	m.date,
	m.time,
	m.[group],
	m.group_phase
from [matches_logo2] m, bets b, users u
where m.match_id = b.match_id and b.user_id = u.id;



GO
/****** Object:  StoredProcedure [dbo].[usp_Bets_insert]    Script Date: 2018-05-29 12:45:22 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
create procedure [dbo].[usp_Bets_insert]
	@login varchar(50), @match_id bigint, @team1 int, @team2 int, @penalties_winner int
as
begin
	declare @user_id bigint;

	select @user_id = id
	from users
	where login = @login;

	insert into Bets
	values (@user_id, @match_id, @team1, @team2, @penalties_winner)
end


GO
/****** Object:  StoredProcedure [dbo].[usp_Bets_update]    Script Date: 2018-05-29 12:45:22 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

create procedure [dbo].[usp_Bets_update]
	@bet1 int,
	@bet2 int,
	@pen_win int,
	@MID int,
	@USR varchar(50)
as 
update Bets
set team1 = @bet1,
	team2 = @bet2,
	winner_after_penalties = @pen_win
where match_id = @MID and user_id = (
	select id
	from users
	where login = @USR
	)
;


GO
/****** Object:  StoredProcedure [dbo].[usp_Results_insert_update]    Script Date: 2018-05-29 12:45:22 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE procedure [dbo].[usp_Results_insert_update]
	@match_id bigint,
	@team1_regular int,
	@team2_regular int,
	@winner_after_penalties int
as
begin
	declare @present bigint;

	select @present = match_id
	from Results
	where match_id = @match_id;

	if not exists (select * from Results where match_id = @match_id)
		begin
			insert into Results
			values (@match_id, @team1_regular, @team2_regular, @winner_after_penalties)
		end;
	else
		begin
			update Results
			set team1_regular = @team1_regular,
			team2_regular = @team2_regular,
			winner_after_penalties = @winner_after_penalties
			where match_id = @match_id
		end;
	
end;



GO
USE [master]
GO
ALTER DATABASE [Russia2018] SET  READ_WRITE 
GO
