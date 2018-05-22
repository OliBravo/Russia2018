USE [Russia2018]
GO

/****** Object:  UserDefinedFunction [dbo].[matches_to_bet]    Script Date: 2018-05-17 11:06:51 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO


create function [dbo].[matches_to_bet](@login varchar(50))
returns table
as return
	select *
	from matches_logo2 m
	where m.match_id not in (
		select match_id
		from Bets b
		where b.user_id = (
			select id
			from users u
			where u.login = @login)
	)
;
GO

