
create function login_check(@login varchar(255), @pass varchar(255))
returns table
as
return
(
	select *
	from users
	where login=@login and PWDCOMPARE(@pass, password) = 1
);

go
