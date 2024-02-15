-module(nus64).
-export([main/0]).

main() ->
	io:format("Hello world!~n", []),
	Args = init:get_plain_arguments(),
	case length(Args) of
		0 ->
			io:format("ERROR: No arguments provided, expected list of folder names to create~n");
		_ ->
			case file:get_cwd() of
				{ok, CWD} ->
					make_directories(CWD, 1, Args);
				{error, Reason} ->
					io:format("[ERROR] Failed to read current directory: ~p~n", [Reason])
			end
	end,
	init:stop().


str_concat(Base, []) ->
	Base;
str_concat(Base, [First | Rest]) ->
	str_concat(string:concat(Base, First), Rest).

space_to_zero(Base, []) ->
	string:concat(Base, "");
space_to_zero(Base, [First | Rest]) ->
	case First of
		" " ->
			space_to_zero(string:concat(Base, "0"), Rest);
		_ ->
			space_to_zero(string:concat(Base, First), Rest)
	end.

make_directories(_, _, []) ->
	ok; % No more arguments are left
make_directories(CWD, Count, [DirName | Rest]) when is_integer(Count) ->
	PaddedNumber = space_to_zero("", [[X] || X <- io_lib:format("~3.B", [Count])]),
	io:format("Number: ~s~n", [PaddedNumber]),
	FolderName = str_concat(PaddedNumber, ["-", DirName]),
	DirPath = filename:join([CWD, FolderName]),
	case filelib:ensure_dir(DirPath) of
		ok ->
			case file:make_dir(DirPath) of
				ok ->
					io:format("[INFO] Created Folder: ~s~n", [FolderName]);
				{_, Reason} ->
					io:format("[ERROR] Failed to create folder ~s: ~p~n", [DirPath, Reason])
			end;
		{error, Reason} ->
			io:format("[ERROR] Failed to create ensure directory for ~s: ~p~n", [FolderName, Reason])
	end,
	make_directories(CWD, Count + 1, Rest).


