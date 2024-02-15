-module(nus64).
-export([main/0]).

main() ->
	io:format("Hello world!~n", []),
	Args = init:get_plain_arguments(),
	case length(Args) of
		0 ->
			io:format("ERROR: No arguments provided, expected list of folder names to create");
		_ ->
			[FirstElement | _] = Args,
%			io:format("[INFO] Wanted folder ~s~n", [filename:join(["./", FirstElement])]),
			{ok, CWD} = file:get_cwd(),
			io:format("[INFO] CWD: ~s~n", [CWD]),
			make_directories(CWD, Args)
	end,
	init:stop().


make_directories(_, []) ->
	ok; % No more arguments are left
make_directories(CWD, [DirName | Rest]) ->
	DirPath = filename:join([CWD, DirName]),
	case filelib:ensure_dir(DirPath) of
		ok ->
			case file:make_dir(DirPath) of
				ok ->
					io:format("[INFO] Created Folder: ~s~n", [DirName]);
				{_, Reason} ->
					io:format("[ERROR] Failed to create folder ~s: ~p~n", [DirPath, Reason])
			end;
		{error, Reason} ->
			io:format("[ERROR] Failed to create ensure directory for ~s: ~p~n", [DirName, Reason])
	end,
	make_directories(CWD, Rest).


