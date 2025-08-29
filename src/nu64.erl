-module(nu64).
-export([main/0]).

err(Message) ->
	err(Message, []).
err(Message, Format) ->
	io:format(standard_error, string:concat("[ERROR] ", Message), Format).

info(Message, Format) ->
	io:format(standard_io, string:concat("[INFO] ", Message), Format).

get_divider() ->
	case init:get_argument(d) of
		{ok, [Divider|_]} ->
			Divider;
		_ ->
			"-"
	end.

get_start_index() ->
	case init:get_argument(idx) of
		{ok, [StringIndex|_]} ->
			case string:to_integer(StringIndex) of
				{IntIndex, _} when is_integer(IntIndex) ->
					IntIndex;
				{error, ParseErrorReason} ->
					err("Failed to parse index: ~p~n", [ParseErrorReason]),
					init:stop(1)
			end;
		_ ->
			0 % Defaults to Index 0
	end.

get_current_working_directory() ->
	case file:get_cwd() of
		{ok, CWD} ->
			CWD;
		{error, GetCWDReason} ->
			err("Failed to read current directory: ~p~n", [GetCWDReason]),
			init:stop(1)
	end.

get_target_directory() ->
	case init:get_argument(t) of
		{ok, [Dir|_]} ->
			Dir;
		_ ->
			get_current_working_directory()
	end.

main() ->
	Divider = get_divider(),
	Index = get_start_index(),
	PlainArgs = init:get_plain_arguments(),
	case length(PlainArgs) of
		0 ->
			err("No arguments provided, expected list of folder names to create~n"),
			init:stop(1);
		_ ->
			Directory = get_target_directory(),
			make_directories(Directory, Divider, Index, PlainArgs),
			init:stop(0)
	end.


str_concat([Base | Items]) when is_list(Items) ->
	str_concat(Base, Items).
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

make_directories(_, _, _, []) ->
	ok; % No more arguments are left
make_directories(TargetDirectory, Divider, Index, [DirName | Rest]) when is_integer(Index) ->
	PaddedNumber = space_to_zero("", [[X] || X <- io_lib:format("~3.B", [Index])]),
	FolderName = str_concat([PaddedNumber, Divider, DirName]),
	DirPath = filename:join([TargetDirectory, FolderName]),
	case filelib:ensure_dir(DirPath) of
		ok ->
			case file:make_dir(DirPath) of
				ok ->
					info("Created Folder: ~s~n", [FolderName]);
				{_, Reason} ->
					err("Failed to create folder ~s: ~p~n", [DirPath, Reason])
			end;
		{error, Reason} ->
			err("Failed to create ensure directory for ~s: ~p~n", [DirPath, Reason])
	end,
	make_directories(TargetDirectory, Divider, Index + 1, Rest).


