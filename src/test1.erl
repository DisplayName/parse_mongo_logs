-module(test1).
-export([foo/0]).



foo() ->
	% {ok, Device} = file:open("/home/svart_ravn/work/projects/trash/parse_mongo_logs/data/test", [read]),
   {ok, Device} = file:open("/home/svart_ravn/work/projects/trash/parse_mongo_logs/src/test", [read]),

   Res = read_file_line_by_line(Device, ""),

	file:close(Device),

   Res.


read_file_line_by_line(Device, Accum) ->
   case io:get_line(Device, "") of
      eof -> Accum;
      Line -> 
         {Bin, UnhandledData} = try_to_get_binary(Accum ++ Line, string:chr(Line, $.)),
         insert_message(Bin),
         read_file_line_by_line(Device, UnhandledData)
   end.

remove_chars(Text) ->
   lists:filter(fun(C) -> not lists:member(C, "$ $\n") end, Text).


try_to_get_binary(Text, 0) -> {[], Text};
try_to_get_binary(Text, _Position) ->
   Position = string:chr(Text, $.),
   Result = remove_chars(string:substr(Text, Position + 1, length(Text) - Position)),
   StringToParse = remove_chars(string:substr(Text, 1, Position)),

   {ok, Tokens, _EndLine} = erl_scan:string(StringToParse),
   {ok, Terms} = erl_parse:parse_term(Tokens),
   {Binary, _Trash} = Terms,
   % ActualTerm = ,

   % insert_message(ActualTerm),

   {binary_to_term(Binary), Result}.

insert_message([]) -> ok;
insert_message({task, _RecipCnt, Msisdns, TranSid, _Tag, _Error}) -> io:format("-", []);
insert_message(D = {Message, UnpackedMessage}) -> io:format("~p~n+", [D]).


message(Text) ->
   io:format("~p~n", [Text]).