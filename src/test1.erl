-module(test1).
-export([foo/0]).



foo() ->
	{ok, Device} = file:open("/home/svart_ravn/work/projects/trash/parse_mongo_logs/data/test", [read]),

	get_data_from_file(Device, "", 0),

	file:close(Device).




get_data_from_file(Device, Accum, Position) ->
   case file:read(Device, 200) of
      eof -> file:close(Device), Accum;
      {_, Data} -> 
         AllData = Accum ++ Data,
         DotPosition = string:chr(Data, $.),
         if (DotPosition > 0) ->
            
            RawMessage = string:substr(AllData, 1, DotPosition - 1),

            % DotPos = string:chr(AllData, $.),
            % RawMessage = string:substr(AllData, 1, DotPos - 1),
            % StartPos = string:chr(RawMessage, $<),
            % EndPos = string:chr(RawMessage, $>),

            % Message = replaceUselessChars(string:substr(RawMessage, StartPos, EndPos - StartPos + 2)),
            % Message = RawMessage,

            % {ok, T, _} = erl_scan:string(Message ++ "."),
            % {ok, Binary} = erl_parse:parse_term(T),     
            % Term = binary_to_term(Binary),
            % TaskType = element(1, Term),
            
            if (true) ->
               % insert_message(MongoOpts, Connection, ?UNFIRED, Term);
               io:format("~p~n~n~n", [RawMessage]);
            true ->
               io:format("failed to unpack", [])
               % insert_message(MongoOpts, Connection, ?FIRED, Binary)
            end,
            
            RestOfData = string:substr(AllData, DotPosition + 1, string:len(AllData) - DotPosition);
         true ->
            RestOfData = AllData
         end,
                     
         get_data_from_file(Device, RestOfData, Position + 200)
   end.	