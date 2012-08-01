-module(unpacker).

% -compile([{parse_transform, lager_transform}]).

%%
%% Exported Functions
%%
-export([decode/1]).

%%
%% API Functions
%%

decode(<<TranSerialIDBytes:8/binary, RecipCntBytes:8/binary, RecipMsisdnsBytes/binary>>) ->
	try
		TranSerialID = binary:decode_unsigned(TranSerialIDBytes, little),
		RecipCnt = binary:decode_unsigned(RecipCntBytes, little),
		RecipsMsisdns = unpack_msisdns(RecipMsisdnsBytes, RecipCnt),
		% ?log_debug("Result: ~p", [ok]),
		{ok, RecipCnt, RecipsMsisdns, TranSerialID}
	catch
		Class:Error ->
			{error, {Class, Error}}
	end;
decode(_Bin) ->
	{error, invalid_message_format}.

%%
%% Local Functions
%%

unpack_msisdns(Bytes, Cnt) ->
	unpack_msisdns([], Bytes, Cnt).

unpack_msisdns(Accum, <<>>, 0) ->
	Accum;
unpack_msisdns(Accum, <<Bytes:8/binary, RecipsSoFar/binary>>, CntSoFar) ->
	unpack_msisdns([ binary:decode_unsigned(Bytes, little) | Accum ], RecipsSoFar, CntSoFar - 1).