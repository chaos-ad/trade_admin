-module(trade_admin_history_resource).
-export([init/1, content_types_provided/2, to_json/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) -> {ok, undefined}.

content_types_provided(ReqData, State) ->
    {[{"application/json", to_json}], ReqData, State}.

to_json(ReqData, State) ->
    Symbol = wrq:get_qs_value("symbol", ReqData),
    Period = wrq:get_qs_value("period", ReqData),
    From   = wrq:get_qs_value("from", ReqData),
    To     = wrq:get_qs_value("to", ReqData),
    {get_history(Symbol, Period, From, To), ReqData, State}.

get_history(Symbol, Period, From, undefined) ->
    T1 = edate:string_to_date(From),
    Data = trade_history:get_history(Symbol, list_to_integer(Period), T1),
    json:decode( lists:map(fun jsonize/1, Data) );

get_history(Symbol, Period, From, To) ->
    T1 = edate:string_to_date(From),
    T2 = edate:string_to_date(To),
    Data = trade_history:get_history(Symbol, list_to_integer(Period), T1, T2),
    json:decode( lists:map(fun jsonize/1, Data) ).

jsonize({T,O,H,L,C,V}) ->
    {[{time, T}, {open, O}, {high, H}, {low, L}, {close, C}, {volume, V}]}.
