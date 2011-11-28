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
    jsonize( trade_history:get_history(Symbol, list_to_integer(Period), T1) );

get_history(Symbol, Period, From, To) ->
    T1 = edate:string_to_date(From),
    T2 = edate:string_to_date(To),
    jsonize( trade_history:get_history(Symbol, list_to_integer(Period), T1, T2) ).

jsonize(History) ->
    "?(\n[" ++
    string:join
    (
        lists:map
        (
            fun(Bar) ->
                lists:flatten(io_lib:format("[~B000, ~p, ~p, ~p, ~p, ~p]", tuple_to_list(Bar)))
            end,
            History
        ),
        ",\n"
    )
    ++ "]);".
%     "[]". %% TODO: Fixme
