-module(trade_admin_history_resource).
-export([init/1, to_html/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) -> {ok, undefined}.

to_html(ReqData, State) ->
    Symbol = wrq:get_qs_value("symbol", ReqData),
    Period = wrq:get_qs_value("period", ReqData),
    From   = wrq:get_qs_value("from", ReqData),
    To     = wrq:get_qs_value("to", ReqData),
    History = get_history(Symbol, Period, From, To),
    {to_json(History), ReqData, State}.

get_history(Symbol, Period, From, To) ->
    case To =:= undefined of
        true  -> trade_history:get_history(Symbol, list_to_integer(Period), From);
        false -> trade_history:get_history(Symbol, list_to_integer(Period), From, To)
    end.

to_json(_) ->
    "[]". %% TODO: Fixme
