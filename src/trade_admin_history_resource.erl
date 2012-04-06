-module(trade_admin_history_resource).
-export([init/1, content_types_provided/2, to_json/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) -> {ok, undefined}.

content_types_provided(ReqData, State) ->
    {[{"application/json", to_json}], ReqData, State}.

to_json(ReqData, State) ->

    {[Seq, Period, From, To], _} = trade_admin_utils:get_args([
        {"seq", required},
        {"period", integer, required},
        {"from", date, required},
        {"to", date, optional}],
        wrq:req_qs(ReqData)
    ),

    lager:debug("Getting history for sequrity: ~p, period: ~p, from: ~p, to: ~p", [Seq, Period, From, To]),
    Data = trade_history:get_history(Seq, Period, From, To),
    {ok, JSON} = json:encode( lists:map(fun jsonize/1, Data) ),
    {JSON, ReqData, State}.

jsonize({T,O,H,L,C,V}) ->
    {[{time, T}, {open, O}, {high, H}, {low, L}, {close, C}, {volume, V}]}.
