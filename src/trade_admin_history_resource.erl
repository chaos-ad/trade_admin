-module(trade_admin_history_resource).
-export([init/1, content_types_provided/2, to_json/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) -> {ok, undefined}.

content_types_provided(ReqData, State) ->
    {[{"application/json", to_json}], ReqData, State}.

to_json(ReqData, State) ->
    Args = wrq:req_qs(ReqData),
    Security = trade_arg_utils:get_required(Args, "security"),
    Period = trade_arg_utils:get_required(Args, "period", integer),
    From = trade_arg_utils:get_required(Args, "from", date),
    To = trade_arg_utils:get_optional(Args, "to", date),

    lager:debug("Getting history for sequrity: ~p, period: ~p, from: ~p, to: ~p", [Security, Period, From, To]),
    Data = trade_history:get_history(Security, Period, From, To),
    {ok, JSON} = json:encode( lists:map(fun jsonize/1, Data) ),
    {JSON, ReqData, State}.

jsonize({T,O,H,L,C,V}) ->
    {[{time, T}, {open, O}, {high, H}, {low, L}, {close, C}, {volume, V}]}.
