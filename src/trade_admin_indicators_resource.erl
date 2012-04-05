-module(trade_admin_indicators_resource).
-export([init/1, content_types_provided/2, to_json/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) ->
    {ok, undefined}.

content_types_provided(ReqData, State) ->
    {[{"application/json", to_json}], ReqData, State}.

to_json(ReqData, State) ->
    try
        {[Name, Symbol, Period, From, To], OtherArgs} = trade_admin_utils:get_args([
            {"name", required},
            {"symbol", required},
            {"period", integer, required},
            {"from", date, required},
            {"to", date, optional}],
            wrq:req_qs(ReqData)
        ),

        Module  = list_to_atom("trade_indicator_" ++ Name),
        History = trade_history:get_history(Symbol, Period, From, To),
        Data    = Module:get_data(History, OtherArgs),

        {ok, JSON} = json:encode(Data),
        {JSON, ReqData, State}

    catch
        error:undef   -> {{halt, 400}, ReqData, State};
        error:no_args -> {{halt, 400}, ReqData, State}
    end.

