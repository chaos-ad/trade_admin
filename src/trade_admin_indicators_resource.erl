-module(trade_admin_indicators_resource).
-export([init/1, content_types_provided/2, to_json/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) ->
    {ok, undefined}.

content_types_provided(ReqData, State) ->
    {[{"application/json", to_json}], ReqData, State}.

to_json(ReqData, State) ->
    case wrq:path_info(name, ReqData) of
        "all" ->
            Indicators = all_indicators(),
            {ok, JSON} = json:encode(Indicators),
            {JSON, ReqData, State};
        Name ->
            Args = wrq:req_qs(ReqData),
            Security = trade_arg_utils:get_required(Args, "security"),
            Period = trade_arg_utils:get_required(Args, "period", integer),
            From = trade_arg_utils:get_required(Args, "from", date),
            To = trade_arg_utils:get_optional(Args, "to", date),

            lager:debug("Getting indicator ~p for sequrity: ~p, period: ~p, from: ~p, to: ~p", [Name, Security, Period, From, To]),

            Module  = indicator_module(Name),
            History = trade_history:get_history(Security, Period, From, To),
            Data    = Module:get_data(History, []),

            {ok, JSON} = json:encode(Data),
            {JSON, ReqData, State}
    end.

all_indicators() ->
    Files = filelib:wildcard("ebin/trade_indicator_*.beam"),
    Names = lists:map(fun indicator_name/1, Files),
    Names.

indicator_module(Name) ->
    list_to_atom("trade_indicator_" ++ Name).

indicator_name(File) ->
    {match,[Name]} = re:run(File, ".*trade_indicator_(.*).beam", [{capture, all_but_first, binary}]), Name.
