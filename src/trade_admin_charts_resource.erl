-module(trade_admin_charts_resource).
-export([init/1, content_types_provided/2, to_json/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) ->
    {ok, undefined}.

content_types_provided(ReqData, State) ->
    {[{"application/json", to_json}], ReqData, State}.

to_json(ReqData, State) ->
    case wrq:path_info(name, ReqData) of
        "all" ->
            {ok, JSON} = json:encode(all_charts()),
            {JSON, ReqData, State};
        Name ->
            Args = wrq:req_qs(ReqData),
            Security = trade_arg_utils:get_required(Args, "security"),
            Period = trade_arg_utils:get_required(Args, "period", integer),
            From = trade_arg_utils:get_required(Args, "from", date),
            To = trade_arg_utils:get_optional(Args, "to", date),

            lager:debug("Getting chart ~p for sequrity: ~p, period: ~p, from: ~p, to: ~p", [Name, Security, Period, From, To]),

            Module  = chart_module(Name),
            History = trade_history:get_history(Security, Period, From, To),
            Result  = Module:get_data(History, Args),

            {ok, JSON} = json:encode(Result),
            {JSON, ReqData, State}
    end.

all_charts() ->
    Files = filelib:wildcard("ebin/trade_chart_*.beam"),
    Names = lists:map(fun chart_name/1, Files),
    Names.

chart_module(Name) ->
    list_to_atom("trade_chart_" ++ Name).

chart_name(File) ->
    {match,[Name]} = re:run(File, ".*trade_chart_(.*).beam", [{capture, all_but_first, binary}]), Name.
