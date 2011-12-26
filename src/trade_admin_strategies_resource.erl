-module(trade_admin_strategies_resource).
-export([init/1, to_html/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) -> {ok, undefined}.

to_html(ReqData, Context) ->
    case dict:find(name, wrq:path_info(ReqData)) of
        {ok, Name} ->
            to_html(list_to_existing_atom(Name), ReqData, Context);
        error ->
            List = lists:map(fun status/1, trade_strategy_mgr:strategies()),
            {ok, Content} = strategies_dtl:render([{strategies, List}]),
            {Content, ReqData, Context}
    end.

status({Name, Pid}) -> {Name, trade_strategy:get_status(Pid)}.

to_html(Name, ReqData, Context) ->
    Pid = trade_strategy_mgr:strategy(Name),
    Stats = trade_strategy:get_stats(Pid),
    {ok, Content} = strategy_dtl:render([{name, Name}, {stats, Stats}]),
    {Content, ReqData, Context}.
