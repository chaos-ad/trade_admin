-module(trade_admin_securities_resource).
-export([init/1, content_types_provided/2, to_json/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) ->
    {ok, undefined}.

content_types_provided(ReqData, State) ->
    {[{"application/json", to_json}], ReqData, State}.

to_json(ReqData, State) ->
    Data = lists:map(fun prepare/1, trade_db:get_all_symbols()),
    {ok, JSON} = json:encode(Data),
    {JSON, ReqData, State}.

prepare({symbol, ID, Code, Name, Market}) ->
    {[{"id", ID}, {code, Code}, {name, Name}, {market, Market}]}.

