-module(trade_admin_securities_resource).

-compile(export_all).

-include_lib("webmachine/include/webmachine.hrl").

-record(state, {securities, etag}).

init([]) ->
    {ok, #state{}}.

content_types_provided(ReqData, State) ->
    {[{"application/json", to_json}], ReqData, State}.

encodings_provided(ReqData, State) ->
    {[{"identity", fun(X) -> X end}, {"gzip", fun zlib:gzip/1}], ReqData, State}.

generate_etag(ReqData, State=#state{etag=undefined, securities=undefined}) ->
    Securities = get_securities(wrq:path_info(market, ReqData)),
    Hash = erlang:phash2(Securities),
    ETag = mochihex:to_hex(Hash),
    {ETag, ReqData, State#state{etag=ETag, securities=Securities}};

generate_etag(ReqData, State=#state{etag=ETag}) ->
    {ETag, ReqData, State}.

to_json(ReqData, State=#state{securities=Securities}) when is_list(Securities) ->
    {ok, JSON} = json:encode(lists:map(fun prepare/1, Securities)),
    {JSON, ReqData, State}.

get_securities("all") -> trade_db:get_all_symbols();
get_securities(undefined) -> trade_db:get_all_symbols();
get_securities(Market) when is_list(Market) -> trade_db:get_all_symbols(list_to_integer(Market)).

prepare({symbol, ID, Code, Name, Market}) ->
    {[{id, ID}, {code, Code}, {name, Name}, {market, Market}]}.
