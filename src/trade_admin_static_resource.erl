-module(trade_admin_static_resource).
-export([init/1,
         content_types_provided/2,
         resource_exists/2,
         content/2]).

-record(context, {root, filepath}).

-include_lib("webmachine/include/webmachine.hrl").

init(Opts) ->
    {ok, App} = application:get_application(),
    PrivDir = code:priv_dir(App),
    RootDir = PrivDir ++ "/" ++ proplists:get_value(root, Opts, []),
    {ok, #context{root=RootDir}}.

content_types_provided(RD, Context) ->
    Path = wrq:disp_path(RD),
    Mime = webmachine_util:guess_mime(Path),
    {[{Mime, content}], RD, Context}.

resource_exists(RD, Context=#context{root=Root}) ->
    FP = filename:join([Root|clean_path(wrq:disp_path(RD))]),
    lager:debug("Static file request: ~p~n", [FP]),
    case filelib:is_regular(FP) of
        true ->
            {true, RD, Context#context{filepath=FP}};
        _ ->
            {false, RD, Context}
    end.

content(RD, Context=#context{filepath=FP}) ->
    {ok, Data} = file:read_file(FP),
    {Data, RD, Context}.

clean_path(Path) ->
    Fun = fun("..", [])       -> [];     % no shallower
             ("..", [_|Rest]) -> Rest;   % one shallower
             (P,    Acc)      -> [P|Acc] % one deeper
          end,
    lists:reverse(lists:foldl(Fun, [], string:tokens(Path, "/"))).
