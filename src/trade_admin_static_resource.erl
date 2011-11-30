-module(trade_admin_static_resource).
-export([init/1, content_types_provided/2, resource_exists/2, provide_content/2]).

-record(context, {docroot, filepath}).

-include_lib("webmachine/include/webmachine.hrl").

init(Opts) ->
    {ok, App} = application:get_application(),
    PrivDir = code:priv_dir(App),
    PathDir = proplists:get_value(root, Opts, []),
    RootDir = filename:join([PrivDir, PathDir]),
    {ok, #context{docroot=RootDir}}.

content_types_provided(ReqProps, Context) ->
    case wrq:disp_path(ReqProps) of
        ""   -> {[{"text/html", provide_content}], ReqProps, Context};
        Path -> {[{webmachine_util:guess_mime(Path), provide_content}], ReqProps, Context}
    end.

resource_exists(ReqProps, Context=#context{docroot=DocRoot}) ->
    case mochiweb_util:safe_relative_path(wrq:disp_path(ReqProps)) of
        undefined -> {false, ReqProps, Context};
        ""        -> {true,  ReqProps, Context#context{filepath=filename:join([DocRoot, "index.html"])}};
        SafePath  ->
            FilePath = filename:join([DocRoot, SafePath]),
            case filelib:is_regular(FilePath) of
                true  -> {true,  ReqProps, Context#context{filepath=FilePath}};
                false -> {false, ReqProps, Context}
            end
    end.

provide_content(ReqProps, Context=#context{filepath=FilePath}) ->
%     lager:debug("Sending static file: ~p", [FilePath]),
    {ok, Value} = file:read_file(FilePath),
    {Value, ReqProps, Context}.
