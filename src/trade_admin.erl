%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc trade_admin startup code

-module(trade_admin).
-author('author <author@example.com>').
-export([start/0, start_link/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

%% @spec start_link() -> {ok,Pid::pid()}
%% @doc Starts the app for inclusion in a supervisor tree
start_link() ->
    ensure_started(inets),
    ensure_started(crypto),
    ensure_started(mochiweb),
    ensure_started(webmachine),
    trade_admin_sup:start_link().

%% @spec start() -> ok
%% @doc Start the trade_admin server.
start() ->
    ensure_started(inets),
    ensure_started(crypto),
    ensure_started(mochiweb),
    ensure_started(webmachine),
    application:start(trade_admin).

%% @spec stop() -> ok
%% @doc Stop the trade_admin server.
stop() ->
    Res = application:stop(trade_admin),
    application:stop(webmachine),
    application:stop(mochiweb),
    application:stop(crypto),
    application:stop(inets),
    Res.
