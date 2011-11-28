%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Callbacks for the trade_admin application.

-module(trade_admin_app).
-author('author <author@example.com>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for trade_admin.
start(_Type, _StartArgs) ->
    trade_admin_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for trade_admin.
stop(_State) ->
    ok.
