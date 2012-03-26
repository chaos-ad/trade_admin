-module(trade_admin_tester_resource).
-export([init/1, content_types_provided/2, to_json/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) ->
    {ok, undefined}.

content_types_provided(ReqData, State) ->
    {[{"application/json", to_json}], ReqData, State}.

to_json(ReqData, State) ->
    Strategy    = get_value("strategy", ReqData, atom),
    Symbol      = get_value("symbol", ReqData),
    Period      = get_value("period", ReqData, integer),
    From        = get_value("from", ReqData, date),
    To          = get_value("to", ReqData, date),
    Stats       = run_test(Strategy, Symbol, Period, From, To),
    {jsonize(Stats), ReqData, State}.

run_test(Strategy, undefined, undefined, undefined, undefined) ->
    lager:debug("starting test from web interface: strategy: ~p", [Strategy]),
    Strategy:run();

run_test(Strategy, Symbol, Period, From, To) ->
    lager:debug("starting test from web interface: strategy: ~p, symbol = ~p, period = ~p, from = ~p, to = ~p", [Strategy, Symbol, Period, From, To]),
    trade_tester:test
    (
        [
            {to,     To},
            {from,   From},
            {symbol, Symbol},
            {period, Period},
            {money, 100000},
            {silent, true}
        ],
        Strategy,
        [
            {rank_range, {1, year}},
            {min_period, 10},
            {max_period, 500},
            {security, Symbol},
            {period,   Period}
        ]
    ).

jsonize({buy, {Time,Lots,  Price}}) ->
    {[{type, buy},  {lots, Lots}, {time, Time}, {price, Price}]};

jsonize({sell,{Time,Lots,_,Price}}) ->
    {[{type, sell}, {lots, Lots}, {time, Time}, {price, Price}]};

jsonize({T,O,H,L,C,V}) ->
    {[{time, T}, {open, O}, {high, H}, {low, L}, {close, C}, {volume, V}]};

jsonize(Stats) ->
    Bids = trade_stats:get_bids(Stats),
    Hist = trade_stats:get_history(Stats),
    Data = {[{bids, lists:map(fun jsonize/1, Bids)}, {history, lists:map(fun jsonize/1, Hist)}]},
    {ok, JSON} = json:encode(Data), JSON.


get_value(Name, Request) ->
    wrq:get_qs_value(Name, Request).

get_value(Name, Request, integer) ->
    case get_value(Name, Request) of
        Val when is_list(Val) -> list_to_integer(Val);
        _                     -> undefined
    end;

get_value(Name, Request, atom) ->
    case get_value(Name, Request) of
        Val when is_list(Val) -> list_to_atom(Val);
        _                     -> undefined
    end;

get_value(Name, Request, date) ->
    case get_value(Name, Request) of
        Val when is_list(Val) -> edate:string_to_date(Val);
        _                     -> undefined
    end.
