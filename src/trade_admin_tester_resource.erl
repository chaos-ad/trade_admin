-module(trade_admin_tester_resource).
-export([init/1, content_types_provided/2, to_json/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) ->
    {ok, undefined}.

content_types_provided(ReqData, State) ->
    {[{"application/json", to_json}], ReqData, State}.

to_json(ReqData, State) ->
    Strategy    = list_to_atom( wrq:get_qs_value("strategy",  ReqData) ),
    Symbol      = wrq:get_qs_value("symbol", ReqData),
    Period      = list_to_integer( wrq:get_qs_value("period", ReqData) ),
    From        = str_to_date( wrq:get_qs_value("from", ReqData) ),
    To          = str_to_date( wrq:get_qs_value("to", ReqData) ),
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

str_to_date(undefined) -> undefined;
str_to_date(Date) -> edate:string_to_date(Date).

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