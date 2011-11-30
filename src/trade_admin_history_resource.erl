-module(trade_admin_history_resource).
-export([init/1, content_types_provided/2, to_json/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) -> {ok, undefined}.

content_types_provided(ReqData, State) ->
    {[{"application/json", to_json}], ReqData, State}.

to_json(ReqData, State) ->
    Symbol = wrq:get_qs_value("symbol", ReqData),
    Period = wrq:get_qs_value("period", ReqData),
    From   = wrq:get_qs_value("from", ReqData),
    To     = wrq:get_qs_value("to", ReqData),
    Mode   = wrq:get_qs_value("mode", ReqData),
    {get_history(Symbol, Period, Mode, From, To), ReqData, State}.

get_history(Symbol, Period, Mode, From, undefined) ->
    T1 = edate:string_to_date(From),
    Data = trade_history:get_history(Symbol, list_to_integer(Period), T1),
    jsonize(Mode, Data);

get_history(Symbol, Period, Mode, From, To) ->
    T1 = edate:string_to_date(From),
    T2 = edate:string_to_date(To),
    Data = trade_history:get_history(Symbol, list_to_integer(Period), T1, T2),
    jsonize(Mode, Data).

jsonize(Mode, History) when is_list(History) ->
    Format = make_format(Mode),
    "[\n" ++ string:join([jsonize(Mode, Format, Bar) || Bar <- History], ",\n") ++ "\n]".

jsonize(Mode, Format, Bar) when is_tuple(Bar) ->
    lists:flatten(io_lib:format(Format, make_args(Mode, Bar))).

make_format(Fmt) -> make_format(Fmt, []).
make_format([], Acc) -> "[" ++ string:join(lists:reverse(Acc), ", ") ++ "]";
make_format([$v|Tail], Acc) -> make_format(Tail, ["~B"|Acc]);
make_format([$o|Tail], Acc) -> make_format(Tail, ["~.2f"|Acc]);
make_format([$h|Tail], Acc) -> make_format(Tail, ["~.2f"|Acc]);
make_format([$l|Tail], Acc) -> make_format(Tail, ["~.2f"|Acc]);
make_format([$c|Tail], Acc) -> make_format(Tail, ["~.2f"|Acc]);
make_format([$t|Tail], Acc) -> make_format(Tail, ["~B000"|Acc]).

make_args(Fmt, Bar) -> make_args(Fmt, Bar, []).
make_args([], _, Acc) -> lists:reverse(Acc);
make_args([$t|Tail], Bar, Acc) -> make_args(Tail, Bar, [trade_utils:time(Bar)|Acc]);
make_args([$o|Tail], Bar, Acc) -> make_args(Tail, Bar, [trade_utils:open(Bar)|Acc]);
make_args([$h|Tail], Bar, Acc) -> make_args(Tail, Bar, [trade_utils:high(Bar)|Acc]);
make_args([$l|Tail], Bar, Acc) -> make_args(Tail, Bar, [trade_utils:low(Bar)|Acc]);
make_args([$c|Tail], Bar, Acc) -> make_args(Tail, Bar, [trade_utils:close(Bar)|Acc]);
make_args([$v|Tail], Bar, Acc) -> make_args(Tail, Bar, [trade_utils:volume(Bar)|Acc]).
