-module(trade_admin_utils).
-compile(export_all).

get_args(Specs, ReqQS) when is_list(ReqQS) ->
    Fun = fun(Spec, {Acc,Args}) -> {Val, Rest} = get_arg(Spec, Args), {[Val|Acc], Rest} end,
    {Result, Rest} = lists:foldl(Fun, {[],ReqQS}, Specs),
    {lists:reverse(Result), Rest}.

get_arg({Name, Requiredness}, Args) ->
    case lists:keytake(Name, 1, Args) of
        {value, {Name, Val}, Rest} -> {Val, Rest};
        false -> {no_value(Requiredness), Args}
    end;

get_arg({Name, Type, Requiredness}, Args) ->
    {Val, Rest} = get_arg({Name, Requiredness}, Args),
    {convert(Type, Val), Rest}.

no_value(required) -> exit(no_args);
no_value(optional) -> undefined;
no_value({default, Default}) -> Default.

convert(_, undefined) -> undefined;
convert(atom, Val) when is_list(Val) -> list_to_atom(Val);
convert(integer, Val) when is_list(Val) -> list_to_integer(Val);
convert(date, Val) when is_list(Val) -> edate:string_to_date(Val).
