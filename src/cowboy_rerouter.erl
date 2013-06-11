-module(cowboy_rerouter).
-behaviour(cowboy_middleware).

-export([compile/1]).
-export([execute/2]).

-type bindings() :: [{atom(), binary()}].
-type tokens() :: [binary()].
-export_type([bindings/0]).
-export_type([tokens/0]).

-type route_match() :: iodata().
-type route() :: {Path::route_match(), Handler::module(), Opts::any()}.
-type routes() :: [route()].
-export_type([routes/0]).

-type dispatch_match() :: {Re::re:mp(), Groups::[atom()]}.
-type dispatch_rule() :: {Path::dispatch_match(), Handler::module(), Opts::any()}.
-opaque dispatch_rules() :: [dispatch_rule()].
-export_type([dispatch_rules/0]).


-spec compile(routes()) -> dispatch_rules().
compile(Routes) ->
    [compile_route(R) || R <- Routes].


-spec compile_route(route()) -> dispatch_rule().
compile_route({Path, Handler, Opts}) ->
    {ok, Re} = re:compile(Path),
    {ok, GroupRe} = re:compile("\\(\\?P<([^>]+)>"),
    Groups = case re:run(Path, GroupRe, [{capture, all_but_first, binary}, global]) of
        {match, NamedGroups} -> [binary_to_atom(G, utf8) || [G] <- NamedGroups];
        nomatch -> []
    end,
    {{Re, Groups}, Handler, Opts}.


-spec execute(Req, Env) ->
    {ok, Req, Env} | {error, 400 | 404, Req} 
    when Req::cowboy_req:req(), Env::cowboy_middleware:env().
execute(Req, Env) ->
    {_, Dispatch} = lists:keyfind(dispatch, 1, Env),
    [Host, Path] = cowboy_req:get([host, path], Req),
    case match(Dispatch, Host, Path) of
        {ok, Handler, HandlerOpts, Bindings, HostInfo, PathInfo} ->
            Req2 = cowboy_req:set_bindings(HostInfo, PathInfo, Bindings, Req),
            {ok, Req2, [{handler, Handler}, {handler_opts, HandlerOpts}|Env]};
        {error, badrequest, path} ->
            {error, 400, Req};
        {error, notfound, path} ->
            {error, 404, Req}
    end.


-spec match(dispatch_rules(), Host::binary() | tokens(), Path::binary()) ->
    {ok, module(), any(), bindings(), tokens(), tokens()}
    | {error, notfound, path} | {error, badrequest, path}.
match([], _, _) ->
    {error, notfound, host};
match([{{PathRe, Groups}, Handler, Opts}|Tail], HostInfo, Path) when is_list(HostInfo) ->
    case re:run(Path, PathRe, [{capture, Groups, binary}]) of
        nomatch ->
            match(Tail, HostInfo, Path);
        match ->
            case split_path(Path) of
                badrequest ->
                    {error, badrequest, path};
                PathInfo ->
                    {ok, Handler, Opts, [], HostInfo, PathInfo}
            end;
        {match, Captured} ->
            Bindings = lists:zip(Groups, Captured),
            case split_path(Path) of
                badrequest ->
                    {error, badrequest, path};
                PathInfo ->
                    {ok, Handler, Opts, Bindings, HostInfo, PathInfo}
            end
    end;
match(Dispatch, Host, Path) ->
    match(Dispatch, split_host(Host), Path).


-spec split_host(binary()) -> tokens().
split_host(Host) ->
    split_host(Host, []).

split_host(Host, Acc) ->
    case binary:match(Host, <<".">>) of
        nomatch when Host =:= <<>> ->
            Acc;
        nomatch ->
            [Host|Acc];
        {Pos, _} ->
            << Segment:Pos/binary, _:8, Rest/bits >> = Host,
            false = byte_size(Segment) == 0,
            split_host(Rest, [Segment|Acc])
    end.    


-spec split_path(binary()) -> tokens() | badrequest.
split_path(<< $/, Path/bits >>) ->
    split_path(Path, []);
split_path(_) ->
    badrequest.

split_path(Path, Acc) ->
    try
        case binary:match(Path, <<"/">>) of
            nomatch when Path =:= <<>> ->
                lists:reverse([cowboy_http:urldecode(S) || S <- Acc]);
            nomatch ->
                lists:reverse([cowboy_http:urldecode(S) || S <- [Path|Acc]]);
            {Pos, _} ->
                << Segment:Pos/binary, _:8, Rest/bits >> = Path,
                split_path(Rest, [Segment|Acc])
        end
    catch
        error:badarg ->
            badrequest
    end.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

compile_test() ->
    [] = compile(
        []),
    [{{_, []}, h, o}] = compile(
        [{"", h, o}]),
    [{{_, [a, b, c]}, h, o}] = compile(
        [{"/(?P<a>\\d)/(?P<b>\\d)/(?P<c>\\d)", h, o}]),
    [{{_, []}, h, o}] = compile(
        [{"/путь/к/ресурсу/", h, o}]).

match_test_() ->
    Dispatch = compile([
        {"^/not/here$", match_not, []},
        {"^/users/(?P<user>\\d+)/mails$", match_capture_user, []},
        {"/(?P<a>\\d)/(?P<b>\\d)/(?P<c>\\d)", match_triplet, []}
    ]),
    Tests = [
        {<<"">>, <<"/users/42/mails">>, 
            {ok, match_capture_user, [], [{user, <<"42">>}]}},
        {<<"">>, <<"/1/2/3">>,
            {ok, match_triplet, [], [{a, <<"1">>}, {b, <<"2">>}, {c, <<"3">>}]}}
    ],
    [{lists:flatten(io_lib:format("~p, ~p", [H, P])), fun() ->
        {ok, Handler, Opts, Binds, _, _} = match(Dispatch, H, P)
    end} || {H, P, {ok, Handler, Opts, Binds}} <- Tests].

-endif.
