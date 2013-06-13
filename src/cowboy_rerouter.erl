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
-type route_include() :: {Path::route_match(), Routes::routes()}.
-type route_rule() :: route() | route_include().
-type routes() :: [route_rule()].
-export_type([routes/0]).

-type dispatch_match() :: {Re::re:mp(), Groups::[atom()]}.
-type dispatch() :: {Path::dispatch_match(), Handler::module(), Opts::any()}.
-type dispatch_include() :: {Path::dispatch_match(), Rules::dispatch_rules()}.
-type dispatch_rule() :: dispatch() | dispatch_include().
-opaque dispatch_rules() :: [dispatch_rule()].
-export_type([dispatch_rules/0]).


-spec compile(routes()) -> dispatch_rules().
compile(Routes) ->
    [compile_route(R) || R <- Routes].


-spec compile_route(route_rule()) -> dispatch_rule().
compile_route({Path, Handler, Opts}) ->
    {compile_match(Path), Handler, Opts};
compile_route({Path, Routes}) ->
    {compile_match(Path), compile(Routes)}.


-spec compile_match(route_match()) -> dispatch_match().
compile_match(Path) ->
    {ok, Re} = re:compile(Path),
    {ok, GroupRe} = re:compile("\\(\\?P<([^>]+)>"),
    Groups = case re:run(Path, GroupRe, [{capture, all_but_first, binary}, global]) of
        {match, NamedGroups} -> [binary_to_atom(G, utf8) || [G] <- NamedGroups];
        nomatch -> []
    end,
    {Re, Groups}.


-spec execute(Req, Env) ->
    {ok, Req, Env} | {error, 400 | 404, Req} 
    when Req::cowboy_req:req(), Env::cowboy_middleware:env().
execute(Req, Env) ->
    {_, Dispatch} = lists:keyfind(dispatch, 1, Env),
    [Host, Path] = cowboy_req:get([host, path], Req),
    HostInfo = split_host(Host),
    PathInfo = split_path(Path),
    case match(Dispatch, Path, []) of
        {ok, Handler, HandlerOpts, Bindings} ->
            Req2 = cowboy_req:set_bindings(HostInfo, PathInfo, Bindings, Req),
            {ok, Req2, [{handler, Handler}, {handler_opts, HandlerOpts}|Env]};
        {error, notfound, path} ->
            {error, 404, Req}
    end.


-spec match(dispatch_rules(), Path::binary(), Bindings::bindings()) ->
    {ok, module(), any(), bindings()} | {error, notfound, path}.
match([], _, _) ->
    {error, notfound, path};
match([{{PathRe, Groups}, Handler, Opts}|Tail], Path, Bindings) ->
    case re:run(Path, PathRe, [{capture, Groups, binary}]) of
        nomatch ->
            match(Tail, Path, Bindings);
        match ->
            {ok, Handler, Opts, Bindings};
        {match, Captured} ->
            Bindings2 = Bindings ++ lists:zip(Groups, Captured),
            {ok, Handler, Opts, Bindings2}
    end;
match([{{PathRe, Groups}, Paths}|Tail], Path, Bindings) ->
    case re:run(Path, PathRe, [{capture, first}]) of
        nomatch ->
            match(Tail, Path, Bindings);
        {match, [{_, End}]} ->
            Path2 = binary:part(Path, {End, byte_size(Path)-End}),
            Bindings2 = case re:run(Path, PathRe, [{capture, Groups, binary}]) of
                match -> Bindings;
                {match, Captured} -> Bindings ++ lists:zip(Groups, Captured)
            end,
            case match(Paths, Path2, Bindings2) of
                {error, notfound, path} -> match(Tail, Path2, Bindings2);
                Match -> Match
            end
    end.


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


-spec split_path(binary()) -> tokens().
split_path(<< $/, Path/bits >>) ->
    split_path(Path, []);
split_path(_) ->
    [].

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
            []
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
        [{"/путь/к/ресурсу/", h, o}]),
    
    [{{_, [a, b, c]}, h, o}, {{_, []}, [{{_, []}, h, o}]}] = compile([
        {"/(?P<a>\\d)/(?P<b>\\d)/(?P<c>\\d)", h, o},
        {"^/include", [
            {"^/about$", h, o}
        ]}
    ]).

match_test_() ->
    Dispatch = compile([
        {"^/not/here$", match_not, []},
        {"^/users/(?P<user>\\d+)/mails$", match_capture_user, []},
        {"^/(?P<a>\\d)/(?P<b>\\d)/(?P<c>\\d)", match_triplet, []},
        {"^/secret", [
            {"^/stuff$", match_secret_stuff, []}
        ]},
        {"^/users/(?P<user>\\d+)", [
            {"^/profile$", match_user_profile, []},
            {"^/tasks/(?P<task>\\w+)$", match_user_task, []}
        ]}
    ]),
    Tests = [
        {<<"">>, <<"/users/42/mails">>, 
            {ok, match_capture_user, [], [{user, <<"42">>}]}},
        {<<"">>, <<"/1/2/3">>,
            {ok, match_triplet, [], [{a, <<"1">>}, {b, <<"2">>}, {c, <<"3">>}]}},
        {<<"">>, <<"/secret/stuff">>,
            {ok, match_secret_stuff, [], []}},
        {<<"">>, <<"/users/42/profile">>,
            {ok, match_user_profile, [], [{user, <<"42">>}]}},
        {<<"">>, <<"/users/42/tasks/tidyup">>,
            {ok, match_user_task, [], [{user, <<"42">>}, {task, <<"tidyup">>}]}}
    ],
    [{lists:flatten(io_lib:format("~p, ~p", [H, P])), fun() ->
        Expect = match(Dispatch, P, [])
    end} || {H, P, Expect} <- Tests].

-endif.
