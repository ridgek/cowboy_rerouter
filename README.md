Cowboy Rerouter
======

Cowboy Rerouter is a regular expression based router/dispatcher for Cowboy.

Usage
-----

    start(_Type, _Args) ->
        Dispatch = cowboy_rerouter:compile([
            {"^/$", example_home_handler, []},
            {"^/about$", example_about_handler, []},
            {"^/user/(?P<id>\\d+)$", example_user_handler, []}
        ]),
        {ok, _} = cowboy:start_http(http, 100, [{port, 8080}], [
            {env, [{dispatch, Dispatch}]},
            {middlewares, [cowboy_rerouter, cowboy_handler]}
        ]),
        example_sup:start_link().

