Cowboy Rerouter
===============

[![Build Status](https://travis-ci.org/ridgek/cowboy_rerouter.png?branch=master)](https://travis-ci.org/ridgek/cowboy_rerouter)


Cowboy Rerouter is a regular expression based router/dispatcher for Cowboy.

Styled after the [Django URL dispatcher][1], it allows you to build complex
URLs not possible with [cowboy_router][2].

For the sake of simplicity it does not allow matching on hostnames, or
contraint functions.

Currently it has no support for reversing URLs, but this may be added in the
future.

[1]: https://docs.djangoproject.com/en/dev/topics/http/urls/
[2]: http://ninenines.eu/docs/en/cowboy/HEAD/guide/routing/

Usage
-----

1. Use `cowboy_rerouter:compile/1` instead of `cowboy_router:compile/1`
2. Add `{middlewares, [cowboy_rerouter, cowboy_handler]}` to the `ProtoOpts`
   argument when starting the cowboy listener.

Your `application:start/2` should look something like this:

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


### Route rules

    Routes = [Route1, Route2, ... RouteN].

Each `route` contains a path matching regular-expression, the handler module,
and the options that will be given to it on initialization.

    Route = {Path, Handler, Opts}.
    RouteA = {"^about/$", about_handler, []}.

You can include multiple routes under a prefix using a `route_include` route.

    Route = {Path, [Route1, Route1, ... RouteN]}.
    RouteP = {"^wheel/(?P<wheel_id>\\d+)/", [
        {"start$", wheel_start_handler, []},
        {"stop$", wheel_stop_handler, []},
        {"change$", wheel_change_handler, []}
    ]}.

This is functionally identical to

    Routes = [
        {"^wheel/(?P<wheel_id>\\d+)/start$", wheel_start_handler, []},
        {"^wheel/(?P<wheel_id>\\d+)/stop$", wheel_stop_handler, []},
        {"^wheel/(?P<wheel_id>\\d+)/change$", wheel_change_handler, []},
        {"^wheel/(?P<wheel_id>)\\d+/start$", wheel_start_handler, []}
    ].


#### Match syntax
Bindings are captured with named groups, the syntax for named
regular-expression groups is `(?P<name>pattern)`, where `name` is the name of
the group and `pattern` is some pattern to match. 
Bindings are retreived using `cowboy_req:binding/{2,3}`.

