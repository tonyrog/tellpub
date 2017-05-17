%%% @author  <tony@rogvall.se>
%%% @copyright (C) 2017, 
%%% @doc
%%%    Start application
%%% @end
%%% Created :  2 Apr 2017 by  <tony@rogvall.se>

-module(tellpub).
-export([start/0]).
-export([status/0]).

status() ->
    io:format("up\n", []).

start() ->
    application:ensure_all_started(tellpub).
