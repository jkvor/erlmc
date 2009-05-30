#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin -sasl errlog_type error -boot start_sasl -noshell

main(_) ->
    etap:plan(unknown),

    Connect = fun() -> case gen_tcp:connect("127.0.0.1", 11211, []) of {ok, _} -> true; _ -> false end end,
    etap:ok(Connect(), "connect to memcached"),
    etap:is(mcerlang:find_next_largest(4, [{1,a}, {2,b}, {5,c}, {7,d}, {14,e}]), c, "find next largest"),
    etap:is(mcerlang:find_next_largest(1, [{1,a}, {2,b}, {5,c}, {7,d}, {14,e}]), b, "find next largest"),
    etap:is(mcerlang:find_next_largest(0, [{1,a}, {2,b}, {5,c}, {7,d}, {14,e}]), a, "find next largest"),
    etap:is(mcerlang:find_next_largest(13, [{1,a}, {2,b}, {5,c}, {7,d}, {14,e}]), e, "find next largest"),
    etap:is(mcerlang:find_next_largest(14, [{1,a}, {2,b}, {5,c}, {7,d}, {14,e}]), a, "find next largest"),
    etap:is(mcerlang:find_next_largest(15, [{1,a}, {2,b}, {5,c}, {7,d}, {14,e}]), a, "find next largest"),
    
    StartLink = fun() -> case mcerlang:start_link([{"127.0.0.1", 11211, 1}]) of {ok, _} -> true; _ -> false end end,
    etap:ok(StartLink(), "start mcerlang"),
    
    io:format("stat ~p~n", [mcerlang:stat()]),
    
    %io:format("add ~p~n", [mcerlang:add("Hello", "World")]),
    
    %io:format("get ~p~n", [mcerlang:get("Hello")]),

    etap:end_tests().