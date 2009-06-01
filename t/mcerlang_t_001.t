#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin -sasl errlog_type error -boot start_sasl -noshell

main(_) ->
    etap:plan(unknown),
    case (catch start()) of
        {'EXIT', Err} ->
            io:format("Err ~p~n", [Err]),
            etap:bail();
        _ ->
            etap:end_tests()
    end,
    ok.
    
start() ->
    {ok, Socket} = gen_tcp:connect("localhost", 11211, [binary, {packet, 0}, {active, false}]),

    gen_tcp:send(Socket, <<128,16,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0>>),    
    etap:ok((fun({ok, _}) -> true; (_) -> false end)(gen_tcp:recv(Socket, 0, 2000)), "stats"),
    
    etap:is(mcerlang:find_next_largest(4, [{1,a}, {2,b}, {5,c}, {7,d}, {14,e}]), c, "find next largest"),
    etap:is(mcerlang:find_next_largest(1, [{1,a}, {2,b}, {5,c}, {7,d}, {14,e}]), b, "find next largest"),
    etap:is(mcerlang:find_next_largest(0, [{1,a}, {2,b}, {5,c}, {7,d}, {14,e}]), a, "find next largest"),
    etap:is(mcerlang:find_next_largest(13, [{1,a}, {2,b}, {5,c}, {7,d}, {14,e}]), e, "find next largest"),
    etap:is(mcerlang:find_next_largest(14, [{1,a}, {2,b}, {5,c}, {7,d}, {14,e}]), a, "find next largest"),
    etap:is(mcerlang:find_next_largest(15, [{1,a}, {2,b}, {5,c}, {7,d}, {14,e}]), a, "find next largest"),
    
    StartLink = fun() -> case mcerlang:start_link([{"127.0.0.1", 11211, 1}]) of {ok, _} -> true; _ -> false end end,
    etap:ok(StartLink(), "start mcerlang"),

    etap:is(mcerlang:set("Hello", "World"), <<>>, "set ok"),
    etap:is(mcerlang:add("Hello", "Fail"), <<"Data exists for key.">>, "add ok"),
    etap:is(mcerlang:get("Hello"), <<"World">>, "get ok"),
    
    ok.