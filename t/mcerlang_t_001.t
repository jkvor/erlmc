#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin -sasl errlog_type error -boot start_sasl -noshell

main(_) ->
    etap:plan(unknown),

	(fun() ->
	    {ok, Socket} = gen_tcp:connect("localhost", 11211, [binary, {packet, 0}, {active, false}]),

	    gen_tcp:send(Socket, <<128,10,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0>>),    
	    etap:ok((fun({ok, _}) -> true; (_) -> false end)(gen_tcp:recv(Socket, 0, 2000)), "noop"),
    
	    gen_tcp:send(Socket, <<128,7,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0>>),
	
		ok
	end)(),
	
	(fun() ->
		etap:is(mcerlang:start(), ok, "mcerlang connect to default memcached server ok"),

	    etap:is(mcerlang:set("Hello", <<"World">>), <<>>, "set ok"),
	    etap:is(mcerlang:add("Hello", <<"Fail">>), <<"Data exists for key.">>, "add ok"),
	    etap:is(mcerlang:get("Hello"), <<"World">>, "get ok"),
	    etap:is(mcerlang:delete("Hello"), <<>>, "delete ok"),
	    etap:is(mcerlang:add("Hello", <<"World2">>), <<>>, "add ok"),
	    etap:is(mcerlang:get("Hello"), <<"World2">>, "get ok"),
	    etap:is(mcerlang:append("Hello", <<"!!!">>), <<>>, "append ok"),
	    etap:is(mcerlang:get("Hello"), <<"World2!!!">>, "get ok"),
	    etap:is(mcerlang:prepend("Hello", <<"$$$">>), <<>>, "prepend ok"),
	    etap:is(mcerlang:get("Hello"), <<"$$$World2!!!">>, "get ok"),
	    etap:is(mcerlang:delete("Hello"), <<>>, "delete ok"),
	    etap:is(mcerlang:get("Hello"), <<>>, "get ok"),

	    mcerlang:set("One", <<"A">>),
	    mcerlang:set("Two", <<"B">>),
	    mcerlang:set("Three", <<"C">>),

	    etap:is(mcerlang:get_many(["One", "Two", "Two-and-a-half", "Three"]), [{"One",<<"A">>},{"Two",<<"B">>},{"Two-and-a-half",<<>>},{"Three",<<"C">>}], "get_many ok"),

	    etap:is(mcerlang:flush(0), [{{"localhost",11211},<<>>}], "flush ok"),

		etap:is(mcerlang:quit(), [{{"localhost",11211},[true]}], "quit ok"),
		
		ok
	end)(),
    
	etap:end_tests().