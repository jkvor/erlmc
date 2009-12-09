#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin -sasl errlog_type error -boot start_sasl -noshell

-include_lib("etap/include/etap.hrl").        

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
		etap:is(erlmc:start(), ok, "erlmc connect to default memcached server ok"),

	    etap:is(erlmc:set("Hello", <<"World">>), <<>>, "set ok"),
	    etap:is(erlmc:add("Hello", <<"Fail">>), <<"Data exists for key.">>, "add ok"),
	    etap:is(erlmc:get("Hello"), <<"World">>, "get ok"),
	    etap:is(erlmc:delete("Hello"), <<>>, "delete ok"),
	    etap:is(erlmc:add("Hello", <<"World2">>), <<>>, "add ok"),
	    etap:is(erlmc:get("Hello"), <<"World2">>, "get ok"),
	    etap:is(erlmc:append("Hello", <<"!!!">>), <<>>, "append ok"),
	    etap:is(erlmc:get("Hello"), <<"World2!!!">>, "get ok"),
	    etap:is(erlmc:prepend("Hello", <<"$$$">>), <<>>, "prepend ok"),
	    etap:is(erlmc:get("Hello"), <<"$$$World2!!!">>, "get ok"),
	    etap:is(erlmc:delete("Hello"), <<>>, "delete ok"),
	    etap:is(erlmc:get("Hello"), <<>>, "get ok"),

	    erlmc:set("One", <<"A">>),
	    erlmc:set("Two", <<"B">>),
	    erlmc:set("Three", <<"C">>),

	    etap:is(erlmc:get_many(["One", "Two", "Two-and-a-half", "Three"]), [{"One",<<"A">>},{"Two",<<"B">>},{"Two-and-a-half",<<>>},{"Three",<<"C">>}], "get_many ok"),
	    
	    etap:is(erlmc:flush(0), [{{"localhost",11211},<<>>}], "flush ok"),
	    
	    ?etap_match(erlmc:stats(), [{{"localhost",11211}, [{_,_}|_]}], "stats/0 ok"),
        ?etap_match(erlmc:stats("localhost",11211), [{_,_}|_], "stats/2 ok"),
	    
		etap:is(erlmc:quit(), [{{"localhost",11211},[true]}], "quit ok"),
		
		ok
	end)(),
    
	etap:end_tests().