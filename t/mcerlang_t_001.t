#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin -sasl errlog_type error -boot start_sasl -noshell

main(_) ->
    etap:plan(unknown),

    etap:is(mcerlang:find_next_largest(4, [{1,a}, {2,b}, {5,c}, {7,d}, {14,e}]), c, "find next largest"),
    etap:is(mcerlang:find_next_largest(1, [{1,a}, {2,b}, {5,c}, {7,d}, {14,e}]), b, "find next largest"),
    etap:is(mcerlang:find_next_largest(0, [{1,a}, {2,b}, {5,c}, {7,d}, {14,e}]), a, "find next largest"),
    etap:is(mcerlang:find_next_largest(13, [{1,a}, {2,b}, {5,c}, {7,d}, {14,e}]), e, "find next largest"),
    etap:is(mcerlang:find_next_largest(14, [{1,a}, {2,b}, {5,c}, {7,d}, {14,e}]), a, "find next largest"),
    etap:is(mcerlang:find_next_largest(15, [{1,a}, {2,b}, {5,c}, {7,d}, {14,e}]), a, "find next largest"),

    etap:end_tests().