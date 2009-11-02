%% Copyright (c) 2009 
%% Jacob Vorreuter <jacob.vorreuter@gmail.com>
%%
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use,
%% copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following
%% conditions:
%%
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%% OTHER DEALINGS IN THE SOFTWARE.
%%
%% http://code.google.com/p/memcached/wiki/MemcacheBinaryProtocol
%% @doc a binary protocol memcached client
-module(erlmc).

-export([start/0, start/1, start_link/0, start_link/1, init/2,
		 add_server/3, remove_server/2, add_connection/2, remove_connection/2]).

%% api callbacks
-export([get/1, get_many/1, add/2, add/3, set/2, set/3, 
		 replace/2, replace/3, delete/1, increment/4, decrement/4,
		 append/2, prepend/2, stats/0, flush/0, flush/1, quit/0, 
		 version/0]).

-include("erlmc.hrl").

-define(TIMEOUT, 60000).

%%--------------------------------------------------------------------
%%% API
%%--------------------------------------------------------------------
start() -> start([{"localhost", 11211, 1}]).
start(CacheServers) when is_list(CacheServers) ->
	random:seed(now()),
	case proc_lib:start(?MODULE, init, [self(), CacheServers], 5000) of
		{ok, _Pid} -> ok;
		Error -> Error
	end.
	
start_link() -> start_link([{"localhost", 11211, 1}]).
start_link(CacheServers) when is_list(CacheServers) ->
	random:seed(now()),
	proc_lib:start_link(?MODULE, init, [self(), CacheServers], 5000).
	
add_server(Host, Port, PoolSize) ->
	erlang:send(?MODULE, {add_server, Host, Port, PoolSize}),
	ok.
	
remove_server(Host, Port) ->
	erlang:send(?MODULE, {remove_server, Host, Port}),
	ok.
	
add_connection(Host, Port) ->
	erlang:send(?MODULE, {add_connection, Host, Port}),
	ok.
	
remove_connection(Host, Port) ->
	erlang:send(?MODULE, {remove_connection, Host, Port}),
	ok.
	
get(Key0) ->
	Key = package_key(Key0),
    gen_server:call(map_key(Key), {get, Key}, ?TIMEOUT).

get_many(Keys) ->
	Self = self(),
	Pids = [spawn(fun() -> 
		Res = (catch ?MODULE:get(Key)),
		Self ! {self(), {Key, Res}}
	 end) || Key <- Keys],
	lists:reverse(lists:foldl(
		fun(Pid, Acc) ->
			receive
				{Pid, {Key, Res}} -> [{Key, Res}|Acc]
			after ?TIMEOUT ->
				Acc
			end
		end, [], Pids)).
    
add(Key, Value) ->
	add(Key, Value, 0).
	
add(Key0, Value, Expiration) when is_binary(Value), is_integer(Expiration) ->
	Key = package_key(Key0),
    gen_server:call(map_key(Key), {add, Key, Value, Expiration}, ?TIMEOUT).

set(Key, Value) ->
	set(Key, Value, 0).
	
set(Key0, Value, Expiration) when is_binary(Value), is_integer(Expiration) ->
	Key = package_key(Key0),
    gen_server:call(map_key(Key), {set, Key, Value, Expiration}, ?TIMEOUT).
    
replace(Key, Value) ->
	replace(Key, Value, 0).
	
replace(Key0, Value, Expiration) when is_binary(Value), is_integer(Expiration) ->
	Key = package_key(Key0),
    gen_server:call(map_key(Key), {replace, Key, Value, Expiration}, ?TIMEOUT).
    
delete(Key0) ->
	Key = package_key(Key0),
    gen_server:call(map_key(Key), {delete, Key}, ?TIMEOUT).

increment(Key0, Value, Initial, Expiration) when is_binary(Value), is_binary(Initial), is_integer(Expiration) ->
	Key = package_key(Key0),
    gen_server:call(map_key(Key), {increment, Key, Value, Initial, Expiration}, ?TIMEOUT).

decrement(Key0, Value, Initial, Expiration) when is_binary(Value), is_binary(Initial), is_integer(Expiration) ->
	Key = package_key(Key0),
    gen_server:call(map_key(Key), {decrement, Key, Value, Initial, Expiration}, ?TIMEOUT).

append(Key0, Value) when is_binary(Value) ->
	Key = package_key(Key0),
    gen_server:call(map_key(Key), {append, Key, Value}, ?TIMEOUT).

prepend(Key0, Value) when is_binary(Value) ->
	Key = package_key(Key0),
    gen_server:call(map_key(Key), {prepend, Key, Value}, ?TIMEOUT).

stats() ->
	multi_call(stats).
	
flush() ->
    multi_call(flush).
    
flush(Expiration) when is_integer(Expiration) ->
    multi_call({flush, Expiration}).
    
quit() ->
	[begin
		{Key, [
			{'EXIT',{shutdown,{gen_server,call,[Pid,quit,?TIMEOUT]}}} == 
				(catch gen_server:call(Pid, quit, ?TIMEOUT)) || Pid <- Pids]}
	 end || {Key, Pids} <- unique_connections()].
    
version() ->
    multi_call(version).

multi_call(Msg) ->
	[begin
		Pid = lists:nth(random:uniform(length(Pids)), Pids),
		{{Host, Port}, gen_server:call(Pid, Msg, ?TIMEOUT)}
	end || {{Host, Port}, Pids} <- unique_connections()].
	
%%--------------------------------------------------------------------
%%% Stateful loop
%%--------------------------------------------------------------------	
init(Parent, CacheServers) ->
	process_flag(trap_exit, true),
	register(erlmc, self()),
	ets:new(erlmc_continuum, [ordered_set, protected, named_table]),
	ets:new(erlmc_connections, [bag, protected, named_table]),
    
    %% Continuum = [{uint(), {Host, Port}}]
	[add_server_to_continuum(Host, Port) || {Host, Port, _} <- CacheServers],
        
    %% Connections = [{{Host,Port}, ConnPid}]
	[begin
		[start_connection(Host, Port) || _ <- lists:seq(1, ConnPoolSize)]
	 end || {Host, Port, ConnPoolSize} <- CacheServers],
        
	proc_lib:init_ack(Parent, {ok, self()}),
	
	loop().
	
loop() ->
	receive
		{add_server, Host, Port, ConnPoolSize} ->
			add_server_to_continuum(Host, Port),
			[start_connection(Host, Port) || _ <- lists:seq(1, ConnPoolSize)];
		{remove_server, Host, Port} ->
			[(catch gen_server:call(Pid, quit, ?TIMEOUT)) || [Pid] <- ets:match(erlmc_connections, {{Host, Port}, '$1'})],
			remove_server_from_continuum(Host, Port);
		{add_connection, Host, Port} ->
			start_connection(Host, Port);
		{remove_connection, Host, Port} ->
			[[Pid]|_] = ets:match(erlmc_connections, {{Host, Port}, '$1'}),
			(catch gen_server:call(Pid, quit, ?TIMEOUT));
		{'EXIT', Pid, Err} ->
			case ets:match(erlmc_connections, {'$1', Pid}) of
				[[{Host, Port}]] -> 
					ets:delete_object(erlmc_connections, {{Host, Port}, Pid}),
					case Err of
						shutdown -> ok;
						_ -> start_connection(Host, Port)
					end;
				_ -> 
					ok
			end
	end,
	loop().
	
start_connection(Host, Port) ->
	case erlmc_conn:start_link([Host, Port]) of
		{ok, Pid} -> ets:insert(erlmc_connections, {{Host, Port}, Pid});
		_ -> ok
	end.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
add_server_to_continuum(Host, Port) ->
	[ets:insert(erlmc_continuum, {hash_to_uint(Host ++ integer_to_list(Port) ++ integer_to_list(I)), {Host, Port}}) || I <- lists:seq(1, 100)].

remove_server_from_continuum(Host, Port) ->
	case ets:match(erlmc_continuum, {'$1', {Host, Port}}) of
		[] -> 
			ok;
		List ->
			[ets:delete(erlmc_continuum, Key) || [Key] <- List]
	end.
	
package_key(Key) when is_atom(Key) ->
    atom_to_list(Key);

package_key(Key) when is_list(Key) ->
    Key;

package_key(Key) when is_binary(Key) ->
    binary_to_list(Key);

package_key(Key) ->
    lists:flatten(io_lib:format("~p", [Key])).

unique_connections() ->
	dict:to_list(lists:foldl(
		fun({Key, Val}, Dict) ->
			dict:append_list(Key, [Val], Dict)
		end, dict:new(), ets:tab2list(erlmc_connections))).
	
%% Consistent hashing functions
%%
%% First, hash memcached servers to unsigned integers on a continuum. To
%% map a key to a memcached server, hash the key to an unsigned integer
%% and locate the next largest integer on the continuum. That integer
%% represents the hashed server that the key maps to.
%% reference: http://www8.org/w8-papers/2a-webserver/caching/paper2.html
hash_to_uint(Key) when is_list(Key) -> 
    <<Int:128/unsigned-integer>> = erlang:md5(Key), Int.

%% @spec map_key(Key) -> Conn
%%		 Key = string()
%%		 Conn = pid()
map_key(Key) when is_list(Key) ->
	First = ets:first(erlmc_continuum),
    {Host, Port} = 
		case find_next_largest(hash_to_uint(Key), First) of
			undefined ->
				case First of
					'$end_of_table' -> exit(erlmc_continuum_empty);
					_ ->
						[{_, Value}] = ets:lookup(erlmc_continuum, First),
						Value
				end;
			Value -> Value
		end,
	case ets:lookup(erlmc_connections, {Host, Port}) of
		[] -> exit({error, {connection_not_found, {Host, Port}}});
		Pids ->
			{_, Pid} = lists:nth(random:uniform(length(Pids)), Pids),
			Pid
    end.
    
%% @todo: use sorting algorithm to find next largest
find_next_largest(_, '$end_of_table') -> 
	undefined;

find_next_largest(Int, Key) when Key > Int ->
	[{_, Val}] = ets:lookup(erlmc_continuum, Key),
	Val;
	
find_next_largest(Int, Key) ->
	find_next_largest(Int, ets:next(erlmc_continuum, Key)).
