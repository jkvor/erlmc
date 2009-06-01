%% Copyright (c) 2009 
%% Nick Gerakines <nick@gerakines.net>
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
-module(mcerlang).
-behaviour(gen_server).

%% gen_server callbacks
-export([start_link/1, init/1, handle_call/3, handle_cast/2, 
         handle_info/2, terminate/2, code_change/3]).

%% api callbacks
-export([get/1, get_many/1, add/2, add/3, set/2, set/3, 
		 replace/2, replace/3, delete/1, increment/4, decrement/4,
		 append/2, prepend/2, stat/0, flush/1, quit/0, version/0]).

-export([find_next_largest/2]).

-include("mcerlang.hrl").

-define(TIMEOUT, 3000).

-record(state, {continuum, sockets}).

%% @spec start_link(CacheServers) -> {ok, pid()}
%%       CacheServers = [{Host, Port, ConnectionPoolSize}]
%%       Host = string()
%%       Port = integer()
%%       ConnectionPoolSize = integer()
start_link(CacheServers) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, CacheServers, []).
    
get(Key) ->
    gen_server:call(?MODULE, {get, Key}).

get_many(Keys) ->
    gen_server:call(?MODULE, {get_many, Keys}).
    
add(Key, Value) ->
	add(Key, Value, 0).
	
add(Key, Value, Expiration) when is_binary(Value), is_integer(Expiration) ->
    gen_server:call(?MODULE, {add, Key, Value, Expiration}).

set(Key, Value) ->
	set(Key, Value, 0).
	
set(Key, Value, Expiration) when is_binary(Value), is_integer(Expiration) ->
    gen_server:call(?MODULE, {set, Key, Value, Expiration}).
    
replace(Key, Value) ->
	replace(Key, Value, 0).
	
replace(Key, Value, Expiration) when is_binary(Value), is_integer(Expiration) ->
    gen_server:call(?MODULE, {replace, Key, Value, Expiration}).
    
delete(Key) ->
    gen_server:call(?MODULE, {delete, Key}).

increment(Key, Value, Initial, Expiration) when is_binary(Value), is_binary(Initial), is_integer(Expiration) ->
    gen_server:call(?MODULE, {increment, Key, Value, Initial, Expiration}).

decrement(Key, Value, Initial, Expiration) when is_binary(Value), is_binary(Initial), is_integer(Expiration) ->
    gen_server:call(?MODULE, {decrement, Key, Value, Initial, Expiration}).

append(Key, Value) when is_binary(Value) ->
    gen_server:call(?MODULE, {append, Key, Value}).

prepend(Key, Value) when is_binary(Value) ->
    gen_server:call(?MODULE, {prepend, Key, Value}).

stat() ->
    gen_server:call(?MODULE, stat).

flush(Expiration) when is_integer(Expiration) ->
    gen_server:call(?MODULE, {flush, Expiration}).
    
quit() ->
    gen_server:call(?MODULE, quit).
    
version() ->
    gen_server:call(?MODULE, version).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%% @hidden
%%--------------------------------------------------------------------
init(CacheServers) ->
    {A,B,C} = now(),
    random:seed(A,B,C),
    
    %% Continuum = [{uint(), {Host, Port}}]
    Continuum = lists:sort(dict:to_list(lists:foldl(
        fun({Host, Port, _}, Dict) ->
            lists:foldl(
                fun(_, Dict1) ->
                    dict:store(hash_to_uint(Host, Port), {Host, Port}, Dict1)
                end, Dict, lists:seq(1,100))
        end, dict:new(), CacheServers))),
    %% Sockets = [{{Host,Port}, [socket()]}]
    Sockets = [begin
        {{Host, Port}, [begin
            %{ok, S} = gen_tcp:connect(Host, Port, [binary, {packet, raw}, {nodelay, true}, {reuseaddr, true}, {active, false}]), S
            {ok, S} = gen_tcp:connect(Host, Port, [binary, {packet, 0}, {active, false}]), S
        end || _ <- lists:seq(1, ConnectionPoolSize)]}
     end || {Host, Port, ConnectionPoolSize} <- CacheServers],
    {ok, #state{continuum=Continuum, sockets=Sockets}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%% @hidden
%%--------------------------------------------------------------------    
handle_call({get, Key0}, _From, State) ->
    Key = package_key(Key0),
    Socket = map_key(State, Key),
    #response{key=Key1, value=Value} = send_recv(Socket, #request{op_code=?OP_GetK, key=list_to_binary(Key)}),
    case binary_to_list(Key1) of
        Key -> {reply, Value, State};
        _ -> {reply, <<>>, State}
    end;

handle_call({get_many, Keys}, _From, State) ->
    SocketDicts = lists:foldl(
        fun(Key, Dict) ->
            Socket = map_key(State, Key),
            send(Socket, #request{op_code=?OP_GetKQ, key=list_to_binary(Key)}),
            case dict:find(Socket, Dict) of
                {ok, Count} -> dict:store(Socket, Count+1, Dict);
                error -> dict:store(Socket, 1, Dict)
            end
        end, dict:new(), Keys),
    Resps = lists:flatten([begin
        send(Socket, #request{op_code=?OP_Noop}),
        [recv(Socket) || _ <- lists:seq(1,Count)]
     end || {Socket, Count} <- dict:to_list(SocketDicts)]),
    Reply = [begin
        case lists:keysearch(list_to_binary(Key), 8, Resps) of
            {value, Resp} -> {Key, Resp#response.value};
            false -> {Key, <<>>}
        end
     end || Key <- Keys],
    {reply, Reply, State};
    
handle_call({add, Key0, Value, Expiration}, _From, State) ->
    Key = package_key(Key0),
    Socket = map_key(State, Key),
    Resp = send_recv(Socket, #request{op_code=?OP_Add, extras = <<16#deadbeef:32, Expiration:32>>, key=list_to_binary(Key), value=Value}),
    {reply, Resp#response.value, State};
    
handle_call({set, Key0, Value, Expiration}, _From, State) ->
    Key = package_key(Key0),
    Socket = map_key(State, Key),
    Resp = send_recv(Socket, #request{op_code=?OP_Set, extras = <<16#deadbeef:32, Expiration:32>>, key=list_to_binary(Key), value=Value}),
    {reply, Resp#response.value, State};

handle_call({replace, Key0, Value, Expiration}, _From, State) ->
    Key = package_key(Key0),
    Socket = map_key(State, Key),
    Resp = send_recv(Socket, #request{op_code=?OP_Replace, extras = <<16#deadbeef:32, Expiration:32>>, key=list_to_binary(Key), value=Value}),
    {reply, Resp#response.value, State};

handle_call({delete, Key0}, _From, State) ->
    Key = package_key(Key0),
    Socket = map_key(State, Key),
    Resp = send_recv(Socket, #request{op_code=?OP_Delete, key=list_to_binary(Key)}),
    {reply, Resp#response.value, State};

handle_call({increment, Key0, Value, Initial, Expiration}, _From, State) ->
	Key = package_key(Key0),
    Socket = map_key(State, Key),
	Resp = send_recv(Socket, #request{op_code=?OP_Increment, extras = <<Value:64, Initial:64, Expiration:32>>, key=list_to_binary(Key)}),
	{reply, Resp, State};
	
handle_call({decrement, Key0, Value, Initial, Expiration}, _From, State) ->
	Key = package_key(Key0),
    Socket = map_key(State, Key),
	Resp = send_recv(Socket, #request{op_code=?OP_Decrement, extras = <<Value:64, Initial:64, Expiration:32>>, key=list_to_binary(Key)}),
	{reply, Resp, State};

handle_call({append, Key0, Value}, _From, State) ->
	Key = package_key(Key0),
    Socket = map_key(State, Key),
	Resp = send_recv(Socket, #request{op_code=?OP_Append, key=list_to_binary(Key), value=Value}),
	{reply, Resp#response.value, State};

handle_call({prepend, Key0, Value}, _From, State) ->
	Key = package_key(Key0),
    Socket = map_key(State, Key),
	Resp = send_recv(Socket, #request{op_code=?OP_Prepend, key=list_to_binary(Key), value=Value}),
	{reply, Resp#response.value, State};
	
handle_call(stat, _From, State) ->
    Reply = send_all(State, #request{op_code=?OP_Stat}),
    {reply, Reply, State};

handle_call({flush, Expiration}, _From, State) ->
    Reply = send_all(State, #request{op_code=?OP_Flush, extras = <<Expiration:32>>}),
    {reply, Reply, State};
    
handle_call(quit, _From, State) ->
    Reply = send_all(State, #request{op_code=?OP_Quit}),
    {reply, Reply, State};
    
handle_call(version, _From, State) ->
    Reply = send_all(State, #request{op_code=?OP_Version}),
    {reply, Reply, State};
			
handle_call(_, _From, State) -> {reply, {error, invalid_call}, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%% @hidden
%%--------------------------------------------------------------------
handle_cast(_Message, State) -> {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%% @hidden
%%--------------------------------------------------------------------
handle_info(_Info, State) -> {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%% @hidden
%%--------------------------------------------------------------------
terminate(_Reason, _State) -> ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%% @hidden
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
send_all(State, Request) ->
    [begin
        {{Host, Port}, begin
            Resp = send_recv(Socket, Request),
            Resp#response.value
        end}
     end || {{Host, Port}, [Socket|_]} <- State#state.sockets].
             
send_recv(Socket, Request) ->
    ok = send(Socket, Request),
    recv(Socket).
    
send(Socket, Request) ->
    Bin = encode_request(Request),
    gen_tcp:send(Socket, Bin).

recv(Socket) ->
    Resp1 = recv_header(Socket),
    Resp2 = recv_body(Socket, Resp1),
    Resp2.
        
encode_request(Request) when is_record(Request, request) ->
    Magic = 16#80,
    Opcode = Request#request.op_code,
    KeySize = size(Request#request.key),
    Extras = Request#request.extras,
    ExtrasSize = size(Extras),
    DataType = Request#request.data_type,
    Reserved = Request#request.reserved,
    Body = <<Extras:ExtrasSize/binary, (Request#request.key)/binary, (Request#request.value)/binary>>,
    BodySize = size(Body),
    Opaque = Request#request.opaque,
    CAS = Request#request.cas,
    <<Magic:8, Opcode:8, KeySize:16, ExtrasSize:8, DataType:8, Reserved:16, BodySize:32, Opaque:32, CAS:64, Body:BodySize/binary>>.

recv_header(Socket) ->
    decode_response_header(recv_bytes(Socket, 24)).
  
recv_body(Socket, #response{key_size = KeySize, extras_size = ExtrasSize, body_size = BodySize}=Resp) ->
    decode_response_body(recv_bytes(Socket, BodySize), ExtrasSize, KeySize, Resp).
    
decode_response_header(<<16#81:8, Opcode:8, KeySize:16, ExtrasSize:8, DataType:8, Status:16, BodySize:32, Opaque:32, CAS:64>>) ->
    #response{
        op_code = Opcode, 
        data_type = DataType, 
        status = Status, 
        opaque = Opaque, 
        cas = CAS, 
        key_size = KeySize,
        extras_size = ExtrasSize,
        body_size = BodySize
    }.
    
decode_response_body(Bin, ExtrasSize, KeySize, Resp) ->
    <<Extras:ExtrasSize/binary, Key:KeySize/binary, Value/binary>> = Bin,
    Resp#response{
        extras = Extras,
        key = Key,
        value = Value
    }.

recv_bytes(_, 0) -> <<>>;
recv_bytes(Socket, NumBytes) ->
    {ok, Bin} = gen_tcp:recv(Socket, NumBytes), Bin.

package_key(Key) when is_atom(Key) ->
    atom_to_list(Key);
    
package_key(Key) when is_list(Key) ->
    Key;
    
package_key(Key) when is_binary(Key) ->
    binary_to_list(Key);
    
package_key(Key) ->
    lists:flatten(io_lib:format("~p", [Key])).
    
%% Consistent hashing functions
%%
%% First, hash memcached servers to unsigned integers on a continuum. To
%% map a key to a memcached server, hash the key to an unsigned integer
%% and locate the next largest integer on the continuum. That integer
%% represents the hashed server that the key maps to.
%% reference: http://www8.org/w8-papers/2a-webserver/caching/paper2.html
hash_to_uint(Host, Port) when is_list(Host), is_integer(Port) ->
    hash_to_uint(Host ++ integer_to_list(Port)).

hash_to_uint(Key) when is_list(Key) -> 
    <<Int:128/unsigned-integer>> = erlang:md5(Key), Int.

map_key(#state{continuum=Continuum, sockets=Sockets}, Key) when is_list(Key) ->
    {Host, Port} = find_next_largest(hash_to_uint(Key), Continuum),
    Pool = proplists:get_value({Host, Port}, Sockets),
    lists:nth(random:uniform(length(Pool)), Pool).
    
find_next_largest(Int, Continuum) ->
    {A,B} = lists:split(length(Continuum) div 2, Continuum),
    case find_next_largest(Int, A, B) of
        undefined ->
            [{_, Val}|_] = Continuum,
            Val;
        Val -> Val
    end.
    
find_next_largest(Int, [], [{Pivot, _}|_]) when Int >= Pivot -> undefined;

find_next_largest(Int, [], [{Pivot, Val}|_]) when Int < Pivot -> Val;

find_next_largest(Int, Front, [{Pivot, Val} | _]) when Int < Pivot ->
    {Last, _} = lists:last(Front),
    case Int >= Last of
        true -> Val;
        false ->
            {A, B} = lists:split(length(Front) div 2, Front),
            find_next_largest(Int, A, B)
    end;
    
find_next_largest(Int, _, [{Pivot,_} | _]=Back) when Int >= Pivot ->
    {A, B} = lists:split(length(Back) div 2, Back),
    find_next_largest(Int, A, B).