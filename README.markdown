# MC Erlang caches beats on your face

Erlang binary protocol memcached client

## Dependencies

Binary protocol build of memcached <http://github.com/dustin/memcached>

## External Documentation

Text Protocol Spec <http://code.sixapart.com/svn/memcached/trunk/server/doc/protocol.txt>

Binary Protocol Spec <http://code.google.com/p/memcached/wiki/MemcacheBinaryProtocol>

## Quick Start

**You must have the binary protocol branch of memcached running as mentioned above**

$> make

$> make test

$> sudo make install


1> mcerlang:start_link([{"localhost", 11211, 1}]).

{ok,<0.37.0>}


2> mcerlang:stats().

[{{"localhost",11211},
  [{evictions,"0"},
   {total_items,"30"},
   {curr_items,"4"},
   {bytes,"257"},
   {...}|...]}]

3> mcerlang:set(hello, <<"World">>).

<<>>

4> mcerlang:get(hello).

<<"World">>

5> mcerlang:add("foo", <<"bar">>).

<<>>

6> mcerlang:get("foo").

<<"bar">>

## Commands

* **get**(Key::any()) -> Val::binary()
* **get_many**([Key::any()]) -> [Val::binary()]
* **add**(Key::any(), Val::binary()) -> Response::binary()
* **add**(Key::any(), Val::binary(), Expiration::integer()) -> Response::binary()
* **set**(Key::any(), Val::binary()) -> Response::binary()
* **set**(Key::any(), Val::binary(), Expiration::integer()) -> Response::binary()
* **replace**(Key::any(), Val::binary()) -> Response::binary()
* **replace**(Key::any(), Val::binary(), Expiration::integer()) -> Response::binary()
* **delete**(Key::any()) -> Response::binary()
* **increment**(Key::any(), Val::binary(), Initial::binary(), Expiration::integer()) -> Response::binary()
* **decrement**(Key::any(), Val::binary(), Initial::binary(), Expiration::integer()) -> Response::binary()
* **append**(Key::any(), Val::binary()) -> Response::binary()
* **prepend**(Key::any(), Val::binary()) -> Response::binary()
* **stat**() -> [{{Host::string(), Port::integer()}, Response::binary()}]
* **flush**(Expiration::integer()) -> [{{Host::string(), Port::integer()}, Response::binary()}]
* **quit**() -> [{{Host::string(), Port::integer()}, Response::binary()}]
* **version**() -> [{{Host::string(), Port::integer()}, Response::binary()}]