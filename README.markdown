## erlmc

Erlang binary protocol memcached client

## External Documentation

Binary Protocol Spec <http://code.google.com/p/memcached/wiki/MemcacheBinaryProtocol>

## Quick Start

**You must have version 1.3 or greater of memcached**

	$> make
	$> make test
	$> sudo make install
	$> memcached -d

	1> erlmc:start().
	ok

	2> erlmc:stats().
	[{{"localhost",11211},
	  [{evictions,"0"},
	   {total_items,"0"},
	   {curr_items,"0"},
	   {bytes,"0"},
	   {...}|...]}]

	3> erlmc:set(hello, <<"World">>).
	<<>>

	4> erlmc:get(hello).
	<<"World">>

	5> erlmc:add("foo", <<"bar">>).
	<<>>

	6> erlmc:get("foo").
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
* **flush**() -> [{{Host::string(), Port::integer()}, Response::binary()}]
* **flush**(Expiration::integer()) -> [{{Host::string(), Port::integer()}, Response::binary()}]
* **quit**() -> [{{Host::string(), Port::integer()}, Response::binary()}]
* **version**() -> [{{Host::string(), Port::integer()}, Response::binary()}]