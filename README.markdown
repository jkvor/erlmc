# README

Erlang binary protocol memcached client

## Dependencies

Binary protocol build of memcached <http://github.com/dustin/memcached>

## External Documentation

Text Protocol Spec <http://code.sixapart.com/svn/memcached/trunk/server/doc/protocol.txt>

Binary Protocol Spec <http://code.google.com/p/memcached/wiki/MemcacheBinaryProtocol>

## Commands

* get(Key::any()) -> Val::binary()
* get_many([Key::any()]) -> [Val::binary()]
* add(Key::any(), Val::binary()) -> Response::binary()
* add(Key::any(), Val::binary(), Expiration::integer()) -> Response::binary()
* set(Key::any(), Val::binary()) -> Response::binary()
* set(Key::any(), Val::binary(), Expiration::integer()) -> Response::binary()
* replace(Key::any(), Val::binary()) -> Response::binary()
* replace(Key::any(), Val::binary(), Expiration::integer()) -> Response::binary()
* delete(Key::any()) -> Response::binary()
* increment(Key::any(), Val::binary(), Initial::binary(), Expiration::integer()) -> Response::binary()
* decrement(Key::any(), Val::binary(), Initial::binary(), Expiration::integer()) -> Response::binary()
* append(Key::any(), Val::binary()) -> Response::binary()
* prepend(Key::any(), Val::binary()) -> Response::binary()
* stat() -> [{{Host::string(), Port::integer()}, Response::binary()}]
* flush(Expiration::integer()) -> [{{Host::string(), Port::integer()}, Response::binary()}]
* quit() -> [{{Host::string(), Port::integer()}, Response::binary()}]
* version() -> [{{Host::string(), Port::integer()}, Response::binary()}]