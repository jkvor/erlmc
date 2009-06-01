# README

Erlang binary protocol memcached client

## Dependencies

Binary protocol build of memcached <http://github.com/dustin/memcached>

## External Documentation

Text Protocol Spec <http://code.sixapart.com/svn/memcached/trunk/server/doc/protocol.txt>
Binary Protocol Spec <http://code.google.com/p/memcached/wiki/MemcacheBinaryProtocol>

## Commands

* get(Key::any())
* get_many([Key::any()])
* add(Key::any(), Val::binary())
* add(Key::any(), Val::binary(), Expiration::integer())
* set(Key::any(), Val::binary())
* set(Key::any(), Val::binary(), Expiration::integer())
* replace(Key::any(), Val::binary())
* replace(Key::any(), Val::binary(), Expiration::integer())
* delete(Key::any())
* increment(Key::any(), Val::binary(), Initial::binary(), Expiration::integer())
* decrement(Key::any(), Val::binary(), Initial::binary(), Expiration::integer())
* append(Key::any(), Val::binary())
* prepend(Key::any(), Val::binary())
* stat()
* flush(Expiration::integer())
* quit()
* version()