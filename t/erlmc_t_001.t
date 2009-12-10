Script started on Thu 10 Dec 2009 11:25:27 AM PST
]0;ddossot@phrog: ~/erlang/dev/erlmc-ddossot[01;32mddossot@phrog[00m:[01;34m~/erlang/dev/erlmc-ddossot[00m$ make test
sh ebin/erlmc.app.in 0.2
mkdir -p ebin/
(cd src;make)
make[1]: Entering directory `/home/ddossot/erlang/dev/erlmc-ddossot/src'
make[1]: Nothing to be done for `all'.
make[1]: Leaving directory `/home/ddossot/erlang/dev/erlmc-ddossot/src'
prove t/*.t
t/erlmc_t_001.t ..                      t/erlmc_t_001.t .. [31mNo subtests run [0m

Test Summary Report
-------------------
[31mt/erlmc_t_001.t (Wstat: 0 Tests: 0 Failed: 0)[0m
[31m  Parse errors: No plan found in TAP output[0m
Files=1, Tests=0,  0 wallclock secs ( 0.01 usr +  0.00 sys =  0.01 CPU)
Result: FAIL
make: *** [test] Error 1
]0;ddossot@phrog: ~/erlang/dev/erlmc-ddossot[01;32mddossot@phrog[00m:[01;34m~/erlang/dev/erlmc-ddossot[00m$ make test; erl -pa ebintest[K[Kmake test; erl -pa ebin[12Phg[C[C[C[C[Cll[Kcd /home/ddossot/erlang/dev/erlmc-ddossot/ll[Khg ebin[12@make ; erl -pa[C[C[C[C[Ctest[K[Kmake test; erl -pa ebin[12Phg[C[C[C[C[Cll[Kcd /home/ddossot/erlang/dev/erlmc-ddossot/ll[Khg ebin[12@make ; erl -pa[C[C[C[C[Ctest[K[Kmake ; script ./t/erlmc_t_001.t
sh ebin/erlmc.app.in 0.2
mkdir -p ebin/
(cd src;make)
make[1]: Entering directory `/home/ddossot/erlang/dev/erlmc-ddossot/src'
make[1]: Nothing to be done for `all'.
make[1]: Leaving directory `/home/ddossot/erlang/dev/erlmc-ddossot/src'
Script started, file is ./t/erlmc_t_001.t
]0;ddossot@phrog: ~/erlang/dev/erlmc-ddossot[01;32mddossot@phrog[00m:[01;34m~/erlang/dev/erlmc-ddossot[00m$ make ; erl -pa ebin[1P[1P[1P[1P[1P[1P[1P
]0;ddossot@phrog: ~/erlang/dev/erlmc-ddossot[01;32mddossot@phrog[00m:[01;34m~/erlang/dev/erlmc-ddossot[00m$ 
]0;ddossot@phrog: ~/erlang/dev/erlmc-ddossot[01;32mddossot@phrog[00m:[01;34m~/erlang/dev/erlmc-ddossot[00m$ escript ./t/erlmc_t_001.t
escript: Premature end of file reached
]0;ddossot@phrog: ~/erlang/dev/erlmc-ddossot[01;32mddossot@phrog[00m:[01;34m~/erlang/dev/erlmc-ddossot[00m$ escript ./t/erlmc_t_001.t
# Current time local 2009-12-10 11:26:25
# Using etap version "0.3.4"
ok 1  - noop
ok 2  - erlmc connect to default memcached server ok
ok 3  - set ok
ok 4  - add ok
ok 5  - get ok
ok 6  - delete ok
ok 7  - add ok
ok 8  - get ok
ok 9  - append ok
ok 10  - get ok
ok 11  - prepend ok
ok 12  - get ok
ok 13  - delete ok
ok 14  - get ok
ok 15  - get_many ok
ok 16  - flush ok
ok 17  - stats/0 ok
ok 18  - stats/2 ok
ok 19  - quit ok
ok 20  - has_server ok
ok 21  - remove ok
ok 22  - second has_server ok
1..22
]0;ddossot@phrog: ~/erlang/dev/erlmc-ddossot[01;32mddossot@phrog[00m:[01;34m~/erlang/dev/erlmc-ddossot[00m$ rake test
rake aborted!
No Rakefile found (looking for: rakefile, Rakefile, rakefile.rb, Rakefile.rb)
/usr/lib/ruby/1.8/rake.rb:2143:in `raw_load_rakefile'
(See full trace by running task with --trace)
]0;ddossot@phrog: ~/erlang/dev/erlmc-ddossot[01;32mddossot@phrog[00m:[01;34m~/erlang/dev/erlmc-ddossot[00m$ maket[K test
sh ebin/erlmc.app.in 0.2
mkdir -p ebin/
(cd src;make)
make[1]: Entering directory `/home/ddossot/erlang/dev/erlmc-ddossot/src'
make[1]: Nothing to be done for `all'.
make[1]: Leaving directory `/home/ddossot/erlang/dev/erlmc-ddossot/src'
prove t/*.t
t/erlmc_t_001.t .. t/erlmc_t_001.t .. 1/?                          t/erlmc_t_001.t .. ok
[32mAll tests successful.
[0mFiles=1, Tests=22,  0 wallclock secs ( 0.04 usr  0.00 sys +  0.14 cusr  0.02 csys =  0.20 CPU)
Result: PASS
]0;ddossot@phrog: ~/erlang/dev/erlmc-ddossot[01;32mddossot@phrog[00m:[01;34m~/erlang/dev/erlmc-ddossot[00m$ git st
# On branch master
# Changed but not updated:
#   (u