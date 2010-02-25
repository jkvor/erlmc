LIBDIR=`erl -eval 'io:format("~s~n", [code:lib_dir()])' -s init stop -noshell`
VERSION=0.2
PKGNAME=erlmc

all: emake

emake: app
	erl -make
	
app:
	sh ebin/$(PKGNAME).app.in $(VERSION)

test: all
	prove t/*.t

clean:
	rm -rf erl_crash.dump ebin/*.beam ebin/*.app

install:
	mkdir -p $(prefix)/$(LIBDIR)/$(PKGNAME)-$(VERSION)/{ebin,include}
	for i in ebin/*.beam ebin/*.app include/*.hrl; do install $$i $(prefix)/$(LIBDIR)/$(PKGNAME)-$(VERSION)/$$i ; done
