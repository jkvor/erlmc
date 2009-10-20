LIBDIR=`erl -eval 'io:format("~s~n", [code:lib_dir()])' -s init stop -noshell`
VERSION=0.2
PKGNAME=erlmc

all: app
	mkdir -p ebin/
	(cd src;$(MAKE))

app:
	sh ebin/$(PKGNAME).app.in $(VERSION)

test: all
	prove t/*.t

clean:
	(cd src;$(MAKE) clean)
	rm -rf erl_crash.dump *.beam *.hrl ebin/*.app

package: clean
	@mkdir $(PKGNAME)-$(VERSION)/ && cp -rf ebin include Makefile README.markdown src support t $(PKGNAME)-$(VERSION)
	@COPYFILE_DISABLE=true tar zcf $(PKGNAME)-$(VERSION).tgz $(PKGNAME)-$(VERSION)
	@rm -rf $(PKGNAME)-$(VERSION)/

install:
	mkdir -p $(prefix)/$(LIBDIR)/$(PKGNAME)-$(VERSION)/{ebin,include}
	for i in ebin/*.beam ebin/*.app include/*.hrl; do install $$i $(prefix)/$(LIBDIR)/$(PKGNAME)-$(VERSION)/$$i ; done
