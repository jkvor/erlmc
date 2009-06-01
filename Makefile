LIBDIR=`erl -eval 'io:format("~s~n", [code:lib_dir()])' -s init stop -noshell`
VERSION=0.1.0
PKGNAME=mcerlang

all:
	mkdir -p ebin/
	(cd src;$(MAKE))

test: all
	prove t/*.t

clean:
	(cd src;$(MAKE) clean)
	rm -rf erl_crash.dump *.beam *.hrl

package: clean
	@mkdir $(PKGNAME)-$(VERSION)/ && cp -rf ebin Makefile README.markdown src support t $(PKGNAME)-$(VERSION)
	@COPYFILE_DISABLE=true tar zcf $(PKGNAME)-$(VERSION).tgz $(PKGNAME)-$(VERSION)
	@rm -rf $(PKGNAME)-$(VERSION)/

install:
	mkdir -p $(prefix)/$(LIBDIR)/$(PKGNAME)-$(VERSION)/{ebin,include}
	for i in ebin/*.beam ebin/*.app include/*.hrl; do install $$i $(prefix)/$(LIBDIR)/$(PKGNAME)-$(VERSION)/$$i ; done
