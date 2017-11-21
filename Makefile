VERSION = 0.1.1
all:
	echo "let version =\"${VERSION}\"" > src/version.ml
	cd src; make
	cp src/gdispl .
clean:
	cd src; make clean
	rm -rf gdispl
install_:
	install -pd $(DEB_DESTDIR)/usr/bin
	install -pm 755 gdispl $(DEB_DESTDIR)/usr/bin
	install -pd $(DEB_DESTDIR)/usr/share/gdispl
	cp -rf lib $(DEB_DESTDIR)/usr/share/gdispl

uninstall:
	rm -rf /usr/bin/gdispl /usr/share/gdispl
test:
	cd test; make test
mkresults:
	cd test; make mkresults
push:
	make clean
	git commit -a
	git push
dev:
	make
	make test
	sudo make install_
ppa_test:
	debuild -S -sa 
	sudo pbuilder --build ../gdis-prolog_${VERSION}.dsc
ppa_up:
	debuild -S -sa 
	dput ppa:h-sakurai/gdis-prolog  ../gdis-prolog_${VERSION}_source.changes 

.PHONY: test
