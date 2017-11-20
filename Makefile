all:
	cd src; make
	cp src/gdispl .
clean:
	cd src; make clean
	rm -rf gdispl
install_:
	install -pd $(DEB_DESTDIR)/usr/bin
	install -pm 755 gdispl $(DEB_DESTDIR)/usr/bin
	install -pd $(DEB_DESTDIR)/usr/share/gdispl
	install -pm 755 lib $(DEB_DESTDIR)/usr/share/gdispl

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
	sudo make install_
	make test
ppa:
	debuild -S -sa 
	sudo pbuilder --build ../gdis-prolog_0.0.9.dsc
up:
	#debuild -S
	debuild -S -sa 
#	dput -f gdis-prolog  ../gdis-prolog_0.0.9_source.changes 
	dput ppa:h-sakurai/gdis-prolog  ../gdis-prolog_0.0.9_source.changes 

.PHONY: test
