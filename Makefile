all:
	cd src; make
	cp src/gdispl .
clean:
	cd src; make clean
	rm -rf gdispl
install_:
	mkdir -p /usr/share/gdispl/
	cp -rf lib /usr/share/gdispl/
	cp gdispl /usr/bin/
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
	debuild -uc -us -S -sa 
	sudo pbuilder --build ../gdis-prolog_0.0.9.dsc
up:
	#debuild -S
	debuild -uc -us -S -sa 
	dput -f gdis-prolog  ../gdis-prolog_0.0.9_source.changes 

.PHONY: test
