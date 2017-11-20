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
	sudo dpkg-buildpackage -us -uc
	sudo pbuilder --build ../gdis-prolog_0.0.7.dsc
.PHONY: test
