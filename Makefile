all:
	cd src; make
	cp src/gdispl .
clean:
	cd src; make clean
	rm -rf gdispl
install:
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
	sudo make install
	make test

.PHONY: test
