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

push:
	make clean
	git commit -a
	git push
