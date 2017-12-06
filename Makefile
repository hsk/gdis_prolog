VERSION = 0.1.7
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
	./gdispl -lib ./lib test/comment.pl > result.txt && diff result.txt test/results/comment.txt
	./gdispl -lib ./lib test/comment2.pl > result.txt && diff result.txt test/results/comment2.txt
	./gdispl -lib ./lib test/call.pl > result.txt && diff result.txt test/results/call.txt
	./gdispl -lib ./lib test/maplist.pl > result.txt && diff result.txt test/results/maplist.txt
	./gdispl -lib ./lib test/foldl.pl > result.txt && diff result.txt test/results/foldl.txt
	./gdispl -lib ./lib test/not.pl > result.txt && diff result.txt test/results/not.txt
	./gdispl -lib ./lib test/sequence.pl > result.txt && diff result.txt test/results/sequence.txt
	./gdispl -lib ./lib test/forall.pl > result.txt && diff result.txt test/results/forall.txt
	./gdispl -lib ./lib test/retract.pl > result.txt && diff result.txt test/results/retract.txt
	./gdispl -lib ./lib test/findall.pl > result.txt && diff result.txt test/results/findall.txt
	./gdispl -lib ./lib test/goal_expansion.pl > result.txt && diff result.txt test/results/goal_expansion.txt
	./gdispl -lib ./lib test/univ.pl > result.txt && diff result.txt test/results/univ.txt
	./gdispl -lib ./lib test/append.pl > result.txt && diff result.txt test/results/append.txt
	./gdispl -lib ./lib test/var.pl > result.txt && diff result.txt test/results/var.txt
	./gdispl -lib ./lib test/current_predicate.pl > result.txt && diff result.txt test/results/current_predicate.txt
	./gdispl -lib ./lib test/repeat.pl > result.txt && diff result.txt test/results/repeat.txt
	./gdispl -lib ./lib test/writeln1.pl > result.txt && diff result.txt test/results/writeln1.txt
	./gdispl -lib ./lib test/writeln2.pl > result.txt && diff result.txt test/results/writeln2.txt
	./gdispl -lib ./lib ./examples/cut.pl > result.txt && diff result.txt test/results/cut.txt
	./gdispl -lib ./lib ./examples/eval2.pl > result.txt && diff result.txt test/results/eval2.txt
	./gdispl -lib ./lib ./examples/eval3.pl > result.txt && diff result.txt test/results/eval3.txt
	./gdispl -lib ./lib ./examples/eval.pl > result.txt && diff result.txt test/results/eval.txt
	./gdispl -lib ./lib ./examples/fib1.pl > result.txt && diff result.txt test/results/fib1.txt
	./gdispl -lib ./lib ./examples/fib2.pl > result.txt && diff result.txt test/results/fib2.txt
	./gdispl -lib ./lib ./examples/fib3.pl > result.txt && diff result.txt test/results/fib3.txt
	./gdispl -lib ./lib ./examples/fib4.pl > result.txt && diff result.txt test/results/fib4.txt
	./gdispl -lib ./lib ./examples/fib5.pl > result.txt && diff result.txt test/results/fib5.txt
	./gdispl -lib ./lib ./examples/hello.pl > result.txt && diff result.txt test/results/hello.txt
	./gdispl -lib ./lib ./examples/lambda.pl > result.txt && diff result.txt test/results/lambda.txt
	./gdispl -lib ./lib ./examples/sum1.pl > result.txt && diff result.txt test/results/sum1.txt
	./gdispl -lib ./lib ./examples/sum2.pl > result.txt && diff result.txt test/results/sum2.txt
	./gdispl -lib ./lib ./examples/sum3.pl > result.txt && diff result.txt test/results/sum3.txt
	./gdispl -lib ./lib ./examples/list.pl > result.txt && diff result.txt test/results/list.txt
	./gdispl -lib ./lib ./examples/op.pl > result.txt && diff result.txt test/results/op.txt
	./gdispl -lib ./lib ./examples/macro.pl > result.txt && diff result.txt test/results/macro.txt
	rm -rf result.txt
	@echo "test ok"
testng:
	./gdispl -lib ./lib test/call2.pl > result.txt && diff result.txt test/results/call2.txt

mkresults:
	./gdispl -lib lib test/append.pl > test/results/append.txt
	./gdispl -lib lib test/var.pl > test/results/var.txt
	./gdispl -lib lib test/comment.pl > test/results/comment.txt
	./gdispl -lib lib test/comment2.pl > test/results/comment2.txt
	./gdispl -lib lib test/call.pl > test/results/call.txt
	./gdispl -lib lib test/maplist.pl > test/results/maplist.txt
	./gdispl -lib lib test/foldl.pl > test/results/foldl.txt
	./gdispl -lib lib test/not.pl > test/results/not.txt
	./gdispl -lib lib test/sequence.pl > test/results/sequence.txt
	./gdispl -lib lib test/forall.pl > test/results/forall.txt
	./gdispl -lib lib test/retract.pl > test/results/retract.txt
	./gdispl -lib lib test/findall.pl > test/results/findall.txt
	./gdispl -lib lib test/goal_expansion.pl > test/results/goal_expansion.txt
	./gdispl -lib lib test/univ.pl > test/results/univ.txt
	./gdispl -lib lib test/current_predicate.pl > test/results/current_predicate.txt
	swipl test/writeln2.pl > test/results/writeln2.txt
	swipl test/writeln1.pl > test/results/writeln1.txt
	swipl test/repeat.pl > test/results/repeat.txt
	swipl test/call2.pl > test/results/call2.txt
	./gdispl -lib lib examples/cut.pl > test/results/cut.txt
	./gdispl -lib lib examples/eval2.pl > test/results/eval2.txt
	./gdispl -lib lib examples/eval3.pl > test/results/eval3.txt
	./gdispl -lib lib examples/eval.pl > test/results/eval.txt
	./gdispl -lib lib examples/fib1.pl > test/results/fib1.txt
	./gdispl -lib lib examples/fib2.pl > test/results/fib2.txt
	./gdispl -lib lib examples/fib3.pl > test/results/fib3.txt
	./gdispl -lib lib examples/fib4.pl > test/results/fib4.txt
	./gdispl -lib lib examples/fib5.pl > test/results/fib5.txt
	./gdispl -lib lib examples/hello.pl > test/results/hello.txt
	./gdispl -lib lib examples/lambda.pl > test/results/lambda.txt
	./gdispl -lib lib examples/sum1.pl > test/results/sum1.txt
	./gdispl -lib lib examples/sum2.pl > test/results/sum2.txt
	./gdispl -lib lib examples/sum3.pl > test/results/sum3.txt
	./gdispl -lib lib examples/list.pl > test/results/list.txt
	./gdispl -lib lib examples/op.pl > test/results/op.txt
	./gdispl -lib lib examples/macro.pl > test/results/macro.txt

clean:
	rm -rf result.txt
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
