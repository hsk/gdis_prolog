test:
	gdispl comment.pl > result.txt && diff result.txt results/comment.txt
	gdispl comment2.pl > result.txt && diff result.txt results/comment2.txt
	gdispl call.pl > result.txt && diff result.txt results/call.txt
	gdispl maplist.pl > result.txt && diff result.txt results/maplist.txt
	gdispl foldl.pl > result.txt && diff result.txt results/foldl.txt
	gdispl not.pl > result.txt && diff result.txt results/not.txt
	gdispl sequence.pl > result.txt && diff result.txt results/sequence.txt
	gdispl forall.pl > result.txt && diff result.txt results/forall.txt
	gdispl retract.pl > result.txt && diff result.txt results/retract.txt
	gdispl findall.pl > result.txt && diff result.txt results/findall.txt
#	gdispl ../examples/a.pl > result.txt && diff result.txt results/a.txt
	gdispl ../examples/cut.pl > result.txt && diff result.txt results/cut.txt
	gdispl ../examples/eval2.pl > result.txt && diff result.txt results/eval2.txt
	gdispl ../examples/eval3.pl > result.txt && diff result.txt results/eval3.txt
	gdispl ../examples/eval.pl > result.txt && diff result.txt results/eval.txt
	gdispl ../examples/fib1.pl > result.txt && diff result.txt results/fib1.txt
	gdispl ../examples/fib2.pl > result.txt && diff result.txt results/fib2.txt
	gdispl ../examples/fib3.pl > result.txt && diff result.txt results/fib3.txt
	gdispl ../examples/fib4.pl > result.txt && diff result.txt results/fib4.txt
	gdispl ../examples/fib5.pl > result.txt && diff result.txt results/fib5.txt
	gdispl ../examples/hello.pl > result.txt && diff result.txt results/hello.txt
	gdispl ../examples/lambda.pl > result.txt && diff result.txt results/lambda.txt
	gdispl ../examples/sum1.pl > result.txt && diff result.txt results/sum1.txt
	gdispl ../examples/sum2.pl > result.txt && diff result.txt results/sum2.txt
	gdispl ../examples/sum3.pl > result.txt && diff result.txt results/sum3.txt
	gdispl ../examples/list.pl > result.txt && diff result.txt results/list.txt
	rm -rf result.txt
	@echo "test ok"
mkresults:
	gdispl comment.pl > results/comment.txt
	gdispl comment2.pl > results/comment2.txt
	gdispl call.pl > results/call.txt
	gdispl maplist.pl > results/maplist.txt
	gdispl foldl.pl > results/foldl.txt
	gdispl not.pl > results/not.txt
	gdispl sequence.pl > results/sequence.txt
	gdispl forall.pl > results/forall.txt
	gdispl retract.pl > results/retract.txt
	gdispl findall.pl > results/findall.txt
#	gdispl ../examples/a.pl > results/a.txt
	gdispl ../examples/cut.pl > results/cut.txt
	gdispl ../examples/eval2.pl > results/eval2.txt
	gdispl ../examples/eval3.pl > results/eval3.txt
	gdispl ../examples/eval.pl > results/eval.txt
	gdispl ../examples/fib1.pl > results/fib1.txt
	gdispl ../examples/fib2.pl > results/fib2.txt
	gdispl ../examples/fib3.pl > results/fib3.txt
	gdispl ../examples/fib4.pl > results/fib4.txt
	gdispl ../examples/fib5.pl > results/fib5.txt
	gdispl ../examples/hello.pl > results/hello.txt
	gdispl ../examples/lambda.pl > results/lambda.txt
	gdispl ../examples/sum1.pl > results/sum1.txt
	gdispl ../examples/sum2.pl > results/sum2.txt
	gdispl ../examples/sum3.pl > results/sum3.txt
	gdispl ../examples/list.pl > results/list.txt

clean:
	rm -rf result.txt
