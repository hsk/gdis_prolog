test:
	../gdispl -lib ../lib comment.pl > result.txt && diff result.txt results/comment.txt
#	../gdispl -lib ../lib comment2.pl > result.txt && diff result.txt results/comment2.txt
	../gdispl -lib ../lib call.pl > result.txt && diff result.txt results/call.txt
	../gdispl -lib ../lib maplist.pl > result.txt && diff result.txt results/maplist.txt
	../gdispl -lib ../lib foldl.pl > result.txt && diff result.txt results/foldl.txt
	../gdispl -lib ../lib not.pl > result.txt && diff result.txt results/not.txt
	../gdispl -lib ../lib sequence.pl > result.txt && diff result.txt results/sequence.txt
	../gdispl -lib ../lib forall.pl > result.txt && diff result.txt results/forall.txt
	../gdispl -lib ../lib retract.pl > result.txt && diff result.txt results/retract.txt
	../gdispl -lib ../lib findall.pl > result.txt && diff result.txt results/findall.txt
#	../gdispl -lib ../lib ../examples/a.pl > result.txt && diff result.txt results/a.txt
	../gdispl -lib ../lib ../examples/cut.pl > result.txt && diff result.txt results/cut.txt
	../gdispl -lib ../lib ../examples/eval2.pl > result.txt && diff result.txt results/eval2.txt
#	../gdispl -lib ../lib ../examples/eval3.pl > result.txt && diff result.txt results/eval3.txt
	../gdispl -lib ../lib ../examples/eval.pl > result.txt && diff result.txt results/eval.txt
	../gdispl -lib ../lib ../examples/fib1.pl > result.txt && diff result.txt results/fib1.txt
	../gdispl -lib ../lib ../examples/fib2.pl > result.txt && diff result.txt results/fib2.txt
#	../gdispl -lib ../lib ../examples/fib3.pl > result.txt && diff result.txt results/fib3.txt
	../gdispl -lib ../lib ../examples/fib4.pl > result.txt && diff result.txt results/fib4.txt
	../gdispl -lib ../lib ../examples/fib5.pl > result.txt && diff result.txt results/fib5.txt
	../gdispl -lib ../lib ../examples/hello.pl > result.txt && diff result.txt results/hello.txt
#	../gdispl -lib ../lib ../examples/lambda.pl > result.txt && diff result.txt results/lambda.txt
	../gdispl -lib ../lib ../examples/sum1.pl > result.txt && diff result.txt results/sum1.txt
	../gdispl -lib ../lib ../examples/sum2.pl > result.txt && diff result.txt results/sum2.txt
	../gdispl -lib ../lib ../examples/sum3.pl > result.txt && diff result.txt results/sum3.txt
	../gdispl -lib ../lib ../examples/list.pl > result.txt && diff result.txt results/list.txt
	rm -rf result.txt
	@echo "test ok"
mkresults:
	../gdispl -lib ../lib comment.pl > results/comment.txt
#	../gdispl -lib ../lib comment2.pl > results/comment2.txt
	../gdispl -lib ../lib call.pl > results/call.txt
	../gdispl -lib ../lib maplist.pl > results/maplist.txt
	../gdispl -lib ../lib foldl.pl > results/foldl.txt
	../gdispl -lib ../lib not.pl > results/not.txt
	../gdispl -lib ../lib sequence.pl > results/sequence.txt
	../gdispl -lib ../lib forall.pl > results/forall.txt
	../gdispl -lib ../lib retract.pl > results/retract.txt
	../gdispl -lib ../lib findall.pl > results/findall.txt
#	../gdispl -lib ../lib ../examples/a.pl > results/a.txt
	../gdispl -lib ../lib ../examples/cut.pl > results/cut.txt
	../gdispl -lib ../lib ../examples/eval2.pl > results/eval2.txt
#	../gdispl -lib ../lib ../examples/eval3.pl > results/eval3.txt
	../gdispl -lib ../lib ../examples/eval.pl > results/eval.txt
	../gdispl -lib ../lib ../examples/fib1.pl > results/fib1.txt
	../gdispl -lib ../lib ../examples/fib2.pl > results/fib2.txt
#	../gdispl -lib ../lib ../examples/fib3.pl > results/fib3.txt
	../gdispl -lib ../lib ../examples/fib4.pl > results/fib4.txt
	../gdispl -lib ../lib ../examples/fib5.pl > results/fib5.txt
	../gdispl -lib ../lib ../examples/hello.pl > results/hello.txt
#	../gdispl -lib ../lib ../examples/lambda.pl > results/lambda.txt
	../gdispl -lib ../lib ../examples/sum1.pl > results/sum1.txt
	../gdispl -lib ../lib ../examples/sum2.pl > results/sum2.txt
	../gdispl -lib ../lib ../examples/sum3.pl > results/sum3.txt
	../gdispl -lib ../lib ../examples/list.pl > results/list.txt

clean:
	rm -rf result.txt
