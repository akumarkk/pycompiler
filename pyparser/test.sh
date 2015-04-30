tests='./testcases/typedargslist_test.py ./testcases/typedargslist_test2.py ./testcases/typedargslist_test3.py ./testcases/typedargslist_test4.py ./testcases/typedargslist_test5.py ./testcases/typedargslist_test6.py ./testcases/typedargslist_test7.py ./testcases/typedargslist_test8.py ./testcases/typedargslist_test9.py ./testcases/typedargslist_test10.py ./testcases/typedargslist_test11.py'

for test in $tests;
do
    echo
    echo
    echo "^^^^^^^^^^^^^^^ TEST START ^^^^^^^^^^^^^^^^^^"
    
    cat $test | pylex | ./pyparse > myoutput.log
    cat $test | pylex | pyparse > mattoutput.log
    echo "Comparing for $test"
    echo "------------ DIFF START ----------------"
    diff myoutput.log mattoutput.log
    echo "____________ DIFF END   ________________"
done

