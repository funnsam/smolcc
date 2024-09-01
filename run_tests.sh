CC="gcc -pedantic -std=c99 -o a.out"
# CC="tcc -o a.out"
# CC="$(dirname -- "$0")/target/debug/smolcc"

failed=0

cd tests
rm a.out stdout stderr output
for f in *.c; do
    function pass {
        echo -e "\e[1;32mPASSED\e[39m $f\e[0m"
        elapsed
        rm a.out stdout stderr output
    }

    function fail {
        echo -e "\e[1;31mFAILED\e[39m $f\e[0m"
        elapsed
        (( failed+=1 ))
    }

    function elapsed {
        time=$(bc <<< "$(date +%s.%N)-$start")
        echo "  took ${time}s"
    }

    start=`date +%s.%N`
    $CC $f >stdout 2>stderr && {
        chmod +x a.out
        ./a.out >output 2>&1 && {
            cmp -s output $f.expected && pass || {
                fail
                cat $f.expected
                echo "\e[1mExpected output:\e[0m"
                echo "\e[1mProgram output:\e[0m"
                cat output
            }
        } || fail
    } || {
        fail
        echo -e "\e[1mCompiler stdout:\e[0m"
        cat stdout
        echo -e "\e[1mCompiler stderr:\e[0m"
        cat stderr
    }
done

echo -e "\e[1;34mSUMMARY\e[0m failed $failed test(s)"
exit $failed
