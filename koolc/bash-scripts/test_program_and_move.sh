#!/bin/bash

if [ $# != 1 ]; then
    echo "USAGE:   test FILE"
    exit
fi

path="testprograms/lab3/valid"
f="$1.kool"
rm -f reference.res ours.res
echo "Running reference..."
scala -cp koolc_2.11-1.3.1.jar:cafebabe_2.11-1.2.jar koolc.Main --ast $path/$f > reference.res
echo "Running ours..."
scala -cp target/scala-2.11/classes koolc.Main --ast $path/$f > ours.res
echo "Comparing with diff..."
if [[ -z $(diff reference.res ours.res) ]]; then
            echo "PASSED: moving to $path/tested/"
            $(mv $path/$f $path/tested)
        else
            echo "FAILED"
            exit
        fi
echo "Results written to reference.res and ours.res respectively"
