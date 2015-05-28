#!/bin/bash

if [ $# != 1 ]; then
    echo "USAGE:   test FILE"
    exit
fi

rm -f reference.res ours.res
echo "Running reference..."
scala -cp cafebabe_2.11-1.2.jar:koolc_2.11-1.3.1.jar koolc.Main $1 1> /dev/null 2> reference.res
echo "Running ours..."
scala -cp target/scala-2.11/classes:cafebabe_2.11-1.2.jar koolc.Main $1 1> /dev/null 2> ours.res
echo "Comparing with diff..."
icdiff reference.res ours.res
echo "Results written to reference.res and ours.res respectively"
rm -f *.class
