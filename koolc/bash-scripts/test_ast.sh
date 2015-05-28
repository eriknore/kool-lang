#!/bin/bash

if [ $# != 1 ]; then
    echo "USAGE:   test FILE"
    exit
fi

rm -f reference.res ours.res
echo "Running reference..."
scala -cp koolc_2.11-1.3.1.jar:cafebabe_2.11-1.2.jar koolc.Main --ast $1 > reference.res
echo "Running ours..."
scala -cp target/scala-2.11/classes:cafebabe_2.11-1.2.jar koolc.Main --ast $1 > ours.res
echo "Comparing with diff..."
diff reference.res ours.res
echo "Results written to reference.res and ours.res respectively"
