#!/bin/bash

if [ $# != 1 ]; then
    echo "USAGE:   ./compile.sh  FILE"
    exit
fi

rm -f *.class ref.res our.res
time scala -cp koolc_2.11-1.3.1.jar:cafebabe_2.11-1.2.jar koolc.Main $1
javap -c *.class > ref.res
rm -f *.class
time scala -cp target/scala-2.11/classes:cafebabe_2.11-1.2.jar koolc.Main $1 
javap -c *.class > our.res
rm -f *.class
icdiff ref.res our.res
