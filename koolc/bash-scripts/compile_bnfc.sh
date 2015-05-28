#!/bin/bash

if [ $# != 1 ]; then
    echo "USAGE:   ./compile.sh  FILE"
    exit
fi

rm -f *.class
scala -cp target/scala-2.11/classes:cafebabe_2.11-1.2.jar koolc.Main --bnfc $1 
