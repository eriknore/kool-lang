#!/bin/bash

if [ $# != 1 ]; then
    echo "USAGE:   ./compile.sh  FILE"
    exit
fi

rm -f *.class
scala -cp koolc_2.11-1.3.1.jar:cafebabe_2.11-1.2.jar koolc.Main $1
