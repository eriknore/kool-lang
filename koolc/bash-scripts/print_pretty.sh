#!/bin/bash

if [ $# != 1 ]; then
    echo "USAGE:   test FILE"
    exit
fi

scala -cp target/scala-2.11/classes koolc.Main --pretty $1 > pretty.res
scala -cp target/scala-2.11/classes koolc.Main pretty.res --pretty > reiterated.res
echo "Compare $1 and reiterated.res"
