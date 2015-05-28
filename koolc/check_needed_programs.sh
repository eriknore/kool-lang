#!/bin/bash

NEEDED_COMMANDS="bnfc ghc alex happy"

for cmd in ${NEEDED_COMMANDS} ; do
    if ! command -v ${cmd} &> /dev/null ; then
    	echo
    	echo "The following programs are needed to generate the kool-parser:"
    	echo "	bnfc ghc alex happy"
    	echo
        echo The following program was not found: ${cmd}
        echo
        exit 1
    fi
done

touch .prerequisites_ok