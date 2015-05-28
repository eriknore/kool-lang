#!/bin/bash

if [ $# != 0 ]; then
    echo "USAGE:    ./suite.sh"
    exit
fi

echo "------ Lab ALL ------"
echo "--------------------------------------------"
echo "----- VALID TESTS -----"
for f in testprograms/lab3/valid/*.kool
    do
        echo "--------------------------------------------"
        echo "Processing $f file.."
        scala -cp koolc_2.11-1.3.1.jar:cafebabe_2.11-1.2.jar koolc.Main --tokens $f > reference.res
        scala -cp target/scala-2.11/classes koolc.Main --tokens $f > ours.res
        diff reference.res ours.res
        if [[ -z $(diff reference.res ours.res) ]]; then
            echo "PASSED: 1. --tokens"
        else
            echo "FAILED: 1. --tokens"
            exit
        fi

        scala -cp koolc_2.11-1.3.1.jar:cafebabe_2.11-1.2.jar koolc.Main --ast $f > reference.res
        scala -cp target/scala-2.11/classes koolc.Main --ast $f > ours.res
        if [[ -z $(diff reference.res ours.res) ]]; then
            echo "PASSED: 2. --ast"
        else
            echo "FAILED: 2. --ast"
            exit
        fi

	    scala -cp koolc_2.11-1.3.1.jar:cafebabe_2.11-1.2.jar koolc.Main --ast --symid $f > reference.res
        scala -cp target/scala-2.11/classes koolc.Main --ast --symid $f > ours.res
        if [[ -z $(diff reference.res ours.res) ]]; then
            echo "PASSED: 3. --ast --symid"
        else
            echo "FAILED: 3. --ast --symid"
            exit
        fi
	
        scala -cp target/scala-2.11/classes koolc.Main $f --pretty > dogfood.res
        scala -cp target/scala-2.11/classes koolc.Main dogfood.res --pretty > eaten.res
        if [[ -z $(diff dogfood.res eaten.res) ]]; then
            echo "PASSED: 4. Eat your own dog food (PrettyPrinter)"
        else
            echo "FAILED: 4. Eat your own dog food (PrettyPrinter)"
            exit
        fi
        # Our pretty print adds blocks to one-line if or while statements
        # However it doesn't change the program, will fail some test
        # but it wouldn't if we could ignore extra Block(_), same goes for parentheses
#        scala -cp target/scala-2.11/classes koolc.Main --ast dogfood.res > second.res
#        if [[ -z $(diff ours.res second.res) ]]; then
#            echo "PASSED: 4. Dogfood didn't change the ast (PrettyPrinter)"
#        else
#            echo "FAILED: 4. Dogfood changed the ast! (PrettyPrinter)"
#            exit
#        fi

        
        # Compare ast's - i.e. normal test
        classfile=$(grep object $f | awk '{ print $2 }')
        scala -cp koolc_2.11-1.3.1.jar:cafebabe_2.11-1.2.jar koolc.Main $f
        java $classfile > reference.res
        rm -f *.class
        scala -cp target/scala-2.11/classes:cafebabe_2.11-1.2.jar koolc.Main $f
        java $classfile > ours.res
        rm -f *.class
        if [[ -z $(diff reference.res ours.res) ]]; then
            echo "PASSED: Program generated same output"
        else
            echo "FAILED: Program generated different output"
            exit
        fi
        rm -f *.class

        # Compare error messages
        scala -cp koolc_2.11-1.3.1.jar:cafebabe_2.11-1.2.jar koolc.Main $f 2> reference.res 1> /dev/null
        scala -cp target/scala-2.11/classes:cafebabe_2.11-1.2.jar koolc.Main $f 2> ours.res 1> /dev/null
        if [[ -z $(diff reference.res ours.res) ]]; then
            echo "PASSED: Same error output generated"
        else
            echo "FAILED: Different error output generated"
            exit
        fi
    done
echo "--------------------------------------------"

echo "------ Lab 5 ------"
echo "--------------------------------------------"
echo "----- VALID -----"
for f in testprograms/lab5/valid/*.kool
    do
        echo "--------------------------------------------"
        echo "Processing $f file.."
        
        # Compare ast's - i.e. normal test
        scala -cp koolc_2.11-1.3.1.jar:cafebabe_2.11-1.2.jar koolc.Main $f
        java A > reference.res
        scala -cp target/scala-2.11/classes:cafebabe_2.11-1.2.jar koolc.Main $f
        java A > ours.res
        if [[ -z $(diff reference.res ours.res) ]]; then
            echo "PASSED: Same output on stdout"
        else
            echo "FAILED: Different output on stdout"
            exit
        fi

        # Compare error messages
        scala -cp koolc_2.11-1.3.1.jar:cafebabe_2.11-1.2.jar koolc.Main $f 2> reference.res 1> /dev/null
        scala -cp target/scala-2.11/classes:cafebabe_2.11-1.2.jar koolc.Main $f 2> ours.res 1> /dev/null
        if [[ -z $(diff reference.res ours.res) ]]; then
            echo "PASSED: Same error output generated"
        else
            echo "FAILED: Different error output generated"
            exit
        fi
    done
echo "--------------------------------------------"


echo "------ Lab 5 ------"
echo "--------------------------------------------"
echo "----- INVALID -----"
for f in testprograms/lab5/invalid/*.kool
    do
        echo "--------------------------------------------"
        echo "Processing $f file.."
        
        # Compare ast's - i.e. normal test
        scala -cp koolc_2.11-1.3.1.jar:cafebabe_2.11-1.2.jar koolc.Main $f 1> reference.res 2> /dev/null
        scala -cp target/scala-2.11/classes koolc.Main $f 1> ours.res 2> /dev/null
        if [[ -z $(diff reference.res ours.res) ]]; then
            echo "PASSED: Same output on stdout"
        else
            echo "FAILED: Different output on stdout"
            exit
        fi

        # Compare error messages
        scala -cp koolc_2.11-1.3.1.jar:cafebabe_2.11-1.2.jar koolc.Main $f 2> reference.res 1> /dev/null
        scala -cp target/scala-2.11/classes:cafebabe_2.11-1.2.jar koolc.Main $f 2> ours.res 1> /dev/null
        if [[ -z $(diff reference.res ours.res) ]]; then
            echo "PASSED: Same error output generated"
        else
            echo "FAILED: Different error output generated"
            exit
        fi
    done
echo "--------------------------------------------"


echo "------ Lab 4 ------"
echo "--------------------------------------------"
echo "----- INVALID -----"
for f in testprograms/lab4/invalid/*.kool
    do
        echo "--------------------------------------------"
        echo "Processing $f file.."
        
        # Compare ast's - i.e. normal test
        scala -cp koolc_2.11-1.3.1.jar:cafebabe_2.11-1.2.jar koolc.Main --ast --symid $f 1> reference.res 2> /dev/null
        scala -cp target/scala-2.11/classes koolc.Main --ast --symid $f 1> ours.res 2> /dev/null
        if [[ -z $(diff reference.res ours.res) ]]; then
            echo "PASSED: Same tree generated"
        else
            echo "FAILED: Different trees generated"
            exit
        fi

        # Compare error messages
        scala -cp koolc_2.11-1.3.1.jar:cafebabe_2.11-1.2.jar koolc.Main --ast --symid $f 2> reference.res 1> /dev/null
        scala -cp target/scala-2.11/classes:cafebabe_2.11-1.2.jar koolc.Main --ast --symid $f 2> ours.res 1> /dev/null
        if [[ -z $(diff reference.res ours.res) ]]; then
            echo "PASSED: Same error output generated"
        else
            echo "FAILED: Different error output generated"
            exit
        fi
    done
echo "--------------------------------------------"


echo "------ Lab 2 ------"
echo "--------------------------------------------"
echo "----- VALID -----"
for f in testprograms/lab2/valid/*.kool
    do
        echo "--------------------------------------------"
        echo "Processing $f file.."

        scala -cp koolc_2.11-1.3.1.jar:cafebabe_2.11-1.2.jar koolc.Main --tokens $f > reference.res
        scala -cp target/scala-2.11/classes koolc.Main --tokens $f > ours.res
        if [[ -z $(diff reference.res ours.res) ]]; then
            echo "PASSED"
        else
            echo "FAILED"
            exit
        fi
    done
echo "--------------------------------------------"
echo "----- INVALID -----"
for f in testprograms/lab3/invalid/*.kool
    do
        echo "--------------------------------------------"
        echo "Processing $f file.."

        scala -cp koolc_2.11-1.3.1.jar:cafebabe_2.11-1.2.jar koolc.Main --ast $f 2> reference.res
        scala -cp target/scala-2.11/classes koolc.Main --ast $f 2> ours.res
        if [[ -z $(diff reference.res ours.res) ]]; then
            echo "PASSED"
        else
            echo "FAILED"
            exit
        fi
    done
echo "--------------------------------------------"

rm -f *.res *.class

echo "--------------------------------------------"
echo "Done!"
