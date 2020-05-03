#!/bin/sh

INPUTS=inputs/*
mkdir usertest
for i in $INPUTS
	do
		filename=${i%.*}
		name=${filename##*/}
		echo "Generating code for input file $i"
		output="${name}Test.out"
		./Micro $i > usertest/$output
	done

OUTPUTS=outputs/*
mkdir outtest
g++ -o tiny tinyNew.C
for j in $OUTPUTS
        do
                filename=${j%.*}
                name=${filename##*/}
                echo "Expected output generated for $j"
                output2="${name}Test_o.out"
		if [[ $j == *"step4_testcase"* ]]
		then
			echo 2 4 25 17 6 32 1 4 15 4 | ./tiny $j > outtest/$output2
			
		elif [[ $j == *"test_mult"* ]]
		then
			echo 3 2 | ./tiny $j > outtest/$output2
		else
			./tiny $j > outtest/$output2
		fi
        done

# Checking your outputs
USERTEST=usertest/*
mkdir userout
#g++ -o tiny tinyNew.C
for j in $USERTEST
        do
                filename=${j%.*}
                name=${filename##*/}
                echo -e "\n\n***Testing output of $j***"
                output2="${name}_out.out"
                outtest="${name}_o.out"
		if [[ $j == *"step4_testcase"* ]]
		then
			echo 2 4 25 17 6 32 1 4 15 4 | ./tiny $j > userout/$output2
			
		elif [[ $j == *"test_mult"* ]]
		then
			echo 3 2 | ./tiny $j > userout/$output2
		else
			./tiny $j > userout/$output2
		fi
		diff -y -B -b userout/$output2 outtest/$outtest
        done
