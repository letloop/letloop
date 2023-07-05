#!/usr/bin/sh

set -x

ROOT=$(pwd)

$LETLOOP check checks/check/ checks/check/check-error.scm
if [ $? -eq 0 ]
then
    exit 1
fi

$LETLOOP check checks/check/ checks/check/check-fail.scm
if [ $? -eq 0 ]
then
    exit 1
fi

$LETLOOP check checks/check/ checks/check/check-empty.scm
if [ $? -eq 0 ]
then
    exit 1
fi

$LETLOOP check checks/check/ checks/check/check-success.scm
if [ $? -eq 0 ]
then
    echo success
else
    exit 1
fi

EXPECTED="b4907e17e91609ea83394b6079794395"
GIVEN=$($LETLOOP check --dry-run checks/check/ checks/check/check-success.scm | md5sum | cut -d " " -f1)
if [ "x$GIVEN" = "x$EXPECTED" ]
then
    echo "success!"
else
    echo "failure..."
    exit 1
fi

echo "* Testing letoop compile, and libraries embedding 000"

WORK=$(mktemp -d "/tmp/letloop-make-check-XXXXXX")
cp examples/blake3sum.scm $WORK/
cd $WORK
$LETLOOP exec blake3sum.scm -- blake3sum.scm
if [ $? -eq 0 ]
then
    echo success
else
    exit 1
fi
$LETLOOP compile blake3sum.scm
if [ $? -eq 0 ]
then
    echo success
else
    exit 1
fi
./a.out blake3sum.scm
if [ $? -eq 0 ]
then
    echo success
else
    exit 1
fi

cd $ROOT

echo "* Testing letoop compile, and libraries embedding 001"

$LETLOOP exec examples/ examples/codex.scm
if [ $? -eq 0 ]
then
    echo success
else
    exit 1
fi

$LETLOOP compile examples/ examples/codex.scm
if [ $? -eq 0 ]
then
    echo success
else
    exit 1
fi

./a.out
if [ $? -eq 0 ]
then
    echo success
else
    exit 1
fi
