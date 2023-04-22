#!/usr/bin/sh

set -x

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
GIVEN=$($LETLOOP check --dry-run checks/check/ checks/check/check-success.scm | md5sum)
if [ "x$GIVEN"="x$EXPECTED" ] 
then
    exit 0
else
    exit 1
fi

