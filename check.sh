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
    exit 0
else
    exit 1
fi

