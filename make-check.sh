#!/usr/bin/sh

set -x

# XXX: Illegal according to github actions.
# set +o pipefail

ROOT=$(pwd)

echo '(scheme-version)' | $LETLOOP repl

$LETLOOP check checks/check/ checks/check/check-error.scm
if [ $? -eq 0 ]; then
  exit 1
fi

$LETLOOP check checks/check/ checks/check/check-fail.scm
if [ $? -eq 0 ]; then
  exit 1
fi

$LETLOOP check checks/check/ checks/check/check-empty.scm
if [ $? -eq 0 ]; then
  exit 1
fi

$LETLOOP check checks/check/ checks/check/check-success.scm
if [ $? -eq 0 ]; then
  echo success
else
  exit 1
fi

EXPECTED="b4907e17e91609ea83394b6079794395"
GIVEN=$($LETLOOP check --dry-run checks/check/ checks/check/check-success.scm | md5sum | cut -d " " -f1)
if [ "x$GIVEN" = "x$EXPECTED" ]; then
  echo "success!"
else
  echo "failure..."
  exit 1
fi

echo "* Testing letoop compile, and libraries embedding 001"

$LETLOOP exec examples/ examples/codex/base.scm codex-usage
if [ $? -eq 0 ]; then
  echo success
else
  exit 1
fi

$LETLOOP compile examples/ examples/codex/base.scm codex-usage
if [ $? -eq 0 ]; then
  echo success
else
  exit 1
fi

scheme -b /tmp/letloop.boot
if [ $? -eq 0 ]; then
  echo success
else
  exit 1
fi

$LETLOOP benchmark library/ checks/check/ checks/check/check-success.scm ~benchmark-000
if [ $? -eq 0 ]; then
  echo success
else
  exit 1
fi

$LETLOOP benchmark library/ checks/check/ checks/check/check-success.scm ~benchmark-000
if [ $? -eq 0 ]; then
  echo success
else
  exit 1
fi
