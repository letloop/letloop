#!/bin/sh

LETLOOP_ROOT=$1

if [ -z ${LETLOOP_ROOT+x} ]; then
    echo "LETLOOP_ROOT is unset"
    exit 42
fi

LETLOOP_PREFIX=$2

if [ -z ${LETLOOP_PREFIX+x} ]; then
    echo "LETLOOP_PREFIX is unset"
    exit 42
fi

LETLOOP_FLAVOR=$3

if [ -z ${LETLOOP_FLAVOR+x} ]; then
    echo "LETLOOP_FLAVOR is unset"
    exit 42
fi

set -xe

if [ "x$LETLOOP_FLAVOR" = "xcisco" ]; then
    rm -rf $LETLOOP_PREFIX/src/chez/cisco
    mkdir -p $LETLOOP_PREFIX/src/chez/cisco

    git clone --depth=1 https://github.com/cisco/ChezScheme/ $LETLOOP_PREFIX/src/chez/cisco/

    PREFIX=$LETLOOP_PREFIX
    mkdir -p $PREFIX

    cd $LETLOOP_PREFIX/src/chez/cisco/

    ./configure --threads --disable-x11 --disable-curses --installprefix=$PREFIX > /dev/null
    make -j $(nproc) > /dev/null
    make install > /dev/null
else
    # Consider racket flavor
    rm -rf $LETLOOP_PREFIX/src/chez/racket
    mkdir -p $LETLOOP_PREFIX/src/chez/racket

    git clone --depth=1 https://github.com/racket/ChezScheme/ $LETLOOP_PREFIX/src/chez/racket/

    PREFIX=$LETLOOP_PREFIX/
    mkdir -p $PREFIX

    cd $LETLOOP_PREFIX/src/chez/racket/

    ./configure --kernelobj --threads --disable-x11 --disable-curses --installprefix=$PREFIX > /dev/null
    make -j $(nproc) > /dev/null
    make install > /dev/null
fi

cp $LETLOOP_ROOT/letloop.md .
cp $LETLOOP_ROOT/letloop.nfo .
cp $LETLOOP_ROOT/main.c .

cd $LETLOOP_ROOT && git config --global --add safe.directory /mnt

$PREFIX/bin/scheme --compile-imported-libraries --libdirs $LETLOOP_ROOT --program $LETLOOP_ROOT/letloop.scm compile $LETLOOP_ROOT $PREFIX/lib/csv*/*/ $LETLOOP_ROOT/letloop.scm $PREFIX/bin/letloop
