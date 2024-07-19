#!/bin/sh

if [ -z ${LETLOOP_ROOT+x} ]; then
    echo "LETLOOP_ROOT is unset"
    exit 42
fi

if [ -z ${LETLOOP_PREFIX+x} ]; then
    echo "LETLOOP_PREFIX is unset"
    exit 42
fi

LETLOOP_ARCH=$1

if [ -z ${LETLOOP_ARCH+x} ]; then
    echo "LETLOOP_ARCH is unset"
    exit 42
fi

case "$LETLOOP_ARCH" in
    "amd64")
        LETLOOP_QEMU=qemu-x86_64-static
        FEED_ARCH=amd64
        ;;
    "aarch64")
        LETLOOP_QEMU=qemu-aarch64-static
        FEED_ARCH=arm64
        ;;
    *)
        echo "Unsupported architecture"
        exit 42
        ;;
esac

LETLOOP_DISTRO=$2

if [ -z ${LETLOOP_DISTRO+x} ]; then
    echo "LETLOOP_DISTRO is unset"
    exit 42
fi

LETLOOP_DISTRO_VERSION=$3

if [ -z ${LETLOOP_DISTRO_VERSION+x} ]; then
    echo "LETLOOP_DISTRO_VERSION is unset"
    exit 42
fi

LETLOOP_FLAVOR=$4

if [ -z ${LETLOOP_FLAVOR+x} ]; then
    echo "LETLOOP_FLAVOR is unset"
    exit 42
fi

set -xe

FEED_URL="https://images.linuxcontainers.org/images/$LETLOOP_DISTRO/$LETLOOP_DISTRO_VERSION/$FEED_ARCH/default"
LATEST=$($LETLOOP_ROOT/local/bin/letloop-rootfs-latest-version.py "$FEED_URL")
ROOTFS_URL="$FEED_URL/$LATEST/rootfs.tar.xz"

WORK="$LETLOOP_PREFIX/rootfs/$LETLOOP_ARCH-$LETLOOP_DISTRO-$LETLOOP_DISTRO_VERSION-$LETLOOP_FLAVOR"
sudo rm -rf $WORK
mkdir -p $WORK

wget $ROOTFS_URL --directory-prefix=$WORK
cd $WORK
tar xf rootfs.tar.xz

DEBIAN_FRONTEND=noninteractive
LD_PRELOAD=""

# TODO: Why? It related to systemd-id128 call below

rm -rf $WORK/etc/machine-id
rm -rf $WORK/etc/resolv.conf
cp /etc/resolv.conf $WORK/etc/resolv.conf

PROOT="sudo systemd-nspawn --uuid=$(systemd-id128 new) -D $WORK --bind=$LETLOOP_ROOT:/mnt"

case "$LETLOOP_DISTRO" in
    "debian"|"ubuntu"|"mint")
        $PROOT apt -qq update --yes
        $PROOT apt -qq upgrade --yes
        $PROOT apt -qq install --yes python-is-python3 curl cmake git build-essential uuid-dev liblz4-dev zlib1g-dev tcl > /dev/null
        ;;
    "alpine")
        $PROOT apk add build-base curl cmake git lz4-dev libuuid util-linux-dev zlib-dev tcl
        ;;
    "fedora"|"centos")
        $PROOT yum group install -y "C Development Tools and Libraries"
        $PROOT yum install -y curl cmake git lz4-devel libuuid-devel zlib-devel tcl
        ;;
    "amazonlinux"|"rockylinux"|"oracle")
        $PROOT yum group install -y "Development Tools"
        $PROOT yum install -y curl git lz4-devel libuuid-devel zlib-devel tcl which python-pip
        $PROOT pip install --upgrade cmake
        ;;
    "archlinux")
        $PROOT pacman -Sy --noconfirm curl cmake base-devel git lz4 util-linux-libs zlib tcl
        ;;
    *)
        echo "distro not supported"
        ;;
esac

# Create a python alias if necessary
FILE=$WORK/usr/bin/python
if [ ! -f "$FILE" ]; then
    cd $WORK/usr/bin/
    ln -s python3 python
fi

cd $WORK

$PROOT /mnt/local/bin/letloop-compile.sh /mnt/ /usr/local/ $LETLOOP_FLAVOR
