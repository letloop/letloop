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

FEED_URL="https://uk.lxd.images.canonical.com/images/$LETLOOP_DISTRO/$LETLOOP_DISTRO_VERSION/$FEED_ARCH/default"
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

rm -rf $(pwd)/etc/machine-id
rm -rf $(pwd)/etc/resolv.conf
cp /etc/resolv.conf $(pwd)/etc/resolv.conf

PROOT="sudo systemd-nspawn --uuid=$(systemd-id128 new) -D $(pwd) --bind=$LETLOOP_ROOT:/mnt"

case "$LETLOOP_DISTRO" in
    "ubuntu"|"debian")
        $PROOT apt -qq update --yes
        $PROOT apt -qq upgrade --yes
        $PROOT apt -qq install --yes git build-essential uuid-dev liblz4-dev zlib1g-dev > /dev/null
        ;;
    "alpine")
        $PROOT apk add build-base git lz4-dev libuuid util-linux-dev zlib-dev
        ;;
    "fedora"|"centos")
        $PROOT yum group install -y "C Development Tools and Libraries"
        $PROOT yum install -y git lz4-devel libuuid-devel zlib-devel
        ;;
    "amazonlinux"|"rockylinux"|"oracle")
        $PROOT yum group install -y "Development Tools"
        $PROOT yum install -y git lz4-devel libuuid-devel zlib-devel
        ;;
    "archlinux")
        $PROOT pacman -Sy --noconfirm base-devel git lz4 util-linux-libs zlib
        ;;
    *)
        echo "distro not supported"
        ;;
esac

$PROOT /mnt/local/bin/letloop-compile.sh /mnt/ / $LETLOOP_FLAVOR
