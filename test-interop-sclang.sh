#!/bin/sh

SCLANG=`which sclang`

if [ $? -eq 0 ]
then
    ./test_interop_sclang.byte 12345 $SCLANG 57120 test/EchoServer.sc
else
    echo "couldn't find sclang"
    exit 1
fi
