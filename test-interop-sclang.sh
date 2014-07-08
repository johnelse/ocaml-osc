#!/bin/sh

ML_PORT=12345
SC_PORT=57120

SCLANG=`which sclang`

if [ $? -eq 0 ]
then
    ./test_interop_sclang.byte $ML_PORT $SCLANG $SC_PORT test/EchoServer.sc
else
    echo "couldn't find sclang"
    exit 1
fi
