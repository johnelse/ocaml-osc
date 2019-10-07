#!/bin/sh

set -eu

ML_PORT=12345
SC_PORT=57120

( cd "$(dirname "$0")/../"
  SCLANG=$(command -v sclang)

  if [ $? -eq 0 ]
  then
    tis=_build/default/test/test_interop_sclang.exe
    XVFB_RUN=$(command -v xvfb-run)
    if [ $? -eq 0 ]
    then
        $XVFB_RUN $tis $ML_PORT "$SCLANG" $SC_PORT test/EchoServer.sc
    else
        $tis $ML_PORT "$SCLANG" $SC_PORT test/EchoServer.sc
    fi
  else
    echo "couldn't find sclang"
    exit 1
  fi
)
