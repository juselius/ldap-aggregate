#!/bin/bash

SCRIPT=`readlink -f $0`
SCRIPTDIR=`dirname $SCRIPT`

cd $SCRIPTDIR
runhaskell -i$SCRIPTDIR/../src clear-trees.hs
runhaskell -i$SCRIPTDIR/../src setup-trees.hs
