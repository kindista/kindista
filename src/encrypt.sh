#!/bin/sh
# usage: encrypt.sh <client-pub-key> <server-auth> <message>

#export PYTHONPATH=$PYTHONPATH:/usr/local/lib/python2.7/site-packages
#one or the other
#use pip with --user flag
#use python virtual enviornment?

PUBKEY=$1
AUTH=$2
MESSAGE=$3
BASEDIR=$(dirname $0)

cd ${BASEDIR}
python2 push-encryption.py $PUBKEY $AUTH "$MESSAGE"
