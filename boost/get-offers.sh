#!/bin/bash

set -e
set -u

SYN="$0 SIZE DURATION"

SIZE=$1
DURATION=$2
NET=$FILECOIN_NETWORK
ADDR_FILE=/root/${NET}-miner.addrs

while read MINER
do
  OFFER=`boost provider storage-ask \
           --size $SIZE \
           --duration $DURATION \
           $MINER 2> /dev/null`
  if [ $? -eq 0 ]
  then
    PRICE=`echo "$OFFER" | grep "Total" | cut -d: -f2 | xargs | cut -d" " -f1`
    if [ "$PRICE" = "0" ]
    then
      echo "${MINER}:NO_PRICE"
    else
      MIPS=`echo "$OFFER" | grep "Min Piece" | cut -d: -f2 | xargs`
      MAPS=`echo "$OFFER" | grep "Max Piece" | cut -d: -f2 | xargs`
      echo "${MINER}:${PRICE}:${MIPS}:${MAPS}"
    fi
  else
    # Output nothing?
  fi
done < <(cat $ADDR_FILE)
