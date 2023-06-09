#!/bin/bash

set -e
set -u

## CFG
if [ "$NET" = "calibnet" ]
then
  SS='https://snapshots.calibrationnet.filops.net/minimal/latest.zst'

elif [ "$NET" = "mainnet" ]
then
  SS='https://snapshots.mainnet.filops.net/minimal/latest.zst'

else
  echo "Unrecognized network: '$NET'"
  exit 3
fi

SS_DIR=/lotus/snapshots
SS_NAME=latest.${NET}.snapshot.zst
SS_PATH="${SS_DIR}/$SS_NAME"

mkdir -p $SS_DIR

## Bootstrap
if $BOOTSTRAP
then
  echo "Bootstrapping"
  echo "Bootstrapping"
  echo "Bootstrapping"

  SS_DL=false
  if [ ! -f $SS_PATH ]
  then
    echo "$SS_PATH does not exists, downloading $SS"
    SS_DL=true
  elif [[ $(find "$SS_PATH" -mtime +1 -print) ]]
  then
    echo "SS on disk is older than 1 day, redownloading $SS"
    SS_DL=true
  else
    echo "Will use cached SS: $SS_PATH"
  fi

  ## DL Bootstrap files:
  if $SS_DL
  then
    echo "Initiating DL: $SS"
    aria2c -q \
           --auto-file-renaming=false \
           --allow-overwrite=true \
           --min-tls-version=TLSv1.2 \
           -x5 --dir="$SS_DIR" -o "$SS_NAME" \
           "$SS"

    echo "Finished DL: $SS"
  fi

  ## Bootstrap
  echo "Importing bootstrap SS"
  echo "Importing bootstrap SS"
  echo "Importing bootstrap SS"
  lotus daemon --import-snapshot "$SS_PATH"  --halt-after-import
fi

## Perpare to run
#trap 'echo "Got SIGUSR1, stopping"; lotus daemon stop' SIGUSR1

#echo "wanna run 'lotus config default'?"
#touch blk
#while [ -f blk ]
#do
#  echo "waiting for '`pwd`/blk' to dissapear"
#  sleep 30
#done

## Run lotus
echo "Starting Lotus Daemon"
echo "Starting Lotus Daemon"
echo "Starting Lotus Daemon"
exec lotus daemon
