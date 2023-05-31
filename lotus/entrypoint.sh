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

  ## DL Bootstrap files:
  if [ ! -f $SS_PATH ] || [[ $(find "$SS_PATH" -mtime +1 -print) ]]
  then
    echo "SS on disk is older than 1 day, redownloading $SS"
    aria2c -x5 --dir="$SS_DIR" -o "$SS_NAME" "$SS"

  fi

  ## Bootstrap
  echo "Importing bootstrap SS"
  echo "Importing bootstrap SS"
  echo "Importing bootstrap SS"
  lotus daemon --import-snapshot "$SS_PATH"  --halt-after-import
fi

## Perpare to run
#trap 'echo "Got SIGUSR1, stopping"; lotus daemon stop' SIGUSR1

## Run lotus
echo "Starting Lotus Daemon"
echo "Starting Lotus Daemon"
echo "Starting Lotus Daemon"
exec lotus daemon
