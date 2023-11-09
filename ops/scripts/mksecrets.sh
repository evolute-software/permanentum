#!/bin/bash
#

cd `dirname $0`/../secrets

[ "$1" = "--force" ] && FORCE=true || FORCE=false

for i in fil-sp-db
do
  if [ -f "$i" ]
  then
    if $FORCE
    then
      echo "Overriding $i";
    else
      echo "$i already exists and no --force flag used, skipping"
      continue
    fi
  else
    echo populating $i
  fi

  openssl rand 66 | base64 -w0 > $i
done

# Filecoin
mkdir -p filecoin
# make ops/secrets/filecoin/storage.wallet
# TODO: ensure lotus already running
if [ -f filecoin/storage.wallet ]
then
  echo "filecoin/storage.wallet exists"
else
  WALLET=`docker compose exec lotus lotus wallet new bls`
  docker compose exec lotus lotus wallet export $WALLET > filecoin/storage.wallet
  echo created BLS wallet $WALLET
  while [ `docker compose exec lotus lotus wallet balance "$WALLET" | head -c1` -eq 0 ]
  do
    echo "Please fund the storage wallet with at least 1 fil and press enter"
    echo "Storage wallet address: $WALLET"
    read
  done
fi


