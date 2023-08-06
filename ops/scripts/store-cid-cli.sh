#!/bin/bash
#
# docker compose commands to store a CID
#
# To test create a random file with:
#
# ```
# openssl rand -out sample.txt -base64 $(( 2**30 * 3/4 ))
# docker cp ../1g-sample.b64 permanentum_ipfs_1:/
# docker-compose exec ipfs ipfs add --cid-version 0 --quieter /1g-sample.b64
# ```


SYN="$0 CID BYTES [WALLET]"

[ $# -lt 2 ] && echo $SYN && exit 3

CID=$1
BYTES=$2

WALLETS=`docker compose exec lotus lotus wallet list --addr-only`
if [ $# -eq 3 ]
then
  # Try to use the passed wallet ot fail
  WALLET=`echo "$WALLETS" | grep $3`
  #echo "$WALLET"
  #echo "$WALLET" | wc -l
  WALLET_NO=`echo "$WALLET" | wc -l`
  [ $WALLET_NO -ne 1 ] && echo "Error, got $WALLET_NO wallets for '$3'" && exit 1

  echo "Using Wallet: $WALLET"
else
  # Select a wallet
  select WALLET in $WALLETS
  do
    [ -z $WALLET ] \
      && echo "You must select one of the above options" \
      && continue
    break
  done
fi

echo "Selected: $WALLET"

# See: https://calibration.filrep.io/
MINERS=(
  t017840 # Europe
  t01024  # t017819 0Fil # Asia
  t01491  # North America
)

DAYS=372 # How long it is intended to store the data
DURATION=$(( $DAYS * 24 * 60 * 60  / 30 ))
# Blocktime 30s https://docs.filecoin.io/basics/the-blockchain/blocks-and-tipsets/#blocktime
# 518400  is 6 months https://lotus.filecoin.io/lotus/manage/lotus-cli/#lotus-client-deal

FEES=()
for i in "${MINERS[@]}"
do
  FEE=`docker compose exec lotus lotus client query-ask --duration $DURATION --size $BYTES $i | grep "Total Price" | cut -d: -f 2 | cut -d" " -f2`
  FEES+=( "$FEE" )
  echo "Miner $i '$FEE'"
done

select M_IDX in "${!MINERS[@]}"
do
  [ -z "$M_IDX" ] \
    && echo "You must specify one of the options" \
    && continue
  break
done

MINER=${MINERS[$M_IDX]}
FEE=${FEES[$M_IDX]}

echo Miner selected: $MINER
echo Fee: $FEE

# Make the deal
CMD="docker compose exec lotus lotus client deal --from $WALLET $CID $MINER $FEE $DURATION"
echo "$CMD"
$CMD

echo '
# check `lotus client list-transfers` for running transfers (lotus MUST remail running untill all transfers stopped)
# check `lotus client list-deals --show-failed` for deal process
'

