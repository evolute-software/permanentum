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

set -e
set -u

SYN="$0 CID BYTES [WALLET]"

[ $# -lt 2 ] && echo $SYN && exit 3

CID=$1
# TODO: Download CID from SPs IPFS gateway, not permanentum's
CAR_URL=http://permanentum-testnet.devices.kindstudios.gr:34021/ipfs/${CID}?format=car
docker compose exec boost-client bash -c "curl -X GET --fail '$CAR_URL' > /tmp/${CID}.car"
COMM=`docker compose exec boost-client boostx | cut -d: -f2- | tr -d " "`
COMMP_CID=`echo "$COMM" | head -n1`
PIECE_SIZE=`echo "$COMM" | head -n-1 | tail -n-1`
CAR_SIZE=`echo "$COMM" | tail -n1`


# TODO: find local option
PUB_CAR=https://ipfs.io/ipfs/$CID?format=car

# See: https://calibration.filrep.io/
MINERS=(
  t017840 # Europe
  t01013
  t03751 # smth smth
  t018199
  t017387
  t016755 # t017819 0Fil # Asia
  t01491  # North America
)

DAYS=372 # How long it is intended to store the data
DURATION=$(( $DAYS * 24 * 60 * 60  / 30 ))
# Blocktime 30s https://docs.filecoin.io/basics/the-blockchain/blocks-and-tipsets/#blocktime
# 518400  is 6 months https://lotus.filecoin.io/lotus/manage/lotus-cli/#lotus-client-deal
TIP="0.0000000000002"
FEES=()
echo "Trying to find deals for $CID ($CAR_SIZE car bytes) for $DURATION epochs"
for i in "${MINERS[@]}"
do
  ASK=`docker compose exec boost-client boost provider storage-ask --size $CAR_SIZE --duration $DURATION`
  if [ ! -z "$FOO" ] then
    FEE=`echo "$ASK" | grep "Price per Block" | cut -d: -f 2 | cut -d" " -f2`
    OFFER=`echo -e "$FEE + $TIP" | bc | sed 's/^\./0./'`
  else
    OFFER="Failure"
  fi

  FEES+=( "$OFFER" )
  echo "Miner $i '$OFFER'"
  echo "$ASK"
  echo

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
CMD="docker compose exec boost-client"
echo "$CMD"
$CMD

echo '
# check `lotus client list-transfers` for running transfers (lotus MUST remail running untill all transfers stopped)
# check `lotus client list-deals --show-failed` for deal process
'

