#!/usr/bin/env bash
#
set -e
set -u

case $CARDANO_NETWORK in

  mainnet)
    echo mainnet
    NET_MAGIC='"mainnet"'
    ;;
  preprod)
    echo preprod
    NET_MAGIC='"preprod"'
    ;;
  *)
    echo "Unknown network: '$CARDANO_NETWORK'"
    exit 1
esac

export NET_MAGIC

mkdir -p /etc/oura
envsubst < /daemon.toml.tpl > /etc/oura/daemon.toml

echo "
CFG:"
cat /etc/oura/daemon.toml
echo "

"

echo "CMD is '$@'"
echo "Metrics under http://$TUN0:$PORT_INDEXER_METRICS/metrics"
exec oura "$@"
