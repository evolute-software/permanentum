#!/bin/bash
#

set -e
set -u

echo "Configuring IPFS dashboard to be available on http://${WEB_UI_IP}:${PORT_IPFS_API}/webui"

ipfs config Addresses.API "/ip4/0.0.0.0/tcp/${PORT_IPFS_API}" # 5001
ipfs config Addresses.Gateway "/ip4/0.0.0.0/tcp/${PORT_IPFS_GW}" # 5001
ipfs config --json Addresses.Swarm "[
      \"/ip4/0.0.0.0/tcp/${PORT_IPFS_GOSSIP}\",
      \"/ip6/::/tcp/${PORT_IPFS_GOSSIP}\",
      \"/ip4/0.0.0.0/udp/${PORT_IPFS_GOSSIP}/quic\",
      \"/ip4/0.0.0.0/udp/${PORT_IPFS_GOSSIP}/quic-v1\",
      \"/ip4/0.0.0.0/udp/${PORT_IPFS_GOSSIP}/quic-v1/webtransport\",
      \"/ip6/::/udp/${PORT_IPFS_GOSSIP}/quic\",
      \"/ip6/::/udp/${PORT_IPFS_GOSSIP}/quic-v1\",
      \"/ip6/::/udp/${PORT_IPFS_GOSSIP}/quic-v1/webtransport\"
    ]"

echo "Increasing Connection resources"
# https://github.com/ipfs/kubo/issues/9432#issuecomment-1330361712
# Remove custom params
#ipfs config --json Swarm.ResourceMgr '{}'

# Set inbound connection limits to a custom value

#ipfs config --json Swarm.ResourceMgr.Limits.System.ConnsInbound 1000
#ipfs config --json Swarm.ResourceMgr.Limits.System.StreamsInbound 1000
#ipfs config --json Swarm.ResourceMgr.Limits.Transient.ConnsInbound 1000
#ipfs config --json Swarm.ResourceMgr.Limits.Transient.StreamsInbound 1000


# Note: The Web UI seems to have issues with being accessed in a non CORS way
#       so adding a wildcard here. This should be safe as the port is only 
#       available on your intranet.
#       https://github.com/ipfs/ipfs-webui/issues/88#issuecomment-149040893
echo "Opening CORS completely"
ipfs config --json \
  API.HTTPHeaders.Access-Control-Allow-Origin \
  '["*"]'
ipfs config --json \
  API.HTTPHeaders.Access-Control-Allow-Methods \
  '["PUT", "POST"]'

echo "Starting KUBO now with general log level $IPFS_LOGGING"
