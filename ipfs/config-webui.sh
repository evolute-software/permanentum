#!/bin/bash
#

set -e
set -u

echo "Configuring IPFS dashboard to be available on http://$WEB_UI_IP:5001/webui"

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
