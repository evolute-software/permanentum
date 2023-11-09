#!/bin/bash
#

SYN="$0 NET

  NET   Either 'calibnet' or 'mainnet'
"

[ $# -ne 1 ] && echo "$SYN" && exit 1

NET=$1
case $NET in
  mainnet)
    URL=https://filrep.io
    ;;
  calibnet)
    URL=https://calibnet.filrep.io
    ;;
  *)
    echo "$SYN"
    exit 2

esac

echo "Downloading lists for $NET ($URL)"

for i in "Asia" "Europe" "Africa" "Oceania" "South America" "Central America" "North America"
do
  TOTAL=10
  OFFSET=0
  LIMIT=0

  while [ $TOTAL -gt $(( $OFFSET * $LIMIT )) ]
  do
  done

done
