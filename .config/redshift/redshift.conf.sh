#!/usr/bin/env bash

set -eu

location=$(curl --silent --show-error --fail --ipv4 "https://location.services.mozilla.com/v1/geolocate?key=geoclue")
lat=$(jq -r .location.lat <<<"$location")
lon=$(jq -r .location.lng <<<"$location")
m4 -DLAT="$lat" -DLON="$lon" "$@"
