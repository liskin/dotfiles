#!/usr/bin/env bash

set -eu

if [[ -z ${LAT-} || -z ${LON-} ]]; then
	api_key=$(pass-extract-field google/api/geolocation password)
	location=$(curl --silent --show-error --fail --ipv4 -X POST "https://www.googleapis.com/geolocation/v1/geolocate?key=$api_key")
	LAT=$(jq -r .location.lat <<<"$location")
	LON=$(jq -r .location.lng <<<"$location")
fi

m4 -DLAT="$LAT" -DLON="$LON" "$@"
