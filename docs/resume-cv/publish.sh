#!/usr/bin/env bash

set -eu -o pipefail

# shellcheck source-path=../..
. "$HOME"/bin/.o

filename="${1:?filename}"
filename_pdf="${filename%%.*}.pdf"
filename_html="${filename%%.*}.selfcont.html"

purpose="${2:?purpose}"
purpose_hmac=$(<<<"$purpose" openssl dgst -sha1 -binary -hmac "$(< /etc/machine-id)" | base32)
purpose_hmac=${purpose_hmac:0:12}
purpose_hmac=${purpose_hmac,,}
pub_dir=".pub-${purpose_hmac}"

name="tomas-janousek"
dest_filename_pdf="${name}-cv-resume.pdf"
dest_filename_html="${name}-cv-resume.html"

url_pdf="https://store.lisk.in/tmp/${purpose_hmac}/${dest_filename_pdf}"
url_html="https://store.lisk.in/tmp/${purpose_hmac}/${dest_filename_html}"

o mkdir -p "${pub_dir}"
cat >"${pub_dir}/purpose.txt" <<<"${purpose}"
cat >"${pub_dir}/meta.yaml" <<-END
	footer: |-
	  [HTML version](${url_html}) | [PDF version](${url_pdf})
	END
o touch "$filename"
o make PANDOC_FLAGS="--metadata-file=${pub_dir}/meta.yaml" "$filename_pdf" "$filename_html"
o cp "${filename_pdf}" "${pub_dir}/${dest_filename_pdf}"
o cp "${filename_html}" "${pub_dir}/${dest_filename_html}"
o rsync "${pub_dir}/${dest_filename_pdf}" "${pub_dir}/${dest_filename_html}" "store.lisk.in:wwwtmp/${purpose_hmac}/"

echo "$url_pdf"
echo "$url_html"
