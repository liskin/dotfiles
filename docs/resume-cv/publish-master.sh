#!/usr/bin/env bash

set -eu -o pipefail

# shellcheck source-path=../..
. "$HOME"/bin/.o

filename="cv.md"
filename_pdf="${filename%%.*}.pdf"
filename_html="${filename%%.*}.selfcont.html"

name="tomas-janousek"
dest_filename_pdf="${name}-cv-resume.pdf"
dest_filename_html="${name}-cv-resume.html"

url_pdf="https://work.lisk.in/cv/${dest_filename_pdf}"
url_html="https://work.lisk.in/cv/${dest_filename_html}"

pub_dir=~/src/work.lisk.in/cv

o mkdir -p "${pub_dir}"
cat >"${pub_dir}/meta.yaml" <<-END
	header-includes:
	  - |-
	    <script data-goatcounter="https://liskin.goatcounter.com/count" data-external="1" async src="//gc.zgo.at/count.js"></script>
	footer: |-
	  [HTML version](${url_html}) | [PDF version](${url_pdf})
	END
o touch "$filename"
o make PUBLIC=1 PANDOC_FLAGS="--metadata-file=${pub_dir}/meta.yaml" "$filename_pdf" "$filename_html"
o cp "${filename_pdf}" "${pub_dir}/${dest_filename_pdf}"
o cp "${filename_html}" "${pub_dir}/${dest_filename_html}"
o rm "${pub_dir}/meta.yaml"
