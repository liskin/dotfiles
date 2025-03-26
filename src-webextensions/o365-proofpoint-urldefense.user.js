// ==UserScript==
// @name        O365 - proofpoint/urldefense decoder
// @namespace   https://github.com/liskin/dotfiles/tree/home/src-webextensions
// @match       https://outlook.office.com/mail/*
// @match       https://outlook.office.com.mcas.ms/mail/*
// @grant       none
// @version     1
// @require     https://cdn.jsdelivr.net/gh/uzairfarooq/arrive@v2.4.1/minified/arrive.min.js
// ==/UserScript==

/* the following originates from https://github.com/cphyc/thunderbird_remove_safelinks/blob/07e3fcf2589352368062f12392d058f3de977304/src/decoders.js */
/* Copyright (c) 2020 Corentin Cadiou; MIT License */
/* SPDX-License-Identifier: MIT */

function verifyUrl(url) {
	try {
		new URL(url);
		return true;
	} catch (_) {
		return false;
	}
}

const reVersion = /^https:\/\/urldefense(?:\.proofpoint)?\.com\/(v[0-9])\/.*/;
const reV1 = /^https:\/\/urldefense(?:\.proofpoint)?\.com\/v1\/url\?u=([^&]*)&k=.*/;
const reV2 = /^https:\/\/urldefense(?:\.proofpoint)?\.com\/v2\/url\?u=([^&]*)&[dc]=.*/;
const reV3 = /^https:\/\/urldefense(?:\.proofpoint)?\.com\/v3\/__(.+)__;([^\!]*).*/;
const reV3token = /\*(\*.)?/g;

function proofPointDecoder(href) {
	const version = href.match(reVersion);
	if (!version)
		return;

	switch (version[1]) {
		case 'v1': {
			return decodeURIComponent(href.match(reV1)[1]);
		}
		case 'v2': {
			const url = href.match(reV2)[1].replace(/-/g, '%').replace(/_/g, '/');
			return decodeURIComponent(url);
		}
		case 'v3': {
			/* proofpoint V3 is explained well here
			 * https://github.com/cardi/proofpoint-url-decoder/blob/main/decode.py
			 */
			const length_codes = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_';
			const url = href.match(reV3);
			const encbytes = atob(url[2].replace(/_/g, '/').replace(/-/g, '+'));
			let encbytes_off = 0;

			return url[1].replace(reV3token, (chunk) => {
				let len = 1;
				if (chunk.length > 1)
				   len = length_codes.search(chunk[2]) + 2;
				const out = encbytes.substring(encbytes_off, encbytes_off + len);
				encbytes_off += len;
				return out;
			});
		}
	}
}

function dropClickEventListeners(el) {
	const el2 = el.cloneNode(true);
	el2.addEventListener('click', (e) => e.stopImmediatePropagation());
	el.parentNode.replaceChild(el2, el);
}

document.arrive('a', function(link) {
	if (!link.href)
		return;

	const decoded = proofPointDecoder(link.href);
	if (decoded && verifyUrl(decoded)) {
		link.href = decoded;
		dropClickEventListeners(link);
	}
});
