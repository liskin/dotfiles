// ==UserScript==
// @name        O365 - proofpoint/urldefense decoder
// @namespace   https://github.com/liskin/dotfiles/tree/home/src-webextensions
// @match       https://outlook.office.com/mail/*
// @match       https://outlook.office.com.mcas.ms/mail/*
// @grant       none
// @version     1
// @require     https://cdn.jsdelivr.net/gh/uzairfarooq/arrive@v2.4.1/minified/arrive.min.js
// ==/UserScript==

/* the following originates from https://github.com/834N5/proofskip/blob/684721fd090d2f1f713670f7afccd560fdba594c/src/proofskip.js */
/* Copyright (c) 2023 834N5; MIT License */

const reUrldefense = /^https?:\/\/urldefense(?:\.proofpoint)?\.com\/.*/;
const reVersion = /^[^:]+:\/\/[^\/]+\/v([123])\/.+?/;

function verifyUrl(url) {
	try {
		new URL(url);
		return true;
	} catch (_) {
		return false;
	}
}

function decryptBase64(base64) {
	base64 = atob(base64);
	base64 = Uint8Array.from(base64, (m) => m.codePointAt(0));
	return(new TextDecoder().decode(base64));
}

/* proofpoint V3 is explained well here
 * https://github.com/cardi/proofpoint-url-decoder/blob/main/decode.py
 */
function decodeV3(req) {
	const REPLACEMENT_MAPPING_NUM = new Map();
	let repStr =
		"ABCDEFGHIJKLMNOPQRSTUVWXYZ" +
		"abcdefghijklmnopqrstuvwxyz" +
		"0123456789-_";
	for (let i = 0; i < repStr.length; ++i) {
		REPLACEMENT_MAPPING_NUM.set(repStr[i], i+2);
	}

	let redirect = req.match(/^.+?__(.+)__;.*$/);
	let base64 = req.match(/^.+;(.*)!!.*$/);
	if (!redirect || !base64)
		return null;

	redirect = redirect[1];
	if (base64[1] == "")
		return redirect;
	else
		base64 = decryptBase64(base64[1]);

	let url = "";
	for (let i = 0, tmpReplace; i < base64.length;) {
		if (tmpReplace = redirect.match(/^[^\*]*\*\*(.+?)/)) {
			// tmpReplace is the char for REPLACEMENT_MAPPING_NUM
			tmpReplace = tmpReplace[1];
			// url += stuff before **
			// url += replacement
			// redirect = stuff after **
			url += redirect.match(/^([^\*]*)\*\*/)[1];
			url += base64.substring(i, i + REPLACEMENT_MAPPING_NUM.get(tmpReplace));
			i += REPLACEMENT_MAPPING_NUM.get(tmpReplace);
			redirect = redirect.match(/^[^\*]*\*\*(.*)/)[1];
		} else if (tmpReplace = redirect.match(/^([^\*]*)\*(.*)/)) {
			// url += stuff before * + replacement
			// redirect = stuff after *
			url += tmpReplace[1] + base64[i++];
			redirect = tmpReplace[2];
		} else {
			return null;
		}
	}
	if (redirect.includes("*"))
		return null;
	url += redirect;

	return verifyUrl(url) ? url : null;
}

document.arrive('a', function(link) {
	if (!link.href || !reUrldefense.test(link.href))
		return;

	const version = reVersion.exec(link.href);
	if (!version)
		return;

	switch (version[1]) {
		case '3':
			const decoded = decodeV3(link.href);
			if (decoded)
				link.href = decoded;
			else
				console.log(`o365-proofpoint-urldefense: could not decode ${link.href}`);
	}
});
