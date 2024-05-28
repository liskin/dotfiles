// ==UserScript==
// @name        O365 - keep session alive
// @namespace   https://github.com/liskin/dotfiles/tree/home/src-webextensions
// @match       https://outlook.office.com/*
// @match       https://outlook.office.com.mcas.ms/*
// @grant       none
// @version     1
// ==/UserScript==

/*
UA=0 is added to background requests to signal that it's not a user action and
that it shouldn't reset the session idle timer; rewrite it to UA=1 to keep the
session alive
*/

const oldFetch = window.fetch;
window.fetch = async function () {
	const args = [...arguments];
	if (`${args[0]}`.includes("UA=0")) {
		args[0] = `${args[0]}`.replaceAll("UA=0", "UA=1");
	}
	return await oldFetch(...args);
};

const oldXMLHttpRequestOpen = window.XMLHttpRequest.prototype.open;
window.XMLHttpRequest.prototype.open = function () {
	const args = [...arguments];
	if (`${args[1]}`.includes("UA=0")) {
		args[1] = `${args[1]}`.replaceAll("UA=0", "UA=1");
	}
	return oldXMLHttpRequestOpen.call(this, ...args);
};
