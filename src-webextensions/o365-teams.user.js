// ==UserScript==
// @name        O365 - Teams fixes
// @namespace   https://github.com/liskin/dotfiles/tree/home/src-webextensions
// @match       https://teams.microsoft.com/*
// @grant       none
// @version     1
// @require     https://cdn.jsdelivr.net/gh/uzairfarooq/arrive@v2.4.1/minified/arrive.min.js
// ==/UserScript==

function dropClickEventListeners(el) {
	const el2 = el.cloneNode(true);
	el2.addEventListener('click', (e) => e.stopImmediatePropagation());
	el2.xxxNoClick = true;
	el.parentNode.replaceChild(el2, el);
}

document.arrive('a', function(link) {
	if (!link.href || link.xxxNoClick)
		return;

	dropClickEventListeners(link);
});
