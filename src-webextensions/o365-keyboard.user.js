// ==UserScript==
// @name        O365 - tweak keyboard shortcuts
// @namespace   https://github.com/liskin/dotfiles/tree/home/src-webextensions
// @match       https://outlook.office.com/*
// @grant       none
// @version     1
// ==/UserScript==

document.addEventListener('keydown', function (e) {
	/* prevent Outlook Calendar from hijacking Alt+[0-9] */
	if (e.altKey && e.key.match(/^\d+$/)) {
		e.stopPropagation();
		e.stopImmediatePropagation();
	}
}, {capture: true});

document.addEventListener('keyup', function (e) {
	/* prevent Outlook Calendar from hijacking Alt+[0-9] */
	if (e.altKey && e.key.match(/^\d+$/)) {
		e.stopPropagation();
		e.stopImmediatePropagation();
	}
}, {capture: true});
