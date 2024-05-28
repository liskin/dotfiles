// ==UserScript==
// @name        O365 - unread mails favicon badge
// @namespace   https://github.com/liskin/dotfiles/tree/home/src-webextensions
// @match       https://outlook.office.com/mail/*
// @match       https://outlook.office.com.mcas.ms/mail/*
// @grant       none
// @version     1
// @require     https://cdn.jsdelivr.net/gh/ejci/favico.js/favico.js
// ==/UserScript==

/* helpers */
const getUnreadCount = () => document.querySelectorAll("div[aria-label~='Unread']").length;
const getShortIconLink = () => document.querySelector("head > link[rel='shortcut icon']");

/* update favicon every 10s */
const favico = new Favico({animation: 'none'});
const updateBadge = function () {
	favico.badge(getUnreadCount());
};
setInterval(updateBadge, 10000);

/* whenever Outlook itself updates the icon (e.g. to show a red dot), update immediately */
const iconChangedCallback = function () {
	const href = getShortIconLink()?.href;
	if (href && href.match(/^http.*office/)) {
		setTimeout(updateBadge, 0);
	}
};
const setupObserver = function () {
	const link = getShortIconLink();
	if (link) {
		new MutationObserver(iconChangedCallback).observe(link, {attributeFilter: ['href']});
	}
};
setupObserver();
