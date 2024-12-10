// ==UserScript==
// @name        O365 - Teams fixes
// @namespace   https://github.com/liskin/dotfiles/tree/home/src-webextensions
// @match       https://teams.microsoft.com/*
// @grant       none
// @version     1
// @require     https://cdn.jsdelivr.net/gh/uzairfarooq/arrive@v2.4.1/minified/arrive.min.js
// @require     https://cdn.jsdelivr.net/gh/ejci/favico.js/favico.js
// ==/UserScript==

// ---
// "safe" (actually just annoying) links
// ---
function dropClickEventListeners(el) {
	const el2 = el.cloneNode(true);
	el2.addEventListener('click', (e) => e.stopImmediatePropagation());
	el2.xxxNoClick = true;
	el.parentNode.replaceChild(el2, el);
}

document.arrive('a', function(link) {
	if (!link.href || link.xxxNoClick)
		return;

	if (link.dataset?.testid != "atp-safelink")
		return;

	dropClickEventListeners(link);
});

// ---
// unread badge in tab icon
// ---
const headTitle = document.querySelector("head > title");
function getUnreadCount() {
	const m = headTitle.text.match(/^\((\d+)\) /);
	if (m) {
		return parseInt(m[1]);
	} else {
		return 0;
	}
}

const favico = new Favico({animation: 'none'});
function updateBadge() {
	favico.badge(getUnreadCount());
};

// update favicon every 10s
setInterval(updateBadge, 10000);

// whenever Teams changes the title, update immediately
new MutationObserver(() => setTimeout(updateBadge, 0)).observe(headTitle, {childList: true, characterData: true});
