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
function stopClickPropagation(event) {
	// only left and middle click
	if (event.button != 0 && event.button != 1)
		return;

	// find the A tag that was clicked
	let link = event.target;
	while (link && link.tagName !== 'A') {
		link = link.parentElement;
	}

	if (!link || !link.href)
		return;

	if (link.dataset?.testid != "atp-safelink")
		return;

	// let Teams handle its own links, we don't want a new tab
	if (link.href.startsWith("https://teams.microsoft.com/"))
		return;

	event.stopPropagation();
}

document.arrive('a', function(link) {
	if (link.parentNode) {
		link.parentNode.addEventListener('click', stopClickPropagation, {capture: true});
		link.parentNode.addEventListener('auxclick', stopClickPropagation, {capture: true});
	}
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
