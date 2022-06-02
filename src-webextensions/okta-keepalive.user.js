// ==UserScript==
// @name        Okta - keep session alive
// @namespace   https://github.com/liskin/dotfiles/tree/home/src-webextensions
// @match       https://*.okta.com/*
// @grant       none
// @version     1
// ==/UserScript==

/* disable session timeout timers */
const maxIntervalId = setTimeout(function(){}, 0);
for (let i = 0; i <= maxIntervalId; ++i) {
	clearInterval(i);
}

/* refresh session token every 10 minutes */
setInterval(async function () {
	if (window.navigator.onLine) {
		document.cookie = `srefresh=${Date.now()}; path=/; secure`;
		await fetch("/api/v1/sessions/me/lifecycle/refresh", {method: "POST", headers: {"Accept": "application/json"}});
	}
}, 600000);
