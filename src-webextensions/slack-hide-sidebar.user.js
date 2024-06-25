// ==UserScript==
// @name        Slack - keyboard shortcut (C-X) to hide the sidebars
// @namespace   https://github.com/liskin/dotfiles/tree/home/src-webextensions
// @match       https://app.slack.com/*
// @grant       none
// @version     1
// ==/UserScript==

let hiddenSidebar = false;

const toggleSidebars = function () {
	hiddenSidebar = !hiddenSidebar;
	console.log(`toggle - ${hiddenSidebar}`);

	const clientWorkspaceWrapper = document.querySelector("div.p-client_workspace_wrapper");
	const clientWorkspaceLayout = document.querySelector("div.p-client_workspace__layout");
	const tabRail = document.querySelector("div.p-tab_rail");
	const controlStrip = document.querySelector("div.p-control_strip");
	if (!clientWorkspaceWrapper || !clientWorkspaceLayout || !tabRail || !controlStrip) {
		console.warn("slack-hide-sidebar.user.js - can't find needed elements");
		console.warn(`clientWorkspaceWrapper = ${clientWorkspaceWrapper}`);
		console.warn(`clientWorkspaceLayout = ${clientWorkspaceLayout}`);
		console.warn(`tabRail = ${tabRail}`);
		console.warn(`controlStrip = ${controlStrip}`);
		return;
	}

	if (hiddenSidebar) {
		clientWorkspaceWrapper.style.gridTemplateColumns = '0 auto';
		clientWorkspaceLayout.style.gridTemplateColumns = '0 auto';
		tabRail.style.display = 'none';
		controlStrip.style.display = 'none';
	} else {
		clientWorkspaceWrapper.style.gridTemplateColumns = null;
		clientWorkspaceLayout.style.gridTemplateColumns = null;
		tabRail.style.display = null;
		controlStrip.style.display = null;
	}
};

const isHideShortcut = function (e) {
	return e.ctrlKey && e.key == 'x';
};
document.addEventListener('keydown', function (e) {
	if (isHideShortcut(e)) {
		e.stopPropagation();
		e.stopImmediatePropagation();

		toggleSidebars();
	}
}, {capture: true});
document.addEventListener('keyup', function (e) {
	if (isHideShortcut(e)) {
		e.stopPropagation();
		e.stopImmediatePropagation();
	}
}, {capture: true});
