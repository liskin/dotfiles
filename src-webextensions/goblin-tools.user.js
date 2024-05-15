// ==UserScript==
// @name        goblin.tools - keyboard shortcuts
// @namespace   https://github.com/liskin/dotfiles/tree/home/src-webextensions
// @match       https://goblin.tools/*
// @grant       none
// @version     1
// ==/UserScript==

const inputText = document.querySelector('textarea#inputText, textarea.inputText');
inputText?.addEventListener('keydown', (event) => {
	if (event.key === "Enter" && event.ctrlKey) {
		const [button] = Array.from(document.querySelectorAll('button.btn-primary')).filter((b) => b.offsetParent);
		button?.click();
	}
});
