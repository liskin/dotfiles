// ==UserScript==
// @name        Slack Web Workspace Sidebar
// @namespace   https://github.com/liskin/dotfiles/tree/home/src-webextensions
// @match       https://app.slack.com/*
// @grant       none
// @version     1
// @description Pretend we're on ChromeOS for Slack to show sidebar
// ==/UserScript==

Object.defineProperty(navigator, 'userAgent', {
    value: navigator.userAgent + ' CrOS'
});
