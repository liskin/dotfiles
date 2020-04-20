// ==UserScript==
// @name        myNoise chrome improvements
// @namespace   https://github.com/liskin/dotfiles/tree/home/src-webextensions
// @match       https://mynoise.net/*
// @grant       none
// @version     1
// @description Reduce power usage when muted, enable media keys.
// ==/UserScript==

function addFakeAudio() {
	/* Add invisible 10 second silent loop that tricks Chrome into showing media controls */
	const fakeAudio = document.createElement('audio');
	fakeAudio.loop = true;
	fakeAudio.src = "data:audio/flac;base64,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";
	fakeAudio.style = "visibility: hidden";
	document.body.appendChild(fakeAudio);

	/* The media needs to start playing for Chrome to detect it */
	fakeAudio.play().then(function () {
		if (bMUTE)
			fakeAudio.pause();
	});

	/* Sync mute status to the fake audio element */
	const origUpdateButtons = updateButtons;
	updateButtons = function () {
		origUpdateButtons();

		if (bMUTE)
			fakeAudio.pause();
		else
			fakeAudio.play();
	};
}

function addMediaHooks() {
	navigator.mediaSession.setActionHandler('play', function () {
		if (bMUTE)
			toggleMute();
	});
	navigator.mediaSession.setActionHandler('pause', function () {
		if (!bMUTE)
			toggleMute();
	});
	navigator.mediaSession.setActionHandler('stop', function () {
		if (!bMUTE)
			toggleMute();
	});
}

function fixMutedPowerDrain() {
	/* Suspend context when muted to reduce power consumption */
	const origFadeOut = fadeOut;
	fadeOut = function (out, steps) {
		if (out == 0 && iFadeState == 0)
			context.resume();

		origFadeOut(out, steps);

		if (out == 1 && iFadeState == 0)
			context.suspend();
	};
}

function initializeMediaSession() {
	if (bFINISHEDLOADING) {
		addFakeAudio();
		addMediaHooks();
		fixMutedPowerDrain();
	} else {
		window.setTimeout(initializeMediaSession, 1000);
	}
}

if ('mediaSession' in navigator && window.updateButtons) {
	initializeMediaSession();
}
