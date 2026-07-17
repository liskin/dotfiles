// ==UserScript==
// @name        Fleggz - GPX download
// @namespace   https://github.com/liskin/dotfiles/tree/home/src-webextensions
// @match       https://account.fleggz.com/*
// @grant       GM.registerMenuCommand
// @grant       unsafeWindow
// @version     1
// @require     https://cdn.jsdelivr.net/npm/file-saver@2.0.5/dist/FileSaver.min.js
// ==/UserScript==

function doc_gpx() {
	const doc = document.implementation.createDocument('http://www.topografix.com/GPX/1/1', 'gpx', null);
	const gpx = doc.children[0];
	gpx.setAttributeNS('http://www.w3.org/2000/xmlns/', 'xmlns:xsi', 'http://www.w3.org/2001/XMLSchema-instance');
	gpx.setAttributeNS('http://www.w3.org/2001/XMLSchema-instance', 'xsi:schemaLocation', 'http://www.topografix.com/GPX/1/1 http://www.topografix.com/GPX/1/1/gpx.xsd');
	return gpx;
}

function el_gpx(s) {
	return document.createElementNS('http://www.topografix.com/GPX/1/1', s);
}

function n_text(s) {
	return document.createTextNode(s);
}

function el_gpx_text(s, t) {
	const n = el_gpx(s);
	n.appendChild(n_text(t));
	return n;
}

function xml(e) {
	return '<?xml version="1.0" encoding="utf-8" standalone="yes" ?>' + new XMLSerializer().serializeToString(e);
}

function save_gpx(data, filename) {
	var blob = new Blob([data], { type: 'application/gpx+xml;charset=utf-8' });
	saveAs(blob, filename);
}

let last_event = null;

function download() {
	const event = last_event;
	if (!event) {
		throw new Error("no last_event");
	}

	const fleggz_name = `${event.e.start_date} - ${event.e.name}`;

	const gpx = doc_gpx();
	gpx.setAttribute('version', '1.1');
	gpx.setAttribute('creator', 'fleggz-gpx');

	const metadata = el_gpx('metadata');
	metadata.appendChild(el_gpx_text('name', fleggz_name));
	gpx.appendChild(metadata);

	for (const track of event.s) {
		const track_name = track.name;
		const track_start = track.type == "R" ? track.coordinates?.[0] : track.coordinates;
		if (!track_start) {
			continue;
		}

		const wpt = el_gpx('wpt');
		wpt.setAttribute('lat', track_start[1]);
		wpt.setAttribute('lon', track_start[0]);
		wpt.appendChild(el_gpx_text('name', track_name));
		wpt.appendChild(el_gpx_text('type', 'GENERIC'));

		gpx.appendChild(wpt);
	}

	const trk = el_gpx('trk');
	trk.appendChild(el_gpx_text('name', fleggz_name));
	const trkseg = el_gpx('trkseg');
	for (const points of event.elpoints.points) {
		for (const point of points) {
			const trkpt = el_gpx('trkpt');
			trkpt.setAttribute('lat', point[0]);
			trkpt.setAttribute('lon', point[1]);
			trkseg.appendChild(trkpt);
		}
	}
	trk.appendChild(trkseg);
	gpx.appendChild(trk);

	save_gpx(xml(gpx), 'fleggz_route.gpx');
}

if (typeof unsafeWindow !== 'undefined') {
	const oldFetch = unsafeWindow.fetch;
	unsafeWindow.fetch = async function () {
		const args = [...arguments];
		const res = await oldFetch(...args);

		if (`${args[0]}`.startsWith("https://account.fleggz.com/r_event/") || `${args[0]}`.startsWith("https://account.fleggz.com/r_my_event/")) {
			last_event = await res.clone().json();
		}

		return res;
	};
}

if (typeof GM !== 'undefined')
	GM.registerMenuCommand('Download GPX', download);
