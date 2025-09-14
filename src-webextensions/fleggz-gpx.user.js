// ==UserScript==
// @name        Fleggz - GPX download
// @namespace   https://github.com/liskin/dotfiles/tree/home/src-webextensions
// @match       https://account.fleggz.com/*
// @grant       GM.registerMenuCommand
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

function download() {
	const gpx = doc_gpx();
	gpx.setAttribute('version', '1.1');
	gpx.setAttribute('creator', 'fleggz-gpx');

	for (const track of FleggzData.getInstance().getCompoundObjects()) {
		const trk = el_gpx('trk');

		trk.appendChild(el_gpx_text('name', track[1]));

		var trkseg = el_gpx('trkseg');
		for (const point of track[5]) {
			const trkpt = el_gpx('trkpt');
			trkpt.setAttribute('lat', point[1]);
			trkpt.setAttribute('lon', point[0]);
			trkseg.appendChild(trkpt);
		}
		trk.appendChild(trkseg);

		gpx.appendChild(trk);
	}

	save_gpx(xml(gpx), 'fleggz_route.gpx');
}

GM.registerMenuCommand('Download GPX', download);
