// ==UserScript==
// @name        Fleggz - GPX download
// @namespace   https://github.com/liskin/dotfiles/tree/home/src-webextensions
// @match       https://account.fleggz.com/*
// @grant       GM.registerMenuCommand
// @version     1
// @require     https://cdn.jsdelivr.net/npm/file-saver@2.0.5/dist/FileSaver.min.js
// ==/UserScript==

function decode_coordinates(e) {
	try {
		const t = [6, 5, 9], i = ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "-", ".", ",", "[", "]"];
		let r = e.toLowerCase(), n = "";
		const o = t.length, a = i.length;
		let s = [];
		for (let e = 0; e < r.length; e++)
			s.push(r.slice(e, 1));
		return Array.from(r).forEach((e, r) => {
			let s = i.indexOf(e);
			if (-1 !== s) {
				let e = (s - t[r % o] + a) % a;
				n += i[e]
			}
		}), n;
	} catch (error) {
		return ""
	}
}

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

async function download_gpx(refno) {
	const response = await fetch(`https://account.fleggz.com/r_event/${refno}`);
	if (!response.ok)
		throw new Error(`fetch of ${refno} failed`);

	const event = await response.json();

	const fleggz_name = `${event.e.start_date} - ${event.e.name}`;
	const fleggz_tracks = event.s.flatMap(t =>
		t.type == 'R' ? [{name: t.name, coordinates: JSON.parse(decode_coordinates(t.coordinates))}] : []
	);

	const gpx = doc_gpx();
	gpx.setAttribute('version', '1.1');
	gpx.setAttribute('creator', 'fleggz-gpx');

	const metadata = el_gpx('metadata');
	metadata.appendChild(el_gpx_text('name', fleggz_name));
	gpx.appendChild(metadata);

	for (const track of fleggz_tracks) {
		const track_name = track.name;
		const track_start = track.coordinates[0];

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
	for (const track of fleggz_tracks) {
		for (const point of track.coordinates) {
			const trkpt = el_gpx('trkpt');
			trkpt.setAttribute('lat', point[1]);
			trkpt.setAttribute('lon', point[0]);
			trkseg.appendChild(trkpt);
		}
	}
	trk.appendChild(trkseg);
	gpx.appendChild(trk);

	save_gpx(xml(gpx), 'fleggz_route.gpx');
}

function download() {
	const refno = document.querySelector('a[href*="/gl/"]')?.href?.match(/^https:\/\/account\.fleggz\.com\/gl\/(.*)$/)?.[1];
	if (refno)
		download_gpx(refno);
}

if (typeof GM !== 'undefined')
	GM.registerMenuCommand('Download GPX', download);
