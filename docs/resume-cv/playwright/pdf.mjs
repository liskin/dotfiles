#!/usr/bin/env node

if (process.argv.length != 4) {
	throw Error("url, pdf args expected")
}

const url = process.argv[2];
const pdf = process.argv[3];

import { chromium } from 'playwright'
const browser = await chromium.launch({ channel: 'chrome' })
const page = await browser.newPage()
const wait = page.waitForEvent('console', (msg) => msg.text() == 'pagedjsDone')
await page.goto(url)
await wait
await page.pdf({ path: pdf, preferCSSPageSize: true })
await browser.close()
