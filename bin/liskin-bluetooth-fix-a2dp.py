#!/usr/bin/env python3

import asyncio
from contextlib import suppress
import shutil
import signal

import pulsectl
import pulsectl_asyncio

bad_devs = {'78_2B_64_A0_68_02'}


async def system(*args):
    proc = await asyncio.create_subprocess_exec(*args)
    await proc.wait()


async def fix(dev):
    await asyncio.sleep(2)

    # Tweak volume up/down to sync absolute volume with pulseaudio
    await system(
        shutil.which("busctl"), "call", "org.bluez", f"/org/bluez/hci0/dev_{dev}",
        "org.bluez.MediaControl1", "VolumeUp"
    )
    await system(
        shutil.which("busctl"), "call", "org.bluez", f"/org/bluez/hci0/dev_{dev}",
        "org.bluez.MediaControl1", "VolumeDown"
    )

    # Switch codec to SBC XQ
    await system(
        shutil.which("pactl"), "send-message", f"/card/bluez_card.{dev}/bluez",
        "switch-codec", '"sbc_xq_552"'
    )


async def listen():
    async with pulsectl_asyncio.PulseAsync('event-printer') as pulse:
        async for event in pulse.subscribe_events('all'):
            if event.t != pulsectl.PulseEventTypeEnum.new:
                continue
            if event.facility != pulsectl.PulseEventFacilityEnum.card:
                continue

            objects = await pulse.card_list()
            obj = next((o for o in objects if o.index == event.index), None)

            if obj is None or not obj.name.startswith("bluez_card."):
                continue

            dev = obj.name.removeprefix("bluez_card.")
            if dev not in bad_devs:
                continue

            print(f"Connected {dev}, attempting fix", flush=True)
            asyncio.create_task(fix(dev))


async def main():
    listen_task = asyncio.create_task(listen())

    loop = asyncio.get_event_loop()
    for sig in (signal.SIGTERM, signal.SIGHUP, signal.SIGINT):
        loop.add_signal_handler(sig, listen_task.cancel)

    with suppress(asyncio.CancelledError):
        await listen_task

asyncio.run(main())
