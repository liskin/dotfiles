#!/usr/bin/env python3

import os
import signal

import click
import platformdirs
from slack_sdk import WebClient
from slack_sdk.errors import SlackApiError
import yaml


class SlackWorkspace:
    resp_set = {}

    def __init__(self, name, token, cookie):
        headers = {}
        if cookie:
            headers["cookie"] = cookie
        self.client = WebClient(token=token, headers=headers)
        self.name = name

    def set_snooze(self, minutes):
        try:
            print(f"{self.name}: dnd_setSnooze", flush=True)
            self.resp_set = self.client.dnd_setSnooze(num_minutes=minutes)
        except SlackApiError as e:
            print(f"{self.name}: error - {e}", flush=True)

    def end_snooze(self):
        if not self.resp_set:
            return

        resp_info = self.client.dnd_info()
        if (
            self.resp_set.get("snooze_enabled", False)
            and resp_info.get("snooze_enabled", False)
            and self.resp_set.get("snooze_endtime", -1)
            == resp_info.get("snooze_endtime", -2)
        ):
            try:
                print(f"{self.name}: dnd_endSnooze", flush=True)
                self.client.dnd_endSnooze()
            except SlackApiError as e:
                print(f"{self.name}: error - {e}", flush=True)


def wait_for_signal(seconds):
    signal.signal(signal.SIGTERM, lambda _1, _2: None)
    signal.sigtimedwait({signal.SIGINT, signal.SIGTERM}, seconds)


@click.group()
@click.option(
    "--workspaces",
    type=click.File(),
    default=os.path.join(
        platformdirs.user_config_dir(appname="liskin-slack-dnd"), "workspaces.yaml"
    ),
    show_default=True,
)
@click.pass_context
def cli(ctx, workspaces):
    sws = [
        SlackWorkspace(name=n, token=c["token"], cookie=c.get("cookie"))
        for n, c in yaml.safe_load(workspaces).items()
    ]

    ctx.ensure_object(dict)
    ctx.obj["sws"] = sws


@cli.command("dnd")
@click.argument("minutes", type=int)
@click.pass_obj
def cli_dnd(obj, minutes):
    sws = obj["sws"]

    for sw in sws:
        sw.set_snooze(minutes)
    wait_for_signal(minutes * 60)
    for sw in sws:
        sw.end_snooze()


if __name__ == "__main__":
    cli()
