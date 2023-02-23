#!/usr/bin/env python3

from datetime import datetime
from datetime import timedelta
from datetime import timezone
import os
import signal

import click
import platformdirs
from slack_sdk import WebClient
from slack_sdk.errors import SlackApiError
import yaml


class SlackWorkspace:
    def __init__(self, name, token, cookie):
        headers = {}
        if cookie:
            headers["cookie"] = cookie
        self.client = WebClient(token=token, headers=headers)
        self.name = name

    def set_snooze(self, minutes):
        try:
            print(f"{self.name}: dnd_setSnooze", flush=True)
            resp_set = self.client.dnd_setSnooze(num_minutes=minutes)
            return lambda: self._end_snooze(resp_set)
        except SlackApiError as e:
            print(f"{self.name}: error - {e}", flush=True)
            return lambda: None

    def _end_snooze(self, resp_set):
        if not resp_set:
            return

        try:
            resp_info = self.client.dnd_info()
            if (
                resp_set.get("snooze_enabled", False)
                and resp_info.get("snooze_enabled", False)
                and resp_set.get("snooze_endtime", -1)
                == resp_info.get("snooze_endtime", -2)
            ):
                print(f"{self.name}: dnd_endSnooze", flush=True)
                self.client.dnd_endSnooze()
        except SlackApiError as e:
            print(f"{self.name}: error - {e}", flush=True)

    def set_lunch(self, minutes):
        expiration = datetime.now(tz=timezone.utc) + timedelta(minutes=minutes)
        try:
            print(f"{self.name}: users_profile_set", flush=True)
            resp_orig = self.client.users_profile_get()
            resp_set = self.client.users_profile_set(
                profile={
                    "status_text": "Lunch",
                    "status_emoji": ":knife_fork_plate:",
                    "status_expiration": int(expiration.timestamp()),
                }
            )
            return lambda: self._end_lunch(resp_orig, resp_set)
        except SlackApiError as e:
            print(f"{self.name}: error - {e}", flush=True)
            return lambda: None

    def _end_lunch(self, resp_orig, resp_set):
        try:
            resp_get = self.client.users_profile_get()
            profile_get = resp_get["profile"]
            profile_set = resp_set["profile"]
            profile_orig = resp_orig["profile"]
            if (
                profile_get["status_text"] == profile_set["status_text"]
                and profile_get["status_emoji"] == profile_set["status_emoji"]
                and profile_get["status_expiration"] == profile_set["status_expiration"]
            ):
                print(f"{self.name}: users_profile_set (reset)", flush=True)
                self.client.users_profile_set(
                    profile={
                        "status_text": profile_orig["status_text"],
                        "status_emoji": profile_orig["status_emoji"],
                        "status_expiration": profile_orig["status_expiration"],
                    }
                )
        except SlackApiError as e:
            print(f"{self.name}: error - {e}", flush=True)
            return lambda: None


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

    ends = [sw.set_snooze(minutes) for sw in sws]
    wait_for_signal(minutes * 60)
    for end in ends:
        end()


@cli.command("lunch")
@click.argument("minutes", type=int)
@click.pass_obj
def cli_lunch(obj, minutes):
    sws = obj["sws"]

    ends = [sw.set_lunch(minutes) for sw in sws]
    wait_for_signal(minutes * 60)
    for end in ends:
        end()


if __name__ == "__main__":
    cli()
