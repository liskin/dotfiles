#!/usr/bin/env python3

from functools import partial
import re

import click
import dbus
from dbus.mainloop.glib import DBusGMainLoop
from gi.repository import GLib


def unit_properties_changed(interface_name, changed_properties, invalidated_properties, path, regex):
    if 'ActiveState' in changed_properties and regex.search(path):
        print(".", flush=True)


def nm_properties_changed(interface_name, changed_properties, invalidated_properties):
    if 'ActiveConnections' in changed_properties:
        print(".", flush=True)


@click.command()
@click.option('--systemd-unit-regex', type=str, default='')
def main(systemd_unit_regex):
    DBusGMainLoop(set_as_default=True)

    system_bus = dbus.SystemBus()
    session_bus = dbus.SessionBus()

    for bus in (system_bus, session_bus):
        manager = bus.get_object('org.freedesktop.systemd1', '/org/freedesktop/systemd1')
        manager.Subscribe(dbus_interface='org.freedesktop.systemd1.Manager')

        bus.add_signal_receiver(
            partial(unit_properties_changed, regex=re.compile(systemd_unit_regex)),
            dbus_interface='org.freedesktop.DBus.Properties',
            signal_name='PropertiesChanged',
            bus_name='org.freedesktop.systemd1',
            arg0='org.freedesktop.systemd1.Unit',
            path_keyword='path',
        )

    nm = system_bus.get_object('org.freedesktop.NetworkManager', '/org/freedesktop/NetworkManager')
    nm.connect_to_signal(
        'PropertiesChanged',
        nm_properties_changed,
        dbus_interface='org.freedesktop.DBus.Properties',
    )

    GLib.MainLoop().run()


if __name__ == "__main__":
    main()
