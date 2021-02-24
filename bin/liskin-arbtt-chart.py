#!/usr/bin/env python3

from functools import reduce
from io import StringIO
from shutil import get_terminal_size
from sys import stdin
import pandas as pd


def read_blank_separated_stdin():
    return filter(None, (s.lstrip("\n") for s in stdin.read().split("\n\n")))


def read_csv(inp):
    def convert_time(t):
        if t.isdigit():
            return pd.to_timedelta(int(t), unit='s')
        else:
            return pd.to_timedelta(t)

    return pd.read_csv(
        StringIO(inp),
        index_col='Tag',
        usecols=['Tag', 'Time'],
        converters={'Time': convert_time}
    )


def preprocess(tables, stacked):
    table = reduce(lambda a, b: a.add(b, fill_value=0), tables)
    table = table.set_index(pd.MultiIndex.from_frame(
        table.index.str.extract(r'^(\(.*\)$|[A-Za-z]+)-?(.*)'),
        names=['Category', 'Detail']))
    table['TimeCategory'] = table.groupby(level='Category').transform('sum')
    table.sort_values(['TimeCategory', 'Time'], ascending=False, inplace=True)
    table_noscreen = table.drop(('(screen)', ''))
    table = table_noscreen.append(table.loc[('(screen)', '')])
    table['Part'] = table['Time'] / table_noscreen['Time'].sum()
    table['PartsAbove'] = table['Part'].shift(1, fill_value=0).cumsum() if stacked else 0
    table.loc[('(screen)', ''), 'PartsAbove'] = 0
    return table


def output_table(width, table):
    time_col = table['Time'].map(lambda x: strfdelta(x, "{hours:02}:{minutes:02}:{seconds:02}"))
    width -= table.index.levels[0].str.len().max() + 1 + table.index.levels[1].str.len().max() + 2
    width -= time_col.str.len().max() + 2
    bar_col = table.apply(lambda r: bar(width, r.PartsAbove, r.Part, r.name), axis=1)
    out = pd.DataFrame({'Time': time_col, '': bar_col})
    out.index.names = [None, None]
    blank = pd.DataFrame({'Time': '', '': ''}, index=[('', '')])
    return pd.concat([out.iloc[:-1], blank, out.iloc[-1:]])


def setup_width():
    width, _ = get_terminal_size()
    pd.set_option('display.width', width)
    pd.set_option('max_colwidth', width)
    return width


def strfdelta(tdelta, fmt):
    tdelta = tdelta.to_pytimedelta()
    d = {}
    d['hours'], rem = divmod(tdelta.seconds, 3600)
    d['hours'] += 24 * tdelta.days
    d['minutes'], d['seconds'] = divmod(rem, 60)
    return fmt.format(**d)


def bar(width, left_pad_frac, bar_frac, name):
    left_pad_width = left_pad_frac * width
    bar_width = bar_frac * width

    left_pad_width_full = int(left_pad_width)
    left_pad_width_sub = int((left_pad_width - left_pad_width_full) * 8)
    left_pad = left_pad_width_full * "·"

    if left_pad_width_sub > 3:
        if bar_width > 0.5:
            bar = "▐"
            bar_width -= 0.5
        else:
            bar = "▕"
            bar_width -= 0.25
    else:
        bar = ""

    bar_width = bar_width if bar_width > 0 and len(left_pad) + len(bar) < width else 0
    bar_width_full = int(bar_width)
    bar_width_sub = int((bar_width - bar_width_full) * 8)
    bar_subchars = " ▏▎▍▌▋▊▉"
    bar += bar_width_full * "█" + (bar_subchars[bar_width_sub] if bar_width_sub > 0 else "")

    right_pad_width = width - len(left_pad) - len(bar)
    right_pad = right_pad_width * "·"

    # TODO: hour markers

    return left_pad + bar + right_pad


inputs = map(read_csv, read_blank_separated_stdin())
output = output_table(setup_width(), preprocess(inputs, stacked=True))
print(output.to_string(header=False))
