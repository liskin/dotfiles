#!/usr/bin/env python3

from functools import reduce
from io import StringIO
from shutil import get_terminal_size
from sys import stdin
import pandas as pd


def read_blank_sep_csvs_from_stdin():
    return (pd.read_csv(StringIO(i), index_col='Tag', usecols=['Tag', 'Time'], converters={'Time': pd.to_timedelta})
            for i in stdin.read().split("\n\n")
            if i)


def preprocess(tables):
    table = reduce(lambda a, b: a.add(b, fill_value=0), tables)
    table = table.sort_values('Time', ascending=False)
    table = table.reindex(table.index[1:].append(table.index[0:1]))  # total to end
    table = table.rename(index=lambda s: s.rpartition(":")[2])  # strip category
    table['Part'] = table['Time'] / table['Time']['(total time)']
    table['PartsAbove'] = table['Part'].shift(1, fill_value=0).cumsum()
    return table


def output_table(width, table, stacked):
    time_col = table['Time'].map(lambda x: strfdelta(x, "{hours:02}:{minutes:02}:{seconds:02}"))
    width -= table.index.str.len().max() + time_col.str.len().max() + 4
    bar_col = table.apply(lambda r: bar(width, r.PartsAbove, r.Part, r.name, stacked), axis=1)
    return pd.DataFrame({'': bar_col, 'Time': time_col})


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


def bar(width, left_pad_frac, bar_frac, name, stacked):
    width -= 2

    if name == "(total time)":
        return f"▏{width * '▒'}▕"

    left_pad_width = left_pad_frac * width
    bar_width = bar_frac * width

    if stacked:
        left_pad_width_full = int(left_pad_width)
        left_pad_width_sub = int((left_pad_width - left_pad_width_full) * 8)
        left_pad = left_pad_width_full * " "

        if left_pad_width_sub > 3:
            if bar_width > 0.5:
                bar = "▐"
                bar_width -= 0.5
            else:
                bar = "▕"
                bar_width -= 0.25
        else:
            bar = ""
    else:
        left_pad = ""
        bar = ""

    bar_width = bar_width if bar_width > 0 and len(left_pad) + len(bar) < width else 0
    bar_width_full = int(bar_width)
    bar_width_sub = int((bar_width - bar_width_full) * 8)
    bar_subchars = " ▏▎▍▌▋▊▉"
    bar += bar_width_full * "█" + (bar_subchars[bar_width_sub] if bar_width_sub > 0 else "")

    right_pad_width = width - len(left_pad) - len(bar)
    right_pad = right_pad_width * " "

    # TODO: hour markers

    return f"▏{left_pad}{bar}{right_pad}▕"


print(output_table(setup_width(), preprocess(read_blank_sep_csvs_from_stdin()), stacked=True))
