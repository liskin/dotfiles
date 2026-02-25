#!/usr/bin/env python3

from dataclasses import dataclass
from pathlib import Path
import re

import click


@dataclass
class Subst:
    comment_start: str
    comment_end: str

    def include(self, m: re.Match[str]) -> str:
        filename = m[1]

        content = self.includes(Path(filename).expanduser().read_text())
        nl = "\n" if not content.endswith("\n") else ""

        cs = self.comment_start
        ce = self.comment_end
        return f"{cs}include {filename}{ce}\n{content}{nl}{cs}end include {filename}{ce}"

    def includes(self, s: str) -> str:
        cs = re.escape(self.comment_start)
        ce = re.escape(self.comment_end)
        regex = f"^{cs}include (\\S+){ce}$.*?^{cs}end include \\1{ce}$"
        return re.sub(regex, self.include, s, flags=re.DOTALL | re.MULTILINE)


@click.command(context_settings={"show_default": True})
@click.option("--comment-start", type=str, default="## ")
@click.option("--comment-end", type=str, default="")
@click.argument("filename", type=click.Path(exists=True, allow_dash=True))
def main(comment_start, comment_end, filename):
    """
    Simple preprocessor for including files in one another.
    Substitution is done in-place: the input file is modified and directives are retained so it can
    serve as an input again.
    """
    with click.open_file(filename, "r") as f:
        input = f.read()
    output = Subst(comment_start=comment_start, comment_end=comment_end).includes(input)
    with click.open_file(filename, "w", atomic=True) as f:
        f.write(output)


if __name__ == "__main__":
    main()
