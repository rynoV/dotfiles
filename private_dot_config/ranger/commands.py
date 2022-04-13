# This is a sample commands.py.  You can add your own commands here.
#
# Please refer to commands_full.py for all the default commands and a complete
# documentation.  Do NOT add them all here, or you may end up with defunct
# commands when upgrading ranger.

# A simple command for demonstration purposes follows.
# -----------------------------------------------------------------------------

from __future__ import (absolute_import, division, print_function)

# You can import any python module as needed.
import os

# You always need to import ranger.api.commands here to get the Command class:
from ranger.api.commands import Command


class rga_fzf(Command):
    """
    :rga_fzf
    Search in PDFs, E-Books and Office documents in current directory.
    Allowed extensions: .epub, .odt, .docx, .fb2, .ipynb, .pdf.

    Usage: rga_fzf <search string>
    """
    def execute(self):
        if self.arg(1):
            search_string = self.rest(1)
        else:
            self.fm.notify("Usage: rga_fzf <search string>", bad=True)
            return
        self.fm.execute_console(f'shell -t fish -c "rga-fzf --no-ignore {search_string}"')


class z(Command):
    """:z
    Uses .z file to set the current directory.
    """

    def execute(self):
        from subprocess import check_output
        try:
            arguments = f'z -e {" ".join(self.args[1:])}'
            cmd = ['fish', '-c', arguments]
            directory = check_output(cmd).decode("utf-8").rstrip("\n")
            self.fm.execute_console("cd " + directory)
        except Exception as e:
            self.fm.notify(str(e), bad=True)
            raise Exception("Directory not found")
