# This plugin adds opened files to `z`

from __future__ import (absolute_import, division, print_function)

import subprocess

import ranger.api
from ranger.ext.spawn import check_output


HOOK_INIT_OLD = ranger.api.hook_init


def hook_init(fm):
    def z_add():
        for fobj in fm.thistab.get_selection():
            try:
                check_output(['fish', '-e', '__z_add', fobj.path])
            except subprocess.CalledProcessError:
                pass
    fm.signal_bind('execute.before', z_add)
    return HOOK_INIT_OLD(fm)


ranger.api.hook_init = hook_init
