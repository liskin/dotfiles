# SPDX-License-Identifier: GPL-3.0-or-later

project(
  'pango-force-subpixel-positioning', 'c',
  license: 'GPL3',
  version: '0.1',
)

cc = meson.get_compiler('c')

wrapped_pango = shared_library(
  'wrapped-pango-1.0',
  version: '0.1',
  sources: ['pango.c'],
  c_args: ['-D_GNU_SOURCE'],
  dependencies: [dependency('pango'), cc.find_library('dl')],
)

run_target('patchelf', command: ['patchelf.sh', wrapped_pango])
