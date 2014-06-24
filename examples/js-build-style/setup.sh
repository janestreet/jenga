#!/bin/sh

js_packages='
jenga
ocaml_plugin
async
async_extra
async_find
async_inotify
async_kernel
async_parallel
async_unix
bin_prot
comparelib
core
core_kernel
custom_printf
enumerate
fieldslib
herelib
pa_bench
pa_ounit
pa_test
pipebang
sexplib
type_conv
typerep
variantslib
'

if [ ! -x packages ]; then
    mkdir packages
    (cd packages; for x in $js_packages; do git clone https://github.com/janestreet/$x; done)
    (cd packages; git clone https://github.com/vincenthz/ocaml-inotify)
    (cd packages; git clone https://github.com/warrenharris/ounit)
    (cd x-files; tar cf - .) | (cd packages; tar xf -)
fi
