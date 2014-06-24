#!/bin/bash

root=$(hg root)
NO_VERSION_UTIL=${1:-} # Any argument passed to this script will disable version-utils

#only check 2 levels down.  saves about 3 seconds during omake reading
#and we don't put anything lower than that by convention

if [ -n "$NO_VERSION_UTIL" ]; then
    echo "NO_VERSION_UTIL";
else
    pushd $root >/dev/null
    for dot_hg_dir in $(find . -maxdepth 2 -type d | grep '\.hg$'); do
        cd $root/$dot_hg_dir
        cd ..
        echo "$(hg showconfig 'paths.default')_$(hg id -i)"
    done
    popd >/dev/null
fi

exit 0
