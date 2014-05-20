#!/bin/sh
if [ "$BUILD_INFO_APP_FIELDS" != "" ]
then
  application_specific_fields=" (application_specific_fields $BUILD_INFO_APP_FIELDS)"
else
  application_specific_fields=""
fi

username="$(whoami)"

if [ -z "$username" ]; then
    username="user-id-$(id -u)"
fi
    
echo -n "((username $username) (hostname $(hostname)) (kernel $(uname -r)) (build_date $(date -u +%Y-%m-%d)) (build_time $(date -u +%H:%M:%S)) (x_library_inlining $X_LIBRARY_INLINING) (ocaml_version $1) (executable_path $2) (build_system $3)$application_specific_fields)"
