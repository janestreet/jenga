#!/bin/sh

ldd $1 | grep ' => ' | sed 's/ =>.*//' | xargs echo
