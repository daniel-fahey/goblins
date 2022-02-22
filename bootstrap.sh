#!/bin/sh

if [ ! -e "configure.ac" ]; then
    hall dist -x
fi
exec autoreconf -vif
