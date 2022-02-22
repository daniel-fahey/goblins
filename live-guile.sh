#!/bin/sh

# Remove stale file if it exists
rm -f /tmp/guile-goblins.sock

echo "***********************************************"
echo "** Setting up Guile to listen on a socket... **"
echo "**                                           **"
echo "** Connect in emacs via:                     **"
echo "**   M-x geiser-connect-local <RET>          **"
echo "**       guile <RET>                         **"
echo "**       /tmp/guile-goblins.sock <RET>       **"
echo "***********************************************"
echo ""

guix shell -m manifest.scm -- ./pre-inst-env guile --listen=/tmp/guile-goblins.sock

# clean up, be nice
rm /tmp/guile-goblins.sock
