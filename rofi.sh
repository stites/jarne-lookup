#!/usr/bin/env bash

echo -en "\0prompt\x1flookup\n"
if [ x"$@" = x"quit" ] || [ x"$@" = x"Jarne not found" ]
then
  exit 0
else
  /persist/home/stites/git/haskell/active/jarnectl/dist-newstyle/build/x86_64-linux/ghc-9.6.5/jarnectl-0.1.0.0/x/lookup/build/lookup/lookup "$@"
  echo -en "\0data\x1f$@\n"
fi
echo "quit"
