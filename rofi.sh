#!/usr/bin/env bash

echo -en "\0prompt\x1flookup\n"
if [ x"$@" = x"quit" ] || [ x"$@" = x"Jarne not found" ]; then
  exit 0
elif ! command -v jarne-lookup 2>&1 >/dev/null; then
  echo "jarne-lookup not found"
else
  jarne-lookup "$@"
  echo -en "\0data\x1f$@\n"
fi
echo "quit"
