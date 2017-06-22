#! /usr/bin/env bash
find target/$1/ -maxdepth 1 -type f -executable -printf "%T@ %p\n" | sort -nr | head -n 1 | cut -f2 -d' '
