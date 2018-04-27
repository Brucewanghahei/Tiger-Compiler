#!/bin/bash
basedir="$1"
rl_fpath=$(realpath --relative-to="$basedir" "$2")
cd $basedir
echo "CM.make \"sources.cm\"; Main.compile \"$rl_fpath\";" | sml
