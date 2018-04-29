#!/bin/bash
# Usage test.sh {src_dir}
if [ $# -ne 2 ]; then
   echo 'args: src_dir log_dir'
   exit
fi
src_dir="$1"
log_dir="$2"
cmd_teml='CM.make "sources.cm"; Main.compile "../testcases/'

[ -d $log_dir ] || mkdir $log_dir
seq 1 49 | xargs -I {} -P 4 bash -c  \
                 "cd $src_dir; echo '${cmd_teml}test{}.tig\";' | sml | sed -n '/\\[New bindings added\\.\\]/,\$p' > ../${log_dir}/{}.log"

printf 'test52\nmerge\nqueens' | xargs -I {} -P 4 bash -c  \
                 "cd $src_dir; echo '${cmd_teml}{}.tig\";' | sml | sed -n '/\\[New bindings added\\.\\]/,\$p' > ../${log_dir}/{}.log"
