#!/bin/bash
# Usage test.sh {src_dir}
if [ $# -ne 1 ]; then
   echo 'pass an arg as source dir(e.g. Translate)'
   exit
fi
src_dir=$1
log_dir="test-results"
cmd_teml='CM.make "sources.cm"; Main.compile "../testcases/'

[ -d $log_dir ] || mkdir $log_dir
seq 1 49 | xargs -I {} -P 4 bash -c  \
                 "cd $src_dir; echo '${cmd_teml}test{}.tig\";' | sml | sed -n '/\\[New bindings added\\.\\]/,\$p' > ../${log_dir}/{}.log"
