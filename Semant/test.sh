#!/bin/bash
#cmd='CM.make "sources.cm";'
rm test.log
cmd_teml='CM.make "sources.cm"; Main.compile "../testcases/'
for i in {1..49}; do
    cmd="$cmd_teml""test$i"'.tig";'
    echo "$cmd" >> test.log
    echo "$cmd" | sml | tail -n +44  >> test.log
    echo "" >> test.log
done
