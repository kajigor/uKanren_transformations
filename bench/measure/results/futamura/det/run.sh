#!/usr/bin/env bash
# Experiment 2 (switch-determinism in the scheduler's ranking): the +sched variants.
cd "$(dirname "$0")/../../../../.."
R=bench/measure/results/futamura/det
SPECS=original+sched,cpd-det+opt+sched,cpd-la+opt+sched,cpd-lap+opt+sched
rm -f $R/done.txt
for d in dppd v2s new papers; do
  (stack exec measure -- --vanilla --answers 10 --fuel 300000 --spec $SPECS --csv $R/$d.csv bench/measure/$d > $R/$d.log 2>&1
   echo "$d exit=$?" >> $R/done.txt) &
done
wait
