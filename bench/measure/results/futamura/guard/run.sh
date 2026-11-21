#!/usr/bin/env bash
# Experiment 3 (size invariants and guards, +guard) and its baselines.
cd "$(dirname "$0")/../../../../.."
R=bench/measure/results/futamura/guard
SPECS=original,original+sched,original+sched+guard,cpd-la+opt,cpd-la+opt+guard,cpd-la+opt+sched,cpd-la+opt+sched+guard,cpd-lap+opt+sched,cpd-lap+opt+sched+guard,cpd-lap+opt+sched+guard+idx
rm -f $R/done.txt
for d in dppd v2s new papers; do
  (stack exec measure -- --vanilla --answers 10 --fuel 300000 --spec $SPECS --csv $R/$d.csv bench/measure/$d > $R/$d.log 2>&1
   echo "$d exit=$?" >> $R/done.txt) &
done
wait
