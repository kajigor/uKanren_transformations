#!/usr/bin/env bash
# Experiment 7 (constructor specialization, +spc) and its baselines.
cd "$(dirname "$0")/../../../../.."
R=bench/measure/results/futamura/spc
SPECS=original,cpd-la+opt,cpd-la+opt+spc,cpd-la+opt+sched,cpd-la+opt+spc+sched,cpd-lap+opt+sched+guard+idx,cpd-lap+opt+spc+sched+guard+idx
rm -f $R/done.txt
for d in dppd v2s new papers; do
  (stack exec measure -- --vanilla --answers 10 --fuel 300000 --spec $SPECS --csv $R/$d.csv bench/measure/$d > $R/$d.log 2>&1
   echo "$d exit=$?" >> $R/done.txt) &
done
wait
