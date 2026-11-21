#!/usr/bin/env bash
# Experiment 1 (constructor analysis + dispatch): the +idx variants and their baselines.
cd "$(dirname "$0")/../../../../.."
R=bench/measure/results/futamura/idx
SPECS=original,cpd-la+opt,cpd-la+opt+idx,cpd-la+opt+sched,cpd-la+opt+sched+idx,cpd-lap+opt,cpd-lap+opt+idx,cpd-lap+opt+sched,cpd-lap+opt+sched+idx
rm -f $R/done.txt
for d in dppd v2s new papers; do
  (stack exec measure -- --vanilla --answers 10 --fuel 300000 --spec $SPECS --csv $R/$d.csv bench/measure/$d > $R/$d.log 2>&1
   echo "$d exit=$?" >> $R/done.txt) &
done
wait
