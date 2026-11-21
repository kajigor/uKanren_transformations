#!/usr/bin/env bash
# Experiment 4 (barriers at calls of unknown termination): the +sched variants, with
# barrier.patch applied (git apply bench/measure/results/futamura/barrier/barrier.patch).
cd "$(dirname "$0")/../../../../.."
R=bench/measure/results/futamura/barrier
SPECS=original+sched,cpd-det+opt+sched,cpd-la+opt+sched,cpd-lap+opt+sched
rm -f $R/done.txt
for d in dppd v2s new papers; do
  (stack exec measure -- --vanilla --answers 10 --fuel 300000 --spec $SPECS --csv $R/$d.csv bench/measure/$d > $R/$d.log 2>&1
   echo "$d exit=$?" >> $R/done.txt) &
done
wait
