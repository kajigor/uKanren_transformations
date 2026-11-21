#!/usr/bin/env bash
# Experiment 5 (greedy ordered-subconjunction embedding): the base specializers.
cd "$(dirname "$0")/../../../../.."
R=bench/measure/results/futamura/embed
SPECS=original,cpd-det,cpd-la,cpd-lap,conspd-det,conspd-la
rm -f $R/done.txt
for d in dppd v2s new papers; do
  (stack exec measure -- --vanilla --answers 10 --fuel 300000 --spec $SPECS --csv $R/$d.csv bench/measure/$d > $R/$d.log 2>&1
   echo "$d exit=$?" >> $R/done.txt) &
done
wait
