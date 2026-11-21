#!/usr/bin/env bash
# Experiment 6 (global-control budget: ConsPD 200, CPD 2000 processed conjunctions; with the exact
# speed-ups subsequencesOfLength and instanceCheckSet): base specializers and +opt+sched.
cd "$(dirname "$0")/../../../../.."
R=bench/measure/results/futamura/budget
SPECS=original,cpd-det,cpd-la,cpd-lap,conspd-det,conspd-la,cpd-la+opt+sched,conspd-la+opt+sched
rm -f $R/done.txt
for d in dppd v2s new papers; do
  (stack exec measure -- --vanilla --answers 10 --fuel 300000 --spec $SPECS --csv $R/$d.csv bench/measure/$d > $R/$d.log 2>&1
   echo "$d exit=$?" >> $R/done.txt) &
done
wait
