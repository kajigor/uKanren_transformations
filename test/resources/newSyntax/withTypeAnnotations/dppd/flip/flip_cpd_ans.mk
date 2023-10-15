flipflip y0 y1 =
  fresh q1 q2 q3 q4 q5 q6
    (((y1 = Leaf q1 /\ y0 = Leaf q1) \/
    (y1 = Tree q2 q3 q4 /\ _flipFlip q5 q4 /\
    y0 = Tree q6 q3 q5 /\ _flipFlip q6 q2)))

_flipFlip y5 y7 =
 fresh q1 q2 q3 q4 q5 q6
  (((y7 = Leaf q1 /\ y5 = Leaf q1) \/
  (y7 = Tree q2 q3 q4 /\ _flipFlip q5 q4 /\
  y5 = Tree q6 q3 q5 /\ _flipFlip q6 q2)))

flipflip x0 x1