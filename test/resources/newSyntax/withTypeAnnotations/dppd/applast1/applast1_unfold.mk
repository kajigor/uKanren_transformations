applasto y0 = fresh q1 q2 ((y0 = (q1 :: q2) /\ appendoLasto q1 q2))
appendoLasto y1 y2 = fresh q1 q2 ((y2 = (q1 :: q2) /\ appendoLasto q1 q2))

applasto x0