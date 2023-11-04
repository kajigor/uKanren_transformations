match y0 = (fresh q1 in ((y0 == [] | (y0 == (S (S (O)) :: q1) & match1 q1))));

match1 y1 = (fresh q1 in ((y1 == [] | (y1 == (S (O) :: q1) & _match1 q1))));

_match1 y2 = (fresh q1 in ((y2 == [] | (y2 == (S (O) :: q1) & __match1 q1))));

__match1 y3 = (fresh q1 in ((y3 == [] | (y3 == (O :: q1) & ___match1 q1))));

___match1 y4 = (fresh q1 in ((y4 == [] | (y4 == (O :: q1) & ____match1 q1))));

____match1 y5 = (fresh q1 in ((y5 == [] | (y5 == (S (S (O)) :: q1) & _____match1 q1))));

_____match1 y6 = (fresh q1 in ((y6 == [] | (y6 == (O :: q1) & ______match1 q1))));

______match1 y7 = (y7 == [] | y7 == [S (O)]);


? match x0