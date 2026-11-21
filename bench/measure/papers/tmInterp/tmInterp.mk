-- Interpreter for a Turing-machine language, specialized to a fixed machine
-- program (first Futamura projection). Source: N. D. Jones, C. K. Gomard,
-- P. Sestoft, "Partial Evaluation and Automatic Program Generation", Prentice
-- Hall, 1993, Ch. 4 (the Turing machine interpreter written in the flow chart
-- language and compiled by mix, the standard first-projection example).
-- Instructions: Right | Left | Write s | Goto l | If s l  (labels are Peano
-- indices into the program). Symbols: Bk (blank), Zr ('0'), On ('1').
-- Tape = Left (reversed) and Right (head of Right = scanned symbol); an empty
-- side reads as blank. The machine stops when it runs off the end of the program;
-- the result here is the whole tape (reverse Left ++ Right) so that the relation
-- can be run backward.
--
-- Static: the JGS example program
--     0: if 0 goto 3;  1: right;  2: goto 0;  3: write 1
-- (find the first '0' and replace it by '1'). Dynamic: initial Right, result.
-- Ideal specializer: JGS's target program, i.e. a single loop
--   scan left right res: if head right = 0 then write 1 and stop else move right
-- with no instruction list, no Goto/If dispatch and no newtail lookup.
neqsym a b =
  (a == Bk & b == Zr) | (a == Bk & b == On) | (a == Zr & b == Bk) |
  (a == Zr & b == On) | (a == On & b == Bk) | (a == On & b == Zr);

firstsym r s rest = (r == [] & s == Bk & rest == []) | r == (s :: rest);

newtail l q qt =
  (l == O & qt == q) |
  (fresh l1, i, q1 in l == S l1 & q == (i :: q1) & newtail l1 q1 qt);

revapp l r res =
  (l == [] & res == r) |
  (fresh h, t in l == (h :: t) & revapp t (h :: r) res);

tm q right res = loop q q [] right res;

loop q qtail left right res =
  (qtail == [] & revapp left right res) |
  (fresh ins, qt in qtail == (ins :: qt) & step q ins qt left right res);

step q ins qt left right res =
  (ins == Right & (fresh s, rr in firstsym right s rr & loop q qt (s :: left) rr res)) |
  (ins == Left & (fresh s, ll in firstsym left s ll & loop q qt ll (s :: right) res)) |
  (fresh s, s0, rr in ins == Write s & firstsym right s0 rr & loop q qt left (s :: rr) res) |
  (fresh l, qt2 in ins == Goto l & newtail l q qt2 & loop q qt2 left right res) |
  (fresh s, l, s0, rr in ins == If s l & firstsym right s0 rr &
     ((s == s0 & (fresh qt2 in newtail l q qt2 & loop q qt2 left right res)) |
      (neqsym s s0 & loop q qt left right res)));

? tm [If Zr (S (S (S O))), Right, Goto O, Write On] right res
