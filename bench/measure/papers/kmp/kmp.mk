-- The KMP test. Source: C. Consel, O. Danvy, "Partial evaluation of pattern
-- matching in strings", Information Processing Letters 30(2), 1989; and
-- M. H. Sørensen, R. Glück, N. D. Jones, "A positive supercompiler", Journal of
-- Functional Programming 6(6), 1996 (the naive matcher below is their
-- loop/next program, made relational: success = "pattern occurs in text").
-- The alphabet is {A, B}, so the mismatch test p /= s is a finite relation.
--
-- Static: pattern [A, A, B, A, B] (longer than the classic AAB). Dynamic: text.
-- Ideal specializer: a KMP-like automaton, i.e. one residual relation per
-- pattern prefix (failure-function states) that consumes each text symbol at
-- most once and never re-scans the text; no pattern list is left in the residual.
-- (dppd/matchSimple and dppd/matchAppend are related but use Peano symbols and a
-- shorter pattern / append-based matcher; this is the explicit SGJ formulation.)
neq a b = (a == A & b == B) | (a == B & b == A);

match p s = loop p s p s;

loop pp ss op os =
  pp == [] |
  (fresh p, pr, s, sr in
     pp == (p :: pr) & ss == (s :: sr) &
     ((p == s & loop pr sr op os) | (neq p s & next op os)));

next op os = fresh s, sr in os == (s :: sr) & loop op sr op sr;

? match [A, A, B, A, B] text
