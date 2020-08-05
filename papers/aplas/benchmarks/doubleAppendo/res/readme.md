# Double appendo

* `forward.pdf`:
  * 1000 iterations
  * 1 result
  * Query: `doubleApp lst [0] [0] q`, where `lst` is a list of zeroes of length from `100` to `700`

* `backward.pdf`:
  * 100 iterations
  * 100 results
  * Query: `q === (x,y,z) /\ doubleApp x y z lst`, where `lst` is a list of zeroes of length from `100` to `700`