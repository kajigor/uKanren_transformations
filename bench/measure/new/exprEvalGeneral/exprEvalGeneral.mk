-- generic interpreter, no static information
add x y z = (x == O & y == z) | (fresh x1, z1 in x == S x1 & z == S z1 & add x1 y z1);

mul x y z = (x == O & z == O) | (fresh x1, z1 in x == S x1 & mul x1 y z1 & add y z1 z);

lookup n env v =
  (fresh t in n == O & env == (v :: t)) |
  (fresh n1, h, t in n == S n1 & env == (h :: t) & lookup n1 t v);

eval e env v =
  (e == Num v) |
  (fresh n in e == Var n & lookup n env v) |
  (fresh a, b, va, vb in e == Add a b & eval a env va & eval b env vb & add va vb v) |
  (fresh a, b, va, vb in e == Mul a b & eval a env va & eval b env vb & mul va vb v);

? eval e env v
