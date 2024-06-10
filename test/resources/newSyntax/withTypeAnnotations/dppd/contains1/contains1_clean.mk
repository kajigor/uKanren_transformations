containso pat str = cono str [] pat;

cono str prefix postfix =
    postfix == [] |
    (fresh new_prefix, new_postfix, h, rem in
      str == (h :: rem) &
      newo h prefix postfix new_prefix new_postfix &
      cono rem new_prefix new_postfix);

newo t prefix postfix new_prefix new_postfix =
    fresh rem_postfix in
      postfix == (t :: new_postfix) &
      appendo1 prefix [t] new_prefix |
      (fresh h, temp, rest, s0 in
        postfix == (h :: rem_postfix) &
        appendo1 prefix [t] temp &
        appendo2 new_prefix rest prefix &
        appendo2 s0 new_prefix temp &
        appendo1 rest (h :: rem_postfix) new_postfix);

appendo1 xs ys rs =
  (xs == [] & rs == ys) |
  (fresh h, t, ts in
    xs == (h :: t) &
    rs == (h :: ts) &
    appendo1 t ys ts);


appendo2 xs ys rs =
  (xs == [] & rs == ys) |
  (fresh h, t, ts in
    xs == (h :: t) &
    rs == (h :: ts) &
    appendo2 t ys ts);

? containso [S O, O, S (S O)] str