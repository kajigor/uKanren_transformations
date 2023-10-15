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
      appendo prefix [t] new_prefix |
      (fresh h, temp, rest, s0 in
        postfix == (h :: rem_postfix) &
        appendo prefix [t] temp &
        appendo new_prefix rest prefix &
        appendo s0 new_prefix temp &
        appendo rest (h :: rem_postfix) new_postfix);


appendo xs ys rs =
  (xs == [] & rs == ys) |
  (fresh h, t, ts in
    xs == (h :: t) &
    rs == (h :: ts) &
    appendo t ys ts);

? containso [S O, O, S (S O)] str