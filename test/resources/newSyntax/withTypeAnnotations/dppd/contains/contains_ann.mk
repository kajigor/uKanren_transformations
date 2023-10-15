filter (dynamic dynamic dynamic)
 appendo xs ys rs = ((xs == [] & rs == ys) | (fresh h, t, ts in ((xs == (h :: t) & rs == (h :: ts) & Memo appendo t ys ts))));
filter (dynamic static static dynamic dynamic)
 newo t prefix postfix new_prefix new_postfix =
  (fresh rem_postfix in
    (((postfix == (t :: new_postfix) &
    Unfold appendo prefix [t] new_prefix) |
    (fresh h, temp, rest, s0 in
      ((postfix == (h :: rem_postfix) &
      Unfold appendo prefix [t] temp &
      Unfold appendo new_prefix rest prefix &
      Unfold appendo s0 new_prefix temp &
      Unfold appendo rest ((h :: rem_postfix)) new_postfix))))));

filter (dynamic static static)
 cono str prefix postfix =
  (postfix == [] |
    (fresh new_prefix, new_postfix, h, rem in
      ((str == (h :: rem) &
      Unfold newo h prefix postfix new_prefix new_postfix &
      Memo cono rem new_prefix new_postfix))));

filter (static dynamic)
 containso pat str = Unfold cono str [] pat;
filter ()
 fail  = Memo fail [];

(fresh pat, str in (Unfold containso [S O, O, S (S O)] str))