
what_to_do_today today weather program =
    fresh daykind, weatherkind in (
      kind_of_dayo today daykind &
      kind_of_weathero weather weatherkind &
      proposalo daykind weatherkind program )
    ;

kind_of_dayo day daykind =
  (day == Monday        & daykind == Workday) |
  (day == Tuesday       & daykind == Workday) |
  (day == Wednesday     & daykind == Workday) |
  (day == Thursday      & daykind == Workday) |
  (day == Friday        & daykind == Workday) |
  (day == Saturday      & daykind == Weekend) |
  (day == Sunday        & daykind == Weekend) |
  (day == Eastern       & daykind == Feastday) |
  (day == FirstOfMay    & daykind == Feastday) |
  (day == Christmas     & daykind == Feastday) |
  (day == NewYearsDay   & daykind == Badday) |
  (day == FridayThe13th & daykind == Badday);

kind_of_weathero weather weatherKind =
  (weather == Sunny & weatherKind == Nice) |
  (weather == Rainy & weatherKind == Nasty) |
  (weather == Foggy & weatherKind == Nasty) |
  (weather == Windy & weatherKind == Nasty);

proposalo daykind weatherkind proposal =
  (daykind == Workday  & proposal == GoToWork) |
  (daykind == Weekend  & weatherkind == Nice  & proposal == GoOutToTheNature) |
  (daykind == Weekend  & weatherkind == Nice  & proposal == VisitTheGolfClub) |
  (daykind == Weekend  & weatherkind == Nice  & proposal == WashYourCar) |
  (daykind == Weekend  & weatherkind == Nasty & proposal == GoOutToTheTown) |
  (daykind == Weekend  & weatherkind == Nasty & proposal == VisitTheBridgeClub) |
  (daykind == Weekend  & weatherkind == Nasty & proposal == EnjoyYourselfAtHome) |
  (daykind == Weekend  & proposal == ItIsFunToLearnJapanese) |
  (daykind == Badday   & proposal == YouHadBetterStayInBed) |
  (daykind == Feastday & proposalo (Weekend) weatherkind proposal);

? what_to_do_today FirstOfMay weather program