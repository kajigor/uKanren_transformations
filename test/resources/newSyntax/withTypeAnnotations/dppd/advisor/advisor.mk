
filter (static dynamic dynamic)
what_to_do_today today weather program =
    fresh daykind, weatherkind in (
      kind_of_dayo today daykind &
      kind_of_weathero weather weatherkind &
      proposalo daykind weatherkind program )
    ;

filter (static dynamic)
kind_of_dayo day daykind =
  (day == C Monday []        & daykind == C Workday []) |
  (day == C Tuesday []       & daykind == C Workday []) |
  (day == C Wednesday []     & daykind == C Workday []) |
  (day == C Thursday []      & daykind == C Workday []) |
  (day == C Friday []        & daykind == C Workday []) |
  (day == C Saturday []      & daykind == C Weekend []) |
  (day == C Sunday []        & daykind == C Weekend []) |
  (day == C Eastern []       & daykind == C Feastday []) |
  (day == C FirstOfMay []    & daykind == C Feastday []) |
  (day == C Christmas []     & daykind == C Feastday []) |
  (day == C NewYearsDay []   & daykind == C Badday []) |
  (day == C FridayThe13th [] & daykind == C Badday []);

filter (dynamic dynamic)
kind_of_weathero weather weatherKind =
  (weather == Sunny & weatherKind == C Nice []) |
  (weather == Rainy & weatherKind == C Nasty []) |
  (weather == Foggy & weatherKind == C Nasty []) |
  (weather == Windy & weatherKind == C Nasty []);

filter (dynamic dynamic dynamic)
proposalo daykind weatherkind proposal =
  (daykind == C Workday []  & proposal == C GoToWork []) |
  (daykind == C Weekend []  & weatherkind == C Nice []  & proposal == C GoOutToTheNature []) |
  (daykind == C Weekend []  & weatherkind == C Nice []  & proposal == C VisitTheGolfClub []) |
  (daykind == C Weekend []  & weatherkind == C Nice []  & proposal == C WashYourCar []) |
  (daykind == C Weekend []  & weatherkind == C Nasty [] & proposal == C GoOutToTheTown []) |
  (daykind == C Weekend []  & weatherkind == C Nasty [] & proposal == C VisitTheBridgeClub []) |
  (daykind == C Weekend []  & weatherkind == C Nasty [] & proposal == C EnjoyYourselfAtHome []) |
  (daykind == C Weekend []  & proposal == C ItIsFunToLearnJapanese []) |
  (daykind == C Badday []   & proposal == C YouHadBetterStayInBed []) |
  (daykind == C Feastday [] & proposalo (C Weekend []) weatherkind proposal);

? what_to_do_today day weather program