:: whatToDoTodayo today weather proposal =
  [ daykind weatherkind:
      {kindOfDayo today daykind} /\
      {kindOfWeathero weather weatherkind} /\
      {proposalo daykind weatherkind proposal}]

:: kindOfDayo day daykind = conde
  (day === <monday:>        /\ daykind === <workday:>)
  (day === <tuesday:>       /\ daykind === <workday:>)
  (day === <wednesday:>     /\ daykind === <workday:>)
  (day === <thursday:>      /\ daykind === <workday:>)
  (day === <friday:>        /\ daykind === <workday:>)
  (day === <saturday:>      /\ daykind === <weekend:>)
  (day === <sunday:>        /\ daykind === <weekend:>)
  (day === <eastern:>       /\ daykind === <feastday:>)
  (day === <firstOfMay:>    /\ daykind === <feastday:>)
  (day === <christmas:>     /\ daykind === <feastday:>)
  (day === <newYearsDay:>   /\ daykind === <badday:>)
  (day === <fridayThe13th:> /\ daykind === <badday:>)

:: kindOfWeathero weather weatherKind = conde
  (weather === <sunny:> /\ weatherKind === <nice:>)
  (weather === <rainy:> /\ weatherKind === <nasty:>)
  (weather === <foggy:> /\ weatherKind === <nasty:>)
  (weather === <windy:> /\ weatherKind === <nasty:>)

:: proposalo daykind weatherkind proposal = conde
  (daykind === <workday:>  /\ proposal === <goToWork:>)
  (daykind === <weekend:>  /\ weatherkind === <nice:>  /\ proposal === <goOutToTheNature:>)
  (daykind === <weekend:>  /\ weatherkind === <nice:>  /\ proposal === <visitTheGolfClub:>)
  (daykind === <weekend:>  /\ weatherkind === <nice:>  /\ proposal === <washYourCar:>)
  (daykind === <weekend:>  /\ weatherkind === <nasty:> /\ proposal === <goOutToTheTown:>)
  (daykind === <weekend:>  /\ weatherkind === <nasty:> /\ proposal === <visitTheBridgeClub:>)
  (daykind === <weekend:>  /\ weatherkind === <nasty:> /\ proposal === <enjoyYourselfAtHome:>)
  (daykind === <weekend:>  /\ proposal === <itIsFunToLearnJapanese:>)
  (daykind === <badday:>   /\ proposal === <youHadBetterStayInBed:>)
  (daykind === <feastday:> /\ {proposalo weekend weather proposal})

? {whatToDoTodayo today weather proposal}