what_to_do_today y0 y1 =
  ((y1 = GoOutToTheNature /\ y0 = Sunny) \/
   (y1 = VisitTheGolfClub /\ y0 = Sunny) \/
   (y1 = WashYourCar /\ y0 = Sunny) \/
   (y1 = ItIsFunToLearnJapanese /\ y0 = Sunny) \/
   (y1 = GoOutToTheTown /\ y0 = Rainy) \/
   (y1 = VisitTheBridgeClub /\ y0 = Rainy) \/
   (y1 = EnjoyYourselfAtHome /\ y0 = Rainy) \/
   (y1 = ItIsFunToLearnJapanese /\ y0 = Rainy) \/
   (y1 = GoOutToTheTown /\ y0 = Foggy) \/
   (y1 = VisitTheBridgeClub /\ y0 = Foggy) \/
   (y1 = EnjoyYourselfAtHome /\ y0 = Foggy) \/
   (y1 = ItIsFunToLearnJapanese /\ y0 = Foggy) \/
   (y1 = GoOutToTheTown /\ y0 = Windy) \/
   (y1 = VisitTheBridgeClub /\ y0 = Windy) \/
   (y1 = EnjoyYourselfAtHome /\ y0 = Windy) \/
   (y1 = ItIsFunToLearnJapanese /\ y0 = Windy))

what_to_do_today x1 x2