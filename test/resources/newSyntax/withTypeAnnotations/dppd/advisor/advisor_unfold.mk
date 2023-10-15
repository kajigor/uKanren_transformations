what_to_do_today y0 y1 =
  ((y1 = C GoOutToTheNature [] /\ y0 = Sunny) \/
   (y1 = C VisitTheGolfClub [] /\ y0 = Sunny) \/
   (y1 = C WashYourCar [] /\ y0 = Sunny) \/
   (y1 = C ItIsFunToLearnJapanese [] /\ y0 = Sunny) \/
   (y1 = C GoOutToTheTown [] /\ y0 = Rainy) \/
   (y1 = C VisitTheBridgeClub [] /\ y0 = Rainy) \/
   (y1 = C EnjoyYourselfAtHome [] /\ y0 = Rainy) \/
   (y1 = C ItIsFunToLearnJapanese [] /\ y0 = Rainy) \/
   (y1 = C GoOutToTheTown [] /\ y0 = Foggy) \/
   (y1 = C VisitTheBridgeClub [] /\ y0 = Foggy) \/
   (y1 = C EnjoyYourselfAtHome [] /\ y0 = Foggy) \/
   (y1 = C ItIsFunToLearnJapanese [] /\ y0 = Foggy) \/
   (y1 = C GoOutToTheTown [] /\ y0 = Windy) \/
   (y1 = C VisitTheBridgeClub [] /\ y0 = Windy) \/
   (y1 = C EnjoyYourselfAtHome [] /\ y0 = Windy) \/
   (y1 = C ItIsFunToLearnJapanese [] /\ y0 = Windy))

what_to_do_today x1 x2