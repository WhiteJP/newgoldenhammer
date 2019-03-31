#try simulate the data using tanh model - try with a few different weights - could also try with a model that updates each game --but prob not worth it. 

#calculate draw rate - use last five seasons for this 
#number of draws = 1 in 2018, 3 in 2017, 0 in 2016, 2 n 2015, 1 in 2014 = 7
#games in 5 seasons = 198 *5

#draws 2018 - 3 v 15
#draws 2017 - 1 v 3, 2 v 13, 1 v 12
#draws 2015 - 2 v 17, 9 v 13
#draws 2014 - 7 v 13
#overall draws seem to show no geater instance when teams are closer on the ladder. Therefore safe to assume that draw rate is constant at 0.7 %

#set drawparam so that tanh(drawparam) = -draw rate
# therefore drawparam = tanh-1(-drawrate)
drawparam <- atanh(-drawrate)

#top team has rating of 18, worst team has rating of 1
#weight paramater - the higher the paramater the more even it is 

probAwins <- function(a, b, w, x = drawparam){
  return((tanh(((a - b)/w) + x) + 1)/ 2)}

1 - (probAwins(18, 1, 1, drawparam)  + probAwins(1, 18, 1, drawparam))
