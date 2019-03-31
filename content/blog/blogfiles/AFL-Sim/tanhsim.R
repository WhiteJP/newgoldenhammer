  # Load data
fixture2019 <- read_excel("HAinfo.xlsx", sheet = 5)
View(fixture2019)

finalTable2018 <- read_excel("HAinfo.xlsx", sheet = 6)
View(finalTable2018)

#set some params and functions
  drawrate <- 7 / (198*5)
probwin <- function(a, b, w){
  return(((tanh((a - b)/w) + 1)/ 2) - (drawrate/2))}

# reverse rankings cause big numbers are better. 
rankings <- rev(finalTable2018$Team)


#####SIMULATIONS##### (per weight coeeficcient) 
Numsim <- 10

#create table for each simulation
gameresults.tanh <- matrix(data = 0, nrow = Numsim, ncol = nrow(fixture2019))

#initilise lists
gameresultslist <- list()
SimResultsList <- list()
Finalsimlist <- list()

#loop over weight factor 
for (w in c(10, 20, 50, 100, 1000, 10000, 99999)){
  
  for (i in 1:nrow(fixture2019)) {
    PW <- probwin(match(fixture2019[i, 1], rankings),
                match(fixture2019[i, 2], rankings), 
                w)
    PD <- drawrate
    PL <- 1 - (PW - (drawrate/2))
    gameresults.tanh[,i] <- sample(x = c("W", "L", "D"), size = Numsim, replace = TRUE, prob = c(PW, PL, PD))
  }
  
  gameresultslist[[w]] <- gameresults.tanh

  #create sim results
  SimResults <- matrix(data = 0, nrow = Numsim, ncol = 18)
  colnames(SimResults) <- strengths$Team
  
  
  for (i in 1:nrow(gameresults.tanh)) {
    
    #create the ladder for each season.
    Ladder <- tibble(Team = Ladder$Team, W = 0, L = 0, D = 0, Pts = 0)
    
    for (j in 1:ncol(gameresults.tanh)){
      
      hometeam.tanh <- as.character(fixture2019[j,1])
      awayteam.tanh <- as.character(fixture2019[j,2])
      
      if(gameresultslist[[w]][i,j] == "W") {  
             Ladder[Ladder$Team == hometeam.tanh,]$W <- Ladder[Ladder$Team == hometeam.tanh,]$W + 1 
             Ladder[Ladder$Team == awayteam.tanh,]$L <- Ladder[Ladder$Team == awayteam.tanh,]$L + 1
             
        } else if (gameresultslist[[w]][i,j] == "L") {
            Ladder[Ladder$Team == hometeam.tanh,]$L <- Ladder[Ladder$Team == hometeam.tanh,]$L + 1
            Ladder[Ladder$Team == awayteam.tanh,]$W <- Ladder[Ladder$Team == awayteam.tanh,]$W + 1 
            
        } else {
          Ladder[Ladder$Team == hometeam.tanh,]$D <- Ladder[Ladder$Team == hometeam.tanh,]$D + 1
          Ladder[Ladder$Team == awayteam.tanh,]$D <- Ladder[Ladder$Team == awayteam.tanh,]$D + 1
        
      }
      
    } 
    
    #calculate points and percentage and order final table
    #note that under AFL rules, if points are equal then ranked by percentage
    Ladder$Pts <- (Ladder$W * 4) + (Ladder$D * 2)
    FinalLadder <- Ladder %>% arrange(desc(Pts), desc(rankings))
    
    #populate simulation results table
    for (k in 1:nrow(FinalLadder)) {
      SimResults[i, FinalLadder$Team[k]] <- k
      SimResultsList[[w]] <- SimResults
    SimResults
    }
 
  }
 
  #Make tabe summarising results of simulation
  Finalsimtabletanh <- cbind.data.frame(MeanPos = colMeans(SimResultsList[[w]]),
                                    MedianPos = apply(SimResultsList[[w]], 2, median),
                                    BestPos = apply(SimResultsList[[w]], 2, min),
                                    WorstPos = apply(SimResultsList[[w]], 2, max),
                                    percentWon =  apply(SimResultsList[[w]] == 1, 2, sum)/nrow(SimResultsList[[w]])*100,
                                    percentTop4 = apply(SimResultsList[[w]] <= 4, 2, sum)/nrow(SimResultsList[[w]])*100,
                                    percentTop8 = apply(SimResultsList[[w]] <= 8, 2, sum)/nrow(SimResultsList[[w]])*100,
                                    percentBottom4 = apply(SimResultsList[[w]] > 14, 2, sum)/nrow(SimResultsList[[w]])*100,
                                    percentLast = apply(SimResultsList[[w]] == 18, 2, sum)/nrow(SimResultsList[[w]])*100) %>%
    cbind.data.frame(Team = strengths$Team)  
  
  Finalsimtabletanh <- Finalsimtabletanh[,c(10, 2, 1, 3:9)] %>% arrange(MedianPos, MeanPos) %>% tibble()
  Finalsimlist[[w]] <- Finalsimtabletanh

}

SimResultsList
Finalsimlist


#could also try to model finals (but prob not)

## could run them all models against last years fixtures, to see which one fitss the best on it. then run them all on this years data. 




