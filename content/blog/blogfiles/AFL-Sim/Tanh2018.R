library(ggcorrplot)

# create fixture 2018 from HA info
HAinfo <- read_excel("HAinfo.xlsx", sheet = 4)
View(HAinfo)

Home <- HAinfo$Team[seq(1, nrow(HAinfo), 2)]
Away <- HAinfo$Team[seq(2, nrow(HAinfo), 2)]
fixture2018 <- cbind(Home, Away)

# load test data

finalTable2018 <- read_excel("HAinfo.xlsx", sheet = 6)
View(finalTable2018)

#set some params and functions
drawrate <- 7 / (198*5)
probwin <- function(a, b, w){
  return(((tanh((a - b)/w) + 1)/ 2) - (drawrate/2))}

# reverse rankings cause big numbers are better. 
rankings <- rev(finalTable2018$Team)


#####SIMULATIONS##### (per weight coefficient) 
Nsim.tanh.2018 <- 500

#create table for each simulation
gameresults.tanh <- matrix(data = 0, nrow = Nsim.tanh.2018, ncol = nrow(fixture2018))

#initilise lists
gameresultslist <- list()
SimResultsList <- list()
Finalsimlist <- list()

#loop over weight factor 
for (w in c(7, 10, 20, 50, 100, 500, 1000, 10000, 50000, 99999)){
  
  for (i in 1:nrow(fixture2018)) {
    PW <- probwin(match(fixture2018[i, 1], rankings),
                  match(fixture2018[i, 2], rankings), 
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
      
      hometeam.tanh <- as.character(fixture2018[j,1])
      awayteam.tanh <- as.character(fixture2018[j,2])
      
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
  
  Finalsimtabletanh <- Finalsimtabletanh[,c(10, 2, 1, 3:9)] %>% arrange(MedianPos, MeanPos)
  Finalsimlist[[w]] <- Finalsimtabletanh
  
}

SimResultsList

##Create order comparisons## 
#should change this to use lapply or try to put loop insid dataframe fucntion  
for (w in c(7, 10, 20, 50, 100, 500, 1000, 10000, 50000, 99999)){
  name <- paste0("Ordertanh.", w)
  assign(name,  match(finalTable2018$Team, Finalsimlist[[w]]$Team))
  
}

ordercomparison.tanhweight <- data.frame(Team = factor(finalTable2018$Team),
                                        Order2018 = 1:18,
                                        Ordertanh.7,
                                        Ordertanh.10,
                                        Ordertanh.20,
                                        Ordertanh.50,
                                        Ordertanh.100, 
                                        Ordertanh.500,
                                        Ordertanh.1000,
                                        Ordertanh.10000,
                                        Ordertanh.50000,
                                        Ordertanh.99999)


cormatrix <- cor(ordercomparison.tanhweight[,2:ncol(ordercomparison.tanhweight)], use = "all.obs", method = "spearman")
cormatrix

# lets find a nice way to graphically look at that
ggcorrplot(cormatrix, lab = TRUE, show.legend = FALSE)
orderCorrPlot <- ggcorrplot(cormatrix, lab = TRUE, type = "lower")


