library(here)
library(readxl)
library(tidyverse)
library(purrr)
library(ggcorrplot)

loc <- here("content", "blog", "blogfiles", "AFL-Sim")
setwd(loc)

####load data####
#2018 points for and against
HAinfo <- read_excel("HAinfo.xlsx", sheet = 4)
names(HAinfo)[names(HAinfo) == 'Points'] <- 'For'

#fixture2018
Hometeam <- HAinfo$Team[seq(1, nrow(HAinfo), 2)]
Awayteam <- HAinfo$Team[seq(2, nrow(HAinfo), 2)]
fixture2018 <- tibble(Hometeam = Hometeam, Awayteam = Awayteam)

#fixture2019
fixture2019 <- read_excel("HAinfo.xlsx", sheet = 5)

#Finaltable 2018 (to check on the accuracy of our model)
finalTable2018 <- read_excel("HAinfo.xlsx", sheet = 6)

####manipulate data####
#separate home and away and impute points against
Hpoints <- HAinfo[seq(1, nrow(HAinfo), 2),]
Apoints <- HAinfo[seq(2, nrow(HAinfo), 2),]
Hpoints$Against <- Apoints$For
Apoints$Against <- Hpoints$For

#join and make data tidy
points <- rbind(Hpoints, Apoints)
pointstidy <- points %>% gather("FororAgainst", "Points", -HorA, -Team)

#get mean points and sd thereof by team, H and A, and fororagainst
pointssummary <- pointstidy %>% group_by(HorA, Team, FororAgainst) %>%
  summarise(sd = sd(Points), Points = mean(Points)) %>%
  ungroup
pointssummary <- pointssummary[, c(2,1,3,4,5)]
pointssummary

#average H or A
Average <- pointssummary %>% group_by(HorA, FororAgainst) %>% summarise(Points = mean(Points)) 
Homeaveragefor <- as.numeric(Average[4, 3])
Homeaverageagainst <- as.numeric(Average[3, 3])
Awayaveragefor <- as.numeric(Average[2, 3])
Awayaverageagainst <- as.numeric(Average[1, 3])

####Get strengths####
# create functions to get Strenths
AttackstrengthH <- function(T){
  as.numeric((pointssummary %>% 
                filter(Team == T, HorA == "H", FororAgainst == "For") %>%
                select(Points))/Homeaveragefor)
}

AttackstrengthA <- function(T){
  as.numeric((pointssummary %>% 
                filter(Team == T, HorA == "A", FororAgainst == "For") %>% 
                select(Points))/Awayaveragefor)
}

DefencestrengthH <- function(T){
  as.numeric(Homeaverageagainst/(pointssummary %>% 
                                   filter(Team == T, HorA == "H", FororAgainst == "Against") %>% 
                                   select(Points)))
}

DefencestrengthA <-function(T){
  as.numeric(Awayaverageagainst/(pointssummary %>% 
                                   filter(Team == T, HorA == "A", FororAgainst == "Against") %>%
                                   select(Points)))
}

#get table with all strengths
#create tibble
strengths <- tibble(Team = unique(pointssummary$Team), AstrengthH = NA, 
                    AstrengthA = NA, DstrengthH = NA, DstrengthA = NA)

# populate with strengths
strengths$AstrengthH <- map_dbl(strengths$Team, AttackstrengthH)
strengths$AstrengthA <- map_dbl(strengths$Team, AttackstrengthA)
strengths$DstrengthH <- map_dbl(strengths$Team, DefencestrengthH)
strengths$DstrengthA <- map_dbl(strengths$Team, DefencestrengthA)

####calculate paramater values: lambda and sd####
lambdas.2018 <- add_column(fixture2018, homelambda = NA, homesd = NA, awaylambda = NA, awaysd = NA)

#create lambda  and sd functions
lambdahometeam <- function(HomeTeam, AwayTeam) {
  Hometeamattackstrength <- as.numeric(strengths[strengths$Team == HomeTeam, "AstrengthH"])
  Awayteamdefencestrength <- as.numeric(strengths[strengths$Team == AwayTeam, "DstrengthA"])
  return(Homeaveragefor * Hometeamattackstrength / Awayteamdefencestrength)
}

lambdaawayteam <- function(AwayTeam, HomeTeam) {
  Awayteamattackstrength <- as.numeric(strengths[strengths$Team == AwayTeam, "AstrengthA"])
  Hometeamdefencestrength <- as.numeric(strengths[strengths$Team == HomeTeam, "DstrengthH"])
  return(Awayaveragefor * Awayteamattackstrength / Hometeamdefencestrength)
}

sdhometeam <- function(HomeTeam, AwayTeam) {
  Hometeamattack <- as.numeric(pointssummary %>% filter(Team == HomeTeam, HorA == "H", FororAgainst == "For") %>%
                                 select(sd))
  Awayteamdefence <- as.numeric(pointssummary %>% filter(Team == AwayTeam, HorA == "A", FororAgainst == "Against") %>%
                                  select(sd))
  # becuase n is equal can simply take average.
  return(mean(Hometeamattack, Awayteamdefence))
}

sdawayteam <- function(AwayTeam, HomeTeam) {
  Awayteamattack <- as.numeric(pointssummary %>% filter(Team == AwayTeam, HorA == "A", FororAgainst == "For") %>%
                                 select(sd))
  Hometeamdefence <- as.numeric(pointssummary %>%  filter(Team == HomeTeam, HorA == "H", FororAgainst == "Against") %>%
                                  select(sd))
  # becuase n is equal can simply take average.
  return(mean(Awayteamattack, Hometeamdefence))
}

#populate lamba values 
for (i in 1:nrow(lambdas.2018)) {
  lambdas.2018$homelambda[i] <- lambdahometeam(lambdas.2018$Hometeam[i], lambdas.2018$Awayteam[i])
  lambdas.2018$awaylambda[i] <- lambdaawayteam(lambdas.2018$Awayteam[i], lambdas.2018$Hometeam[i])
  lambdas.2018$homesd[i] <- sdhometeam(lambdas.2018$Hometeam[i], lambdas.2018$Awayteam[i])
  lambdas.2018$awaysd[i] <- sdawayteam(lambdas.2018$Awayteam[i], lambdas.2018$Hometeam[i])
}

####POISSON SIMULATION####
Nsim.pois.2018 <- 1000

#set up simulation results table
SimResults <- matrix(data = 0, nrow = Nsim.pois.2018, ncol = 18)
colnames(SimResults) <- strengths$Team

for (i in 1:Nsim.pois.2018){
  
  #create Ladder for each season
  Ladder <- tibble(Team = strengths$Team, W = 0, L = 0, D = 0, Pts = 0,
                   For = 0, Against = 0, Pct = 0)
  
  #simulate each season.
  for (j in 1:nrow(lambdas.2018)) {
    
    #Here is the model working -- each score is simply a random drawing 
    #from a poisson distribution described by our calculated lambas
    homescore <- rpois(1, lambdas.2018$homelambda[j])
    awayscore <- rpois(1, lambdas.2018$awaylambda[j])
    hometeam <- lambdas.2018$Hometeam[j]
    awayteam <- lambdas.2018$Awayteam[j]
    
    #populate scores
    Ladder[Ladder$Team == hometeam,]$For <- Ladder[Ladder$Team == hometeam,]$For + homescore
    Ladder[Ladder$Team == hometeam,]$Against <- Ladder[Ladder$Team == hometeam,]$Against + awayscore 
    Ladder[Ladder$Team == awayteam,]$For <- Ladder[Ladder$Team == awayteam,]$For + awayscore
    Ladder[Ladder$Team == awayteam,]$Against <- Ladder[Ladder$Team == awayteam,]$Against + homescore
    
    #populate table for draw
    if (homescore == awayscore) {
      #hometeam changes
      Ladder[Ladder$Team == hometeam,]$D <- Ladder[Ladder$Team == hometeam,]$D + 1
      
      #awayteam changes
      Ladder[Ladder$Team == awayteam,]$D <- Ladder[Ladder$Team == awayteam,]$D + 1
      
      #populate table for hometeam win
    } else if (homescore > awayscore) {
      #hometeam changes
      Ladder[Ladder$Team == hometeam,]$W <- Ladder[Ladder$Team == hometeam,]$W + 1
      
      #awayteam changes
      Ladder[Ladder$Team == awayteam,]$L <- Ladder[Ladder$Team == awayteam,]$L + 1
      
      #populate for away team win
    } else {
      #hometeam changes
      Ladder[Ladder$Team == hometeam,]$L <- Ladder[Ladder$Team == hometeam,]$L + 1
      
      #awayteam changes
      Ladder[Ladder$Team == awayteam,]$W <- Ladder[Ladder$Team == awayteam,]$W + 1
    }
    
  }
  
  #calculate points and percentage and order final table
  #note that under AFL rules, if points are equal then ranked by percentage
  Ladder$Pct <- Ladder$For / Ladder$Against * 100
  Ladder$Pts <- (Ladder$W * 4) + (Ladder$D * 2)
  FinalLadder <- Ladder %>% arrange(desc(Pts), desc(Pct))
  
  #populate simulation results table
  for (k in 1:nrow(FinalLadder)) {
    SimResults[i, FinalLadder$Team[k]] <- k
  }
} 

#Print and inspect results of Simulations
SimResultspois.2018 <- as_tibble(SimResults)


#Make table summarising results of simulation
Finalsimtable <- cbind.data.frame(MeanPos = colMeans(SimResultspois.2018),
                                  MedianPos = apply(SimResultspois.2018, 2, median),
                                  BestPos = apply(SimResultspois.2018, 2, min),
                                  WorstPos = apply(SimResultspois.2018, 2, max),
                                  percentWon =  apply(SimResultspois.2018 == 1, 2, sum)/nrow(SimResultspois.2018)*100,
                                  percentTop4 = apply(SimResultspois.2018 <= 4, 2, sum)/nrow(SimResultspois.2018)*100,
                                  percentTop8 = apply(SimResultspois.2018 <= 8, 2, sum)/nrow(SimResultspois.2018)*100,
                                  percentBottom4 = apply(SimResultspois.2018 > 14, 2, sum)/nrow(SimResultspois.2018)*100,
                                  percentLast = apply(SimResultspois.2018 == 18, 2, sum)/nrow(SimResultspois.2018)*100) %>%
  cbind.data.frame(Team = strengths$Team) 

Finalsimtablepois.2018 <- Finalsimtable[,c(10, 2, 1, 3:9)] %>% arrange(MedianPos, MeanPos)

####NORM SIMULATIONS####
Nsim.norm.2018 <- 1000

#set up simulation results table
SimResults <- matrix(data = 0, nrow = Nsim.norm.2018, ncol = 18)
colnames(SimResults) <- strengths$Team

for (i in 1:Nsim.norm.2018){
  
  #create Ladder for each season
  Ladder <- tibble(Team = strengths$Team, W = 0, L = 0, D = 0, Pts = 0,
                   For = 0, Against = 0, Pct = 0)
  
  #simulate each season.
  for (j in 1:nrow(lambdas.2018)) {
    
    #Here is the model working -- each score is simply a random drawing 
    #from a normal dist characterised by our lambda as mean, and sd
    homescore <- rnorm(1, mean = lambdas.2018$homelambda[j], sd = lambdas.2018$homesd[j])
    awayscore <- rnorm(1, lambdas.2018$awaylambda[j], sd = lambdas.2018$awaysd[j])
    hometeam <- lambdas.2018$Hometeam[j]
    awayteam <- lambdas.2018$Awayteam[j]
    
    #populate scores
    Ladder[Ladder$Team == hometeam,]$For <- Ladder[Ladder$Team == hometeam,]$For + homescore
    Ladder[Ladder$Team == hometeam,]$Against <- Ladder[Ladder$Team == hometeam,]$Against + awayscore 
    Ladder[Ladder$Team == awayteam,]$For <- Ladder[Ladder$Team == awayteam,]$For + awayscore
    Ladder[Ladder$Team == awayteam,]$Against <- Ladder[Ladder$Team == awayteam,]$Against + homescore
    
    #populate table for draw
    if (homescore == awayscore) {
      #hometeam changes
      Ladder[Ladder$Team == hometeam,]$D <- Ladder[Ladder$Team == hometeam,]$D + 1
      
      #awayteam changes
      Ladder[Ladder$Team == awayteam,]$D <- Ladder[Ladder$Team == awayteam,]$D + 1
      
      #populate table for hometeam win
    } else if (homescore > awayscore) {
      #hometeam changes
      Ladder[Ladder$Team == hometeam,]$W <- Ladder[Ladder$Team == hometeam,]$W + 1
      
      #awayteam changes
      Ladder[Ladder$Team == awayteam,]$L <- Ladder[Ladder$Team == awayteam,]$L + 1
      
      #populate for away team win
    } else {
      #hometeam changes
      Ladder[Ladder$Team == hometeam,]$L <- Ladder[Ladder$Team == hometeam,]$L + 1
      
      #awayteam changes
      Ladder[Ladder$Team == awayteam,]$W <- Ladder[Ladder$Team == awayteam,]$W + 1
    }
    
  }
  
  #calculate points and percentage and order final table
  #note that under AFL rules, if points are equal then ranked by percentage
  Ladder$Pct <- Ladder$For / Ladder$Against * 100
  Ladder$Pts <- (Ladder$W * 4) + (Ladder$D * 2)
  FinalLadder <- Ladder %>% arrange(desc(Pts), desc(Pct))
  
  #populate simulation results table
  for (k in 1:nrow(FinalLadder)) {
    SimResults[i, FinalLadder$Team[k]] <- k
  }
} 

#Print and inspect results of Simulation
SimResultsnorm.2018 <- as_tibble(SimResults)


#Make tabe summarising results of simulation
Finalsimtable.2018 <- cbind.data.frame(MeanPos = colMeans(SimResultsnorm.2018),
                                  MedianPos = apply(SimResultsnorm.2018, 2, median),
                                  BestPos = apply(SimResultsnorm.2018, 2, min),
                                  WorstPos = apply(SimResultsnorm.2018, 2, max),
                                  percentWon =  apply(SimResultsnorm.2018 == 1, 2, sum)/nrow(SimResultsnorm.2018)*100,
                                  percentTop4 = apply(SimResultsnorm.2018 <= 4, 2, sum)/nrow(SimResultsnorm.2018)*100,
                                  percentTop8 = apply(SimResultsnorm.2018 <= 8, 2, sum)/nrow(SimResultsnorm.2018)*100,
                                  percentBottom4 = apply(SimResultsnorm.2018 > 14, 2, sum)/nrow(SimResultsnorm.2018)*100,
                                  percentLast = apply(SimResultsnorm.2018 == 18, 2, sum)/nrow(SimResultsnorm.2018)*100) %>%
  cbind.data.frame(Team = strengths$Team)

Finalsimtablenorm.2018 <- Finalsimtable[,c(10, 2, 1, 3:9)] %>% arrange(MedianPos, MeanPos)

####TANH MODEL SIMULATIONS####
#set some params and functions
drawrate <- 7 / (198*5)
drawparam <- atanh(-drawrate)
probwin <- function(a, b, w, d = drawparam){
  return((tanh(((a - b)/w) + d) + 1)/ 2)}

# reverse ladder order to get rankings needed for the model
rankings <- rev(finalTable2018$Team)

##SIMULATIONS## 
Nsim.tanh.2018 <- 1000 # for each weight parameter looped over

#create table for each simulation
gameresults.tanh <- matrix(data = 0, nrow = Nsim.tanh.2018, ncol = nrow(fixture2018))

#initilise lists
gameresultslist<- list()
SimResultsList.2018 <- list()
Finalsimlist.2018 <- list()

#loop over weight factor 
for (w in c(1, 20, 50, 100, 500, 1000)){
  
  for (i in 1:nrow(fixture2018)) {
    PW <- probwin(match(fixture2018[i, 1], rankings),
                  match(fixture2018[i, 2], rankings), 
                  w)
    PL <- probwin(match(fixture2018[i, 2], rankings),
                  match(fixture2018[i, 1], rankings), 
                  w)
    PD <- 1 - PW - PL 
    gameresults.tanh[,i] <- sample(x = c("W", "L", "D"), size = Nsim.tanh.2018, replace = TRUE, prob = c(PW, PL, PD))
  }
  
  gameresultslist[[w]] <- gameresults.tanh
  
  #create sim results
  SimResults <- matrix(data = 0, nrow = Nsim.tanh.2018, ncol = 18)
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
      SimResultsList.2018[[w]] <- SimResults

    }
    
  }
  
  #Make tabe summarising results of simulation
  Finalsimtabletanh <- cbind.data.frame(MeanPos = colMeans(SimResultsList.2018[[w]]),
                                        MedianPos = apply(SimResultsList.2018[[w]], 2, median),
                                        BestPos = apply(SimResultsList.2018[[w]], 2, min),
                                        WorstPos = apply(SimResultsList.2018[[w]], 2, max),
                                        percentWon =  apply(SimResultsList.2018[[w]] == 1, 2, sum)/nrow(SimResultsList.2018[[w]])*100,
                                        percentTop4 = apply(SimResultsList.2018[[w]] <= 4, 2, sum)/nrow(SimResultsList.2018[[w]])*100,
                                        percentTop8 = apply(SimResultsList.2018[[w]] <= 8, 2, sum)/nrow(SimResultsList.2018[[w]])*100,
                                        percentBottom4 = apply(SimResultsList.2018[[w]] > 14, 2, sum)/nrow(SimResultsList.2018[[w]])*100,
                                        percentLast = apply(SimResultsList.2018[[w]] == 18, 2, sum)/nrow(SimResultsList.2018[[w]])*100) %>%
    cbind.data.frame(Team = strengths$Team)  
  
  Finalsimtabletanh <- Finalsimtabletanh[,c(10, 2, 1, 3:9)] %>% arrange(MedianPos, MeanPos)
  Finalsimlist.2018[[w]] <- Finalsimtabletanh
  
}

