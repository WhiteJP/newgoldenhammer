library(here)
library(readxl)
library(tidyverse)
library(purrr)

loc <- here()
setwd(loc)

HAinfo <- read_excel("HAinfo.xlsx", sheet = 4)
View(HAinfo)

names(HAinfo)[names(HAinfo) == 'Points'] <- 'For'

#separate home and away and impute points against
Hpoints <- HAinfo[seq(1, nrow(HAinfo), 2),]
Apoints <- HAinfo[seq(2, nrow(HAinfo), 2),]
Hpoints$Against <- Apoints$For
Apoints$Against <- Hpoints$For

#join and make data tidy
points <- rbind(Hpoints, Apoints)
pointstidy <- points %>% gather("FororAgainst", "Points", -HorA, -Team)
pointstidy

#get mean points and sd thereof by team, H and A, and fororagainst
pointssummary <- pointstidy %>% group_by(HorA, Team, FororAgainst) %>%
                                summarise(sd = sd(Points), Points = mean(Points)) %>%
                                ungroup
pointssummary <- pointssummary[, c(2,1,3,4,5)]

#average H or A
Average <- pointssummary %>% group_by(HorA, FororAgainst) %>% summarise(Points = mean(Points)) 
Homeaveragefor <- as.numeric(Average[4, 3])
Homeaverageagainst <- as.numeric(Average[3, 3])
Awayaveragefor <- as.numeric(Average[2, 3])
Awayaverageagainst <- as.numeric(Average[1, 3])

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

fixture2019 <- read_excel("HAinfo.xlsx", sheet = 5)
View(fixture2019)

  lambdas <- add_column(fixture2019, homelambda = NA, homesd = NA, awaylambda = NA, awaysd = NA)
  
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
  for (i in 1:nrow(lambdas)) {
    lambdas$homelambda[i] <- lambdahometeam(lambdas$Hometeam[i], lambdas$Awayteam[i])
    lambdas$awaylambda[i] <- lambdaawayteam(lambdas$Awayteam[i], lambdas$Hometeam[i])
    lambdas$homesd[i] <- sdhometeam(lambdas$Hometeam[i], lambdas$Awayteam[i])
    lambdas$awaysd[i] <- sdawayteam(lambdas$Awayteam[i], lambdas$Hometeam[i])
  }

##### SIMULATIONS #####
Nsim <- 100

#set up simulation results table
SimResults <- matrix(data = 0, nrow = Nsim, ncol = 18)
colnames(SimResults) <- strengths$Team

for (i in 1:Nsim){

  #create Ladder for each season
  Ladder <- tibble(Team = strengths$Team, W = 0, L = 0, D = 0, Pts = 0,
                   For = 0, Against = 0, Pct = 0)
  
  #simulate each season.
  for (j in 1:nrow(lambdas)) {
    
    #Here is the model working -- each score is simply a random drawing 
    #from a normal dist characterised by our lambda as mean, and sd
    homescore <- rnorm(1, mean = lambdas$homelambda[j], sd = lambdas$homesd[j])
    awayscore <- rnorm(1, lambdas$awaylambda[j], sd = lambdas$homesd[j])
    hometeam <- lambdas$Hometeam[j]
    awayteam <- lambdas$Awayteam[j]
    
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
SimResultsnorm <- as_tibble(SimResults)
SimResultsnorm

#Make tabe summarising results of simulation
Finalsimtable <- cbind.data.frame(MeanPos = colMeans(SimResultsnorm),
                                  MedianPos = apply(SimResultsnorm, 2, median),
                                  BestPos = apply(SimResultsnorm, 2, min),
                                  WorstPos = apply(SimResultsnorm, 2, max),
                                  percentWon =  apply(SimResultsnorm == 1, 2, sum)/nrow(SimResultsnorm)*100,
                                  percentTop4 = apply(SimResultsnorm <= 4, 2, sum)/nrow(SimResultsnorm)*100,
                                  percentTop8 = apply(SimResultsnorm <= 8, 2, sum)/nrow(SimResultsnorm)*100,
                                  percentBottom4 = apply(SimResultsnorm > 14, 2, sum)/nrow(SimResultsnorm)*100,
                                  percentLast = apply(SimResultsnorm == 18, 2, sum)/nrow(SimResultsnorm)*100) %>%
  cbind.data.frame(Team = strengths$Team)

Finalsimtablenorm <- Finalsimtable[,c(10, 2, 1, 3:9)] %>% arrange(MedianPos, MeanPos) %>% tibble()
Finalsimtablenorm

#could also try to model finals (but prob not)