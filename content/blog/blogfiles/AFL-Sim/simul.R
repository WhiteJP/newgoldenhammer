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

#get points for by team, H and A
pointssummary <- pointstidy %>% group_by(HorA, Team, FororAgainst) %>% summarise(Points = mean(Points)) %>% ungroup
pointssummary <- pointssummary[, c(2,1,3,4)]

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

lambdas <- add_column(fixture2019, homelambda = NA, awaylambda = NA)

#create lambda functions
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

#populate lamba values. 
for (i in 1:nrow(lambdas)) {
  lambdas$homelambda[i] <- lambdahometeam(lambdas$Hometeam[i], lambdas$Awayteam[i])
  lambdas$awaylambda[i] <- lambdaawayteam(lambdas$Awayteam[i], lambdas$Hometeam[i])
}

# do one season 

  # create table
leaguetable <- tibble(Team = strengths$Team, W = 0, L = 0, D = 0, Pts = 0,
                      For = 0, Against = 0, Pct = 0)

  #simulate season.
for (i in 1:nrow(lambdas)) {
  homescore <- rpois(1, lambdas$homelambda[i])
  awayscore <- rpois(1, lambdas$awaylambda[i])
  
  #populate scores
  leaguetable[leaguetable$Team == lambdas$Hometeam[i],]$For <- leaguetable[leaguetable$Team == lambdas$Hometeam[i],]$For + homescore
  leaguetable[leaguetable$Team == lambdas$Hometeam[i],]$Against <- leaguetable[leaguetable$Team == lambdas$Hometeam[i],]$Against + awayscore 
  leaguetable[leaguetable$Team == lambdas$Awayteam[i],]$For <- leaguetable[leaguetable$Team == lambdas$Awayteam[i],]$For + awayscore
  leaguetable[leaguetable$Team == lambdas$Awayteam[i],]$Against <- leaguetable[leaguetable$Team == lambdas$Awayteam[i],]$Against + homescore
  
  #populate table for draw
  if (homescore == awayscore) {
    #hometeam changes
    leaguetable[leaguetable$Team == lambdas$Hometeam[i],]$D <- leaguetable[leaguetable$Team == lambdas$Hometeam[i],]$D + 1
    leaguetable[leaguetable$Team == lambdas$Hometeam[i],]$Pts <- leaguetable[leaguetable$Team == lambdas$Hometeam[i],]$Pts + 2
    #awayteam changes
    leaguetable[leaguetable$Team == lambdas$Awayteam[i],]$D <- leaguetable[leaguetable$Team == lambdas$Awayteam[i],]$D + 1
    leaguetable[leaguetable$Team == lambdas$Awayteam[i],]$Pts <- leaguetable[leaguetable$Team == lambdas$Awayteam[i],]$Pts + 2
    
  #populate table for hometeam win
  } else if (homescore > awayscore) {
    #hometeam changes
    leaguetable[leaguetable$Team == lambdas$Hometeam[i],]$W <- leaguetable[leaguetable$Team == lambdas$Hometeam[i],]$W + 1
    leaguetable[leaguetable$Team == lambdas$Hometeam[i],]$Pts <- leaguetable[leaguetable$Team == lambdas$Hometeam[i],]$Pts + 4
    
    #awayteam changes
    leaguetable[leaguetable$Team == lambdas$Awayteam[i],]$L <- leaguetable[leaguetable$Team == lambdas$Awayteam[i],]$L + 1
    
  #populate for away team win
  } else {
    #hometeam changes
    leaguetable[leaguetable$Team == lambdas$Hometeam[i],]$L <- leaguetable[leaguetable$Team == lambdas$Hometeam[i],]$L + 1
    
    #awayteam changes
    leaguetable[leaguetable$Team == lambdas$Awayteam[i],]$W <- leaguetable[leaguetable$Team == lambdas$Awayteam[i],]$W + 1
    leaguetable[leaguetable$Team == lambdas$Awayteam[i],]$Pts <- leaguetable[leaguetable$Team == lambdas$Awayteam[i],]$Pts + 4
  }
}

#calculate percentage and order final table
leaguetable$Pct <- leaguetable$For / leaguetable$Against * 100
FinalTable <- leaguetable %>% arrange(desc(Pts), desc(Pct))
FinalTable

###Now to try to simulate
Nsim <- 100

#set up simulation results table
SimResults <- matrix(data = 0, nrow = Nsim, ncol = 18)
colnames(SimResults) <- strengths$Team
SimResults <- as_tibble(SimResults)

for (i in 1:Nsim){
  leaguetable <- tibble(Team = strengths$Team, W = 0, L = 0, D = 0, Pts = 0,
                        For = 0, Against = 0, Pct = 0)
  
  for (j in 1:nrow(lambdas)){
    #homescore <- vector(mode = "numeric", length = nrow(lambdas))
    #awayscore <- vector(mode = "numeric", length = nrow(lambdas))
    homescore <- rpois(1, lambdas$homelambda[j])
    awayscore <- rpois(1, lambdas$awaylambda[j])
    
    #populate scores
    leaguetable[leaguetable$Team == lambdas$Hometeam[j],]$For <- leaguetable[leaguetable$Team == lambdas$Hometeam[j],]$For + homescore
    leaguetable[leaguetable$Team == lambdas$Hometeam[j],]$Against <- leaguetable[leaguetable$Team == lambdas$Hometeam[j],]$Against + awayscore 
    leaguetable[leaguetable$Team == lambdas$Awayteam[j],]$For <- leaguetable[leaguetable$Team == lambdas$Awayteam[j],]$For + awayscore
    leaguetable[leaguetable$Team == lambdas$Awayteam[j],]$Against <- leaguetable[leaguetable$Team == lambdas$Awayteam[j],]$Against + homescore
    
    #populate table for draw
    if (homescore == awayscore) {
      #hometeam changes
      leaguetable[leaguetable$Team == lambdas$Hometeam[j],]$D <- leaguetable[leaguetable$Team == lambdas$Hometeam[j],]$D + 1
      leaguetable[leaguetable$Team == lambdas$Hometeam[j],]$Pts <- leaguetable[leaguetable$Team == lambdas$Hometeam[j],]$Pts + 2
      #awayteam changes
      leaguetable[leaguetable$Team == lambdas$Awayteam[j],]$D <- leaguetable[leaguetable$Team == lambdas$Awayteam[j],]$D + 1
      leaguetable[leaguetable$Team == lambdas$Awayteam[j],]$Pts <- leaguetable[leaguetable$Team == lambdas$Awayteam[j],]$Pts + 2
      
      #populate for hometeam win
    } else if (homescore > awayscore) {
      #hometeam changes
      leaguetable[leaguetable$Team == lambdas$Hometeam[j],]$W <- leaguetable[leaguetable$Team == lambdas$Hometeam[j],]$W + 1
      leaguetable[leaguetable$Team == lambdas$Hometeam[j],]$Pts <- leaguetable[leaguetable$Team == lambdas$Hometeam[j],]$Pts + 4
      
      #awayteam changes
      leaguetable[leaguetable$Team == lambdas$Awayteam[j],]$L <- leaguetable[leaguetable$Team == lambdas$Awayteam[j],]$L + 1
      
      #populate for away team win
    } else {
      #hometeam changes
      leaguetable[leaguetable$Team == lambdas$Hometeam[j],]$L <- leaguetable[leaguetable$Team == lambdas$Hometeam[j],]$L + 1
      
      #awayteam changes
      leaguetable[leaguetable$Team == lambdas$Awayteam[j],]$W <- leaguetable[leaguetable$Team == lambdas$Awayteam[j],]$W + 1
      leaguetable[leaguetable$Team == lambdas$Awayteam[j],]$Pts <- leaguetable[leaguetable$Team == lambdas$Awayteam[j],]$Pts + 4
    }
  }
  
  #calculate percentage and order final table
  leaguetable$Pct <- leaguetable$For / leaguetable$Against * 100
  FinalTable <- leaguetable %>% arrange(desc(Pts), desc(Pct))
  
  #populate simulation results table
    for (k in 1:nrow(FinalTable)) {
        SimResults[i, FinalTable$Team[k]] <- k
     }
} 
SimResults


Finalsimtable <- cbind.data.frame(MeanPos = colMeans(SimResults),
                       MedianPos = apply(SimResults, 2, median),
                       BestPos = apply(SimResults, 2, min),
                       WorstPos = apply(SimResults, 2, max)) %>%
                 cbind.data.frame(Team = strengths$Team)
                 
Finalsimtable <- Finalsimtable[,c(5, 2, 1, 3, 4)] %>% arrange(MedianPos, MeanPos)
Finalsimtable
