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
lambdas.2019 <- add_column(fixture2019, homelambda = NA, homesd = NA, awaylambda = NA, awaysd = NA)

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
for (i in 1:nrow(lambdas.2019)) {
  lambdas.2019$homelambda[i] <- lambdahometeam(lambdas.2019$Hometeam[i], lambdas.2019$Awayteam[i])
  lambdas.2019$awaylambda[i] <- lambdaawayteam(lambdas.2019$Awayteam[i], lambdas.2019$Hometeam[i])
  lambdas.2019$homesd[i] <- sdhometeam(lambdas.2019$Hometeam[i], lambdas.2019$Awayteam[i])
  lambdas.2019$awaysd[i] <- sdawayteam(lambdas.2019$Awayteam[i], lambdas.2019$Hometeam[i])
}

####POISSON SIMULATION####
Nsim.pois.2019 <- 10

#set up simulation results table
SimResults <- matrix(data = 0, nrow = Nsim.pois.2019, ncol = 18)
colnames(SimResults) <- strengths$Team

for (i in 1:Nsim.pois.2019){
  
  #create Ladder for each season
  Ladder <- tibble(Team = strengths$Team, W = 0, L = 0, D = 0, Pts = 0,
                   For = 0, Against = 0, Pct = 0)
  
  #simulate each season.
  for (j in 1:nrow(lambdas.2019)) {
    
    #Here is the model working -- each score is simply a random drawing 
    #from a poisson distribution described by our calculated lambas
    homescore <- rpois(1, lambdas.2019$homelambda[j])
    awayscore <- rpois(1, lambdas.2019$awaylambda[j])
    hometeam <- lambdas.2019$Hometeam[j]
    awayteam <- lambdas.2019$Awayteam[j]
    
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
SimResultspois.2019 <- as_tibble(SimResults)


#Make table summarising results of simulation
Finalsimtable <- cbind.data.frame(MeanPos = colMeans(SimResultspois.2019),
                                  MedianPos = apply(SimResultspois.2019, 2, median),
                                  BestPos = apply(SimResultspois.2019, 2, min),
                                  WorstPos = apply(SimResultspois.2019, 2, max),
                                  percentWon =  apply(SimResultspois.2019 == 1, 2, sum)/nrow(SimResultspois.2019)*100,
                                  percentTop4 = apply(SimResultspois.2019 <= 4, 2, sum)/nrow(SimResultspois.2019)*100,
                                  percentTop8 = apply(SimResultspois.2019 <= 8, 2, sum)/nrow(SimResultspois.2019)*100,
                                  percentBottom4 = apply(SimResultspois.2019 > 14, 2, sum)/nrow(SimResultspois.2019)*100,
                                  percentLast = apply(SimResultspois.2019 == 18, 2, sum)/nrow(SimResultspois.2019)*100) %>%
  cbind.data.frame(Team = strengths$Team) 

Finalsimtablepois.2019 <- Finalsimtable[,c(10, 2, 1, 3:9)] %>% arrange(MedianPos, MeanPos)

####NORM SIMULATIONS####
Nsim.norm.2019 <- 10

#set up simulation results table
SimResults <- matrix(data = 0, nrow = Nsim.norm.2019, ncol = 18)
colnames(SimResults) <- strengths$Team

for (i in 1:Nsim.norm.2019){
  
  #create Ladder for each season
  Ladder <- tibble(Team = strengths$Team, W = 0, L = 0, D = 0, Pts = 0,
                   For = 0, Against = 0, Pct = 0)
  
  #simulate each season.
  for (j in 1:nrow(lambdas.2019)) {
    
    #Here is the model working -- each score is simply a random drawing 
    #from a normal dist characterised by our lambda as mean, and sd
    homescore <- rnorm(1, mean = lambdas.2019$homelambda[j], sd = lambdas.2019$homesd[j])
    awayscore <- rnorm(1, lambdas.2019$awaylambda[j], sd = lambdas.2019$awaysd[j])
    hometeam <- lambdas.2019$Hometeam[j]
    awayteam <- lambdas.2019$Awayteam[j]
    
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
SimResultsnorm.2019 <- as_tibble(SimResults)


#Make tabe summarising results of simulation
Finalsimtable.2019 <- cbind.data.frame(MeanPos = colMeans(SimResultsnorm.2019),
                                  MedianPos = apply(SimResultsnorm.2019, 2, median),
                                  BestPos = apply(SimResultsnorm.2019, 2, min),
                                  WorstPos = apply(SimResultsnorm.2019, 2, max),
                                  percentWon =  apply(SimResultsnorm.2019 == 1, 2, sum)/nrow(SimResultsnorm.2019)*100,
                                  percentTop4 = apply(SimResultsnorm.2019 <= 4, 2, sum)/nrow(SimResultsnorm.2019)*100,
                                  percentTop8 = apply(SimResultsnorm.2019 <= 8, 2, sum)/nrow(SimResultsnorm.2019)*100,
                                  percentBottom4 = apply(SimResultsnorm.2019 > 14, 2, sum)/nrow(SimResultsnorm.2019)*100,
                                  percentLast = apply(SimResultsnorm.2019 == 18, 2, sum)/nrow(SimResultsnorm.2019)*100) %>%
  cbind.data.frame(Team = strengths$Team)

Finalsimtablenorm.2019 <- Finalsimtable[,c(10, 2, 1, 3:9)] %>% arrange(MedianPos, MeanPos)

####TANH MODEL SIMULATIONS####
#set some params and functions
drawrate <- 7 / (198*5)
drawparam <- atanh(-drawrate)
probwin <- function(a, b, w, d = drawparam){
  return((tanh(((a - b)/w) + d) + 1)/ 2)}

# reverse ladder order to get rankings needed for the model
rankings <- rev(finalTable2018$Team)

##SIMULATIONS## 
Nsim.tanh.2019 <- 10 # for each weight parameter looped over
w <- 20
#create table for each simulation
gameresults.tanh <- matrix(data = 0, nrow = Nsim.tanh.2019, ncol = nrow(fixture2019))

#initilise lists
gameresultslist<- list()
SimResultsList.2019 <- list()
Finalsimlist.2019 <- list()

#loop over weight factor 
  for (i in 1:nrow(fixture2019)) {
    PW <- probwin(match(fixture2019[i, 1], rankings),
                  match(fixture2019[i, 2], rankings), 
                  w)
    PL <- probwin(match(fixture2019[i, 2], rankings),
                  match(fixture2019[i, 1], rankings), 
                  w)
    PD <- 1 - PW - PL 
    gameresults.tanh[,i] <- sample(x = c("W", "L", "D"), size = Nsim.tanh.2019, replace = TRUE, prob = c(PW, PL, PD))
  }
  
  gameresultslist[[w]] <- gameresults.tanh
  
  #create sim results
  SimResults <- matrix(data = 0, nrow = Nsim.tanh.2019, ncol = 18)
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
      SimResultsList.2019[[w]] <- SimResults

    }
    
  }
  
  #Make tabe summarising results of simulation
  Finalsimtabletanh <- cbind.data.frame(MeanPos = colMeans(SimResultsList.2019[[w]]),
                                        MedianPos = apply(SimResultsList.2019[[w]], 2, median),
                                        BestPos = apply(SimResultsList.2019[[w]], 2, min),
                                        WorstPos = apply(SimResultsList.2019[[w]], 2, max),
                                        percentWon =  apply(SimResultsList.2019[[w]] == 1, 2, sum)/nrow(SimResultsList.2019[[w]])*100,
                                        percentTop4 = apply(SimResultsList.2019[[w]] <= 4, 2, sum)/nrow(SimResultsList.2019[[w]])*100,
                                        percentTop8 = apply(SimResultsList.2019[[w]] <= 8, 2, sum)/nrow(SimResultsList.2019[[w]])*100,
                                        percentBottom4 = apply(SimResultsList.2019[[w]] > 14, 2, sum)/nrow(SimResultsList.2019[[w]])*100,
                                        percentLast = apply(SimResultsList.2019[[w]] == 18, 2, sum)/nrow(SimResultsList.2019[[w]])*100) %>%
    cbind.data.frame(Team = strengths$Team)  
  
  Finalsimtabletanh <- Finalsimtabletanh[,c(10, 2, 1, 3:9)] %>% arrange(MedianPos, MeanPos)
  Finalsimlist.2019[[w]] <- Finalsimtabletanh

#create mode function
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
} 

#graphing and comparing the two models. 

###GRAPH FOR POISSON MODEL###
#transpose data to tidy form for use in ggplot
SimResultspois.2019$simNumber <- sprintf("sim%d", seq(1:Nsim.pois.2019))
SimResultspois.2019.tidy <- gather(SimResultspois.2019,"Team", "Position", -simNumber)


#create ordered factor to order graph
SimResultspois.2019.tidy$Team <- factor(SimResultspois.2019.tidy$Team, levels = Finalsimtablepois.2019$Team)

#create graph 
pois2019graph <- ggplot(SimResultspois.2019.tidy, aes(x = Team, y = Position)) + 
                  geom_violin(aes(fill = Team, col = Team), trim = TRUE, kernel = "gaussian", adjust = 3) + 
                  stat_summary(fun.y=median, geom="point", size=4, color="black", shape = 0) +
                  stat_summary(fun.y=mean, geom="point", size=3, color="black", shape = 2) +
                  stat_summary(fun.y=Mode, geom="point", size=1, color="black", shape = 19) +
                  coord_flip(ylim = c(1,18)) +
                  scale_y_continuous(breaks = 1:18, labels = 1:18) +
                  theme(legend.position = "none", panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) 

###GRAPH FOR TANH MODEL###

#transpose data to tidy form for use in ggplot
SimResultstanh.2019 <- SimResultsList.2019[[20]] %>% as.data.frame()
SimResultstanh.2019$simNumber <- sprintf("sim%d", seq(1:Nsim.tanh.2019))
SimResultstanh.2019.tidy <- gather(SimResultstanh.2019,"Team", "Position", -simNumber)

#create ordered factor to order graph
SimResultstanh.2019.tidy$Team <- factor(SimResultstanh.2019.tidy$Team, levels = Finalsimlist.2019[[w]]$Team)

#create graph 
tanh2019graph <- ggplot(SimResultstanh.2019.tidy, aes(x = Team, y = Position)) + 
                  geom_violin(aes(fill = Team, col = Team), trim = TRUE, kernel = "gaussian", adjust = 3) + 
                  stat_summary(fun.y=median, geom="point", size=4, color="black", shape = 0) +
                  stat_summary(fun.y=mean, geom="point", size=3, color="black", shape = 2) +
                  stat_summary(fun.y=Mode, geom="point", size=1, color="black", shape = 19) +
                  coord_flip(ylim = c(1,18)) +
                  scale_y_continuous(breaks = 1:18, labels = 1:18) +
                  theme(legend.position = "none", panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) 

###GRAPH FOR NORM MODEL###

#transpose data to tidy form for use in ggplot
SimResultsnorm.2019$simNumber <- sprintf("sim%d", seq(1:Nsim.norm.2019))
SimResultsnorm.2019.tidy <- gather(SimResultsnorm.2019,"Team", "Position", -simNumber)

#create ordered factor to order graph
SimResultsnorm.2019.tidy$Team <- factor(SimResultsnorm.2019.tidy$Team, levels = Finalsimtablenorm.2019$Team)

#create graph 
norm2019graph <- ggplot(SimResultsnorm.2019.tidy, aes(x = Team, y = Position)) + 
                    geom_violin(aes(fill = Team, col = Team), trim = TRUE, kernel = "gaussian", adjust = 3) + 
                    stat_summary(fun.y=median, geom="point", size=4, color="black", shape = 0) +
                    stat_summary(fun.y=mean, geom="point", size=3, color="black", shape = 2) +
                    stat_summary(fun.y=Mode, geom="point", size=1, color="black", shape = 19) +
                    coord_flip(ylim = c(1,18)) +
                    scale_y_continuous(breaks = 1:18, labels = 1:18) +
                    theme(legend.position = "none", panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) 

## create comparison graphs ##

#combine two graphs##
Finalsimtablenorm.2019$Model <- "norm"
Finalsimtablepois.2019$Model <- "pois"
Finalsimlist.2019[[20]]$Model <- "tanh"

comparisonSimTable.2019 <- rbind(Finalsimtablenorm.2019, Finalsimtablepois.2019, Finalsimlist.2019[[20]])
comparisonSimTable.2019$Model <- factor(comparisonSimTable.2019$Model)

# create tidy data to compare a lot in one go
comparisonSimTable.2019.tidy <- gather(comparisonSimTable.2019, "Type", "Percent", 6:10)
comparisonSimTable.2019.tidy 

#create ordered factor to order graph
comparisonSimTable.2019.tidy$Team <- factor(comparisonSimTable.2019.tidy$Team, levels = Finalsimtablepois.2019$Team)

#create ordered factor of 'Type' to order facet
comparisonSimTable.2019.tidy$Type <- factor(comparisonSimTable.2019.tidy$Type, 
                                            levels = c("percentWon", "percentTop4", "percentTop8",
                                                       "percentBottom4", "percentLast"))

#draw graphs
ggplot(comparisonSimTable.2019, aes(x = Team, y = percentWon, fill = Model)) + 
  geom_bar(stat = "identity", position = 'dodge')
ggplot(comparisonSimTable.2019, aes(x = Team, y = percentTop8, fill = Model)) +
  geom_bar(stat = "identity", position = 'dodge')

ggplot(comparisonSimTable.2019.tidy, aes(x = Team, y = Percent, fill = Model)) + 
  geom_bar(stat = "identity", position = 'dodge') + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6)) +
  facet_wrap(~Type, nrow = 1)

