library(ggcorrplot)

#graphing and comparing the two models. 

#create mode function
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
###GRAPH FOR POISSON MODEL###

#transpose data to tidy form for use in ggplot
SimResultspois$simNumber <- sprintf("sim%d", seq(1:Nsim))
SimResultspois.tidy <- gather(SimResultspois,"Team", "Position", -simNumber)
SimResultspois.tidy 

#create ordered factor to order graph
SimResultspois.tidy$Team <- factor(SimResultspois.tidy$Team, levels = Finalsimtablepois$Team)

#create graph 
ggplot(SimResultspois.tidy, aes(x = Team, y = Position)) + 
  geom_violin(aes(fill = Team, col = Team), trim = TRUE, kernel = "gaussian", adjust = 3) + 
  stat_summary(fun.y=median, geom="point", size=4, color="black", shape = 0) +
  stat_summary(fun.y=mean, geom="point", size=3, color="black", shape = 2) +
  stat_summary(fun.y=Mode, geom="point", size=1, color="black", shape = 19) +
  coord_flip(ylim = c(1,18)) +
  scale_y_continuous(breaks = 1:18, labels = 1:18) +
  theme(legend.position = "none", panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) 
  
###GRAPH FOR NORM MODEL###

#transpose data to tidy form for use in ggplot
SimResultsnorm$simNumber <- sprintf("sim%d", seq(1:Nsim))
SimResultsnorm.tidy <- gather(SimResultsnorm,"Team", "Position", -simNumber)
SimResultsnorm.tidy 

#create ordered factor to order graph
SimResultsnorm.tidy$Team <- factor(SimResultsnorm.tidy$Team, levels = Finalsimtablenorm$Team)

#create graph 
ggplot(SimResultsnorm.tidy, aes(x = Team, y = Position)) + 
  geom_violin(aes(fill = Team, col = Team), trim = TRUE, kernel = "gaussian", adjust = 3) + 
  stat_summary(fun.y=median, geom="point", size=4, color="black", shape = 0) +
  stat_summary(fun.y=mean, geom="point", size=3, color="black", shape = 2) +
  stat_summary(fun.y=Mode, geom="point", size=1, color="black", shape = 19) +
  coord_flip(ylim = c(1,18)) +
  scale_y_continuous(breaks = 1:18, labels = 1:18) +
  theme(legend.position = "none", panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) 


## create comparison graphs ##

#combine two graphs##
Finalsimtablenorm$Model <- "norm"
Finalsimtablepois$Model <- "pois"

comparisonSimTable <- rbind(Finalsimtablenorm, Finalsimtablepois)
comparisonSimTable$Model <- factor(comparisonSimTable$Model)

# create tidy data to compare a lot in one go
comparisonSimTable.tidy <- gather(comparisonSimTable, "Type", "Percent", 6:10)
comparisonSimTable.tidy 

#create ordered factor to order graph
comparisonSimTable.tidy$Team <- factor(comparisonSimTable.tidy$Team, levels = Finalsimtablepois$Team)

#create ordered factor of 'Type' to order facet
comparisonSimTable.tidy$Type <- factor(comparisonSimTable.tidy$Type, 
                                       levels = c("percentWon", "percentTop4", "percentTop8", "percentBottom4", "percentLast"))


#draw graph
ggplot(comparisonSimTable, aes(x = Team, y = percentWon, fill = Model)) + geom_bar(stat = "identity", position = 'dodge')
ggplot(comparisonSimTable, aes(x = Team, y = percentTop8, fill = Model)) + geom_bar(stat = "identity", position = 'dodge')

ggplot(comparisonSimTable.tidy, aes(x = Team, y = Percent, fill = Model)) + 
  geom_bar(stat = "identity", position = 'dodge') + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6)) +
  facet_wrap(~Type, nrow = 1)

## WHich of these two models best captures last years fnal table.
# Personally, I think the normal distribution model is by far the best of these two, It alligns with the variable nature of afl football and gives more beleiveable results.
# It also allows for teams which footy intution says have a chance of winning the cup to win the cup. # but lets see which dataset best comports with last years data. 
# lets import last years finaltable data.  
#also note that comparing with last year isnt the best method, because we have a different fixture (unlike football in the UK everyteam does not play eachother twice )

finalTable2018 <- read_excel("HAinfo.xlsx", sheet = 6)
View(finalTable2018)

#create table with different orders. 
ordercomparison <- data.frame(Team = factor(finalTable2018$Team),
                              Order2018 = 1:18,
                              OrderNorm = match(finalTable2018$Team, Finalsimtablenorm$Team),
                              OrderPois = match(finalTable2018$Team, Finalsimtablepois$Team))
ordercomparison

#run correlation matrix
cormatrix <- cor(ordercomparison[,2:4], use = "all.obs", method = "spearman")
cormatrix

# lets find a nice way to graphically look at that
ggcorrplot(cormatrix, lab = TRUE)
orderCorrPlot <- ggcorrplot(cormatrix, lab = TRUE, type = "lower")

#well as you can see there isnt much difference between the data. 

#now lets do some more detailed scatter plots
y2018.v.norm <- ggplot(ordercomparison, aes(x = Order2018, y = OrderNorm, label = Team)) + 
                geom_text(size = 2.5) +
                geom_abline(slope = 1 ,intercept = 0, alpha = .5) + 
                scale_x_continuous(breaks = 1:18, labels = 1:18) +
                scale_y_continuous(breaks = 1:18, labels = 1:18) +
                theme(legend.position = "none", panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank()) 
  
y2018.v.pois <- ggplot(ordercomparison, aes(x = Order2018, y = OrderPois, label = Team)) + 
                geom_text(size = 2.5) +
                geom_abline(slope = 1 ,intercept = 0, alpha = .5) + 
                scale_x_continuous(breaks = 1:18, labels = 1:18) +
                scale_y_continuous(breaks = 1:18, labels = 1:18) +
                theme(legend.position = "none", panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank()) 

pois.v.norm <- ggplot(ordercomparison, aes(x = OrderPois, y = OrderNorm, label = Team)) + 
                geom_text(size = 2.5) +
                geom_abline(slope = 1 ,intercept = 0, alpha = .5) + 
                scale_x_continuous(breaks = 1:18, labels = 1:18) +
                scale_y_continuous(breaks = 1:18, labels = 1:18) +
                theme(legend.position = "none", panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank())

#grph with vertical cowplot
cowplot::plot_grid(y2018.v.norm, y2018.v.pois, pois.v.norm, ncol = 1)

# makeshift scatterplotmatrix

norm.v.2018 <- ggplot(ordercomparison, aes(x = OrderNorm, y = Order2018, label = Team)) + 
  geom_text(size = 2.5) +
  geom_abline(slope = 1 ,intercept = 0, alpha = .5) + 
  scale_x_continuous(breaks = 1:18, labels = 1:18) +
  scale_y_continuous(breaks = 1:18, labels = 1:18) +
  theme(legend.position = "none", panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank()) 

pois.v.2018 <- ggplot(ordercomparison, aes(x = OrderPois, y = Order2018, label = Team)) + 
  geom_text(size = 2.5) +
  geom_abline(slope = 1 ,intercept = 0, alpha = .5) + 
  scale_x_continuous(breaks = 1:18, labels = 1:18) +
  scale_y_continuous(breaks = 1:18, labels = 1:18) +
  theme(legend.position = "none", panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank()) 

#make cowplots
row1 <- cowplot::plot_grid(orderCorrPlot, pois.v.norm, ncol = 2)
row2 <- cowplot::plot_grid(norm.v.2018, pois.v.2018, ncol = 2)
cowplot::plot_grid(row1, row2, ncol = 1)

