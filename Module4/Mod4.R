setwd("C:/Users/Popsu/Videos/opiskelu/videot/SABR101x_Sabermetrics_edX/Module4")
win_estimators <- read.csv("win_estimators.csv")
summary(win_estimators)
mean(win_estimators$WPct)
sd(win_estimators$WPct)

which.max(sapply(win_estimators[,10:15], median))
mean(win_estimators$WPct)
sd(win_estimators$WPct)

mean(win_estimators$R)
1/162

#x= avgRuns - squareroot(avgRuns2/(1/(.500 - 1/162) - 1))

avgRuns <- mean(win_estimators$R)

x <- avgRuns - sqrt(avgRuns^2/(1/(.500 - 1/162)-1))

# (avgRuns - x)2/((avgRuns - x)2+avgRuns2)=.500 - 1/162

(avgRuns - x)^2 / (( avgRuns - x)^2 + avgRuns^2)
.500 - 1/162

########

estimators <- win_estimators[c('RperG', 'RAperG', 'WPct','Cook_WPct',  'Soolman_WPct',  'Kross_WPct', 'Smyth_WPct', 'BJames_Pythag_WPct', 'BJames_Pythag_WPctII')]
require(lattice)
splom(estimators, xlab = "Win Estimators")

plot(estimators$RperG, estimators$RAperG)

fit <- lm(estimators$RperG ~ estimators$RAperG)

#####

wpct_95th_pct = quantile(win_estimators$WPct, .95)
top_winners = win_estimators[win_estimators$WPct >= wpct_95th_pct, ]

summary(top_winners)
mean(top_winners$WPct)
which.max(sapply(top_winners[,10:15], median))

cor(top_winners$WPct, top_winners$BJames_Pythag_WPct)
cor(estimators$WPct, estimators$BJames_Pythag_WPct)
splom(top_winners[,9:15], xlab = "Win Estimators")

#####

#AL East (Baltimore Orioles, Boston Red Sox, New York Yankees, Tampa Bay Rays, Toronto Blue Jays). Call this new data frame AL_East_estimator
#NL_West_estimator, including only teams from the NL West (Arizona Diamondbacks, Colorado Rockies, Los Angeles Dodgers, San Diego Padres, and San Francisco Giants).

AL_East <- c("BAL", "BOS", "NYA", "TBA", "TOR")
NL_West <- c("ARI", "COL", "LAN", "SDN", "SFN")

ALWin <- win_estimators[win_estimators$teamID %in% AL_East,]
NLWest <- win_estimators[win_estimators$teamID %in% NL_West,]

ALWin2 <- ALWin[ALWin$WPct <= 0.75 & ALWin$WPct >= 0.3,]
hist(ALWin2$WPct, breaks = seq(0.3, 0.75, by = 0.05), right = FALSE)
#sd(ALWin$WPct)
#sd(ALWin$Kross_WPct)
#sd(ALWin$Soolman_WPct)

sapply(ALWin[,c("WPct", "Kross_WPct", "Soolman_WPct")], sd)
sapply(NLWest[,c("WPct", "Kross_WPct", "Soolman_WPct")], sd)

# We have to replace avgRuns with avgRuns for these leagues, and inside the sqrt with avgRunsAgainst
#x <- avgRuns - sqrt(avgRuns^2/(1/(.500 - 1/162)-1))
# thus: x <- avgRuns - sqrt(avgRunsAgainst^2/(1/(.500 - 1/162)-1))

avgWPctAL <- mean(ALWin$WPct) # Warning, we are averaging averages so might be wrong
avgRunsAL <- mean(ALWin$R)
avgRunsAgainstAL <- mean(ALWin$RA)

avgWPctNL <- mean(NLWest$WPct) # Warning, we are averaging averages so might be wrong
avgRunsNL <- mean(NLWest$R)
avgRunsAgainstNL <- mean(NLWest$RA)

xAL <- avgRunsAL - sqrt(avgRunsAgainstAL^2/(1/(avgWPctAL - 1/162)-1))
xNL <- avgRunsNL - sqrt(avgRunsAgainstNL^2/(1/(avgWPctNL - 1/162)-1))

