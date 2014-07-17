#####
setwd("C:/Users/Popsu/Videos/opiskelu/videot/SABR101x_Sabermetrics_edX/Module6")
rox <- read.csv("coors_park_factors.csv")

park <- subset(rox, home == "COL")
away <- subset(rox, visitor == "COL")

park_ratio <- with(park, sum(home_hr + visitor_hr) / sum(home_ab + visitor_ab))

#park_ratio <- sum(park$home_hr + park$visitor_hr) / sum(park$home_ab + park$visitor_ab)

away_ratio <- with(away, sum(home_hr + visitor_hr) / sum(home_ab + visitor_ab))

park_factor <- 100*park_ratio/away_ratio

######
require(plyr)

pf_all <- read.csv("retrosheet_sandbox.csv")
pf_all <- within(pf_all, levels(home)[levels(home)=="FLO"] <- "MIA")
pf_all <- within(pf_all, levels(visitor)[levels(visitor) == "FLO"] <- "MIA")

#changed code slightly so you can input season_year as a vector also for multiple years
pf_stat_teams <- function(stat, data, season_year=2013) {
    data <- subset(data, year %in% season_year) 
    home_stat <- paste("home", stat, sep="_")
    visitor_stat <- paste("visitor", stat, sep="_")
    pf_stat <- paste("pf", stat, season_year[1], sep="_")
    cols <- c(home_stat, visitor_stat, "home_ab", "visitor_ab")
    park_sums <- ddply(data, .(home), colwise(sum, cols))
    away_sums <- ddply(data, .(visitor), colwise(sum, cols))
    park_sums$park_ratio <- (park_sums[[home_stat]] + park_sums[[visitor_stat]]) / (park_sums[["home_ab"]] + park_sums[["visitor_ab"]])
    away_sums$away_ratio <- (away_sums[[home_stat]] + away_sums[[visitor_stat]]) / (away_sums[["home_ab"]] + away_sums[["visitor_ab"]])
    pf <- merge(park_sums, away_sums, by.x = "home", by.y = "visitor")
    pf[[pf_stat]] <- with(pf, pf$park_ratio / pf$away_ratio)
    pf <- rename(pf, replace = c("home" = "team"))
    pf <- subset(pf, select = c("team", pf_stat))

    return(pf)
}

hr_2013 <- pf_stat_teams("hr", pf_all)

a2b_2010 <- pf_stat_teams("2b", pf_all, 2010)
which.min(a2b_2010[,2]) 

bb_2011 <- pf_stat_teams("bb", pf_all, 2011)
which.max(bb_2011[,2])
bb_2011[which.max(bb_2011[,2]),]


##### Own testing below

allParkFactors <- pf_stat_teams("score", pf_all, 2009:2015)




