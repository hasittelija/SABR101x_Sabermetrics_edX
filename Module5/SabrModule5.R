#####
setwd("../SABR101x_Sabermetrics_edX/Module5/")
require(lattice)

attendance_data <- read.csv("game_statistics.csv")
summary(attendance_data)
attach(attendance_data)

Game_logs = attendance_data[c("attendance", "game_minutes", "outs")]
splom(Game_logs, xlab = "Retrosheet Gamelogs Scatterplot Matrix")

plot(outs, game_minutes)
TOGVouts = lm(game_minutes~outs)
TOGVouts
abline(TOGVouts)
#####

game_statistics <- attendance_data
game_statistics$total_runs <- with(game_statistics, home_score + visitor_score)

total_time_data <- game_statistics[c("game_minutes", "total_runs", "outs")]

splom(total_time_data)

cor(total_time_data)

projected_minutes <- lm(game_minutes ~ total_runs + outs, data = total_time_data)
projected_minutes

#####
game_statistics$RedSox_playing <- with(game_statistics, ifelse(home == "BOS" | visitor == "BOS", 1, 0))

RedSox_games <- game_statistics[game_statistics$RedSox_playing == 1,]
head(RedSox_games)
colnames(RedSox_games)
mean(RedSox_games[,"total_runs"])
mean(game_statistics[,"total_runs"])

hist(RedSox_games$total_runs, breaks = 40)

#####

game_statistics$BrianGorman <- with(game_statistics, ifelse(hp_ump_name == "Brian Gorman", 1, 0))
game_statistics$JimJoyce <- with(game_statistics, ifelse(hp_ump_name == "Jim Joyce", 1, 0))
game_statistics$DaleScott <- with(game_statistics, ifelse(hp_ump_name == "Dale Scott", 1, 0))
game_statistics$TimWelke <- with(game_statistics, ifelse(hp_ump_name == "Tim Welke", 1, 0))

colnames(game_statistics)
colMeans(game_statistics[,18:21])

BG_games <- game_statistics[game_statistics$BrianGorman == 1,]
JJ_games <- game_statistics[game_statistics$JimJoyce == 1,]
DS_games <- game_statistics[game_statistics$DaleScott == 1,]
TW_games <- game_statistics[game_statistics$TimWelke == 1,]

tapply(game_statistics$total_runs, game_statistics$hp_ump_name, mean)
mean(game_statistics$total_runs) # 8.807244

colnames(game_statistics)
game_statistics$ChosenRef <- with(game_statistics, xor(BrianGorman,xor(JimJoyce,xor(DaleScott,TimWelke))))
colSums(game_statistics[,18:22])

RedSoxUmpiresFit <- lm(total_runs ~ RedSox_playing + BrianGorman + JimJoyce + DaleScott + TimWelke, data = game_statistics)

#RedSoxUmpiresFit <- lm(total_runs ~ RedSox_playing + ChosenRef, data = game_statistics)

RedSoxUmpiresFit
confint(RedSoxUmpiresFit)

