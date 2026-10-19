byVenue <- split(ashford_matches, ashford_matches$home_away)

venueSummary <- do.call(rbind, lapply(names(byVenue), function(v) {
  d <- byVenue[[v]]
  data.frame(
    home_away = v,
    n = nrow(d),
    goalsFor = sum(d$goals_scored),
    goalsAgainst = sum(d$goals_conceded),
    avgFor = mean(d$goals_scored),
    avgAgainst = mean(d$goals_conceded),
    winRate = mean(d$goals_scored > d$goals_conceded),
    drawRate = mean(d$goals_scored == d$goals_conceded),
    lossRate = mean(d$goals_scored < d$goals_conceded)
  )
})) # Creates table of data
print(venueSummary)

ashford_matches$points <- ifelse(ashford_matches$goals_scored > ashford_matches$goals_conceded, 3,
                         ifelse(ashford_matches$goals_scored == ashford_matches$goals_conceded, 1, 0))
ashford_matches$gd <- ashford_matches$goals_scored - ashford_matches$goals_conceded

pointsSummary <- do.call(rbind, lapply(split(ashford_matches, ashford_matches$home_away), function(d) {
  data.frame(home_away = d$home_away[1],
             ppg = mean(d$points),
             avg_gd = mean(d$gd)) # Creates table of data
}))
print(pointsSummary)

h <- ashford_matches[ashford_matches$home_away == 'Home', ]
a <- ashford_matches[ashford_matches$home_away == 'Away', ]

png('GoalsvsRank.png', width = 800, height = 600)
plot(ashford_matches$opponent_rank, ashford_matches$goals_scored, 
     col = ifelse(ashford_matches$home_away == 'Home', 'blue', 'red'),
     pch = 19, 
     xlab = "Opponent rank (Lower = Stronger)",
     ylab = "Goals scored", 
     main = "Goals scored vs Opponent rank") # Creates png

abline(lm(goals_scored ~ opponent_rank, data = h), col = 'blue', lwd = 2)
abline(lm(goals_scored ~ opponent_rank, data = a), col = 'red', lwd = 2)

legend('topright', legend = c('Home', 'Away'), col = c('blue', 'red'),
       pch = 19, lty = 1)

dev.off() # Writes image to disk

png('PointsvsRank', width = 800, height = 600)
plot(jitter(ashford_matches$opponent_rank), jitter(ashford_matches$points),
     col = ifelse(ashford_matches$home_away == 'Home', 'blue', 'red'),
     pch = 19,
     xlab = "Opponents rank (Lower = Stronger)",
     ylab = "Points",
     main = "Points vs Opponent rank") # Creates png
abline(lm(points ~ opponent_rank, data = h), col = 'blue', lwd = 2)
abline(lm(points ~ opponent_rank, data = a), col = 'red', lwd = 2)

legend('topright', legend = c('Home', 'Away'), col = c('blue', 'red'),
       pch = 19, lty = 1)

dev.off() # Writes image to disk

