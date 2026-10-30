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

png('PointsvsRank.png', width = 800, height = 600)
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

# -----------------------------
# First GLM
# -----------------------------

mGoals = glm(goals_scored ~ home_away + opponent_rank,
             family = poisson(link = "log"),
             data = ashford_matches)
summary(mGoals)

rr = exp(cbind(estimate = coef(mGoals), confint(mGoals)))
print(round(rr, 3))

newData = data.frame(
  home_away = c("Home", "Home", "Away", "Away"),
  opponent_rank = c(1, 24, 1, 24)
)

newData$predicted_goals = predict(mGoals, newData, type = "response")
print(newData)

disp = sum(residuals(mGoals, type = "pearson")^2) / df.residual(mGoals)
cat("Poisson dispersion:", round(disp, 3), "\n")

# Dispersion at 0.99 is close enough to 1 that we do not need to change distribution to quasi-poisson or negative binomial

mPoints = lm(points ~ home_away + opponent_rank, data = ashford_matches)
summary(mPoints)
confint(mPoints)

# ------------------------------------
# Injury exploration
# ------------------------------------

posTab = aggregate(injured ~ position, data = league_players, FUN = function(x)
  c(n = length(x), injured = sum(x), rate = mean(x)))
print(posTab)

tlTab = aggregate(injured ~ training_load, data = league_players, FUN = function(x)
  c(n = length(x), injured = sum(x), rate = mean(x)))
print(tlTab)

png("AgebyInjury.png", width = 800, height = 800)
boxplot(age ~ injured, data = league_players,
        names = c("Not Injured", "Injured"),
        xlab = "Injured statues", ylab = "Age",
        main = "Age by injury status", col = c("lightgreen", "salmon"))

dev.off()

# ------------------------------------------
# Logistic regression
# ------------------------------------------

mInjury = glm(injured ~ age + minutes_played + training_load + prev_injuries + bmi + position,
              family = binomial(link = "logit"),
              data = league_players)
summary(mInjury)

orTab = exp(cbind(OR = coef(mInjury), confint(mInjury))) # Odds ratio
print(round(orTab, 3))

league_players$pred_inj = predict(mInjury, type = "response")
print(tapply(league_players$pred_inj, league_players$injured, mean))

actual = league_players$injured
pred = league_players$pred_inj
n1 = sum(actual == 1); n0 = sum(actual == 0)
auc = (sum(rank(pred)[actual == 1]) - n1*(n1 + 1)/2) / (n1*n0)

cat("Injured model AUC:", round(auc, 3), "\n")




