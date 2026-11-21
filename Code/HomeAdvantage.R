matches$result = factor(
  ifelse(matches$goals_scored > matches$goals_conceded, "Win",
         ifelse(matches$goals_scored < matches$goals_conceded, "Loss", "Draw")),
  levels = c("Loss", "Draw", "Win"), ordered = TRUE
)

matches$home_away = factor(matches$home_away, levels = c("Away", "Home"))
matches$rank10 = matches$opponent_rank / 10

table(matches$result)

venueTable = table(matches$home_away, matches$result)
venueTable

chisq.test(venueTable)
chisq.test(venueTable, simulate.p.value = TRUE, B = 20000)
fisher.test(venueTable)

matches$rankBand = cut(matches$opponent_rank, c(0, 6, 12, 24),
                        labels = c("Top", "Middle", "Low"))
table(matches$rankBand, matches$result)
aggregate(opponent_rank ~ result, data = matches, FUN = mean)

# ----------------------------
# polr model fitting
# ----------------------------

f_0 = polr(result ~ 1                 , data = matches, Hess = TRUE)
f_h = polr(result ~ home_away         , data = matches, Hess = TRUE)
f_r = polr(result ~ rank10            , data = matches, Hess = TRUE)
f_a = polr(result ~ home_away + rank10, data = matches, Hess = TRUE)
f_i = polr(result ~ home_away * rank10, data = matches, Hess = TRUE)

aicTable = data.frame(model = c("null", "venue", "rank", "venue + rank", "venue x rank"),
                      df = sapply(list(f_0, f_h, f_r, f_a, f_i), function(f) length(coef(f)) + length(f$zeta)),
                      AIC = AIC(f_0, f_h, f_r, f_a, f_i)$AIC,
                      BIC = BIC(f_0, f_h, f_r, f_a, f_i)$BIC)
aicTable

anova(f_h, f_a)
anova(f_r, f_a)
anova(f_0, f_a)
anova(f_a, f_i)

# ----------------------------
# polr model selection
# ----------------------------

summary(f_a)
coefTable = coef(summary(f_a))
coefTable = cbind(coefTable, p = 2 * pnorm(abs(coefTable[, "t value"]), lower.tail = FALSE))
round(coefTable, 3)

confi = confint(f_a)
odds = exp(cbind(OR = coef(f_a), confi))
round(odds, 2)
exp(coef(f_a)["rank10"] / 10)

# ----------------------------
# diagnostics
# ----------------------------

# nominal test for proportional odds assumption
f_clm = clm(result ~ home_away + rank10, data = matches)
nominal_test(f_clm)
f_nom = clm(result ~ 1, nominal = ~ home_away + rank10, data = matches)
anova(f_clm, f_nom)
# observed vs expected counts
P = predict(f_a, type = "probs")
rbind(expected = colSums(P), observed = as.numeric(table(matches$result)))
for (v in c("Away", "Home")) {
  i = matches$home_away == v
  print(rbind(expected = colSums(P[i, ]), observed = as.numeric(table(matches$result[i]))))
}

# seperate binary logits at each cut point
b1 = glm(I(result != "Loss") ~ home_away + rank10, data = matches, family = binomial)
b2 = glm(I(result == "Win") ~ home_away + rank10, data = matches, family = binomial)
rbind(`Loss vs Draw/Win` = coef(b1)[-1], `Loss/Draw vs Win` = coef(b2)[-1])


# predicted probabilities with uncertainty
new = expand.grid(home_away = factor(c("Away", "Home"), levels = c("Away", "Home")),
                  opponent_rank = c(3, 12, 20))
new$rank10 = new$opponent_rank / 10
pred = predict(f_a, newdata = new, type = "probs")
cbind(new[, c("home_away", "opponent_rank")], round(pred, 3))

simpred = function(B = 5000) {
  th = c(coef(f_a), f_a$zeta)
  V = vcov(f_a)
  draws = MASS::mvrnorm(B, th, V)
  out = array(NA, c(B, nrow(new), 3))
  for (b in seq_len(B)) {
    beta = draws[b, 1:2]; zeta = draws[b, 3:4]
    eta = beta[1] * (new$home_away == "Home") + beta[2] * new$rank10
    c1 = plogis(zeta[1] - eta); c2 = plogis(zeta[2] - eta)
    out[b, , ] = cbind(c1, c2 - c1, 1 - c2)
  }
  out
}

S = simpred()
ciPred = apply(S, c(2, 3), quantile, probs = c(0.025, 0.975))
dimnames(ciPred) = list(c("lo", "hi"), paste(new$home_away, new$opponent_rank), c("Loss", "Draw", "Win"))
round(ciPred, 3)


# predicted result probabilities by opponent rank
grid = expand.grid(home_away = factor(c("Away", "Home"), levels = c("Away", "Home")),
                   opponent_rank = 1:24)
grid$rank10 = grid$opponent_rank / 10
pg = predict(f_a, newdata = grid, type = "probs")
long = do.call(rbind, lapply(colnames(pg), function(k)
  data.frame(grid[, c("home_away", "opponent_rank")], result = k, prob = pg[, k])))
long$result = factor(long$result, levels = c("Loss", "Draw", "Win"))

ggplot(long, aes(opponent_rank, prob, color = result)) + 
  geom_line(linewidth = 1) + 
  facet_wrap(~ home_away) + 
  labs(x = "opponent_rank (1 = highest rank)", y = "Predicted Probability",
       color = "Result",
       title = "Ashford united: predicted result probabilities") + 
  theme_minimal(base_size = 12)
