source("https://raw.githubusercontent.com/andrewmy615/Regression-Cup/refs/heads/main/cup_check.R")
source("https://raw.githubusercontent.com/andrewmy615/Regression-Cup/refs/heads/main/holdout_check.R")
train <- read.csv("https://raw.githubusercontent.com/andrewmy615/Regression-Cup/refs/heads/main/train.csv")
test <- read.csv("https://raw.githubusercontent.com/andrewmy615/Regression-Cup/refs/heads/main/test_predictors.csv")


# comparing one predictor OLS models with holdout_check()
predictors <- setdiff(names(train), c("county_id", "ev_rate_per_1k"))
all_predictors <- c("Baseline", predictors)

results_list <- lapply(all_predictors, function(p) {

  if (p == "Baseline") {
    f <- log(ev_rate_per_1k) ~ 1
  } else {
    f <- as.formula(paste("log(ev_rate_per_1k) ~", p))
  }

  output <- holdout_check(f, data = train)

  data.frame(
    predictor      = p,
    point_skill    = output$average[output$measure == "point_skill"],
    interval_skill = output$average[output$measure == "interval_skill"],
    coverage       = output$average[output$measure == "coverage"]
  )
})

comparison_df <- do.call(rbind, results_list) 
comparison_df$avg_skill <- (comparison_df$point_skill + comparison_df$interval_skill) / 2
comparison_df <- comparison_df[order(-comparison_df$avg_skill), ]
rownames(comparison_df) <- NULL
comparison_df |> knitr::kable(digits = 3)

# visualizing the top 5 predictors (based on avg of point_skill, interval_skill) 
top_5 <- c("median_hh_income", "some_college", "adult_smoking", 
           "physical_inactivity", "broadband")

par(mfrow = c(2, 3), mar = c(4, 4, 2, 1))
for (i in top_5) {
  plot(
    train[[i]], log(train$ev_rate_per_1k),
    xlab = i, ylab = "log(ev_rate_per_1k)",
    main = i, pch = 16, col = rgb(0.2, 0.2, 0.5, 0.3),
    cex = 0.7
  )
  fit <- lm(as.formula(paste("log(ev_rate_per_1k) ~", i)), data = train)
  abline(fit, col = "red", lwd = 2)
}
par(mfrow = c(1, 1))

# testing log transformations of top 5
log_results_list <- lapply(top_5, function(p) {

  f <- as.formula(paste("log(ev_rate_per_1k) ~ log(", p, ")"))
  
  output <- holdout_check(f, data = train)
  
  data.frame(
    predictor      = paste0("log(", p, ")"),
    point_skill    = output$average[output$measure == "point_skill"],
    interval_skill = output$average[output$measure == "interval_skill"],
    coverage       = output$average[output$measure == "coverage"]
  )
})

log_comparison_df <- do.call(rbind, log_results_list)
log_comparison_df$avg_skill <- (log_comparison_df$point_skill + log_comparison_df$interval_skill) / 2
log_comparison_df <- log_comparison_df[order(-log_comparison_df$avg_skill), ]
log_comparison_df |> knitr::kable(digits = 3) |> print()

# plotting the transformations
par(mfrow = c(2, 3), mar = c(4, 4, 3, 1))
for (p in top_5) {
  plot(
    log(train[[p]]), log(train$ev_rate_per_1k),
    xlab = paste0("log(", p, ")"), 
    ylab = "log(ev_rate_per_1k)",
    main = paste0("log(", p, ")"),
    pch = 16, 
    col = rgb(0.2, 0.2, 0.5, 0.3),
    cex = 0.7,
    cex.main = 0.9
  )
  fit_log <- lm(as.formula(paste("log(ev_rate_per_1k) ~ log(", p, ")")), data = train)
  abline(fit_log, col = "red", lwd = 2)
}

par(mfrow = c(1, 1))

# median_hh_income best single predictor, M2 submission portion
library(quantreg)

fit_M2  <- lm(log(ev_rate_per_1k) ~ median_hh_income, data = train)
pred_M2 <- predict(fit_M2, newdata = test,
                    interval = "prediction", level = 0.90)

M2_submission <- "single_predictor_M2.csv"
write_submission(test$county_id,
                 pred_M2[, "fit"], pred_M2[, "lwr"], pred_M2[, "upr"],
                 scale = "log", file = M2_submission)

summary(fit_M2, digits=3)
check_submission(M2_submission)

#also submit, since we have two attempts, next best is log(adult_smoking)

fit_M2.2  <- lm(log(ev_rate_per_1k) ~ log(adult_smoking), data = train)
pred_M2.2 <- predict(fit_M2.2, newdata = test,
                    interval = "prediction", level = 0.90)

M2.2_submission <- "single_predictor_M2_2.csv"
write_submission(test$county_id,
                 pred_M2.2[, "fit"], pred_M2.2[, "lwr"], pred_M2.2[, "upr"],
                 scale = "log", file = M2.2_submission)

summary(fit_M2.2, digits=3)
check_submission(M2.2_submission)