training_data <- read.csv('https://raw.githubusercontent.com/andrewmy615/Regression-Cup/refs/heads/main/train.csv')
head(training_data)

training_data   <- read.csv("/Users/alex/Regression-Cup/train.csv")
test_predictors <- read.csv("/Users/alex/Regression-Cup/test_predictors.csv")

stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
f_ols <- paste0("submission_ols_log_", stamp, ".csv")
f_lad <- paste0("submission_lad_rate_", stamp, ".csv")

# Method 1: OLS on log scale (squared error loss), submitted on log scale
fit_ols  <- lm(log(ev_rate_per_1k) ~ 1, data = training_data)
pred_ols <- predict(fit_ols, newdata = test_predictors,
                    interval = "prediction", level = 0.90)

write_submission(test_predictors$county_id,
                 pred_ols[, "fit"], pred_ols[, "lwr"], pred_ols[, "upr"],
                 scale = "log", file = f_ols)

# Method 2: LAD via quantile regression (absolute error loss), rate scale
qr_fit  <- rq(ev_rate_per_1k ~ 1, data = training_data, tau = c(0.05, 0.5, 0.95))
pred_qr <- predict(qr_fit, newdata = test_predictors)

write_submission(test_predictors$county_id,
                 pred_qr[, 2], pred_qr[, 1], pred_qr[, 3],
                 scale = "rate", file = f_lad)

check_submission(f_ols)
check_submission(f_lad)