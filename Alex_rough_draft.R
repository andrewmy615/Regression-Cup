training_data <- read.csv('https://raw.githubusercontent.com/andrewmy615/Regression-Cup/refs/heads/main/train.csv')
head(training_data)

# Method 1: OLS on log scale (squared error loss)
fit_ols <- lm(log(ev_rate_per_1k) ~ 1, data = training_data)
ols <- exp(predict(fit_ols, newdata = data.frame(1),
                   interval = "prediction", level = 0.90))

# Method 2: LAD (absolute error loss)
lad_pt <- median(training_data$ev_rate_per_1k)
lad_pi <- quantile(training_data$ev_rate_per_1k, c(0.05, 0.95))

rbind(OLS_log = ols[1, ],
      LAD     = c(fit = lad_pt, lad_pi))