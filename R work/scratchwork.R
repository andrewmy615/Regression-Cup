library("L1pack", "quantreg");

train <- read.csv("train.csv");
test_predictors <- read.csv("test_predictors.csv");

predictors <- subset(train, select = -c(county_id, state_id));
lm <- lm(log(ev_rate_per_1k) ~ ., data = predictors);
b <- c(lm$coefficients);
residuals <- c(lm$residuals);
RSS <- sum(residuals^2);
S2 <- RSS/2177;


constant <- lm(log(ev_rate_per_1k) ~ 1, data = predictors);
b0 <- c(constant$coefficients);
residuals0 <- c(constant$residuals);
RSS0 <- sum(residuals0^2);
S2_0 <- RSS0/2177;



lad <- lad(log(ev_rate_per_1k) ~ ., data = predictors);
b_lad <- c(lad$coefficients);
residuals_lad <- c(lad$residuals);
RSS_lad <- sum(residuals_lad^2);
S2_lad <- RSS_lad/2177;


ladconstant <- lad(log(ev_rate_per_1k) ~ 1, data = predictors);
b_lad0 <- c(ladconstant$coefficients);
residuals_lad0 <- c(ladconstant$residuals);
RSS_lad0 <- sum(residuals_lad0^2);
S2_lad0 <- RSS_lad0/2177;



test <- cbind(rep(1,941) ,data.matrix(subset(test_predictors, select = -c(county_id,state_id))));
e <- rnorm(941, 0, sqrt(S2));
y_test <- c((test %*% b) + e);