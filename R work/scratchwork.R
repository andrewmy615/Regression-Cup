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
S2 <- RSS0/2177;


test <- cbind(rep(1,941) ,data.matrix(subset(test_predictors, select = -c(county_id,state_id))));
e <- rnorm(941, 0, sqrt(S2));
y_test <- c((test %*% b) + e);