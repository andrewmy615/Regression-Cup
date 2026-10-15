train <- read.csv("train.csv");
test_predictors <- read.csv("test_predictors.csv");

predictors <- subset(train, select = -c(county_id, state_id));
lm <- lm(ev_rate_per_1k ~ ., data = predictors);
constant <- lm(ev_rate_per_1k ~ 1, data = predictors);
b <- c(lm(ev_rate_per_1k ~ ., data = predictors)$coefficients);
b0 <- c(lm(ev_rate_per_1k ~ 1, data = predictors)$coefficients);
residuals <- c(lm(ev_rate_per_1k ~ ., data = predictors)$residuals);
residuals0 <- c(lm(ev_rate_per_1k ~ 1, data = predictors)$residuals);
RSS <- sum(residuals^2);
RSS0 <- sum(residuals0^2);
S2 <- RSS/2177;
S2 <- RSS0/2177;


test <- cbind(rep(1,941) ,data.matrix(subset(test_predictors, select = -c(county_id,state_id))));
e <- rnorm(941, 0, sqrt(S2));
y_test <- c((test %*% b) + e);