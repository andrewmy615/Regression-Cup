train <- read.csv("train.csv");
test_predictors <- read.csv("test_predictors.csv");

predictors <- subset(train, select = -c(county_id, state_id));
lm <- lm(ev_rate_per_1k ~ ., data = predictors);
b <- c(lm(ev_rate_per_1k ~ ., data = predictors)$coefficients);
residuals <- c(lm(ev_rate_per_1k ~ ., data = predictors)$residuals);
RSS <- sum(residuals^2);
S2 <- RSS/2177;


test <- cbind(rep(1,941) ,data.matrix(subset(test_predictors, select = -c(county_id,state_id))));
e <- rnorm(941, 0, sqrt(S2));
y_test <- c((test %*% b) + e);