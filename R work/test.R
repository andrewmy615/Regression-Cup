source("https://raw.githubusercontent.com/andrewmy615/Regression-Cup/refs/heads/main/cup_check.R") # on Canvas, next to the data
train <- read.csv("https://raw.githubusercontent.com/andrewmy615/Regression-Cup/refs/heads/main/train.csv")
test <- read.csv("https://raw.githubusercontent.com/andrewmy615/Regression-Cup/refs/heads/main/test_predictors.csv")


log_y <- log(train$ev_rate_per_1k)
#print(head(log_y))

mu_hat <- mean(log_y)
sigma_hat <- sd(log_y)
print(mu_hat)
print(sigma_hat)

#two 90% confidence intervals
#qnorm(p) looks at the standard normal distribution and finds z where the area to the left is p
#qt(p, df): looks at the t-distribution with provided df and finds t where the area to the left is p.

#1, z_score
z_score <- qnorm(0.95)
print(z_score)
z_lower <- mu_hat - z_score * sigma_hat
z_upper <- mu_hat + z_score * sigma_hat

#2, t_distribution
n <- nrow(train)
df <- n - 1
t_score <- qt(0.95, df)
print(t_score)
t_lower <- mu_hat - t_score * sigma_hat
t_upper <- mu_hat + t_score * sigma_hat

#now on test data with train values
n_test <- nrow(test)

#rep() just replicates the mu_hat value for every test country
z_pred_vec <- rep(mu_hat, n_test)
z_lower_vec <- rep(z_lower, n_test)
z_upper_vec <- rep(z_upper, n_test)

t_pred_vec <- rep(mu_hat, n_test)
t_lower_vec <- rep(t_lower, n_test)
t_upper_vec <- rep(t_upper, n_test)

#from handout, creates submission for z and t results respectively
write_submission(
  test$county_id,
  z_pred_vec,
  z_lower_vec,
  z_upper_vec,
  scale = "log",
  file = "team_submission_z.csv"
)

check_submission("team_submission_z.csv") # the same checks the app runs

write_submission(
  test$county_id,
  t_pred_vec,
  t_lower_vec,
  t_upper_vec,
  scale = "log",
  file = "team_submission_t.csv"
)

check_submission("team_submission_t.csv")