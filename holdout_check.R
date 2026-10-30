holdout_check <- function(
  formula,
  data,
  level = 0.90,
  reps = 20,
  prop = 0.8,
  seed = 1
) {
  set.seed(seed)
  alpha <- 1 - level
  int_score <- function(y, lo, hi) {
    (hi - lo) +
      2 / alpha * (lo - y) * (y < lo) +
      2 / alpha * (y - hi) * (y > hi)
  }
  one_split <- function(r) {
    idx <- sample(nrow(data), floor(prop * nrow(data)))
    fit_part <- data[idx, ]
    hold <- data[-idx, ]
    # drop held-out rows whose categories never appear in the fitting part
    for (v in all.vars(formula)[-1]) {
      if (is.character(data[[v]]) || is.factor(data[[v]])) {
        hold <- hold[hold[[v]] %in% unique(fit_part[[v]]), ]
      }
    }
    fit <- lm(formula, data = fit_part)
    y <- model.response(model.frame(formula, hold))
    pr <- predict(fit, newdata = hold, interval = "prediction", level = level)
    # benchmark: constant mean of the fitting part, normal-theory interval
    y_fit <- model.response(model.frame(formula, fit_part))
    m0 <- mean(y_fit)
    s0 <- sd(y_fit)
    q <- qnorm(1 - alpha / 2)
    c(
      point_skill = 1 - sum((y - pr[, "fit"])^2) / sum((y - m0)^2),
      interval_skill = 1 -
        sum(int_score(y, pr[, "lwr"], pr[, "upr"])) /
          sum(int_score(y, m0 - q * s0, m0 + q * s0)),
      coverage = mean(pr[, "lwr"] <= y & y <= pr[, "upr"]),
      miss_low = mean(y < pr[, "lwr"]),
      miss_high = mean(y > pr[, "upr"])
    )
  }
  out <- sapply(seq_len(reps), one_split)
  data.frame(
    measure = rownames(out),
    average = rowMeans(out),
    sd_across_splits = apply(out, 1, sd),
    row.names = NULL
  )
}

# The benchmark: the constant-mean model
holdout_check(log(ev_rate_per_1k) ~ 1, train) |> knitr::kable(digits = 3)