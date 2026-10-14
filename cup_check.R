# =============================================================================
# cup_check.R: the Regression Cup submission checker
#
# These are exactly the checks the Cup app runs before it scores a file.
# Students: put this file next to test_predictors.csv and run
#
#   source("cup_check.R")
#   check_submission("team_submission.csv")
#
# To write a file from your predictions (either scale), use write_submission():
#
#   write_submission(test$county_id, pred, lower, upper, scale = "log",
#                    file = "team_submission.csv")
#
# A file that passes here will pass the app's format checks.
# =============================================================================

# Two accepted formats; the column names declare the scale.
cup_formats <- list(
  rate = c("county_id", "pred", "lower", "upper"),             # EVs per 1,000 vehicles
  log  = c("county_id", "log_pred", "log_lower", "log_upper")  # natural log of the rate
)
cup_required_cols <- cup_formats$rate   # kept for backward compatibility

# Read a submission CSV. Returns a data frame, or a character error message.
read_submission <- function(file) {
  x <- tryCatch(
    suppressWarnings(utils::read.csv(file, stringsAsFactors = FALSE, check.names = FALSE,
                                     strip.white = TRUE, fileEncoding = "UTF-8-BOM",
                                     na.strings = c("", "NA", "NaN"))),
    error = function(e) conditionMessage(e))
  if (is.character(x)) return(paste("The file could not be read as a CSV:", x))
  if (!is.data.frame(x) || nrow(x) == 0) return("The file is empty.")
  names(x) <- tolower(trimws(gsub('"', "", names(x))))
  x
}

# Show at most k examples, e.g. "C0012, C0040, C0101, and 4 more"
cup_examples <- function(v, k = 5) {
  v <- unique(as.character(v))
  if (length(v) <= k) return(paste(v, collapse = ", "))
  paste0(paste(v[1:k], collapse = ", "), ", and ", length(v) - k, " more")
}

# Check a submission against the list of test county IDs.
# Returns list(ok, errors, warnings, scale, data). When ok is TRUE, data has
# columns county_id, log_pred, log_lower, log_upper (LOG scale, which is what
# is scored), in the order of test_ids; scale says which format the file used.
validate_submission <- function(sub, test_ids) {
  errors <- character(0); warnings <- character(0); scale <- NA_character_
  done <- function() list(ok = length(errors) == 0, errors = errors, warnings = warnings,
                          scale = scale, data = if (length(errors) == 0) sub else NULL)
  if (is.character(sub)) { errors <- sub; return(done()) }

  # Columns: which format is this?
  has <- vapply(cup_formats, function(f) all(f %in% names(sub)), logical(1))
  if (all(has)) {
    errors <- c(errors, paste(
      "The file has both pred/lower/upper and log_pred/log_lower/log_upper columns.",
      "Keep one set: pred, lower, upper for EVs per 1,000 vehicles, or log_pred, log_lower, log_upper for the log scale."))
    return(done())
  }
  if (!any(has)) {
    near <- if (any(grepl("^log", names(sub)))) cup_formats$log else cup_formats$rate
    errors <- c(errors, sprintf(paste(
      "Missing column(s): %s. Use exactly one of these headers:",
      "county_id, pred, lower, upper (EVs per 1,000 vehicles) or",
      "county_id, log_pred, log_lower, log_upper (log scale). (Your file has: %s.)"),
      paste(setdiff(near, names(sub)), collapse = ", "), paste(names(sub), collapse = ", ")))
    return(done())
  }
  scale <- names(cup_formats)[has]
  cols <- cup_formats[[scale]]
  vcols <- cols[-1]
  extra <- setdiff(names(sub), cols)
  if (length(extra) > 0)
    warnings <- c(warnings, sprintf("Extra column(s) ignored: %s.", paste(extra, collapse = ", ")))
  sub <- sub[cols]
  sub$county_id <- trimws(as.character(sub$county_id))
  line <- seq_len(nrow(sub)) + 1          # line number in the file (line 1 is the header)

  # Rows and IDs
  if (nrow(sub) != length(test_ids))
    errors <- c(errors, sprintf("The file has %d data rows, but there are %d test counties (one row each).",
                                nrow(sub), length(test_ids)))
  dup <- sub$county_id[duplicated(sub$county_id)]
  if (length(dup) > 0)
    errors <- c(errors, sprintf("%d county_id value(s) appear more than once: %s.",
                                length(unique(dup)), cup_examples(dup)))
  bad_id <- setdiff(sub$county_id, test_ids)
  if (length(bad_id) > 0)
    errors <- c(errors, sprintf(
      "%d county_id value(s) are not test counties: %s. (Did you predict the training file, or change the IDs?)",
      length(bad_id), cup_examples(bad_id)))
  miss_id <- setdiff(test_ids, sub$county_id)
  if (length(miss_id) > 0)
    errors <- c(errors, sprintf("%d test count%s missing: %s.", length(miss_id),
                                if (length(miss_id) == 1) "y is" else "ies are", cup_examples(miss_id)))

  # Values: numeric and finite
  for (v in vcols) {
    num <- suppressWarnings(as.numeric(sub[[v]]))
    bad <- !is.finite(num)
    if (any(bad))
      errors <- c(errors, sprintf("Column %s has %d missing or non-numeric value(s) (file line %s).",
                                  v, sum(bad), cup_examples(line[bad])))
    sub[[v]] <- num
  }
  if (length(errors) > 0) return(done())

  # Rate scale: values must be positive (the scorer takes logs)
  if (scale == "rate") {
    nonpos <- sub$pred <= 0 | sub$lower <= 0 | sub$upper <= 0
    if (any(nonpos)) {
      looks_log <- stats::median(sub$pred) < 3
      errors <- c(errors, paste0(sprintf(
        "%d row(s) have a value that is zero or negative (file line %s). ", sum(nonpos), cup_examples(line[nonpos])),
        if (looks_log)
          "These look like log-scale values. If they are, rename the columns to log_pred, log_lower, log_upper (or use write_submission(..., scale = \"log\"))."
        else
          "A rate cannot be zero or negative, and the scorer takes logs. If an interval built on the original scale runs below zero, that is worth thinking about: an interval built on the log scale never does."))
      return(done())
    }
  }

  # Order within each row
  bad <- sub[[vcols[2]]] > sub[[vcols[1]]] | sub[[vcols[1]]] > sub[[vcols[3]]]
  if (any(bad)) {
    errors <- c(errors, sprintf(
      "In %d row(s), the prediction is not inside its interval (%s <= %s <= %s fails; file line %s).",
      sum(bad), vcols[2], vcols[1], vcols[3], cup_examples(line[bad])))
    return(done())
  }

  # Convert to the scored (log) scale and sanity-check the scale
  if (scale == "rate") {
    out <- data.frame(county_id = sub$county_id, log_pred = log(sub$pred),
                      log_lower = log(sub$lower), log_upper = log(sub$upper))
    if (stats::median(sub$pred) < 3 && max(sub$upper) < 8)
      errors <- c(errors, paste(
        "Your values look like log-scale values: the median prediction is below 3 EVs per 1,000 and no upper end",
        "reaches 8, although the typical training county has about 6.5. If they are logs, rename the columns to",
        "log_pred, log_lower, log_upper (or use write_submission(..., scale = \"log\")).",
        "If they really are rates, contact your instructor."))
  } else {
    out <- sub
    if (stats::median(sub$log_pred) > 4.5)
      errors <- c(errors, paste(
        "Your log_pred values look like rates, not logs: a median above 4.5 on the log scale means more than",
        "90 EVs per 1,000, although the typical training county has about 6.5. If they are rates, rename the",
        "columns to pred, lower, upper. If they really are logs, contact your instructor."))
  }
  if (length(errors) > 0) return(done())
  sub <- out[match(test_ids, out$county_id), ]
  rownames(sub) <- NULL
  done()
}

# Write a submission file from your predictions, on either scale.
#   ids:   the county_id column of test_predictors.csv
#   pred, lower, upper: your point predictions and 90% interval endpoints,
#          in the same order as ids
#   scale: "log" if your model predicts log(rate), "rate" if it predicts the
#          rate itself (EVs per 1,000 vehicles)
# The file is checked before it is written.
write_submission <- function(ids, pred, lower, upper, scale = c("log", "rate"),
                             file = "team_submission.csv", test_file = "test_predictors.csv") {
  scale <- match.arg(scale)
  df <- data.frame(ids, pred, lower, upper)
  names(df) <- cup_formats[[scale]]
  test_ids <- if (file.exists(test_file)) utils::read.csv(test_file, stringsAsFactors = FALSE)$county_id else ids
  res <- validate_submission(df, test_ids)
  if (!res$ok) {
    cat("NOT WRITTEN: fix these problems first:\n")
    cat(paste0("  - ", res$errors, collapse = "\n"), "\n")
    return(invisible(FALSE))
  }
  utils::write.csv(df, file, row.names = FALSE)
  cat(sprintf("Wrote %s (%d counties, %s scale).\n", file, nrow(df), scale))
  if (length(res$warnings) > 0) cat("Notes:\n", paste0("  - ", res$warnings, collapse = "\n"), "\n")
  invisible(TRUE)
}

# Student-facing wrapper: prints a friendly report and returns TRUE/FALSE invisibly.
check_submission <- function(file, test_file = "test_predictors.csv") {
  if (!file.exists(test_file)) stop("Cannot find ", test_file, ". Put it in your working directory or give its path.")
  test_ids <- utils::read.csv(test_file, stringsAsFactors = FALSE)$county_id
  res <- validate_submission(read_submission(file), test_ids)
  if (res$ok) {
    cat(sprintf("PASSED: the file is ready to submit (read as %s).\n",
                if (res$scale == "log") "log scale" else "EVs per 1,000 vehicles"))
  } else {
    cat("NOT READY: fix these problems first:\n")
    cat(paste0("  - ", res$errors, collapse = "\n"), "\n")
  }
  if (length(res$warnings) > 0) cat("Notes:\n", paste0("  - ", res$warnings, collapse = "\n"), "\n")
  invisible(res$ok)
}
