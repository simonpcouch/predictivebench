# read in files ----------------------------------------------------------------
introduction_files <- list.files(
  "DSBench/data_modeling/data/task",
  full.names = TRUE
)
introduction_basenames <- gsub(".txt", "", basename(introduction_files))

splits_files <- list.files(
  "DSBench/data_modeling/data/data_resplit",
  full.names = TRUE
)
splits_basenames <- basename(splits_files)

# quick data checks ------------------------------------------------------------
introduction_basenames[!introduction_basenames %in% splits_basenames]
#>  [1] "20-newsgroups-ciphertext-challenge"
#>  [2] "amazon-employee-access-challenge"
#>  [3] "bioresponse"
#>  [4] "chaii-hindi-and-tamil-question-answering"
#>  [5] "ciphertext-challenge-iii"
#>  [6] "contradictory-my-dear-watson"
#>  [7] "covid19-global-forecasting-week-5"
#>  [8] "data-science-london-scikit-learn"
#>  [9] "digit-recognizer"
#> [10] "kaggle-llm-science-exam"
#> [11] "llm-prompt-recovery"
#> [12] "otto-group-product-classification-challenge"
#> [13] "playground-series-s3e26"
#> [14] "predict-who-is-more-influential-in-a-social-network"
#> [15] "reducing-commercial-aviation-fatalities"
#> [16] "tabular-playground-series-jun-2021"
#> [17] "tabular-playground-series-may-2021"

splits_basenames[!splits_basenames %in% introduction_basenames]
#> character(0)

introduction_files_kept <- introduction_files[
  introduction_basenames %in% splits_basenames
]
introduction_basenames_kept <- gsub(".txt", "", introduction_files_kept)

introductions <- purrr::map(
  introduction_files_kept,
  readLines,
  warn = FALSE
)

# rewriting introductions ------------------------------------------------------
# currently, these introductions include quite a bit of extraneous information,
# "This is a Kaggle Competition" soft openings, and Acknowledgements. We'd
# like to rephrase them to be concise and direct.
# (Note: no actual rewriting implemented yet, using original introductions)

# metric names ----------------------------------------------------------------
# the names of the metrics corresponding to the targets are in
# DSBench/data_modeling/evaluation
files <- list.files(
  "DSBench/data_modeling/evaluation",
  pattern = "\\.py$",
  full.names = TRUE
)

extract_metric <- function(file_path) {
  lines <- readLines(file_path, warn = FALSE)
  perf_line <- grep("performance\\s*=\\s*\\w+\\(", lines, value = TRUE)
  if (length(perf_line) == 0) {
    return(NA_character_)
  }
  regmatches(
    perf_line,
    regexpr("(?<=performance\\s=\\s)\\w+", perf_line, perl = TRUE)
  )
}

metrics <- sapply(files, extract_metric)
names(metrics) <- gsub("_eval.py", "", basename(names(metrics)), fixed = TRUE)

sort(table(unname(unlist(metrics))))
#>
#>          calculate_rmse        cohen_kappa_score                   mcrmse
#>                       1                        1                        1
#>  mean_column_wise_rmsle           mean_spearmanr    median_absolute_error
#>                       1                        1                        1
#>                mpa_at_3          normalized_gini quadratic_weighted_kappa
#>                       1                        1                        1
#>                r2_score                     rmse                      sum
#>                       1                        2                        2
#>                f1_score                 log_loss                    smape
#>                       3                        3                        3
#>     mean_absolute_error           accuracy_score                    rmsle
#>                       4                        6                        7
#>           roc_auc_score
#>                      18

# TODO: we probably want to implement rmsle for this
metrics_tbl <-
  tibble::tibble(
    metric_name = unname(metrics),
    file = names(metrics)
  ) |>
  tidyr::unnest(metric_name) |>
  # rename to match yardstick metric function
  dplyr::mutate(
    metric_name = dplyr::case_when(
      metric_name == "roc_auc_score" ~ "roc_auc",
      metric_name == "accuracy_score" ~ "accuracy",
      metric_name == "mean_absolute_error" ~ "mae",
      metric_name == "smape" ~ "smape",
      metric_name == "log_loss" ~ "mn_log_loss",
      metric_name == "f1_score" ~ "f_meas",
      metric_name == "rmse" ~ "rmse",
      metric_name == "r2_score" ~ "rsq",
      .default = NA_character_
    )
  ) |>
  dplyr::filter(!is.na(metric_name))

metrics <- setNames(metrics_tbl$metric_name, metrics_tbl$file)

metrics <- metrics[names(metrics) %in% basename(introduction_basenames_kept)]
introduction_basenames_kept <- introduction_basenames_kept[
  basename(introduction_basenames_kept) %in% names(metrics)
]


# targets ----------------------------------------------------------------------
# target error metrics are not included in the source repository, and they
# report being able to reproduce only 22 of them. instead, we can grab the error
# metric of the winning kaggle submission (as of July 22, 2025) on the test data.
# get_kaggle_best_score <- function(competition_url) {
#   cat(competition_url)
#   cat("\n")
#   comp_name <- basename(competition_url)
#   leaderboard_url <- paste0(competition_url, "/leaderboard")
#
#   b <- chromote::ChromoteSession$new()
#
#   b$go_to(leaderboard_url)
#
#   Sys.sleep(2)
#
#   js_code <- '
#   var allElements = document.querySelectorAll("*");
#   var scores = [];
#   for (var i = 0; i < allElements.length; i++) {
#     var text = allElements[i].innerText;
#     if (text && /^[0-9]+\\.?[0-9]*$/.test(text.trim())) {
#       scores.push(text.trim());
#     }
#   }
#   scores.length >= 2 ? scores[1] : null;
#   '
#
#   score_result <- b$Runtime$evaluate(js_code, returnByValue = TRUE)
#   b$close()
#
#   as.numeric(score_result$result$value)
# }
#
# get_all_kaggle_best_scores <- function(
#   data_json_path = "DSBench/data_modeling/data.json"
# ) {
#   competition_lines <- readLines(data_json_path)
#   competitions <- lapply(competition_lines, jsonlite::fromJSON)
#   competitions <- do.call(rbind, lapply(competitions, as.data.frame))
#
#   competitions$best_score <- sapply(competitions$url, get_kaggle_best_score)
#
#   competitions
# }

#best_scores <- get_all_kaggle_best_scores()
load(system.file("data-raw/best_scores.rda", package = "predictivebench"))

targets <- best_scores
targets <- setNames(targets$best_score, targets$name)

# targets ----------------------------------------------------------------------
# target error metrics are in save_performance/baseline
baseline_files <-
  list.files(
    "DSBench/data_modeling/save_performance/baseline",
    full.names = TRUE
  )

baselines <- list()
for (file in baseline_files) {
  baselines[[gsub(".txt", "", basename(file))]] <- as.numeric(readLines(
    file.path(file, "result.txt"),
    warn = FALSE
  ))
}

baselines <- baselines[basename(introduction_basenames_kept)]

introduction_basenames_kept <- introduction_basenames_kept[
  basename(introduction_basenames_kept) %in% names(baselines)
]

# reorder introductions to match final introduction_basenames_kept
original_order <- gsub(".txt", "", introduction_files_kept)
reorder_indices <- match(
  basename(introduction_basenames_kept),
  basename(original_order)
)
introductions <- introductions[reorder_indices]


# putting it all together ------------------------------------------------------
splits_paths <- file.path(
  "DSBench/data_modeling/data/data_resplit",
  introduction_basenames_kept
)

inputs <- list()
for (i in seq_along(introduction_basenames_kept)) {
  inputs[[i]] <- tibble::tibble(
    knowledge = paste0(introductions[[i]], collapse = "\n"),
    dir = file.path(
      "DSBench/data_modeling/data/data_resplit",
      basename(introduction_basenames_kept[i])
    )
  )
}

modeling_d <-
  tibble::tibble(
    id = basename(introduction_basenames_kept),
    input = unname(inputs),
    target = unlist(targets[basename(introduction_basenames_kept)]),
    baseline = unlist(baselines[basename(introduction_basenames_kept)]),
    metric_name = unlist(metrics[basename(introduction_basenames_kept)])
  )


modeling_dataset <- dplyr::relocate(modeling_d, input, .after = id)

# calculate deterministic baselines using tidymodels null_model() (#8) ---------
library(tidymodels)
library(yardstick)

calculate_null_baseline <- function(id, metric_name) {
  train_path <- file.path(
    "DSBench/data_modeling/data/data_resplit",
    id,
    "train.csv"
  )
  test_path <- file.path(
    "DSBench/data_modeling/data/answers",
    id,
    "test_answer.csv"
  )

  if (!file.exists(train_path) || !file.exists(test_path)) {
    cli::cli_abort("Could not find file.")
  }

  train_data <- readr::read_csv(train_path, show_col_types = FALSE)
  test_data <- readr::read_csv(test_path, show_col_types = FALSE)

  target_name <- "target"
  if (!"target" %in% names(train_data)) {
    target_name <- names(test_data)[names(test_data) != "id"]
    if (length(target_name) != 1) {
      test_path_wo_outcome <- file.path(
        "DSBench/data_modeling/data/data_resplit",
        id,
        "test.csv"
      )
      test_data_wo_outcome <- readr::read_csv(
        test_path_wo_outcome,
        show_col_types = FALSE
      )
      target_name <- names(test_data)[
        !names(test_data) %in% names(test_data_wo_outcome)
      ]
      if (length(target_name) != 1) {
        return(list(target_name = NA, baseline = NA))
      }
    }
    train_data$target <- train_data[[target_name]]
    test_data$target <- test_data[[target_name]]
  }

  metric_fn <- get(metric_name, envir = asNamespace("yardstick"))
  mode <- if (inherits(metric_fn, "numeric_metric")) {
    "regression"
  } else {
    "classification"
  }

  null_spec <- null_model() %>%
    set_engine("parsnip") %>%
    set_mode(mode)

  if (identical(mode, "classification")) {
    train_data$target <- as.factor(train_data$target)
    test_data$target <- as.factor(test_data$target)
  }

  split <- rsample::make_splits(
    x = train_data["target"],
    assessment = test_data["target"]
  )
  null_fit <- last_fit(
    object = null_spec,
    preprocessor = target ~ NULL,
    split = split,
    metrics = metric_set(metric_fn)
  )

  list(
    target_name = target_name,
    baseline = null_fit[[".metrics"]][[1]]$.estimate
  )
}

new_baselines <- vector("list", nrow(modeling_dataset))

cli::cli_progress_bar(
  "Calculating null model baselines",
  total = nrow(modeling_dataset),
  type = "tasks"
)

for (i in seq_len(nrow(modeling_dataset))) {
  cli::cli_progress_update(
    status = paste("Processing", modeling_dataset$id[i])
  )
  new_baselines[[i]] <- calculate_null_baseline(
    modeling_dataset$id[i],
    modeling_dataset$metric_name[i]
  )
}

new_baselines_tbl <- dplyr::bind_rows(new_baselines)

modeling_dataset <-
  modeling_dataset %>%
  dplyr::select(-baseline) %>%
  dplyr::bind_cols(new_baselines_tbl) %>%
  dplyr::relocate(baseline, .after = target) %>%
  dplyr::filter(!is.na(target_name)) %>%
  # if the target truly wasn't found, the target_name would be NA.
  # otherwise, if there's an NA baseline, the metric is rsq and the
  # worst possible value is 0.
  dplyr::mutate(baseline = if_else(is.na(baseline), 0, baseline)) %>%
  tidyr::unnest(input) %>%
  dplyr::mutate(target_variable = target_name) %>%
  tidyr::nest(input = c(target_variable, knowledge, dir)) %>%
  dplyr::relocate(input, .after = id)

usethis::use_data(modeling_dataset, overwrite = TRUE)
