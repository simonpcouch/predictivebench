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
load(system.file("data-raw/best_scores.rda", package = "dseval"))

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
    question = list(introductions[[i]]),
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

# store the results of the deterministic processing before rephrasing via LLMs
save(modeling_d, file = "inst/data-raw/modeling_d.Rda")

modeling_dataset <- tidyr::unnest(modeling_d, input)
modeling_dataset$question <- vapply(
  modeling_dataset$question,
  paste0,
  character(1),
  collapse = "\n"
)

split_prompts <- glue::glue_data(
  modeling_dataset,
  "
The following is an introduction to a data modeling competition. Taken together with some data files, the introduction provides the full context needed to build a machine learning model based on the data, optimized against some metric.

{{introduction}}\n{question}\n{{/introduction}}\n 

I'd like to split this introduction into two parts:

1. question: A brief initial prompt (3-4 sentences) that an analyst might write in first person when asking an AI assistant for help with a data modeling problem. This should note that the user wants to model some data called `train` in the current wd, give a high-level description of what the data is about, and note the error metric of interest if it's included in the introduction. (Note metrics by name--leave implementation details for `knowledge`.) Notably, there should be no mention that the data is from a competition. The question should sound like a typical analyst might have written it when initially approaching an AI assistant for help and be written in first-person; write colloquially.

2. knowledge: The remaining detailed information, context, and specifics from the introduction. Include all of the factual information from the introduction. Exclude any information that wouldn't be helpful for answering the question; information describing the history and context on a data modeling competition generally rather than the task at hand should be excluded. From this additional context, a reader should be able to understand the modeling problem as it relates to some scientific or business objective but not be aware the modeling problem arose from a competition.
"
)

ch_split <- chat_anthropic(model = "claude-sonnet-4-20250514")
split_res <- parallel_chat_structured(
  ch_split,
  as.list(split_prompts),
  type_object(
    question = type_string(
      description = "A brief initial prompt (3-4 sentences) in first person that an analyst might write when asking an AI assistant for help with a data modeling problem."
    ),
    knowledge = type_string(
      description = "The remaining detailed information, context, and specifics from the introduction, excluding any advertising or context about the data modeling competition that isn't related to this task specifically."
    )
  )
)

save(split_res, file = "inst/data-raw/split_res_modeling.rda")

modeling_dataset <- dplyr::bind_cols(
  modeling_dataset[!names(modeling_dataset) %in% names(split_res)],
  split_res
)

modeling_dataset <- modeling_dataset %>%
  tidyr::nest(input = c(question, knowledge, dir))

modeling_dataset <- dplyr::relocate(modeling_dataset, input, .after = id)

usethis::use_data(modeling_dataset, overwrite = TRUE)
