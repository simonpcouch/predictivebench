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
introductions_rewritten <- introductions

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
names(metrics) <- gsub("_eval.py", "", basename(files), fixed = TRUE)

metrics <- metrics[names(metrics) %in% basename(introduction_basenames_kept)]
introduction_basenames_kept <- introduction_basenames_kept[
  basename(introduction_basenames_kept) %in% names(metrics)
]


# targets ----------------------------------------------------------------------
# target error metrics are in save_performance/baseline
baseline_files <-
  list.files(
    "DSBench/data_modeling/save_performance/baseline",
    full.names = TRUE
  )

targets <- list()
for (file in baseline_files) {
  targets[[gsub(".txt", "", basename(file))]] <- as.numeric(readLines(
    file.path(file, "result.txt"),
    warn = FALSE
  ))
}

targets <- targets[basename(introduction_basenames_kept)]

introduction_basenames_kept <- introduction_basenames_kept[
  basename(introduction_basenames_kept) %in% names(targets)
]

# putting it all together ------------------------------------------------------
splits_paths <- file.path(
  "DSBench/data_modeling/data/data_resplit",
  introduction_basenames_kept
)

inputs <- list()
for (i in seq_along(introduction_basenames_kept)) {
  inputs[[i]] <- list(
    question = introductions_rewritten,
    dir = file.path(
      "DSBench/data_modeling/data/data_resplit",
      introduction_basenames_kept[i]
    )
  )
}

modeling_dataset <-
  tibble::tibble(
    id = basename(introduction_basenames_kept),
    input = unname(inputs),
    target = unlist(targets[basename(introduction_basenames_kept)]),
    metric_name = unlist(metrics)[basename(introduction_basenames_kept)]
  )

usethis::use_data(modeling_dataset, overwrite = TRUE)
