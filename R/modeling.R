#' Predictive modeling benchmark
#'
#' @description
#' Objects prefixed with `modeling_` correspond to the predictive modeling
#' benchmark in dseval.
#'
#' * `modeling_dataset` is the dataset underlying the eval.
#' * `modeling_solver` is the solver, which just prompts the provided chat with the
#' instruction and working directory provided in `modeling_dataset$input` and
#' allows the model to "converse" with a mock analyst—an LLM instance that
#' just gently prods the model along without providing any specific guidance.
#' * The scorer for the predictive modeling task is inspired by the original
#'   benchmark's "Relative Performance Gap" metric. The best analysis set
#'   error metric found by the model is compared to the best test set error
#'   metric from the Kaggle competition.
#' * `modeling_task` combines the dataset, solver, and scorer, as well as a
#'   measurement Mean(Relative Performance Gap), into a [vitals::Task].
#'
#' @examples
#' modeling_dataset
#'
#' tsk <- modeling_task()
#' tsk
#'
#' tsk$eval(solver_chat = predictive:::predictive_client())
#'
#' @name modeling

# dataset ----------------------------------------------------------------------
#' @rdname modeling
"modeling_dataset"

# solver -----------------------------------------------------------------------
#' @rdname modeling
#' @export
modeling_solver <- function(solver_chat = NULL) {
  chat <- solver_chat
  function(inputs, ..., solver_chat = chat) {
    ch <- solver_chat$clone()
    res <- lapply(inputs, modeling_solve_one, chat = ch)
    list(
      # TODO: this should be the resulting metric
      result = purrr::map(
        purrr::map(res, "submission"),
        function(submission) {
          if (is.character(submission) && grepl("failed", submission)) {
            "Failed to submit."
          } else {
            "Submission successful."
          }
        }
      ),
      solver_chat = purrr::map(res, "chat"),
      solver_metadata = purrr::map(res, "submission")
    )
  }
}

modeling_solve_one <- function(input, chat) {
  dir <- input$dir
  instruction <- input$question

  dir_prepped <- prepare_modeling_directory(dir)
  withr::defer(unlink(dir_prepped, recursive = TRUE))

  # TODO: this should probably actually live inside of a docker container or
  # something more properly sandboxed
  withr::local_dir(dir_prepped)

  assistant <- chat$clone()
  assistant$set_system_prompt(paste0(
    c(
      assistant$get_system_prompt(),
      "At some point in the conversation, once you're satisfied with the model you've built, save the predictions on the test set as 'submission.csv' in the current working directory. At that point, you should notify the user that you have written a submission with the keyword '[SUBMITTED]'. Make sure to include the keyword in all caps, surrounded in square brackets, as written.",
      "",
      "The training data is called train.csv and the testing data test.csv. Resample the training data with vfold_cv.",
      "",
      readLines(system.file("submission_format.md", package = "dseval"))
    ),
    collapse = "\n"
  ))

  res <- converse(assistant, instruction = instruction)

  list(chat = res, submission = read_submission())
}

# makes a directory that only has the files for analysis inside of it
prepare_modeling_directory <- function(dir) {
  fs::dir_create(".vitals/solver")

  files <- fs::dir_ls(dir, regexp = "train|test", type = "file")
  fs::file_copy(files, ".vitals/solver")

  ".vitals/solver"
}

read_submission <- function() {
  tryCatch(
    read.csv("submission.csv"),
    # TODO: should this eval be supplying some sort of writeLines tool
    # or is it safe to assume that some facility to write lines is present?
    error = function(e) {
      "The model failed to present a submission."
    }
  )
}

# scorer -----------------------------------------------------------------------
#' @section Scoring:
#' LLM agents are scored on their ability to build predictive models and store
#' the modeling results in persistent files.
#'
#' The modeling results are benchmarked using the "Relative Performance Gap"
#' introduced in the original paper. The best error metric achieved by the
#' model is juxtaposed with 1) a baseline metric, generated using no predictors
#' and 2), the best metric achieved on Kaggle.
#'
#' For a given modeling problem, 0 means observed performance equals baseline
#' (no improvement), and 1 means the observed performance is equal to the
#' best performance on Kaggle (optimal). Values greater than 1 mean that the
#' solver did better than the best Kaggle submission, and are more likely to
#' indicate an issue with the eval than actual improvement on the best Kaggle
#' submission. NA means the model failed to successfully present a
#' submission.
#'
#' When summing across all modeling problems, NAs are treated as zeroes, and the
#' mean is taken and then multiplied by 100. So, a score of 100 means
#' "on average across modeling competitions, as good as the winning
#' Kaggle submissions."
#'
#' @rdname modeling
#' @export
modeling_scorer <- function(samples) {
  samples$metric <- purrr::pmap(samples, calculate_metric)
  samples$directions <- purrr::map(samples$metric_name, get_metric_direction)
  res <- samples[c("result", "target", "baseline", "directions")]
  names(res) <- c("observed", "best", "baseline", "direction")

  list(
    score = purrr::pmap(res, relative_performance_gap),
    scorer_metadata = list(metric = samples$metric)
  )
}

calculate_metric <- function(...) {
  dots <- list(...)
  submission <- dots$solver_metadata$submission
  metric_fn <- getFromNamespace(sample$metric_name, "yardstick")
  metric_fn(submission)
}

get_metric_direction <- function(metric_name) {
  metric_fn <- getFromNamespace(metric_name, "yardstick")
  attr(metric_n, "direction")
}

# observed, best, and baseline are error metric values.
# observed as what the model achieved,
# best is the winning result on kaggle, and
# baseline is the metric resulting from a null model.
relative_performance_gap_i <- function(observed, best, baseline, direction) {
  if (identical(direction, "minimize")) {
    observed <- -observed
    best <- -best
    baseline <- -baseline
  }

  max((observed - baseline) / (best - baseline), 0)
}

relative_performance_gap <- function(scores) {
  scores[is.na(scores)] <- 0
  mean(scores) * 100
}

# task -------------------------------------------------------------------------
#' @rdname modeling
#' @export
modeling_task <- function(epochs = 1, dir = ".vitals/logs/modeling/") {
  vitals::Task$new(
    dataset = modeling_dataset[1, ],
    solver = modeling_solver(),
    scorer = modeling_scorer,
    metrics = list(relative_performance_gap = relative_performance_gap),
    epochs = epochs,
    name = "dseval",
    dir = dir
  )
}
