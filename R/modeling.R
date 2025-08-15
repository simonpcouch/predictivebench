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
  question <- input$question
  knowledge <- input$knowledge

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

  res <- converse(
    assistant,
    question = question,
    analyst = mock_modeler(knowledge),
    keyword = "[SUBMITTED]",
    hook = has_not_submitted()
  )

  list(chat = res, submission = read_submission())
}

mock_modeler <- function(knowledge) {
  chat_openai(
    system_prompt = glue::glue(
      "
You are role-playing an analyst using an AI assistant.

The assistant is helping you carry out an analysis and may occasionally ask for your feedback on some analytical decision. You have access to a knowledge bank; when the assistant asks you about something in your knowledge bank, provide the answer. Your knowledge bank is as follows:

{{knowledge_bank}}\n{knowledge}\n{{/knowledge_bank}}\n 

If the assistant just checks in to get your thumbs-up on some decision, you might say 'Sounds good.' or something of the like.

If the assistant asks you an open-ended question that isn't in your knowledge bank, just ask the assistant to use its best judgment.

If the assistant asks you whether some final answer to your question is satisfactory, affirm it and tell it to move forward in submitting the solution.

Be terse."
    ),
    model = "gpt-4.1-mini"
  )
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

# return TRUE while the assistant should keep going
has_not_submitted <- function() {
  counter <- 0
  function(...) {
    counter <<- counter + 1
    !file.exists("submission.csv") && counter < 100
  }
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
modeling_scorer <- function(samples, ...) {
  samples$metric <- purrr::pmap(samples, calculate_metric)
  samples$directions <- purrr::map(samples$metric_name, get_metric_direction)
  res <- samples[c("metric", "target", "baseline", "directions")]
  names(res) <- c("observed", "best", "baseline", "direction")

  list(
    score = purrr::pmap_dbl(res, relative_performance_gap_i),
    scorer_metadata = list(metric = samples$metric)
  )
}

# TODO: this won't work for classification
calculate_metric <- function(...) {
  dots <- list(...)

  submission <- dots$solver_metadata
  # TODO: this should actually live in the eval itself / in inst

  truth_path <- file.path(
    "DSBench/data_modeling/data/answers",
    basename(dots$input$dir),
    "test_answer.csv"
  )
  truth <- read.csv(truth_path)
  in_common <- colnames(truth)[colnames(truth) %in% colnames(submission)]
  outcome_name <- colnames(truth)[!colnames(truth) %in% c(in_common, "id")]
  pred_names <- colnames(submission)[!colnames(submission) %in% in_common]
  result <- dplyr::left_join(truth, submission, by = in_common)
  metric_fn <- getFromNamespace(dots$metric_name, "yardstick")
  metric_fn(result, truth = outcome_name, estimate = pred_names)$.estimate
}

get_metric_direction <- function(metric_name) {
  metric_fn <- getFromNamespace(metric_name, "yardstick")
  attr(metric_fn, "direction")
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
    dataset = modeling_dataset[3, ],
    solver = modeling_solver(),
    scorer = modeling_scorer,
    metrics = list(relative_performance_gap = relative_performance_gap),
    epochs = epochs,
    name = "dseval",
    dir = dir
  )
}
