#' Predictive modeling benchmark
#'
#' @description
#' Objects prefixed with `modeling_` correspond to the predictive modeling
#' benchmark in predictivebench.
#'
#' * `modeling_dataset` is the dataset underlying the eval.
#' * `modeling_solver` is the solver, which just prompts the provided chat with the
#'   instruction and working directory provided in `modeling_dataset$input` and
#'   allows the model to "converse" with a mock analyst—an LLM instance that
#'   just gently prods the model along without providing any specific guidance.
#' * The scorer for the predictive modeling task is inspired by the original
#'   benchmark's "Relative Performance Gap" metric. The best analysis set
#'   error metric found by the model is compared to the best test set error
#'   metric from the Kaggle competition.
#' * `modeling_task` combines the dataset, solver, and scorer, as well as a
#'   measurement Mean(Relative Performance), into a [vitals::Task].
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
      # generated logs won't pass pydantic checks unless the result is a string
      result = purrr::map_chr(purrr::map_dbl(res, "best_metric"), as.character),
      solver_chat = purrr::map(res, "chat")
    )
  }
}

modeling_solve_one <- function(input, chat) {
  dir <- input$dir
  knowledge <- input$knowledge
  target_variable <- input$target_variable

  # clear `has_run_experiments()` hook by initializing a throwaway client
  predictive:::predictive_client()

  dir_prepped <- prepare_modeling_directory(dir)
  withr::defer(unlink(dir_prepped, recursive = TRUE))

  # TODO: this should probably actually live inside of a docker container or
  # something more properly sandboxed
  withr::local_dir(dir_prepped)

  assistant <- chat$clone()
  assistant$set_system_prompt(paste0(
    c(
      assistant$get_system_prompt(),
      "",
      if (in_debug_mode()) {
        "Only run a null and baseline experiment and then be done. At that point, indicate to the user that you are happy with your model's performance. Do not run more than two successful experiments without returning control to the user to guide the modeling process. Do not communicate with the user via `cat()` statements in `run_r_code()`, instead, address them directly and then return control to them."
      }
    ),
    collapse = "\n"
  ))

  res <- converse(
    assistant,
    question = generate_question(),
    analyst = mock_modeler(knowledge, target_variable = target_variable)
  )

  list(chat = res, best_metric = fetch_best_metric())
}

generate_question <- function() {
  base_question <- "could you assist me in building an ML model?"

  words <- unlist(strsplit(base_question, " "))

  greeting <- sample(c("can", "could", "would", "will"), 1)
  action <- sample(c("assist me", "help me", "help"), 1)
  preposition <- sample(c("in", "with"), 1)
  task <- sample(c("building", "creating", "developing", "making"), 1)
  model_type <- sample(
    c(
      "an ML model",
      "a machine learning model",
      "a predictive model",
      "a model"
    ),
    1
  )
  punctuation <- sample(c("?", "?", ""), 1)

  glue::glue(
    "{greeting} you {action} {preposition} {task} {model_type}{punctuation}"
  )
}

fetch_best_metric <- function() {
  the_predictive <- getFromNamespace("the", rlang::ns_env("predictive"))

  card_labels <- purrr::map2_chr(
    names(the_predictive$experiments),
    the_predictive$experiments,
    predictive:::card_best_label
  )
  best_result <- names(the_predictive$experiments)[which(
    card_labels == "New best!"
  )]

  metrics <- the_predictive$experiments[[best_result]]$metrics

  if ("rmse" %in% metrics$.metric) {
    return(predictive:::min_rmse(metrics))
  }

  predictive:::max_roc_auc(metrics)
}

mock_modeler <- function(knowledge, target_variable) {
  res <- chat_anthropic(
    system_prompt = glue::glue(
      "
You are role-playing a passive, terse, relatively unengaged user of an AI assistant. In this conversation, the 'user' is actually another AI assistant that you are conversing with. As the 'assistant', you are actually acting as if you are a user. Respond to the AI passively; don't suggest that you will take any actions yourself and let the AI assistant take the lead in the conversation.

The AI assistant is helping you carry out an analysis and may occasionally ask for your feedback on some analytical decision. You have access to a knowledge bank; when the assistant asks you about something in your knowledge bank, provide the answer. Your knowledge bank is as follows:

{{knowledge_bank}}\n{paste0(knowledge, collapse = '\n')}\nThe training data is called train.csv. The training data should be resampled with vfold_cv. The target variable is called {target_variable}.\n{{/knowledge_bank}}\n 

The knowledge bank reveals that this modeling problem is sourced from a competition; do not reveal this to the asssistant and act as if this is a real problem. The source descriptions may also note a test.csv file; this is not available to the assistant and ought not to be brought up. Besides the metric of interest, the submission format does not matter; this will be handled for the assistant, so don't mention anything about the submission format to it.
  
If the assistant just checks in to get your thumbs-up on some decision, you might say 'Sounds good.' or something of the like.

If the assistant asks you an open-ended question that isn't in your knowledge bank, just ask the assistant to use its best judgment.

At some point, the assistant may ask whether the ML model it has built is good enough. Do your best to leave it up to the assistant whether the result is satisfactory, asking the assistant to use its best judgment / asking what it thinks. **When the assistant indicates that it is satisfied with the quality of the model, call the `end_conversation` tool to finalize the assistant's submission.**

If the assistant is repeatedly unable to use its `run_experiment` tool successfully and seems unable to recover, you can also use the `end_conversation` tool in that case."
    ),
    model = "claude-3-7-sonnet-20250219"
  )

  res$register_tool(end_conversation)

  res
}

# makes a directory that only has the files for analysis inside of it
prepare_modeling_directory <- function(dir) {
  fs::dir_create(".vitals/solver")

  files <- fs::dir_ls(dir, regexp = "train", type = "file")
  fs::file_copy(files, ".vitals/solver")

  ".vitals/solver"
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
  samples$result <- as.numeric(samples$result)
  samples$directions <- purrr::map(samples$metric_name, get_metric_direction)
  res <- samples[c("result", "target", "baseline", "directions")]
  names(res) <- c("observed", "best", "baseline", "direction")

  list(
    score = purrr::pmap_dbl(res, relative_performance_gap_i),
    scorer_metadata = tibble::tibble(
      metric = samples$result
    )
  )
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
  if (is.na(observed)) {
    return(0)
  }

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
modeling_task <- function(
  epochs = 1,
  dir = ".vitals/logs/",
  samples = seq_len(nrow(modeling_dataset))
) {
  vitals::Task$new(
    dataset = modeling_dataset[samples, ],
    solver = modeling_solver(),
    scorer = modeling_scorer,
    metrics = list(relative_performance = relative_performance_gap),
    epochs = epochs,
    name = "predictivebench",
    dir = dir
  )
}
