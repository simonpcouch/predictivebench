# Data analysis benchmark
#
# @description
# Objects prefixed with `analysis_` correspond to the data analysis benchmark in
# predictivebench.
#
# * `analysis_dataset` is the dataset underlying the eval.
# * `analysis_solver` is the solver, which just prompts the provided chat with the
# instruction and working directory provided in `analysis_dataset$input` and
# allows the model to "converse" with a mock analyst—an LLM instance that
# just gently prods the model along without providing any specific guidance.
# * The scorer for the data analysis task is [vitals::model_graded_qa()].
# * `analysis_task` combines the dataset, solver, and scorer into a [vitals::Task].
#
#
# @examples
# analysis_dataset
#
# tsk <- analysis_task()
# tsk
#
# tsk$eval(solver_chat = databot:::chat_bot())
#
# @name analysis
# @aliases predictivebench

# dataset ----------------------------------------------------------------------
# @rdname analysis
"analysis_dataset"

# solver -----------------------------------------------------------------------
# @rdname analysis
# @export
analysis_solver <- function(solver_chat = NULL) {
  chat <- solver_chat
  function(inputs, ..., solver_chat = chat) {
    ch <- solver_chat$clone()
    res <- lapply(inputs, analysis_solve_one, chat = ch)
    list(
      result = purrr::map(res, function(c) {
        if (inherits(c, "Chat")) c$last_turn()@text else c
      }),
      solver_chat = res
    )
  }
}

analysis_solve_one <- function(input, chat) {
  dir <- input$dir
  instruction <- input$question

  dir_prepped <- prepare_analysis_directory(dir)
  withr::defer(unlink(dir_prepped, recursive = TRUE))

  # TODO: this should probably actually live inside of a docker container or
  # something more properly sandboxed
  withr::local_dir(dir_prepped)

  assistant <- chat$clone()
  assistant$set_system_prompt(paste0(
    assistant$get_system_prompt(),
    "At some point in the conversation, once you've answered the question, you should notify the user that you have come to an answer with the keyword 'ANSWER: ' followed by your answer. Be sure to ask the user whether it's fine to submit an answer before you do so. Make sure to include the keyword in all caps, followed by a colon, as written."
  ))

  res <- converse(assistant, question = instruction, keyword = "ANSWER:")

  res
}

# makes a directory that only has the files for analysis inside of it
prepare_analysis_directory <- function(dir) {
  fs::dir_create(".vitals/solver")

  extensions <- c(
    "\\.xlsx$",
    "\\.pdf$",
    "\\.png$",
    "\\.jpg$",
    "\\.jpeg$",
    "\\.csv$"
  )
  pattern <- paste(extensions, collapse = "|")

  files <- fs::dir_ls(dir, regexp = pattern, type = "file")
  files <- files[!grepl("answer", basename(files), ignore.case = TRUE)]

  fs::file_copy(files, ".vitals/solver")

  ".vitals/solver"
}

# task -------------------------------------------------------------------------
# @rdname analysis
# @export
analysis_task <- function(epochs = 1, dir = ".vitals/logs/analysis/") {
  vitals::Task$new(
    dataset = analysis_dataset[1:3, ],
    solver = analysis_solver(),
    scorer = vitals::model_graded_qa(),
    epochs = epochs,
    name = "predictivebench",
    dir = dir
  )
}
