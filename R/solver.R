#' DSEval solver
#' @export
dseval_solver <- function(solver_chat = NULL) {
  chat <- solver_chat
  function(inputs, ..., solver_chat = chat) {
    ch <- solver_chat$clone()
    res <- lapply(inputs, dseval_solve_one, chat = ch)
    list(
      result = purrr::map(res, function(c) {
        if (inherits(c, "Chat")) c$last_turn()@text else c
      }),
      solver_chat = res
    )
  }
}

dseval_solve_one <- function(input, chat) {
  dir <- input$dir
  instruction <- input$instruction

  dir_prepped <- prepare_directory(dir)
  withr::defer(unlink(dir_prepped, recursive = TRUE))

  instruction_prepped <- paste0(
    # prompt from the original eval
    "You are a data analyst. I will give you a background introduction and data analysis question. You must answer the question. ",
    # custom instructions
    "You're situated inside of a directory with access to all of the files you should need to complete the task. Do not explore files outside of the directory. You're working autonomously; do not end your turn until you have a final result. There is no end user to respond to questions you may have.",
    instruction,
    collapse = "\n\n"
  )

  chat <- prepare_chat(chat)

  # TODO: this should probably actually live inside of a docker container or
  # something more properly sandboxed
  withr::local_dir(dir_prepped)
  chat$chat(instruction_prepped, echo = FALSE)

  chat
}

prepare_chat <- function(chat) {
  chat_ <- chat$clone()
  system_prompt <- chat_$get_system_prompt()
  if (grepl("## Running code", system_prompt, fixed = TRUE)) {
    parts <- strsplit(system_prompt, "## Running code")[[1]]
    chat_$set_system_prompt(parts[2])
  }
  chat_
}

# makes a directory that only has the files for analysis inside of it
prepare_directory <- function(dir) {
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
