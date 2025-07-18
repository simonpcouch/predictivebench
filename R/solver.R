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
  instruction <- input$question

  dir_prepped <- prepare_directory(dir)
  withr::defer(unlink(dir_prepped, recursive = TRUE))

  # TODO: this should probably actually live inside of a docker container or
  # something more properly sandboxed
  withr::local_dir(dir_prepped)

  assistant <- chat$clone()
  assistant$set_system_prompt(paste0(
    assistant$get_system_prompt(),
    "At some point in the conversation, once you've answered the question, you should notify the user that you have come to an answer with the keyword 'ANSWER: ' followed by your answer. Be sure to ask the user whether it's fine to submit an answer before you do so. Make sure to include the keyword in all caps, followed by a colon, as written."
  ))

  res <- converse(assistant, instruction = instruction)

  res
}

mock_analyst <- function() {
  ellmer::chat_openai(
    system_prompt = paste(c(
      "You are role-playing an analyst using an AI assistant.",
      "The assistant is helping you carry out a data analysis and may occasionally ask for your feedback on some analytical decision.",
      "Your job is to keep the conversation going while saying as little as possible.",
      "If the assistant just checks in to get your thumbs-up on some decision, you might say 'Sounds good.' or something of the like.",
      "If the assistant asks you an open-ended question, just ask the assistant to use its best judgment.",
      "If the assistant asks you whether some final answer to your question is satisfactory, affirm it and tell it to use the answer keyword.",
      "Be terse."
    )),
    model = "gpt-4.1-mini"
  )
}

converse <- function(assistant, instruction, analyst = mock_analyst()) {
  assistant_response <- assistant$chat("Hello!", echo = FALSE)
  assistant_response <- assistant$chat(instruction)
  analyst$set_turns(list(
    ellmer::Turn("user", instruction),
    ellmer::Turn("assistant", assistant$last_turn()@text)
  ))

  while (!grepl("ANSWER:", assistant_response)) {
    analyst_response <- analyst$chat(assistant_response)
    assistant_response <- assistant$chat(analyst_response)
  }

  assistant
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
