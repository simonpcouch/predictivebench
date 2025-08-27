the <- rlang::new_environment()
the$assistant_is_finished <- FALSE

end_conversation <- ellmer::tool(
  function() {
    the$assistant_is_finished <- TRUE
  },
  description = "
As the mock user of an AI tool, it is your responsibility to determine when your conversation with the assistant (the conversant other than you) is finished.

Call this tool either when 1) the assistant is satisfied the performance of the best ML model it has built so far, or 2) the assistant is unable to recover from errors it is encountering with the `run_experiment` tool.
",
  name = "end_conversation"
)

converse <- function(
  assistant,
  question,
  analyst
) {
  the$assistant_is_finished <- FALSE
  analyst_response <- question

  while (!the$assistant_is_finished) {
    assistant_response <- assistant$chat(analyst_response, echo = FALSE)
    if (in_debug_mode()) {
      return(assistant)
    }
    analyst_response <- analyst$chat(assistant_response, echo = FALSE)
  }

  assistant
}

in_debug_mode <- function() {
  identical(Sys.getenv("PREDICTIVEBENCH_DEBUG_MODE"), "true")
}
