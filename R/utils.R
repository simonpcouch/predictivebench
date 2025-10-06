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

    if (in_debug_mode() && has_run_experiments()) {
      return(assistant)
    }

    analyst_response <- analyst_chat(
      # Don't maintain context in the analyst at all--just reassemble it
      # from the assistant each time.
      analyst$clone(),
      assistant$get_turns()
    )
  }

  assistant
}

# The mock modeler can get confused about which role it's supposed to be playing.
# To combat this, format the messages ourselves to reinforce the role-play.
analyst_chat <- function(
  analyst,
  assistant_turns
) {
  assembled_message <- paste0(
    vapply(
      assistant_turns,
      function(turn) {
        role_label <- if (turn@role == "assistant") {
          "User [ACTUALLY ANOTHER AI MODEL]"
        } else {
          "Assistant [THIS IS YOU, A MOCK USER]"
        }
        paste0(role_label, ": ", ellmer::contents_markdown(turn))
      },
      character(1)
    ),
    collapse = "\n\n"
  )

  analyst$chat(
    paste0(
      assembled_message,
      "\n\n",
      "Assistant [THIS IS YOU, A MOCK USER]:"
    ),
    echo = FALSE
  )
}

in_debug_mode <- function() {
  identical(Sys.getenv("PREDICTIVEBENCH_DEBUG_MODE"), "true")
}


has_run_experiments <- function() {
  the_predictive <- getFromNamespace("the", rlang::ns_env("predictive"))

  length(the_predictive$experiments) >= 2
}
