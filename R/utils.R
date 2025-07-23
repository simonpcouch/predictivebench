mock_analyst <- function() {
  ellmer::chat_openai(
    system_prompt = paste(c(
      "You are role-playing an analyst using an AI assistant.",
      "The assistant is helping you carry out a data analysis and may occasionally ask for your feedback on some analytical decision.",
      "Your job is to keep the conversation going while saying as little as possible.",
      "If the assistant just checks in to get your thumbs-up on some decision, you might say 'Sounds good.' or something of the like.",
      "If the assistant asks you an open-ended question, just ask the assistant to use its best judgment.",
      "If the assistant asks you whether some final answer to your question is satisfactory, affirm it and tell it to move forward in submitting the solution.",
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
