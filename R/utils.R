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

converse <- function(
  assistant,
  question,
  analyst = mock_analyst(),
  keyword,
  hook = function(chat) {
    FALSE
  }
) {
  assistant_response <- assistant$chat(question, echo = FALSE)
  analyst$set_turns(list(
    ellmer::Turn("user", "How can I help you?"),
    ellmer::Turn("assistant", question)
  ))

  while (!grepl(keyword, assistant_response) || hook(assistant)) {
    analyst_response <- analyst$chat(assistant_response, echo = FALSE)
    assistant_response <- assistant$chat(analyst_response, echo = FALSE)
  }

  assistant
}
