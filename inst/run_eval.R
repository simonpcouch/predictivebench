library(ellmer)
library(tidyverse)

# models of interest ----------------------------------------------------------
claude_4_sonnet <- chat_anthropic(model = "claude-sonnet-4-20250514")
gpt_4_1 <- chat_openai(model = "gpt-4.1")
gemini_2_5_flash <- chat_google_gemini(
  model = "gemini-2.5-flash",
  api_args = list(
    generationConfig = list(
      thinkingConfig = list(
        thinkingBudget = 0
      )
    )
  )
)

# solvers of interest ---------------------------------------------------------
solver_run_r_code <- function(chat) {
  ch <- chat$clone()
  ch$set_tools(list(predictive:::tool_run_r_code))
  ch
}


solver_run_r_code_and_prompt <- function(chat) {
  ch <- chat$clone()
  ch$set_tools(list(predictive:::tool_run_r_code))
  # This prompt is just the original `predictive` prompt with
  # mentions of the run_experiment() tool removed.
  ch$set_system_prompt(system.file(
    "prompt_sans_run_experiment.md",
    package = "predictivebench"
  ))
  ch
}

# predictive's prompt and tools run_r_code, run_experiment, btw docs tools
solver_predictive <- function(chat) {
  ch <- chat$clone()
  predictive:::predictive_client(ch, default_turns = list())
}

# evals to run -----------------------------------------------------------------
tsk <- modeling_task()

# run_r_code only ----------------------------------------------------------------
tsk_claude_4_sonnet_run_r_code <- tsk$clone()
tsk_claude_4_sonnet_run_r_code$eval(
  solver_chat = solver_run_r_code(claude_4_sonnet)
)

save(
  tsk_claude_4_sonnet_run_r_code,
  file = ".vitals/tasks/tsk_claude_4_sonnet_run_r_code.rda"
)

tsk_gpt_4_1_run_r_code <- tsk$clone()
tsk_gpt_4_1_run_r_code$eval(
  solver_chat = solver_run_r_code(gpt_4_1)
)

save(
  tsk_gpt_4_1_run_r_code,
  file = ".vitals/tasks/tsk_gpt_4_1_run_r_code.rda"
)

tsk_gemini_2_5_flash_run_r_code <- tsk$clone()
tsk_gemini_2_5_flash_run_r_code$eval(
  solver_chat = solver_run_r_code(gemini_2_5_flash)
)

save(
  tsk_gemini_2_5_flash_run_r_code,
  file = ".vitals/tasks/tsk_gemini_2_5_flash_run_r_code.rda"
)

# run_r_code and prompt ----------------------------------------------------------
tsk_claude_4_sonnet_run_r_code_and_prompt <- tsk$clone()
tsk_claude_4_sonnet_run_r_code_and_prompt$eval(
  solver_chat = solver_run_r_code_and_prompt(claude_4_sonnet)
)

save(
  tsk_claude_4_sonnet_run_r_code_and_prompt,
  file = ".vitals/tasks/tsk_claude_4_sonnet_run_r_code_and_prompt.rda"
)

tsk_gpt_4_1_run_r_code_and_prompt <- tsk$clone()
tsk_gpt_4_1_run_r_code_and_prompt$eval(
  solver_chat = solver_run_r_code_and_prompt(gpt_4_1)
)

save(
  tsk_gpt_4_1_run_r_code_and_prompt,
  file = ".vitals/tasks/tsk_gpt_4_1_run_r_code_and_prompt.rda"
)

tsk_gemini_2_5_flash_run_r_code_and_prompt <- tsk$clone()
tsk_gemini_2_5_flash_run_r_code_and_prompt$eval(
  solver_chat = solver_run_r_code_and_prompt(gemini_2_5_flash)
)

save(
  tsk_gemini_2_5_flash_run_r_code_and_prompt,
  file = ".vitals/tasks/tsk_gemini_2_5_flash_run_r_code_and_prompt.rda"
)

# predictive --------------------------------------------------------------------
tsk_claude_4_sonnet_predictive <- tsk$clone()
tsk_claude_4_sonnet_predictive$eval(
  solver_chat = solver_predictive(claude_4_sonnet)
)

save(
  tsk_claude_4_sonnet_predictive,
  file = ".vitals/tasks/tsk_claude_4_sonnet_predictive.rda"
)

tsk_gpt_4_1_predictive <- tsk$clone()
tsk_gpt_4_1_predictive$eval(
  solver_chat = solver_predictive(gpt_4_1)
)

save(
  tsk_gpt_4_1_predictive,
  file = ".vitals/tasks/tsk_gpt_4_1_predictive.rda"
)

tsk_gemini_2_5_flash_predictive <- tsk$clone()
tsk_gemini_2_5_flash_predictive$eval(
  solver_chat = solver_predictive(gemini_2_5_flash)
)

save(
  tsk_gemini_2_5_flash_predictive,
  file = ".vitals/tasks/tsk_gemini_2_5_flash_predictive.rda"
)
