library(ellmer)
library(tidyverse)
devtools::load_all()

# models of interest ----------------------------------------------------------
claude_4_sonnet <- chat_anthropic(model = "claude-sonnet-4-20250514")
gpt_4_1 <- chat_openai(model = "gpt-4.1")
gemini_2_5_pro <- chat_google_gemini(
  model = "gemini-2.5-pro"
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
uses_default_metrics <- modeling_dataset$metric_name %in%
  c("accuracy", "roc_auc", "rmse", "rsq")


# dirs <- list.files("DSBench/data_modeling/data/data_resplit", full.names = TRUE)
# nrows <- tibble::tibble(
#   id = basename(dirs),
#   nrow = sapply(dirs, function(d) {
#     length(readLines(file.path(d, "train.csv"))) - 1
#   })
# )
#
# save(nrows, file = "inst/data-raw/nrows.rda")

nrows <- NULL
load("inst/data-raw/nrows.rda")

nrows %>% dplyr::arrange(nrow)
#> # A tibble: 75 × 2
#>    id                                    nrow
#>    <chr>                                <dbl>
#>  1 dont-overfit-ii                        200
#>  2 playground-series-s3e12                331
#>  3 playground-series-s3e13                565
#>  4 titanic                                712
#>  5 playground-series-s3e22                988
#>  6 playground-series-s3e3                1341
#>  7 playground-series-s3e5                1644
#>  8 tmdb-box-office-prediction            2400
#>  9 santander-value-prediction-challenge  3567
#> 10 playground-series-s3e9                4325

sample_by_size <-
  modeling_dataset[which(uses_default_metrics), ] %>%
  left_join(nrows, by = join_by(id)) %>%
  arrange(nrow) %>%
  pull(id) %>%
  match(modeling_dataset$id)

tsk <- modeling_task(samples = sample_by_size[1])

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

tsk_gemini_2_5_pro_run_r_code <- tsk$clone()
tsk_gemini_2_5_pro_run_r_code$eval(
  solver_chat = solver_run_r_code(gemini_2_5_pro)
)

save(
  tsk_gemini_2_5_pro_run_r_code,
  file = ".vitals/tasks/tsk_gemini_2_5_pro_run_r_code.rda"
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

tsk_gemini_2_5_pro_run_r_code_and_prompt <- tsk$clone()
tsk_gemini_2_5_pro_run_r_code_and_prompt$eval(
  solver_chat = solver_run_r_code_and_prompt(gemini_2_5_pro)
)

save(
  tsk_gemini_2_5_pro_run_r_code_and_prompt,
  file = ".vitals/tasks/tsk_gemini_2_5_pro_run_r_code_and_prompt.rda"
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

tsk_gemini_2_5_pro_predictive <- tsk$clone()
tsk_gemini_2_5_pro_predictive$eval(
  solver_chat = solver_predictive(gemini_2_5_pro)
)

save(
  tsk_gemini_2_5_pro_predictive,
  file = ".vitals/tasks/tsk_gemini_2_5_pro_predictive.rda"
)

# quicker evals to test against
quick_predictive <- function(chat) {
  ch <- chat$clone()
  ch <- predictive:::predictive_client(ch, default_turns = list())
  ch$set_system_prompt("")
  ch
}

Sys.setenv("PREDICTIVEBENCH_DEBUG_MODE" = "true")

tsk <- modeling_task(samples = sample_by_size[1])
tsk_claude_4_sonnet_predictive <- tsk$clone()
tsk_claude_4_sonnet_predictive$eval(
  solver_chat = quick_predictive(claude_4_sonnet)
)
