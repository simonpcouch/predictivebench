library(vitals)
library(tidyverse)
library(rlang)

task_files <- list.files(".vitals/tasks", full.names = TRUE)

tasks <- rlang::new_environment()

for (file in task_files) {
  load(file, envir = tasks)
}

modeling_results_raw <- vitals_bind(!!!as.list(tasks))


modeling_results <-
  modeling_results_raw |>
  mutate(
    model = case_when(
      str_detect(task, "gemini_2_5_pro") ~ "gemini_2_5_pro",
      str_detect(task, "claude_4_sonnet") ~ "claude_4_sonnet"
    ),
    configuration = case_when(
      str_detect(task, "predictive$") ~ "predictive",
      str_detect(task, "run_r_code_and_prompt$") ~ "run_r_code_and_prompt",
      str_detect(task, "run_r_code$") ~ "run_r_code"
    ),
    score = score * 100
  ) |>
  select(id, model, configuration, score) %>%
  mutate(
    model = case_when(
      model == "gemini_2_5_pro" ~ "Gemini Pro 2.5",
      model == "claude_4_sonnet" ~ "Claude Sonnet 4",
      TRUE ~ model
    ),
    configuration = case_when(
      configuration == "predictive" ~ "Predictive",
      configuration == "run_r_code_and_prompt" ~ "Databot",
      configuration == "run_r_code" ~ "`run_r_code()` tool only",
      TRUE ~ configuration
    )
  )


modeling_results %>%
  group_by(model, configuration) %>%
  summarize(mean_score = mean(score, na.rm = TRUE), .groups = "drop") %>%
  ggplot(
    aes(x = model, y = mean_score, fill = configuration)
  ) +
  geom_col(position = position_dodge()) +
  labs(
    x = "Model",
    y = "Relative performance",
    fill = "Configuration",
    caption = "The additional prompting and tools from predictive don't\nmake modern LLMs better at developing ML models;\na `run_r_code()` tool is enough."
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(angle = 45, hjust = 1),
    plot.caption = element_text(face = "italic", hjust = 0)
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = c(0, 50, 100),
    labels = c("(Baseline model) - 0", "50", "(Best human modelers) - 100")
  ) +
  scale_fill_brewer(palette = "Set2")
