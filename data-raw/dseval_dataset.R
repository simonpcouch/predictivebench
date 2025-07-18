format_answers <- function(data) {
  purrr::map_chr(
    data,
    ~ {
      if (length(.x) == 1 && is.atomic(.x) && is.null(names(.x))) {
        .x
      } else {
        paste(names(.x), .x, sep = " = ", collapse = ", ")
      }
    }
  )
}

format_input <- function(input) {
  competition_question <- strsplit(input, "_")[[1]]
  competition <- competition_question[1]
  question <- competition_question[2]

  competition_dir <- file.path("DSBench", "data", competition)
  competition_introduction <- readLines(
    file.path(
      competition_dir,
      "introduction.txt"
    ),
    warn = FALSE
  )
  competition_introduction <- paste0(competition_introduction, collapse = "\n")

  question_text <- readLines(
    file.path(competition_dir, paste0(question, ".txt")),
    warn = FALSE
  )
  question_text <- paste0(question_text, collapse = "\n")

  tibble::tibble(
    dir = competition_dir,
    introduction = competition_introduction,
    question = question_text,
  )
}

create_dseval_dataset <- function() {
  # the original json is malformed:
  #>   Error in `parse_con()`:
  #> ! parse error: trailing garbage
  #>           1626, 323272], "year": 2016} {"id": "00000002", "name": "201
  #>                      (right here) ------^
  #
  # so, read it line-by-line
  metadata_lines <- readLines("DSBench/data_analysis/data.json", warn = FALSE)

  metadata_json <- list()
  for (line in metadata_lines) {
    line_json <- jsonlite::parse_json(line)
    metadata_json <- c(metadata_json, list(line_json))
  }

  res <- dplyr::bind_rows(metadata_json)

  # all of the questions are length 1, but not necessarily the answers
  #> sum(unlist(lapply(res$answers, length)) == 1)
  #> [1] 451
  res <- tidyr::unnest(res, questions)
  res <- dplyr::rename(
    res,
    input = questions,
    target = answers,
    competition_id = id
  )
  res <- dplyr::mutate(res, id = paste(competition_id, input, sep = "_"))

  res$input <- lapply(res$id, format_input)

  res <- dplyr::relocate(res, c(id, input, target), .before = name)
  res <- dplyr::relocate(res, c(competition_id), .after = target)
  res <- tidyr::nest(
    res,
    .by = c(id, input, target, competition_id),
    .key = "metadata"
  )
  res[["target"]] <- format_answers(res[["target"]])
  res
}

dseval_raw <- create_dseval_dataset()

dseval_d <- tidyr::unnest(dseval_raw, input)

# Now, we have:
# * target: The multiple choice corresponding to the correct answer
# * input$introduction: The full problem question (sometimes referencing question IDs)
# * input$question: The question, in multiple choice format
# * input$dir: The directory to the full question metadata
#
# We want:
# * target: The _actual_ correct answer
# * input$introduction: The full problem question (with no references to question IDs,
#   but possibly containing the additional information for that question)
# * input$question: The question (with no suggested, multiple choice answers)
# * input$dir: As it was.

prompts <- glue::glue_data(
  dseval_d,
  "
Here's some information on a multiple choice question:

ID:\n {id}\n\n
INTRODUCTION:\n {introduction}\n\n
QUESTION:\n {question}\n\n
TARGET (current):\n {target}\n\n
DIRECTORY:\n {dir}\n\n

I would like to make this multiple choice question in an open-ended question by
extracting:

- target: The actual correct answer text (not the letter). e.g., if the target says 'D' and the multiple choice question has options A. 72, B. 73, C. 74, D. 75, just return '75'.
- introduction: The full introduction, unchanged besides 1) references to irrelevant questions removed, and 2) references from this relevant question incorporated into the introduction with no question ID/number.
- question: The question text only, with no multiple choice options. If the question is ambiguous in its current writing (e.g. if there's a free text response that requires specific componenets), rephrase the question to ask for the components required in the target.
- dir: As it was provided
"
)

library(ellmer)
ch <- chat_anthropic(model = "claude-sonnet-4-20250514")
res <- parallel_chat_structured(
  ch,
  as.list(prompts),
  type_object(
    target = type_string(
      description = "The actual correct answer text (not the letter). e.g., if the target says 'D' and the
   multiple choice question has options A. 72, B. 73, C. 74, D. 75, just return '75'."
    ),
    introduction = type_string(
      description = "The full introduction, unchanged besides 1) references to irrelevant questions 
  removed, and 2) references from this relevant question incorporated into the introduction with no question ID/number."
    ),
    question = type_string(
      description = "The question text only, with no multiple choice options. If the question is 
  ambiguous in its current writing (e.g. if there's a free text response that requires specific components), rephrase the 
  question to ask for the components required in the target."
    ),
    dir = type_string(description = "As it was provided")
  )
)

d <- dplyr::bind_cols(dseval_d[!names(dseval_d) %in% names(res)], res)
d <- dplyr::mutate(d, question = paste(introduction, question, sep = "\n\n"))
d <- dplyr::select(d, -c(introduction))
d <- tidyr::nest(d, input = c(question, dir))
d <- tidyr::unnest(d, metadata)
d <- dplyr::nest_by(d, id, input, target, .key = "metadata")

dseval_dataset <- dplyr::ungroup(d)

usethis::use_data(dseval_dataset, overwrite = TRUE)
