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
    instruction = paste0(
      c(competition_introduction, question_text),
      collapse = "\n\n"
    )
  )
}

#' DSEval dataset
#' @export
dseval_dataset <- create_dseval_dataset()
