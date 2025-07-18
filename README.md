
<!-- README.md is generated from README.Rmd. Please edit that file -->

# DSEval

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/DSBench)](https://CRAN.R-project.org/package=DSBench)
<!-- badges: end -->

This repository implements an adaptation of
[DSBench](https://arxiv.org/abs/2409.07703), a data science LLM
evaluation, in R using [vitals](https://vitals.tidyverse.org/).

The original benchmark contains 466 data analysis questions:

- In DSBench, each question has a multiple choice answer. This is an
  issue: very seldomly in real life do data science tasks end with a
  multiple choice answer. Also, this increases the risk of sandbagging,
  where models will realize they’re being evaluated and their behavior
  might change as a result. DSEval rephrases questions to have
  open-ended answers.
- In DSBench, the .xslx, .csv, and other source files are inlined into
  the user prompt. In DSEval, models are situated in a working directory
  where those files exist.
- File names and references are renamed to be less indicative of
  competition data science / model evaluations, also to descrease the
  risk of sandbagging.
- Rather than requiring the agent to complete the task in one fell
  swoop, the agent is allowed to ask a user questions. That “user” is
  actually a “model-in-the-middle”, instructed to generally provide a
  thumbs-up and little guidance.

> IMPORTANT
>
> This evaluation is highly experimental and much of its documentation
> is aspirational.

# Example

``` r
library(tibble)
library(DSEval)

dseval_dataset
#> # A tibble: 466 × 5
#>    id                  input            target competition_id metadata        
#>    <chr>               <list>           <chr>  <chr>          <list>          
#>  1 00000001_question6  <tibble [1 × 2]> D      00000001       <tibble [1 × 4]>
#>  2 00000001_question7  <tibble [1 × 2]> D      00000001       <tibble [1 × 4]>
#>  3 00000001_question8  <tibble [1 × 2]> I      00000001       <tibble [1 × 4]>
#>  4 00000001_question9  <tibble [1 × 2]> A      00000001       <tibble [1 × 4]>
#>  5 00000001_question10 <tibble [1 × 2]> H      00000001       <tibble [1 × 4]>
#>  6 00000001_question11 <tibble [1 × 2]> C      00000001       <tibble [1 × 4]>
#>  7 00000001_question12 <tibble [1 × 2]> E      00000001       <tibble [1 × 4]>
#>  8 00000001_question13 <tibble [1 × 2]> H      00000001       <tibble [1 × 4]>
#>  9 00000001_question14 <tibble [1 × 2]> D      00000001       <tibble [1 × 4]>
#> 10 00000001_question15 <tibble [1 × 2]> A      00000001       <tibble [1 × 4]>
#> # ℹ 456 more rows
```

``` r
tsk <- dseval_task()
tsk

tsk$eval(solver_chat = databot:::chat_bot())
```
