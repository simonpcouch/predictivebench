
<!-- README.md is generated from README.Rmd. Please edit that file -->

# predictivebench: A predictive modeling LLM evaluation

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/DSBench)](https://CRAN.R-project.org/package=DSBench)
<!-- badges: end -->

This repository implements an adaptation of the data modeling portion of
[DSBench](https://arxiv.org/abs/2409.07703), a data science LLM
evaluation, in R using [vitals](https://vitals.tidyverse.org/).

This implementation of the benchmark diverges from the original in a few
ways:

- predictivebench implements only the data modeling section of DSBench.
- In DSBench, the .xslx, .csv, and other source files are inlined into
  the user prompt. In predictivebench, models are situated in a working
  directory where those files exist and must call tools to read them.
- File names and references are renamed to be less indicative of
  competition data science / model evaluations. This decreases the risk
  of sandbagging, where models will realize they’re being evaluated and
  their behavior might change as a result.
- Rather than requiring the agent to complete the task in one fell
  swoop, the agent is allowed to ask a “user” questions (and *must* do
  so to perform very well on the eval). This allows for a number of
  advantages:
  - In many real-world agent use cases, the agent interacts with the
    user regularly. It’s possible that agents may behave differently
    when acting completely independently versus in tandem with a user.
  - Seldomly in real-world data science would a model be provided with
    all of the necessary information to complete a task in its initial
    prompt. Instead, rather than providing the full description of a
    data science competition and all of the necessary context needed to
    complete it in the first prompt, the agent is provided with a much
    more sparse (3-4 sentence) description of the problem and must
    elicit information from the user.
  - The rest of the needed context lives with a “user” that is actually
    an LLM itself. This mock user has access to a knowledge bank for the
    specific question with some relevant information for the question,
    and is instructed to provide information from the bank if asked for
    it. Otherwise, it will passively nudge the model along when asked
    for input.

> IMPORTANT
>
> This evaluation is highly experimental and much of its documentation
> is aspirational.

# Example

``` r
library(tibble)
library(predictivebench)

modeling_dataset
#> # A tibble: 38 × 5
#>    id                              input            target baseline metric_name
#>    <chr>                           <list>            <dbl>    <dbl> <chr>      
#>  1 cat-in-the-dat-ii               <tibble [1 × 3]>  0.788   0.5    roc_auc    
#>  2 cat-in-the-dat                  <tibble [1 × 3]>  0.803   0.5    roc_auc    
#>  3 commonlitreadabilityprize       <tibble [1 × 3]>  0.446   1.39   rmse       
#>  4 demand-forecasting-kernels-only <tibble [1 × 3]> 12.6    46.2    smape      
#>  5 dont-overfit-ii                 <tibble [1 × 3]>  0.886   0.5    roc_auc    
#>  6 instant-gratification           <tibble [1 × 3]>  0.976   0.5    roc_auc    
#>  7 liverpool-ion-switching         <tibble [1 × 3]>  0.985   0.0361 f_meas     
#>  8 lmsys-chatbot-arena             <tibble [1 × 3]>  0.969   1.10   mn_log_loss
#>  9 microsoft-malware-prediction    <tibble [1 × 3]>  0.676   0.5    roc_auc    
#> 10 nlp-getting-started             <tibble [1 × 3]>  1       0      f_meas     
#> # ℹ 28 more rows
```

Here’s an example question:

``` r
cat(modeling_dataset$input[[1]]$question)
#> I have a dataset called `train` in my current working directory that
#> contains only categorical features, and I need to build a model to
#> predict the probability of a binary target. The features include binary
#> variables, nominal variables with different cardinalities, ordinal
#> variables, and some potentially cyclical features like day and month.
#> I'm looking to optimize my model based on area under the ROC curve -
#> can you help me get started with encoding these categorical variables
#> effectively?
```

The mock user, in this question, would have access to the following
information that it can provide to the agent as requested:

``` r
cat(modeling_dataset$input[[1]]$knowledge)
#> The dataset contains exclusively categorical features with the
#> following types: binary features (bin_*), low- and high-cardinality
#> nominal features (nom_*), low- and high-cardinality ordinal features
#> (ord_*), and potentially cyclical features including day (of the week)
#> and month. The string ordinal features ord_3, ord_4, and ord_5 are
#> lexically ordered according to string.ascii_letters. The target
#> variable is binary and requires probability predictions between 0 and
#> 1. The dataset includes missing values and feature interactions, adding
#> complexity to the encoding task. Files include train.csv (training
#> set), test.csv (test set for predictions), and sample_submission.csv
#> (example submission format). Submissions should be evaluated on area
#> under the ROC curve between predicted probabilities and observed
#> targets. The submission format requires id and target columns, where
#> target represents the predicted probability for each test case.
```

Run the task like so:

``` r
tsk <- modeling_task()
tsk

tsk$eval(solver_chat = predictive:::predictive_client())
```
