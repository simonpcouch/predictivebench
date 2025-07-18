## DSBench

-   Questions compiled from about 20 sets of competitive data science challenges.

    -   Each challenge has 10-30 questions, 466 questions total.

    -   Answering each question requires some reading/manipulation of .xlsx / .csv files and, sometimes, image files.

        -   In the original eval implementation, the source data is inlined into the prompt.

    -   The questions can be answered independently of each other.

-   Answers are multiple choice.

    -   This is an issue:

        -   Utility: Very seldomly in real life do data science tasks end with a multiple choice answer.

        -   Sandbagging: Models will realize they're being evaluated and their behavior might change as a result.

    -   We could make an alternative version of the evaluation that tries to make this feel as real as possible by removing multiple choice options, renaming files, etc.
