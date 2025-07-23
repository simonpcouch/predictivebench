## Submission format

The return value is a submission.csv file with predictions with the **same number of rows as the data being predicted** and in the same order. For observations with missing data such that a prediction cannot be generated, return `NA`. If you're using tidymodels, the submission can be `write.csv(collect_predictions(last_fit(<some_workflow>)), file = "submission.csv")`.
  
If you're not using tidymodels, here's more information on the return format:

* For univariate, numeric point estimates, the column should be named `.pred`. For multivariate numeric predictions (excluding probabilities), the columns should be named `.pred_{outcome name}`.
* Class predictions should be factors with the same levels as the original outcome and named `.pred_class`. 
* For class probability predictions, the columns should be named the same as the factor levels, e.g., `.pred_{level}`, and there should be as many columns as factor levels. 
