#' DSEval evaluation task
#' @export
dseval_task <- function(epochs = 1, dir = ".vitals/logs/") {
  vitals::Task$new(
    dataset = dseval_dataset[1:3, ],
    solver = dseval_solver(),
    scorer = vitals::model_graded_qa(),
    epochs = epochs,
    name = "DSBench",
    dir = dir
  )
}
