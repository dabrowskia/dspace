#' accuracy_ds
#'
#' Calculates the accuracy measure based on caret's ranger model for the regionalization done 
#' by polygon_ds or points_ds. It represents how accurate would random forest model predict 
#' appropriate community based only on their attributes.
#'
#' @param x data frame with class attribute and the data that has been taken
#'   into regionalization
#'
#' @return the accuracy measure of random forest classification


accuracy_ds <- function(x)
{
  x$class <- as.factor(x$class)
  x <- x[stats::complete.cases(x), ]
  garbage <- utils::capture.output(model.rf <- caret::train(
    class ~ .,
    x,
    method = "ranger",
    trControl = caret::trainControl(
      number = 5,
      method = "cv",
      verboseIter = TRUE
    )
  ))
  round(max(model.rf$results$Accuracy), 2)
}
