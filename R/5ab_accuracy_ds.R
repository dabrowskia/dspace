#' accuracy_ds
#'
#' Calculates the accuracy measure based on caret's ranger model for the regionalization done 
#' by polygon_ds or points_ds. It represents how accurate would random forest model predict 
#' appropriate community based only on their attributes.
#'
#' @param data.to.accu data frame with class attribute and the data that has been taken
#'   into regionalization
#'
#' @return the accuracy measure of random forest classification


accuracy_ds <- function(data.to.accu)
{
  data.to.accu$class <- as.factor(data.to.accu$class)
  data.to.accu <- data.to.accu[stats::complete.cases(data.to.accu), ]

# mlr ---------------------------------------------------------------------

  
  trainTask <- mlr::makeClassifTask(data = data.to.accu,
                               target = "class")
  
  rf <- mlr::makeLearner("classif.randomForest",
                    predict.type = "response",
                    par.vals = list(ntree = 200, mtry = 3))
  rancontrol <- mlr::makeTuneControlRandom(maxit = 50L)
  set_cv <- mlr::makeResampleDesc("CV", iters = 5L)
  rf_param <- ParamHelpers::makeParamSet(
    ParamHelpers::makeIntegerParam("ntree",lower = 50, upper = 500),
    ParamHelpers::makeIntegerParam("mtry", lower = 3, upper = 10),
    ParamHelpers::makeIntegerParam("nodesize", lower = 10, upper = 50)
  )
  rf_tune <- mlr::tuneParams(learner = rf,
                             resampling = set_cv,
                             task = trainTask,
                             control = rancontrol,
                             par.set = rf_param,
                             measures = mlr::acc,
                             show.info = F)
  
  round(rf_tune$y, 2)
}
