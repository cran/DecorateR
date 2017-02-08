#' Predict method for DECORATE objects
#'
#' Prediction of new data using DECORATE
#'
#' @param object an object of the class \code{DECORATE}, as created by the function \code{DECORATE}.
#' @param newdata a data frame containing the same predictors as in the training phase.
#' @param all Return the predictions per tree instead of the average (default = FALSE).
#' @param ... Not used currently.
#'
#' @return  vector containing the response probabilities.
#'
#'
#' @examples
#'
#' data(iris)
#' y <- as.factor(ifelse(iris$Species[1:100]=="setosa",0,1))
#' x <- iris[1:100,-5]
#' dec <- DECORATE(x = x, y = y)
#' predict(object=dec,newdata=x)
#'
#'


predict.DECORATE <- function (object, newdata, all =FALSE, ...) {
  final <- newdata

  if (!is.data.frame(final)) stop("newdata must be a data frame")

  predicted <- matrix(NA,nrow=nrow(final),ncol=length(object))

  for (i in 1:length(object)){
    predicted[,i] <-predict(object[[i]], final, 'prob')[,2]
  }

  if(all) {
    return(predicted)
  } else {
   finalpredicted <- rowMeans(predicted)
   return(finalpredicted)
  }

}
