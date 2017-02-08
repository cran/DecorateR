#' Binary classification with DECORATE (Melville and Mooney, 2005)
#'
#' \code{DECORATE} (Diverse Ensemble Creation by Oppositional Relabeling of
#' Artificial Training Examples) builds an ensemble of J48 trees by recursively
#' adding artificial samples of the training data.
#'
#' @param x a data frame of predictor (numeric, integer or factors). Character
#'   variables should be transformed to factors.
#' @param y a vector of response labels. Only \{0, 1\} is allowed.
#' @param C the desired ensemble size. Set to 15 as recommended by Melville and
#'   Mooney (2005).
#' @param I the maximum number of iterations. Set to 50 as recommended by
#'   Melville and Mooney (2005).
#' @param R the amount of articially generated examples, expressed as a fraction
#'   of the number of training examples. R is set to 1, meaning that the number
#'   of artificially created samples is equal to the training set size.
#' @param verbose TRUE or FALSE. Should information be printed on the screen?
#'
#' @return an object of class \code{DECORATE}.
#'
#' @references Melville, P., & Mooney, R. J. (2005). Creating diversity in ensembles using artificial data. Information Fusion, 6(1), 99-111. <doi: 10.1016/j.inffus.2004.04.001>
#'
#' @seealso \code{\link{predict.DECORATE}}
#'
#' @author Authors: Matthias Bogaert, Michel Ballings and Dirk Van den Poel, Maintainer: \email{Matthias.Bogaert@@UGent.Be}
#' @examples
#'
#' data(iris)
#' y <- as.factor(ifelse(iris$Species[1:100]=="setosa",0,1))
#' x <- iris[1:100,-5]
#' dec <- DECORATE(x = x, y = y)
#'
DECORATE <- function(x,y, C = 15,I = 50, R = 1, verbose = FALSE) {

  #Error handling
  if (!is.data.frame(x)) stop("x must be a data frame")
  if (is.null(x) || is.null(y)) {
        stop("x or y cannot be NULL.")
    }
  else if (any(is.na(x)) || any(is.na(y))) {
        stop("NAs not permitted.")
    }
  if (!is.factor(y)) stop("y must be a factor")
  if (any(table(y) == 0)) stop("Cannot have empty classes in y.")
  if (length(unique(y)) != 2)  stop("Must have 2 classes.")
  if (length(y) != nrow(x)) stop("x and y have to be of equal length.")
  if (is.infinite(R) || is.infinite(I)) {
    stop("R and I must have a finite length.")
  }
  if (any(sapply(x,is.character))) stop("No character values allowed. Transform to factor.")

  # determine parameters
  m <- nrow(x)
  Rsize <- R * m

  #initialize iterators
  i = 1
  trials = 1
  ensemble <- list()

  # build a J48 tree
  if (verbose) cat(" Building J48 tree ...................  \n")
  ensemble[[i]] <- J48(formula = y  ~ ., data = x)
  pred <- predict(ensemble[[i]],x,'prob')[,2]

  #Look at error
  labels <- factor(ifelse(pred > 0.5 , 1,0), levels = c('0','1'))
  error <- sum(labels != y)/m

  ###### Loop until max ensemble size is reached or max iterations
  if (verbose) cat(" Starting loop ...................  \n")

  while (i <= C & trials <= I) {

    if (verbose) cat(" i = ", i ,"and trial = ", trials ,"  \n")

    #### Create the articical training set

    #Numerical
    num <- sapply(x,is.numeric)

    if (any(num) ==TRUE) {
      x_num <- x[,num, drop = FALSE]

      artificial_num <- data.frame(sapply(x_num, function (x) {
        mean <- mean(x)
        stdev <- sd(x)
        rnorm(n = Rsize, mean = mean, sd = stdev)
      }, simplify = FALSE))
    }

    #Factors
    fact <- sapply(x, is.factor)
    if (any(fact == TRUE)) {
      x_fact <- x[, fact, drop = FALSE]

      artificial_fact <- data.frame(sapply(x_fact, function (x) {
        as.factor(sample(x = levels(x), size = Rsize, replace = TRUE, prob = table(x)/length(x)))
      }, simplify = FALSE))
    }

    #Combine everything
    if (any(num) == TRUE & any(fact) == TRUE) {
      x_artificial <- data.frame(artificial_num, artificial_fact)
      rm(list = c('artificial_num', 'artificial_fact'))
    } else if (any(num) == TRUE) {
      x_artificial <- data.frame(artificial_num)
      rm(list = c('artificial_num'))
    } else {
      x_artificial <- data.frame(artificial_fact)
      rm(list = c('artificial_fact'))
    }

    # construct the labels of the artificial members
    # replace the 0s with small number and scale
    # Then if scaled, bigger than 0 are 1s
    pred_scaled <- ifelse(pred == 0, 0.01,pred)
    pred_scaled <- scale(pred_scaled)
    y_artificial <- (1/pred_scaled)/(sum((1/pred_scaled)))
    y_artificial <- factor(ifelse(y_artificial > 0 , 1,0), levels = c('0','1'))

    #add the new data sets
    x_new <- rbind(x, x_artificial)
    y_new <- as.factor(c(as.character(y), as.character(y_artificial)))

    ####### remodel
    tree_new <- J48(formula = y_new ~.,data= x_new)

    ####### Make predictions on the ensemble (combination of the old and the new one)
    ensemble_new <- list()
    for (j in 1:(length(ensemble))) {
      ensemble_new[[j]] <- ensemble[[j]]
    }
    ensemble_new[[length(ensemble) +1]] <- tree_new

    # Make predictions on the ensemble:
    # Make predictions on the original dataset and on the augmented
    # Take the average for the final predictions
    preds <- matrix(NA, ncol = length(ensemble_new), nrow = m)
    for(ii in 1:length (ensemble_new)) {
      preds[,ii] <- predict(ensemble_new[[ii]], x, 'prob')[,2]
    }
    pred_new <- rowMeans(preds)
    labels_new <- factor(ifelse(pred_new > 0.5 , 1,0), levels = c('0','1'))

    #estimate the error
    error_new <- sum(labels_new != y)/m

    if (error_new <= error) {
      ensemble <- ensemble_new
      pred <- pred_new
      error <- error_new
      i <- i + 1
    }
    trials <- trials + 1
  }

  if (verbose) cat("Finished ...................  \n")

  class(ensemble) <-'DECORATE'
  return(ensemble)
}
