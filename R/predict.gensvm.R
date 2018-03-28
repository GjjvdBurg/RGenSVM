#' @title Predict class labels with the GenSVM model
#'
#' @description This function predicts the class labels of new data using a 
#' fitted GenSVM model.
#'
#' @param fit Fitted \code{gensvm} object
#' @param x.test Matrix of new values for \code{x} for which predictions need 
#' to be made.
#' @param \dots further arguments are ignored
#'
#' @return a vector of class labels, with the same type as the original class 
#' labels.
#'
#' @author
#' Gerrit J.J. van den Burg, Patrick J.F. Groenen \cr
#' Maintainer: Gerrit J.J. van den Burg <gertjanvandenburg@gmail.com>
#'
#' @references
#' Van den Burg, G.J.J. and Groenen, P.J.F. (2016). \emph{GenSVM: A Generalized 
#' Multiclass Support Vector Machine}, Journal of Machine Learning Research, 
#' 17(225):1--42. URL \url{http://jmlr.org/papers/v17/14-526.html}.
#'
#' @seealso
#' \code{\link{plot.gensvm}}, \code{\link{predict.gensvm.grid}}, 
#' \code{\link{gensvm}}, \code{\link{gensvm-package}}
#'
#' @export
#' @aliases predict
#'
#' @examples
#' x <- iris[, -5]
#' y <- iris[, 5]
#'
#' # create a training and test sample
#' attach(gensvm.train.test.split(x, y))
#' fit <- gensvm(x.train, y.train)
#'
#' # predict the class labels of the test sample
#' y.test.pred <- predict(fit, x.test)
#'
#' # compute the accuracy with gensvm.accuracy
#' gensvm.accuracy(y.test, y.test.pred)
#'
predict.gensvm <- function(fit, x.test, ...)
{
    ## Implementation note:
    ## - It might seem that it would be faster to do the prediction directly in 
    ## R here, since we then don't need to switch to C, construct model and 
    ## data structures, copy the data, etc. before doing the prediction.  
    ## However, if you actually implement it and compare, we find that calling 
    ## the C implementation is *much* faster than doing it in R.

    # Sanity check
    if (ncol(x.test) != fit$n.features) {
        cat("Error: Number of features of fitted model and testing",
            "data disagree.\n")
        return
    }

    x.train <- fit$X.train
    if (fit$kernel != 'linear' && is.null(x.train)) {
        cat("Error: The training data is needed to compute predictions for ",
             "nonlinear GenSVM. This data is not present in the fitted ",
             "model!\n", sep="")
    }
    if (!is.null(x.train) && ncol(x.train) != fit$n.features) {
        cat("Error: Number of features of fitted model and training",
            "data disagree.\n")
        return
    }

    if (fit$kernel == 'linear') {
        y.pred.c <- .Call("R_gensvm_predict",
                      as.matrix(x.test),
                      as.matrix(fit$V),
                      as.integer(nrow(x.test)),
                      as.integer(ncol(x.test)),
                      as.integer(fit$n.classes)
                      )
    } else {
        kernels <- c("linear", "poly", "rbf", "sigmoid")
        kernel.idx <- which(kernels == fit$kernel) - 1
        y.pred.c <- .Call("R_gensvm_predict_kernels",
                          as.matrix(x.test),
                          as.matrix(x.train),
                          as.matrix(fit$V),
                          as.integer(nrow(fit$V)),
                          as.integer(ncol(fit$V)),
                          as.integer(nrow(x.train)),
                          as.integer(nrow(x.test)),
                          as.integer(fit$n.features),
                          as.integer(fit$n.classes),
                          as.integer(kernel.idx),
                          fit$gamma,
                          fit$coef,
                          fit$degree,
                          fit$kernel.eigen.cutoff
                          )
    }

    yhat <- fit$classes[y.pred.c]

    return(yhat)
}
