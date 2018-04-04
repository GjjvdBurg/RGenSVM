#' @title Predict class labels with the GenSVM model
#'
#' @description This function predicts the class labels of new data using a 
#' fitted GenSVM model.
#'
#' @param object Fitted \code{gensvm} object
#' @param newdata Matrix of new data for which predictions need to be made.
#' @param add.rownames add the rownames from the training data to the 
#' predictions
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
#' @method predict gensvm
#'
#' @export
#'
#' @importFrom stats predict
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
predict.gensvm <- function(object, newdata, add.rownames=FALSE, ...)
{
    ## Implementation note:
    ## - It might seem that it would be faster to do the prediction directly in 
    ## R here, since we then don't need to switch to C, construct model and 
    ## data structures, copy the data, etc. before doing the prediction.  
    ## However, if you actually implement it and compare, we find that calling 
    ## the C implementation is *much* faster than doing it in R.

    if (missing(newdata)) {
        newdata <- eval.parent(object$call$x)
    }
    x.test <- as.matrix(newdata)

    # Sanity check
    if (ncol(x.test) != object$n.features) {
        cat("Error: Number of features of fitted model and testing",
            "data disagree.\n")
        return(invisible(NULL))
    }

    x.train <- eval.parent(object$call$x)
    if (object$kernel == 'linear') {
        y.pred.c <- .Call("R_gensvm_predict",
                      as.matrix(x.test),
                      as.matrix(object$V),
                      as.integer(nrow(x.test)),
                      as.integer(ncol(x.test)),
                      as.integer(object$n.classes)
                      )
    } else {
        kernels <- c("linear", "poly", "rbf", "sigmoid")
        kernel.idx <- which(kernels == object$kernel) - 1
        y.pred.c <- .Call("R_gensvm_predict_kernels",
                          as.matrix(x.test),
                          as.matrix(x.train),
                          as.matrix(object$V),
                          as.integer(nrow(object$V)),
                          as.integer(ncol(object$V)),
                          as.integer(nrow(x.train)),
                          as.integer(nrow(x.test)),
                          as.integer(object$n.features),
                          as.integer(object$n.classes),
                          as.integer(kernel.idx),
                          object$gamma,
                          object$coef,
                          object$degree,
                          object$kernel.eigen.cutoff
                          )
    }

    yhat <- object$classes[y.pred.c]

    if (add.rownames) {
        yhat <- matrix(yhat, length(yhat), 1)
        rownames(yhat) <- rownames(x.train)
    }

    return(yhat)
}
