#' @title Fitted labels for the GenSVMGrid class
#'
#' @description Wrapper to get the fitted class labels from the best estimator 
#' of the fitted GenSVMGrid model. Only works if refit was enabled.
#'
#' @param object A \code{gensvm.grid} object
#' @param \dots further arguments are passed to fitted
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
#' 17(225):1--42. URL \url{https://jmlr.org/papers/v17/14-526.html}.
#'
#' @seealso
#' \code{\link{plot.gensvm}}, \code{\link{predict.gensvm.grid}}, 
#' \code{\link{gensvm}}, \code{\link{gensvm-package}}
#'
#' @method fitted gensvm.grid
#'
#' @export
#'
#' @examples
#' x <- iris[, -5]
#' y <- iris[, 5]
#'
#' # fit GenSVM and compute training set predictions
#' fit <- gensvm(x, y)
#' yhat <- fitted(fit)
#'
#' # compute the accuracy with gensvm.accuracy
#' gensvm.accuracy(y, yhat)
#'
fitted.gensvm.grid <- function(object, ...)
{
    return(predict(object, ...))
}
