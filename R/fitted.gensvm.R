#' @title Show fitted labels for the GenSVM model
#'
#' @description This function shows the fitted class labels of training data 
#' using a fitted GenSVM model.
#'
#' @param fit Fitted \code{gensvm} object
#' @param \dots further arguments are passed to predict
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
#' @aliases fitted
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
fitted.gensvm <- function(fit, ...)
{
    return(predict(fit, ...))
}
