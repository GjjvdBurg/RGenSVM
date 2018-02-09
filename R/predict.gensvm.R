#' @title Predict class labels with the GenSVM model
#'
#' @description This function predicts the class labels of new data using a 
#' fitted GenSVM model.
#'
#' @param object Fitted \code{gensvm} object
#' @param newx Matrix of new values for \code{x} for which predictions need to 
#' be made.
#' @param \dots further arguments are ignored
#'
#' @return a vector of class labels, with the same type as the original class 
#' labels.
#'
#' @export
#' @aliases predict
#'
#' @author
#' Gerrit J.J. van den Burg, Patrick J.F. Groenen
#' Maintainer: Gerrit J.J. van den Burg <gertjanvandenburg@gmail.com>
#'
#' @references
#' Van den Burg, G.J.J. and Groenen, P.J.F. (2016). \emph{GenSVM: A Generalized 
#' Multiclass Support Vector Machine}, Journal of Machine Learning Research, 
#' 17(225):1--42. URL \url{http://jmlr.org/papers/v17/14-526.html}.
#'
#' @examples
#'
#'
#'
predict.gensvm <- function(object, newx, ...)
{
    # TODO: C library fitting prediction here (or not? with the column-major 
    # order it may be faster to do it directly in R)

    return(yhat)
}
