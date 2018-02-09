#' @title Get the coefficients of the fitted GenSVM model
#'
#' @description Returns the model coefficients of the GenSVM object
#'
#' @param object a \code{gensvm} object
#' @param \dots further arguments are ignored
#'
#' @return The coefficients of the GenSVM model. This is a matrix of size
#' \eqn{(n_{features} + 1) x (n_{classes} - 1)}. This matrix is used to project 
#' the input data to a low dimensional space using the equation: \eqn{XW + t} 
#' where \eqn{X} is the input matrix, \eqn{t} is the first row of the matrix 
#' returned by this function, and \eqn{W} is the \eqn{n_{features} x 
#' (n_{classes} - 1)} matrix formed by the remaining rows.
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
#' @method coef gensvm
#' @export
#'
#' @examples
#'
#'
coef.gensvm <- function(object, ...)
{
    return(object$V)
}
