#' @title Get the parameter grid from a GenSVM Grid object
#'
#' @description Returns the parameter grid of a \code{gensvm.grid} object.
#'
#' @param object a \code{gensvm.grid} object
#' @param \dots further arguments are ignored
#'
#' @return The parameter grid of the GenSVMGrid object as a data frame.
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
#' \code{\link{gensvm.grid}}, \code{\link{gensvm-package}}
#'
#' @method coef gensvm.grid
#'
#' @export
#'
#' @importFrom stats coef
#'
#' @examples
#' x <- iris[, -5]
#' y <- iris[, 5]
#'
#' grid <- gensvm.grid(x, y)
#' pg <- coef(grid)
#'
coef.gensvm.grid <- function(object, ...)
{
    return(object$param.grid)
}
