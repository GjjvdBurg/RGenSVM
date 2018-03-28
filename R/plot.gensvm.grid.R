#' @title Plot the simplex space of the best fitted model in the GenSVMGrid
#'
#' @description This is a wrapper which calls the plot function for the best 
#' model in the provided GenSVMGrid object. See the documentation for 
#' \code{\link{plot.gensvm}} for more information.
#'
#' @param grid A \code{gensvm.grid} object trained with refit=TRUE
#' @param x the dataset to plot
#' @param ... further arguments are passed to the plot function
#'
#' @return returns the object passed as input
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
#' \code{\link{plot.gensvm}}, \code{\link{gensvm.grid}}, 
#' \code{\link{predict.gensvm.grid}}, \code{\link{gensvm-package}}
#'
#' @export
#'
#' @examples
#' x <- iris[, -5]
#' y <- iris[, 5]
#'
#' grid <- gensvm.grid(x, y)
#' plot(grid, x)
#'
plot.gensvm.grid <- function(grid, x, ...)
{
    if (is.null(grid$best.estimator)) {
        cat("Error: Can't plot, the best.estimator element is NULL\n")
        return
    }
    fit <- grid$best.estimator
    return(plot(fit, x, ...))
}
