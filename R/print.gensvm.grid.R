#' @title Print the fitted GenSVMGrid model
#'
#' @description Prints the summary of the fitted GenSVMGrid model
#'
#' @param x a \code{gensvm.grid} object to print
#' @param \dots further arguments are ignored
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
#' \code{\link{gensvm.grid}}, \code{\link{predict.gensvm.grid}}, 
#' \code{\link{plot.gensvm.grid}}, \code{\link{gensvm.grid}}, 
#' \code{\link{gensvm-package}}
#'
#' @method print gensvm.grid
#' @export
#'
#' @examples
#' x <- iris[, -5]
#' y <- iris[, 5]
#'
#' # fit a grid search and print the resulting object
#' grid <- gensvm.grid(x, y)
#' print(grid)
#'
print.gensvm.grid <- function(x, ...)
{
    cat("Data:\n")
    cat("\tn.objects:", x$n.objects, "\n")
    cat("\tn.features:", x$n.features, "\n")
    cat("\tn.classes:", x$n.classes, "\n")
    if (is.factor(x$classes))
        cat("\tclasses:", levels(x$classes), "\n")
    else
        cat("\tclasses:", x$classes, "\n")
    cat("Config:\n")
    cat("\tNumber of cv splits:", x$n.splits, "\n")
    not.run <- sum(is.na(x$cv.results$rank.test.score))
    if (not.run > 0) {
        cat("\tParameter grid size:", dim(x$param.grid)[1])
        cat(" (", not.run, " incomplete)", sep="")
        cat("\n")
    } else {
        cat("\tParameter grid size:", dim(x$param.grid)[1], "\n")
    }
    cat("Results:\n")
    cat("\tTotal grid search time:", x$total.time, "\n")
    if (!is.na(x$best.index)) {
        best <- x$cv.results[x$best.index, ]
        cat("\tBest mean test score:", best$mean.test.score, "\n")
        cat("\tBest mean fit time:", best$mean.fit.time, "\n")
        for (name in colnames(x$best.params)) {
            val <- x$best.params[[name]]
            val <- if(is.factor(val)) levels(val)[val] else val
            cat("\tBest parameter", name, "=", val, "\n")
        }
    }

    invisible(x)
}
