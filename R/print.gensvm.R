#' @title Print the fitted GenSVM model
#'
#' @description Prints a short description of the fitted GenSVM model
#'
#' @param x A \code{gensvm} object to print
#' @param \dots further arguments are ignored
#'
#' @return returns the object passed as input. This can be useful for chaining 
#' operations on a fit object.
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
#' \code{\link{gensvm}}, \code{\link{predict.gensvm}}, 
#' \code{\link{plot.gensvm}}, \code{\link{gensvm-package}}
#'
#' @method print gensvm
#' @export
#'
#' @examples
#' x <- iris[, -5]
#' y <- iris[, 5]
#'
#' # fit and print the model
#' fit <- gensvm(x, y)
#' print(fit)
#'
#' # (advanced) use the fact that print returns the fitted model
#' fit <- gensvm(x, y)
#' predict(print(fit), x)
#'
print.gensvm <- function(x, ...)
{
    cat("Data:\n")
    cat("\tn.objects:", x$n.objects, "\n")
    cat("\tn.features:", x$n.features, "\n")
    cat("\tn.classes:", x$n.classes, "\n")
    if (is.factor(x$classes))
        cat("\tclasses:", levels(x$classes), "\n")
    else
        cat("\tclasses:", x$classes, "\n")
    cat("Parameters:\n")
    cat("\tp:", x$p, "\n")
    cat("\tlambda:", x$lambda, "\n")
    cat("\tkappa:", x$kappa, "\n")
    cat("\tepsilon:", x$epsilon, "\n")
    cat("\tweights:", x$weights, "\n")
    cat("\tmax.iter:", x$max.iter, "\n")
    cat("\trandom.seed:", x$random.seed, "\n")
    if (is.factor(x$kernel)) {
        cat("\tkernel:", levels(x$kernel)[as.numeric(x$kernel)], "\n")
    } else {
        cat("\tkernel:", x$kernel, "\n")
    }
    if (x$kernel %in% c("poly", "rbf", "sigmoid")) {
        cat("\tkernel.eigen.cutoff:", x$kernel.eigen.cutoff, "\n")
        cat("\tgamma:", x$gamma, "\n")
    }
    if (x$kernel %in% c("poly", "sigmoid"))
        cat("\tcoef:", x$coef, "\n")
    if (x$kernel == 'poly')
        cat("\tdegree:", x$degree, "\n")
    cat("Results:\n")
    cat("\ttime:", x$training.time, "\n")
    cat("\tn.iter:", x$n.iter, "\n")
    cat("\tn.support:", x$n.support, "\n")

    invisible(x)
}
