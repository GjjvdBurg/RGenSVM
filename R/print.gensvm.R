#' @title Print the fitted GenSVM model
#'
#' @description Prints a short description of the fitted GenSVM model
#'
#' @param fit A \code{gensvm} object to print
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
print.gensvm <- function(fit, ...)
{
    cat("Data:\n")
    cat("\tn.objects:", fit$n.objects, "\n")
    cat("\tn.features:", fit$n.features, "\n")
    cat("\tn.classes:", fit$n.classes, "\n")
    if (is.factor(fit$classes))
        cat("\tclasses:", levels(fit$classes), "\n")
    else
        cat("\tclasses:", fit$classes, "\n")
    cat("Parameters:\n")
    cat("\tp:", fit$p, "\n")
    cat("\tlambda:", fit$lambda, "\n")
    cat("\tkappa:", fit$kappa, "\n")
    cat("\tepsilon:", fit$epsilon, "\n")
    cat("\tweights:", fit$weights, "\n")
    cat("\tmax.iter:", fit$max.iter, "\n")
    cat("\trandom.seed:", fit$random.seed, "\n")
    if (is.factor(fit$kernel)) {
        cat("\tkernel:", levels(fit$kernel)[as.numeric(fit$kernel)], "\n")
    } else {
        cat("\tkernel:", fit$kernel, "\n")
    }
    if (fit$kernel %in% c("poly", "rbf", "sigmoid")) {
        cat("\tkernel.eigen.cutoff:", fit$kernel.eigen.cutoff, "\n")
        cat("\tgamma:", fit$gamma, "\n")
    }
    if (fit$kernel %in% c("poly", "sigmoid"))
        cat("\tcoef:", fit$coef, "\n")
    if (fit$kernel == 'poly')
        cat("\tdegree:", fit$degree, "\n")
    cat("Results:\n")
    cat("\ttime:", fit$training.time, "\n")
    cat("\tn.iter:", fit$n.iter, "\n")
    cat("\tn.support:", fit$n.support, "\n")

    invisible(fit)
}
