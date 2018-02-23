#' @title Print the fitted GenSVM model
#'
#' @description Prints a short description of the fitted GenSVM model
#'
#' @param object A \code{gensvm} object to print
#' @param \dots further arguments are ignored
#'
#' @return returns the object passed as input
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
#' @method print gensvm
#' @export
#'
#' @examples
#'
#'
print.gensvm <- function(object, ...)
{
    cat("\nCall:\n")
    dput(object$call)
    cat("\nData:\n")
    cat("\tn.objects:", object$n.objects, "\n")
    cat("\tn.features:", object$n.features, "\n")
    cat("\tn.classes:", object$n.classes, "\n")
    cat("\tclasses:", object$classes, "\n")
    cat("Parameters:\n")
    cat("\tp:", object$p, "\n")
    cat("\tlambda:", object$lambda, "\n")
    cat("\tkappa:", object$kappa, "\n")
    cat("\tepsilon:", object$epsilon, "\n")
    cat("\tweights:", object$weights, "\n")
    cat("\tmax.iter:", object$max.iter, "\n")
    cat("\trandom.seed:", object$random.seed, "\n")
    cat("\tkernel:", object$kernel, "\n")
    if (object$kernel %in% c("poly", "rbf", "sigmoid")) {
        cat("\tkernel.eigen.cutoff:", object$kernel.eigen.cutoff, "\n")
        cat("\tgamma:", object$gamma, "\n")
    }
    if (object$kernel %in% c("poly", "sigmoid"))
        cat("\tcoef:", object$coef, "\n")
    if (object$kernel == 'poly')
        cat("\tdegree:", object$degree, "\n")
    cat("Results:\n")
    cat("\tn.iter:", object$n.iter, "\n")
    cat("\tn.support:", object$n.support, "\n")

    invisible(object)
}
