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

    # TODO: fill this out
    #
    #
    invisible(object)
}
