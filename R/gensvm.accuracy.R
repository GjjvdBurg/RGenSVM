#' @title Compute the accuracy score
#'
#' @param y.true vector of true labels
#' @param y.pred vector of predicted labels
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
#' \code{\link{predict.gensvm.grid}}, \code{\link{predict.gensvm}}, 
#' \code{\link{gensvm-package}}
#'
#' @export
#'
#' @examples
#' x <- iris[, -5]
#' y <- iris[, 5]
#'
#' fit <- gensvm(x, y)
#' gensvm.accuracy(predict(fit, x), y)
#'
gensvm.accuracy <- function(y.true, y.pred)
{
    n <- length(y.true)
    if (n != length(y.pred)) {
        cat("Error: Can't compute accuracy if vector don't have the ",
            "same length\n")
        return
    }

    return (sum(y.true == y.pred) / n)
}
