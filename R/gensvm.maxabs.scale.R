#' @title Scale each column of a matrix by its maximum absolute value
#'
#' @description Scaling a dataset can creatly decrease the computation time of 
#' GenSVM. This function scales the data by dividing each column of a matrix by 
#' the maximum absolute value of that column. This preserves sparsity in the 
#' data while mapping each column to the interval [-1, 1].
#'
#' Optionally a test dataset can be provided as well. In this case, the scaling 
#' will be computed on the first argument (\code{x}) and applied to the test 
#' dataset. Note that the return value is a list when this argument is 
#' supplied.
#'
#' @param x a matrix to scale
#' @param x.test (optional) a test matrix to scale as well.
#'
#' @return if x.test=NULL a scaled matrix where the maximum value of the 
#' columns is 1 and the minimum value of the columns isn't below -1. If x.test 
#' is supplied, a list with elements \code{x} and \code{x.test} representing 
#' the scaled datasets.
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
#' \code{\link{gensvm}}, \code{\link{gensvm.grid}}, 
#' \code{\link{gensvm.train.test.split}}, \code{\link{gensvm-package}}
#'
#' @export
#'
#' @examples
#' x <- iris[, -5]
#'
#' # check the min and max of the columns
#' apply(x, 2, min)
#' apply(x, 2, max)
#'
#' # scale the data
#' x.scale <- gensvm.maxabs.scale(x)
#'
#' # check again (max should be 1.0, min shouldn't be below -1)
#' apply(x.scale, 2, min)
#' apply(x.scale, 2, max)
#'
#' # with a train and test dataset
#' split <- gensvm.train.test.split(x)
#' x.train <- split$x.train
#' x.test <- split$x.test
#' scaled <- gensvm.maxabs.scale(x.train, x.test)
#' x.train.scl <- scaled$x
#' x.test.scl <- scaled$x.test
#'
gensvm.maxabs.scale <- function(x, x.test=NULL)
{
    xm <- as.matrix(x)
    max.abs <- apply(apply(xm, 2, abs), 2, max)
    max.abs[max.abs == 0] <- 1

    scaled <- xm %*% diag(1.0 / max.abs)
    colnames(scaled) <- colnames(x)
    rownames(scaled) <- rownames(x)

    if (!is.null(x.test)) {
        xtm <- as.matrix(x.test)
        scaled.test <- xtm %*% diag(1.0 / max.abs)
        colnames(scaled.test) <- colnames(x.test)
        rownames(scaled.test) <- rownames(x.test)

        ret.val <- list(x=scaled, x.test=scaled.test)
    } else {
        ret.val <- scaled
    }

    return(ret.val)
}
