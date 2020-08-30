#' @title Predict class labels from the GenSVMGrid class
#'
#' @description Predict class labels using the best model from a grid search.  
#' After doing a grid search with the \code{\link{gensvm.grid}} function, this 
#' function can be used to make predictions of class labels. It uses the best 
#' GenSVM model found during the grid search to do the predictions. Note that 
#' this model is only available if \code{refit=TRUE} was specified in the 
#' \code{\link{gensvm.grid}} call (the default).
#'
#' @param object A \code{gensvm.grid} object trained with \code{refit=TRUE}
#' @param newdata Matrix of new values for \code{x} for which predictions need 
#' to be computed.
#' @param \dots further arguments are passed to predict.gensvm()
#'
#' @return a vector of class labels, with the same type as the original class 
#' labels provided to gensvm.grid()
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
#' \code{\link{gensvm}}, \code{\link{predict.gensvm.grid}}, 
#' \code{\link{plot.gensvm}}, \code{\link{gensvm-package}}
#'
#' @method predict gensvm.grid
#'
#' @export
#'
#' @importFrom stats predict
#'
#' @examples
#' \donttest{
#' x <- iris[, -5]
#' y <- iris[, 5]
#'
#' # run a grid search
#' grid <- gensvm.grid(x, y)
#'
#' # predict training sample
#' y.hat <- predict(grid, x)
#' }
#'
predict.gensvm.grid <- function(object, newdata, ...)
{
    if (is.null(object$best.estimator)) {
        stop("Error: Can't predict, the best.estimator element is NULL\n")
    }

    return(predict(object$best.estimator, newdata, ...))
}
