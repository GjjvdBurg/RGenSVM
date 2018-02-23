#' @title Predict class labels with the GenSVM model
#'
#' @description This function predicts the class labels of new data using a 
#' fitted GenSVM model.
#'
#' @param fit Fitted \code{gensvm} object
#' @param newx Matrix of new values for \code{x} for which predictions need to 
#' be made.
#' @param \dots further arguments are ignored
#'
#' @return a vector of class labels, with the same type as the original class 
#' labels.
#'
#' @export
#' @aliases predict
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
#' @examples
#'
#'
#'
predict.gensvm <- function(fit, newx, ...)
{
    ## Implementation note:
    ## - It might seem that it would be faster to do the prediction directly in 
    ## R here, since we then don't need to switch to C, construct model and 
    ## data structures, copy the data, etc. before doing the prediction.  
    ## However, if you actually implement it and compare, we find that calling 
    ## the C implementation is *much* faster than doing it in R.

    # Sanity check
    if (ncol(newx) != fit$n.features)
        stop("Number of features of fitted model and supplied data disagree.")

    y.pred.c <- .Call("R_gensvm_predict",
                      as.matrix(newx),
                      as.matrix(fit$V),
                      as.integer(nrow(newx)),
                      as.integer(ncol(newx)),
                      as.integer(fit$n.classes)
                      )
    yhat <- fit$classes[y.pred.c]

    return(yhat)
}
