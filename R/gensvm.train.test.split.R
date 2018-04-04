#' @title Create a train/test split of a dataset
#'
#' @description Often it is desirable to split a dataset into a training and 
#' testing sample. This function is included in GenSVM to make it easy to do 
#' so. The function is inspired by a similar function in Scikit-Learn.
#'
#' @param x array to split
#' @param y another array to split (typically this is a vector)
#' @param train.size size of the training dataset. This can be provided as 
#' float or as int. If it's a float, it should be between 0.0 and 1.0 and 
#' represents the fraction of the dataset that should be placed in the training 
#' dataset.  If it's an int, it represents the exact number of samples in the 
#' training dataset. If it is NULL, the complement of \code{test.size} will be 
#' used.
#' @param test.size size of the test dataset. Similarly to train.size both a 
#' float or an int can be supplied. If it's NULL, the complement of train.size 
#' will be used. If both train.size and test.size are NULL, a default test.size 
#' of 0.25 will be used.
#' @param shuffle shuffle the rows or not
#' @param random.state seed for the random number generator (int)
#' @param return.idx whether or not to return the indices in the output
#'
#' @return a list with \code{x.train} and \code{x.test} splits of the \code{x} 
#' array provided. If \code{y} is provided, also \code{y.train} and 
#' \code{y.test}. If \code{return.idx} is TRUE, also \code{idx.train} and 
#' \code{idx.test}.
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
#' \code{\link{gensvm}}, \code{\link{gensvm-package}}
#'
#' @export
#'
#' @examples
#' x <- iris[, -5]
#' y <- iris[, 5]
#'
#' # using the default values
#' split <- gensvm.train.test.split(x, y)
#'
#' # using the split in a GenSVM model
#' fit <- gensvm(split$x.train, split$y.train)
#' gensvm.accuracy(split$y.test, predict(fit, split$x.test))
#'
#' # using attach makes the results directly available
#' attach(gensvm.train.test.split(x, y))
#' fit <- gensvm(x.train, y.train)
#' gensvm.accuracy(y.test, predict(fit, x.test))
#'
gensvm.train.test.split <- function(x, y=NULL, train.size=NULL, test.size=NULL, 
                                    shuffle=TRUE, random.state=NULL, 
                                    return.idx=FALSE)
{
    if (!is.null(y) && dim(as.matrix(x))[1] != dim(as.matrix(y))[1]) {
        cat("Error: First dimension of x and y should be equal.\n")
        return
    }

    n.objects <- dim(as.matrix(x))[1]

    if (is.null(train.size) && is.null(test.size)) {
        test.size <- round(0.25 * n.objects)
        train.size <- n.objects - test.size
    }
    else if (is.null(train.size)) {
        if (test.size > 0.0 && test.size < 1.0)
            test.size <- round(n.objects * test.size)
        train.size <- n.objects - test.size
    }
    else if (is.null(test.size)) {
        if (train.size > 0.0 && train.size < 1.0)
            train.size <- round(n.objects * train.size)
        test.size <- n.objects - train.size
    }
    else {
        if (train.size > 0.0 && train.size < 1.0)
            train.size <- round(n.objects * train.size)
        if (test.size > 0.0 && test.size < 1.0)
            test.size <- round(n.objects * test.size)
    }

    if (!is.null(random.state))
        set.seed(random.state)

    if (shuffle) {
        train.idx <- sample(n.objects, train.size)
        diff <- setdiff(1:n.objects, train.idx)
        test.idx <- sample(diff, test.size)
    } else {
        train.idx <- 1:train.size
        diff <- setdiff(1:n.objects, train.idx)
        test.idx <- diff[1:test.size]
    }

    x.train <- x[train.idx, ]
    x.test <- x[test.idx, ]

    if (!is.null(y)) {
        if (is.matrix(y)) {
            y.train <- y[train.idx, ]
            y.test <- y[test.idx, ]
        } else {
            y.train <- y[train.idx]
            y.test <- y[test.idx]
        }
    }

    out <- list(
                x.train = x.train,
                x.test = x.test
                )
    if (!is.null(y)) {
        out$y.train <- y.train
        out$y.test <- y.test
    }
    if (return.idx) {
        out$idx.train <- train.idx
        out$idx.test <- test.idx
    }

    return(out)
}
