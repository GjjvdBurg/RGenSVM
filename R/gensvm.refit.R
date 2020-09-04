#' @title Train an already fitted model on new data
#'
#' @description This function can be used to train an existing model on new 
#' data or fit an existing model with slightly different parameters. It is 
#' useful for retraining without having to copy all the parameters over. One 
#' common application for this is to refit the best model found by a grid 
#' search, as illustrated in the examples.
#'
#' @param fit Fitted \code{gensvm} object
#' @param x Data matrix of the new data
#' @param y Label vector of the new data
#' @param p if NULL use the value from \code{fit} in the new model, otherwise 
#' override with this value.
#' @param lambda if NULL use the value from \code{fit} in the new model, 
#' otherwise override with this value.
#' @param kappa if NULL use the value from \code{fit} in the new model, 
#' otherwise override with this value.
#' @param epsilon if NULL use the value from \code{fit} in the new model, 
#' otherwise override with this value.
#' @param weights if NULL use the value from \code{fit} in the new model, 
#' otherwise override with this value.
#' @param kernel if NULL use the value from \code{fit} in the new model, 
#' otherwise override with this value.
#' @param gamma if NULL use the value from \code{fit} in the new model, 
#' otherwise override with this value.
#' @param coef if NULL use the value from \code{fit} in the new model, 
#' otherwise override with this value.
#' @param degree if NULL use the value from \code{fit} in the new model, 
#' otherwise override with this value.
#' @param kernel.eigen.cutoff if NULL use the value from \code{fit} in the new 
#' model, otherwise override with this value.
#' @param max.iter if NULL use the value from \code{fit} in the new model, 
#' otherwise override with this value.
#' @param verbose if NULL use the value from \code{fit} in the new model, 
#' otherwise override with this value.
#' @param random.seed if NULL use the value from \code{fit} in the new model, 
#' otherwise override with this value.
#'
#' @return a new fitted \code{gensvm} model
#'
#' @author
#' Gerrit J.J. van den Burg, Patrick J.F. Groenen \cr
#' Maintainer: Gerrit J.J. van den Burg <gertjanvandenburg@gmail.com>
#'
#' @references
#' Van den Burg, G.J.J. and Groenen, P.J.F. (2016). \emph{GenSVM: A Generalized 
#' Multiclass Support Vector Machine}, Journal of Machine Learning Research, 
#' 17(225):1--42. URL \url{https://jmlr.org/papers/v17/14-526.html}.
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
#' # fit a standard model and refit with slightly different parameters
#' fit <- gensvm(x, y)
#' fit2 <- gensvm.refit(fit, x, y, epsilon=1e-8)
#'
#' \donttest{
#' # refit a model returned by a grid search
#' grid <- gensvm.grid(x, y)
#' fit <- gensvm.refit(fit, x, y, epsilon=1e-8)
#' }
#'
#' # refit on different data
#' idx <- runif(nrow(x)) > 0.5
#' x1 <- x[idx, ]
#' x2 <- x[!idx, ]
#' y1 <- y[idx]
#' y2 <- y[!idx]
#'
#' fit1 <- gensvm(x1, y1)
#' fit2 <- gensvm.refit(fit1, x2, y2)
#'
gensvm.refit <- function(fit, x, y, p=NULL, lambda=NULL, kappa=NULL, 
                         epsilon=NULL, weights=NULL, kernel=NULL, gamma=NULL, 
                         coef=NULL, degree=NULL, kernel.eigen.cutoff=NULL, 
                         max.iter=NULL, verbose=NULL, random.seed=NULL)
{
    p <- if(is.null(p)) fit$p else p
    lambda <- if(is.null(lambda)) fit$lambda else lambda
    kappa <- if(is.null(kappa)) fit$kappa else kappa
    epsilon <- if(is.null(epsilon)) fit$epsilon else epsilon
    weights <- if(is.null(weights)) fit$weights else weights
    kernel <- if(is.null(kernel)) fit$kernel else kernel
    gamma <- if(is.null(gamma)) fit$gamma else gamma
    coef <- if(is.null(coef)) fit$coef else coef
    degree <- if(is.null(degree)) fit$degree else degree
    kernel.eigen.cutoff <- (if(is.null(kernel.eigen.cutoff)) 
                            fit$kernel.eigen.cutoff else kernel.eigen.cutoff)
    max.iter <- if(is.null(max.iter)) fit$max.iter else max.iter
    verbose <- if(is.null(verbose)) fit$verbose else verbose
    random.seed <- if(is.null(random.seed)) fit$random.seed else random.seed

    # Setting the error handler here is necessary in case the user interrupts 
    # this call to gensvm. If we don't set the error handler, R will 
    # unnecessarily drop to a browser() session. We reset the error handler 
    # after the call to gensvm().
    errfunc <- getOption('error')
    options(error=function() {})
    newfit <- gensvm(x, y, p=p, lambda=lambda, kappa=kappa, epsilon=epsilon, 
                     weights=weights, kernel=kernel, gamma=gamma, coef=coef,
                     degree=degree, kernel.eigen.cutoff=kernel.eigen.cutoff,
                     verbose=verbose, max.iter=max.iter, seed.V=coef(fit))
    options(error=errfunc)

    return(newfit)
}
