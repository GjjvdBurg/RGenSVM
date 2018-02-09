#' @title Cross-validated grid search for GenSVM
#'
#' @description This function performs a cross-validated grid search of the 
#' model parameters to find the best hyperparameter configuration for a given 
#' dataset. This function takes advantage of GenSVM's ability to use warm 
#' starts to speed up computation. The function also uses the GenSVM C library 
#' for speed. 
#'
#' There are two ways to use this function: either by providing a data frame 
#' with the parameter configurations to try or by giving each of the function 
#' inputs a vector of values to evaluate. In the latter case all combinations 
#' of the provided values will be used (i.e. the product set).
#'
#' @param X training data matrix. We denote the size of this matrix by 
#' n_samples x n_features.
#' @param y training vector of class labes of length n_samples. The number of 
#' unique labels in this vector is denoted by n_classes.
#' @param df Data frame with parameter configurations to evaluate.  
#' If this is provided it overrides the other parameter ranges provided. The 
#' data frame must provide *all* required columns, as described below.
#' @param p vector of values to try for the \eqn{p} hyperparameter 
#' for the \eqn{\ell_p} norm in the loss function. All values should be on the 
#' interval [1.0, 2.0].
#' @param lambda vector of values for the regularization parameter 
#' \eqn{\lambda} in the loss function. All values should be larger than 0.
#' @param kappa vector of values for the hinge function parameter in 
#' the loss function. All values should be larger than -1.
#' @param weights vector of values for the instance weights. Values 
#' should be either 'unit', 'group', or both.
#' @param kernel vector of values for the kernel type. Possible 
#' values are: 'linear', 'rbf', 'poly', or 'sigmoid', or any combination of 
#' these values. See the article \link[=gensvm-kernels]{Kernels in GenSVM} for 
#' more information.
#' @param gamma kernel parameter for the 'rbf', 'poly', and 'sigmoid' kernels.  
#' If it is 'auto', 1/n_features will be used. See the article 
#' \link[=gensvm-kernels]{Kernels in GenSVM} for more information.
#' @param coef kernel parameter for the 'poly' and 'sigmoid' 
#' kernels. See the article \link[=gensvm-kernels]{Kernels in GenSVM} for more 
#' information.
#' @param degree kernel parameter for the 'poly' kernel. See the 
#' article \link[=gensvm-kernels]{Kernels in GenSVM} for more information.
#' @param max.iter maximum number of iterations to run in the 
#' optimization algorithm.
#' @param refit boolean variable. If true, the best model from cross validation 
#' is fitted again on the entire dataset.
#' @param scoring metric to use to evaluate the classifier performance during 
#' cross validation. The metric should be an R function that takes two 
#' arguments: y_true and y_pred and that returns a float such that higher 
#' values are better. If it is NULL, the accuracy score will be used.
#' @param cv the number of cross-validation folds to use or a vector with the 
#' same length as \code{y} where each unique value denotes a test split.
#' @param verbose boolean variable to indicate whether training details should 
#' be printed.
#'
#' @return A "gensvm.grid" S3 object with the following items:
#' \item{cv.results}{A data frame with the cross validation results}
#' \item{best.estimator}{If refit=TRUE, this is the GenSVM model fitted with 
#' the best hyperparameter configuration, otherwise it is NULL}
#' \item{best.score}{Mean cross-validated score for the model with the best 
#' hyperparameter configuration}
#' \item{best.params}{Parameter configuration that provided the highest mean 
#' cross-validated score}
#' \item{best.index}{Row index of the cv.results data frame that corresponds to 
#' the best hyperparameter configuration}
#' \item{n.splits}{The number of cross-validation splits}
#'
#'
#'
#' @section Using a DataFrame:
#' ...
#'
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
#' @seealso
#' \code{\link{coef}}, \code{\link{print}}, \code{\link{predict}}, 
#' \code{\link{plot}}, and \code{\link{gensvm.grid}}.
#'
#'
#' @export
#'
#' @examples
#' X <- 
#'

gensvm.grid <- function(X, y,
                        df=NULL,
                        p=c(1.0, 1.5, 2.0), 
                        lambda=c(1e-8, 1e-6, 1e-4, 1e-2, 1),
                        kappa=c(-0.9, 0.5, 5.0),
                        weights=c('unit', 'group'),
                        kernel=c('linear'),
                        gamma=c('auto'),
                        coef=c(0.0),
                        degree=c(2.0),
                        max.iter=c(1e8),
                        refit=TRUE,
                        scoring=NULL,
                        cv=3,
                        verbose=TRUE)
{
    call <- match.call()



    object <- list(...)
    class(object) <- "gensvm.grid"
    return(object)
}
