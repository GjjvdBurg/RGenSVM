#' GenSVM: A Generalized Multiclass Support Vector Machine
#'
#' The GenSVM classifier is a generalized multiclass support vector machine 
#' (SVM). This classifier simultaneously aims to find decision boundaries that 
#' separate the classes with as wide a margin as possible. In GenSVM, the loss 
#' functions that measures how misclassifications are counted is very flexible.  
#' This allows the user to tune the classifier to the dataset at hand and 
#' potentially obtain higher classification accuracy. Moreover, this 
#' flexibility means that GenSVM has a number of alternative multiclass SVMs as 
#' special cases. One of the other advantages of GenSVM is that it is trained 
#' in the primal, allowing the use of warm starts during optimization. This 
#' means that for common tasks such as cross validation or repeated model 
#' fitting, GenSVM can be trained very quickly.
#'
#' This package provides functions for training the GenSVM model either as a 
#' separate model or through a cross-validated parameter grid search. In both 
#' cases the GenSVM C library is used for speed. Auxiliary functions for 
#' evaluating and using the model are also provided.
#'
#' @section GenSVM functions:
#' The main GenSVM functions are:
#' \describe{
#' \item{\code{\link{gensvm}}}{Fit a GenSVM model for specific model 
#' parameters.}
#' \item{\code{\link{gensvm.grid}}}{Run a cross-validated grid search for 
#' GenSVM.}
#' }
#'
#' Other available functions are:
#' \describe{
#' \item{\code{\link{plot}}}{Plot the low-dimensional \emph{simplex} space 
#' where the decision boundaries are fixed.}
#' \item{\code{\link{predict}}}{Predict the class labels of new data using the 
#' GenSVM model.}
#' \item{\code{\link{coef}}}{Get the coefficients of the GenSVM model}
#' \item{\code{\link{print}}}{Print a short description of the fitted GenSVM 
#' model}
#' }
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
#' @name gensvm-package
#' @docType package
#' @import
NULL
#>NULL
