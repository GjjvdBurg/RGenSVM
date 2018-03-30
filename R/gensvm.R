#' @title Fit the GenSVM model
#'
#' @description Fits the Generalized Multiclass Support Vector Machine model 
#' with the given parameters. See the package documentation 
#' (\code{\link{gensvm-package}}) for more general information about GenSVM.
#'
#' @param x data matrix with the predictors. \cr\cr
#' Note that for SVMs categorical features should be converted to binary dummy 
#' features. This can be done with using the \code{\link{model.matrix}} 
#' function (i.e. \code{model.matrix( ~ var - 1)}).
#' @param y class labels
#' @param p parameter for the L_p norm of the loss function (1.0 <= p <= 2.0)
#' @param lambda regularization parameter for the loss function (lambda > 0)
#' @param kappa parameter for the hinge function in the loss function (kappa > 
#' -1.0)
#' @param weights type of instance weights to use. Options are 'unit' for unit 
#' weights and 'group' for group size correction weight (eq. 4 in the paper).
#' @param kernel the kernel type to use in the classifier. It must be one of 
#' 'linear', 'poly', 'rbf', or 'sigmoid'. See the section "Kernels in GenSVM" 
#' in \code{\link{gensvm-package}} for more info.
#' @param gamma kernel parameter for the rbf, polynomial, and sigmoid kernel.  
#' If gamma is 'auto', then 1/n_features will be used.
#' @param coef parameter for the polynomial and sigmoid kernel.
#' @param degree parameter for the polynomial kernel
#' @param kernel.eigen.cutoff Cutoff point for the reduced eigendecomposition 
#' used with kernel-GenSVM. Eigenvectors for which the ratio between their 
#' corresponding eigenvalue and the largest eigenvalue is smaller than this 
#' cutoff value will be dropped.
#' @param verbose Turn on verbose output and fit progress
#' @param random.seed Seed for the random number generator (useful for 
#' reproducible output)
#' @param max.iter Maximum number of iterations of the optimization algorithm.
#' @param seed.V Matrix to warm-start the optimization algorithm. This is 
#' typically the output of \code{coef(fit)}. Note that this function will 
#' silently drop seed.V if the dimensions don't match the provided data.
#'
#' @return A "gensvm" S3 object is returned for which the print, predict, coef, 
#' and plot methods are available. It has the following items:
#' \item{call}{The call that was used to construct the model.}
#' \item{p}{The value of the lp norm in the loss function}
#' \item{lambda}{The regularization parameter used in the model.}
#' \item{kappa}{The hinge function parameter used.}
#' \item{epsilon}{The stopping criterion used.}
#' \item{weights}{The instance weights type used.}
#' \item{kernel}{The kernel function used.}
#' \item{gamma}{The value of the gamma parameter of the kernel, if applicable}
#' \item{coef}{The value of the coef parameter of the kernel, if applicable}
#' \item{degree}{The degree of the kernel, if applicable}
#' \item{kernel.eigen.cutoff}{The cutoff value of the reduced 
#' eigendecomposition of the kernel matrix.}
#' \item{verbose}{Whether or not the model was fitted with progress output}
#' \item{random.seed}{The random seed used to seed the model.}
#' \item{max.iter}{Maximum number of iterations of the algorithm.}
#' \item{n.objects}{Number of objects in the dataset}
#' \item{n.features}{Number of features in the dataset}
#' \item{n.classes}{Number of classes in the dataset}
#' \item{classes}{Array with the actual class labels}
#' \item{V}{Coefficient matrix}
#' \item{n.iter}{Number of iterations performed in training}
#' \item{n.support}{Number of support vectors in the final model}
#' \item{training.time}{Total training time}
#'
#' @note
#' This function returns partial results when the computation is interrupted by 
#' the user.
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
#' \code{\link{coef}}, \code{\link{print}}, \code{\link{predict}}, 
#' \code{\link{plot}}, \code{\link{gensvm.grid}}, \code{\link{gensvm-package}}
#'
#' @export
#' @useDynLib gensvm_wrapper, .registration = TRUE
#'
#' @examples
#' x <- iris[, -5]
#' y <- iris[, 5]
#'
#' # fit using the default parameters
#' fit <- gensvm(x, y)
#'
#' # fit and show progress
#' fit <- gensvm(x, y, verbose=T)
#'
#' # fit with some changed parameters
#' fit <- gensvm(x, y, lambda=1e-8)
#'
#' # Early stopping defined through epsilon
#' fit <- gensvm(x, y, epsilon=1e-3)
#'
#' # Early stopping defined through max.iter
#' fit <- gensvm(x, y, max.iter=1000)
#'
#' # Nonlinear training
#' fit <- gensvm(x, y, kernel='rbf')
#' fit <- gensvm(x, y, kernel='poly', degree=2, gamma=1.0)
#'
#' # Setting the random seed and comparing results
#' fit <- gensvm(x, y, random.seed=123)
#' fit2 <- gensvm(x, y, random.seed=123)
#' all.equal(coef(fit), coef(fit2))
#'
#'
gensvm <- function(X, y, p=1.0, lambda=1e-8, kappa=0.0, epsilon=1e-6, 
                   weights='unit', kernel='linear', gamma='auto', coef=1.0, 
                   degree=2.0, kernel.eigen.cutoff=1e-8, verbose=FALSE, 
                   random.seed=NULL, max.iter=1e8, seed.V=NULL)
{
    call <- match.call()

    if (dim(as.matrix(y))[2] > 1) {
        cat("Error: y can not have more than one column\n")
        return(invisible(NULL))
    }

    # Generate the random.seed value in R if it is NULL. This way users can 
    # reproduce the run because it is returned in the output object.
    if (is.null(random.seed))
        random.seed <- runif(1) * (2**31 - 1)

    n.objects <- nrow(X)
    n.features <- ncol(X)
    n.classes <- length(unique(y))

    # Convert labels to integers
    classes <- sort(unique(y))
    y.clean <- match(y, classes)

    # Convert gamma if it is 'auto'
    if (gamma == 'auto')
        gamma <- 1.0/n.features

    if (!gensvm.validate.params(p=p, kappa=kappa, lambda=lambda,
                                epsilon=epsilon, gamma=gamma, weights=weights,
                                kernel=kernel))
        return(invisible(NULL))

    # Convert weights to index
    weight.idx <- which(c("unit", "group") == weights)

    # Convert kernel to index (remember off-by-one for R vs. C)
    kernel.idx <- which(c("linear", "poly", "rbf", "sigmoid") == kernel) - 1

    seed.rows <- if(is.null(seed.V)) -1 else nrow(seed.V)
    seed.cols <- if(is.null(seed.V)) -1 else ncol(seed.V)

    # Call the C train routine
    out <- .Call("R_gensvm_train",
                  as.matrix(X),
                  as.integer(y.clean),
                  p,
                  lambda,
                  kappa,
                  epsilon,
                  weight.idx,
                  as.integer(kernel.idx),
                  gamma,
                  coef,
                  degree,
                  kernel.eigen.cutoff,
                  as.integer(verbose),
                  as.integer(max.iter),
                  as.integer(random.seed),
                  seed.V,
                  as.integer(seed.rows),
                  as.integer(seed.cols),
                  as.integer(n.objects),
                  as.integer(n.features),
                  as.integer(n.classes))

    # build the output object
    object <- list(call = call, p = p, lambda = lambda, kappa = kappa, 
                   epsilon = epsilon, weights = weights, kernel = kernel, 
                   gamma = gamma, coef = coef, degree = degree, 
                   kernel.eigen.cutoff = kernel.eigen.cutoff, 
                   verbose = verbose, random.seed = random.seed, 
                   max.iter = max.iter, n.objects = n.objects, 
                   n.features = n.features, n.classes = n.classes, 
                   classes = classes, V = out$V, n.iter = out$n.iter, 
                   n.support = out$n.support, 
                   training.time = out$training.time,
    class(object) <- "gensvm"

    return(object)
}
