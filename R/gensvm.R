#' @title Fit the GenSVM model
#'
#' @description Fits the Generalized Multiclass Support Vector Machine model 
#' with the given parameters.
#'
#' @param X data matrix with the predictors
#' @param y class labels
#' @param p parameter for the L_p norm of the loss function (1.0 <= p <= 2.0)
#' @param lambda regularization parameter for the loss function (lambda > 0)
#' @param kappa parameter for the hinge function in the loss function (kappa > 
#' -1.0)
#' @param weights type of instance weights to use. Options are 'unit' for unit 
#' weights and 'group' for group size correction weight (eq. 4 in the paper).
#' @param kernel the kernel type to use in the classifier. It must be one of 
#' 'linear', 'poly', 'rbf', or 'sigmoid'.
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
#'
#' @return A "gensvm" S3 object is returned for which the print, predict, coef, 
#' and plot methods are available. It has the following items:
#' \item{call}{The call that was used to construct the model.}
#' \item{lambda}{The regularization parameter used in the model.}
#' \item{kappa}{The hinge function parameter used.}
#' \item{epsilon}{The stopping criterion used.}
#' \item{weights}{The instance weights type used.}
#' \item{kernel}{The kernel function used.}
#' \item{gamma}{The value of the gamma parameter of the kernel, if applicable}.
#' \item{coef}{The value of the coef parameter of the kernel, if applicable}
#' \item{degree}{The degree of the kernel, if applicable}
#' \item{kernel.eigen.cutoff}{The cutoff value of the reduced 
#' eigendecomposition of the kernel matrix}
#' \item{random.seed}{The random seed used to seed the model.}
#' \item{max.iter}{Maximum number of iterations of the algorithm.}
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
#' @export
#' @useDynLib gensvm_wrapper, .registration = TRUE
#'
#' @examples
#'
gensvm <- function(X, y, p=1.0, lambda=1e-5, kappa=0.0, epsilon=1e-6, 
                   weights='unit', kernel='linear', gamma='auto', coef=0.0, 
                   degree=2.0, kernel.eigen.cutoff=1e-8, verbose=0, 
                   random.seed=NULL, max.iter=1e8, seed.V=NULL)
{
    call <- match.call()

    # Generate the random.seed value in R if it is NULL. This way users can 
    # reproduce the run because it is returned in the output object.
    if (is.null(random.seed))
        random.seed <- runif(1) * (2**31 - 1)

    # TODO: Store a labelencoder in the object, preferably as a partially 
    # hidden item. This can then be used with prediction.

    n.objects <- nrow(X)
    n.features <- ncol(X)
    n.classes <- length(unique(y))

    # Convert labels to integers
    classes <- sort(unique(y))
    y.clean <- match(y, classes)

    # Convert gamma if it is 'auto'
    if (gamma == 'auto')
        gamma <- 1.0/n.features

    # Convert weights to index
    weight.idx <- which(c("unit", "group") == weights)
    if (length(weight.idx) == 0) {
        stop("Incorrect weight specification. ",
             "Valid options are 'unit' and 'group'")
    }

    # Convert kernel to index (remember off-by-one for R vs. C)
    kernel.idx <- which(c("linear", "poly", "rbf", "sigmoid") == kernel) - 1
    if (length(kernel.idx) == 0) {
        stop("Incorrect kernel specification. ",
             "Valid options are 'linear', 'poly', 'rbf', and 'sigmoid'")
    }

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
                  as.integer(n.objects),
                  as.integer(n.features),
                  as.integer(n.classes))

    object <- list(call = call, p = p, lambda = lambda, kappa = kappa, 
                   epsilon = epsilon, weights = weights, kernel = kernel, 
                   gamma = gamma, coef = coef, degree = degree, 
                   kernel.eigen.cutoff = kernel.eigen.cutoff, 
                   random.seed = random.seed, max.iter = max.iter, 
                   n.objects = n.objects, n.features = n.features, 
                   n.classes = n.classes, classes = classes, V = out$V, 
                   n.iter = out$n.iter, n.support = out$n.support)
    class(object) <- "gensvm"

    return(object)
}
