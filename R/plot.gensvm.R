#' @title Plot the simplex space of the fitted GenSVM model
#' 
#' @description This function creates a plot of the simplex space for a fitted 
#' GenSVM model and the given data set, as long as the dataset consists of only 
#' 3 classes.  For more than 3 classes, the simplex space is too high 
#' dimensional to easily visualize.
#'
#' @param fit A fitted \code{gensvm} object
#' @param x the dataset to plot
#' @param y.true the true data labels. If provided the objects will be colored 
#' using the true labels instead of the predicted labels. This makes it easy to 
#' identify misclassified objects.
#' @param with.margins plot the margins
#' @param with.shading show shaded areas for the class regions
#' @param with.legend show the legend for the class labels
#' @param center.plot ensure that the boundaries and margins are always visible 
#' in the plot
#' @param xlim allows the user to force certain plot limits. If set, these 
#' bounds will be used for the horizontal axis.
#' @param ylim allows the user to force certain plot limits. If set, these 
#' bounds will be used for the vertical axis and the value of center.plot will 
#' be ignored
#' @param ... further arguments are passed to the builtin plot() function
#'
#' @return returns the object passed as input
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
#' \code{\link{plot.gensvm.grid}}, \code{\link{predict.gensvm}}, 
#' \code{\link{gensvm}}, \code{\link{gensvm-package}}
#'
#' @method plot gensvm
#' @export
#'
#' @examples
#' x <- iris[, -5]
#' y <- iris[, 5]
#'
#' # train the model
#' fit <- gensvm(x, y)
#'
#' # plot the simplex space
#' plot(fit, x)
#'
#' # plot and use the true colors (easier to spot misclassified samples)
#' plot(fit, x, y.true=y)
#'
#' # plot only misclassified samples
#' x.mis <- x[predict(fit, x) != y, ]
#' y.mis.true <- y[predict(fit, x) != y]
#' plot(fit, x.mis)
#' plot(fit, x.mis, y.true=y.mis.true)
#'
plot.gensvm <- function(fit, x, y.true=NULL, with.margins=TRUE, 
                        with.shading=TRUE, with.legend=TRUE, center.plot=TRUE,
                        xlim=NULL, ylim=NULL, ...)
{
    if (!(fit$n.classes %in% c(2,3))) {
        cat("Error: Can only plot with 2 or 3 classes\n")
        return(invisible(NULL))
    }

    # Sanity check
    if (ncol(x) != fit$n.features) {
        cat("Error: Number of features of fitted model and testing data 
            disagree.\n")
        invisible(NULL)
    }

    x.train <- fit$X.train
    if (fit$kernel != 'linear' && is.null(x.train)) {
        cat("Error: The training data is needed to plot data for ",
             "nonlinear GenSVM. This data is not present in the fitted ",
             "model!\n", sep="")
        invisible(NULL)
    }
    if (!is.null(x.train) && ncol(x.train) != fit$n.features) {
        cat("Error: Number of features of fitted model and training data ",
            "disagree.\n", sep="")
        invisible(NULL)
    }

    x <- as.matrix(x)

    if (fit$kernel == 'linear') {
        V <- coef(fit)
        Z <- cbind(matrix(1, dim(x)[1], 1), x)
        S <- Z %*% V
        y.pred.orig <- predict(fit, x)
    } else {
        kernels <- c("linear", "poly", "rbf", "sigmoid")
        kernel.idx <- which(kernels == fit$kernel) - 1
        plotdata <- .Call("R_gensvm_plotdata_kernels",
                          as.matrix(x),
                          as.matrix(x.train),
                          as.matrix(fit$V),
                          as.integer(nrow(fit$V)),
                          as.integer(ncol(fit$V)),
                          as.integer(nrow(x.train)),
                          as.integer(nrow(x)),
                          as.integer(fit$n.features),
                          as.integer(fit$n.classes),
                          as.integer(kernel.idx),
                          fit$gamma,
                          fit$coef,
                          fit$degree,
                          fit$kernel.eigen.cutoff
                          )
        S <- plotdata$ZV
        y.pred.orig <- plotdata$y.pred
    }

    classes <- fit$classes
    if (is.factor(y.pred.orig)) {
        y.pred <- match(y.pred.orig, classes)
    } else {
        y.pred <- y.pred.orig
    }

    labels <- if(is.null(y.true)) y.pred else y.true
    classes <- unique(labels)

    colors <- gensvm.plot.colors(length(classes))
    markers <- gensvm.plot.markers(length(classes))

    indices <- match(labels, classes)

    col.vector <- colors[indices]
    mark.vector <- markers[indices]

    if (fit$n.classes == 2)
        S <- cbind(S, matrix(0, nrow(S), 1))

    par(pty="s")
    if (center.plot) {
        if (is.null(xlim))
            xlim <- c(min(min(S[, 1]), -1.2), max(max(S[, 1]), 1.2))
        if (is.null(ylim))
            ylim <- c(min(min(S[, 2]), -0.75), max(max(S[, 2]), 1.2))
        plot(S[, 1], S[, 2], col=col.vector, pch=mark.vector, ylab='', xlab='', 
             asp=1, xlim=xlim, ylim=ylim, ...)
    } else {
        plot(S[, 1], S[, 2], col=col.vector, pch=mark.vector, ylab='', 
             xlab='', asp=1, xlim=xlim, ylim=ylim, ...)
    }

    if (fit$n.classes == 3)
        gensvm.plot.2d(classes, with.margins, with.shading, with.legend, 
                       center.plot)
    else
        gensvm.plot.1d(classes, with.margins, with.shading, with.legend, 
                       center.plot)

    invisible(fit)
}


gensvm.plot.2d <- function(classes, with.margins, with.shading, 
                           with.legend, center.plot)
{
    limits <- par("usr")
    xmin <- limits[1]
    xmax <- limits[2]
    ymin <- limits[3]
    ymax <- limits[4]

    # draw the fixed boundaries
    segments(0, 0, 0, ymin)
    segments(0, 0, xmax, xmax/sqrt(3))
    segments(xmin, abs(xmin)/sqrt(3), 0, 0)

    if (with.margins) {
        # margin from left below decision boundary to center
        segments(xmin, -xmin/sqrt(3) - sqrt(4/3), -1, -1/sqrt(3), lty=2)

        # margin from left center to down
        segments(-1, -1/sqrt(3), -1, ymin, lty=2)

        # margin from right center to middle
        segments(1, -1/sqrt(3), 1, ymin, lty=2)

        # margin from right center to right boundary
        segments(1, -1/sqrt(3), xmax, xmax/sqrt(3) - sqrt(4/3), lty=2)

        # margin from center to top left
        segments(xmin, -xmin/sqrt(3) + sqrt(4/3), 0, sqrt(4/3), lty=2)

        # margin from center to top right
        segments(0, sqrt(4/3), xmax, xmax/sqrt(3) + sqrt(4/3), lty=2)
    }

    if (with.shading) {
        fill <- gensvm.fill.colors()
        # bottom left
        polygon(c(xmin, -1, -1, xmin), c(ymin, ymin, -1/sqrt(3), -xmin/sqrt(3) - 
                                         sqrt(4/3)), col=fill$green, border=NA)
        # bottom right
        polygon(c(1, xmax, xmax, 1), c(ymin, ymin, xmax/sqrt(3) - sqrt(4/3), 
                                       -1/sqrt(3)), col=fill$blue, border=NA)
        # top
        polygon(c(xmin, 0, xmax, xmax, xmin), 
                c(-xmin/sqrt(3) + sqrt(4/3), sqrt(4/3), xmax/sqrt(3) + sqrt(4/3),
                  ymax, ymax), col=fill$orange, 
                border=NA)
    }

    if (with.legend) {
        offset <- abs(xmax - xmin) * 0.05
        colors <- gensvm.plot.colors()
        markers <- gensvm.plot.markers()
        legend(xmax + offset, ymax, classes, col=colors, pch=markers, xpd=T)
    }
}

gensvm.plot.1d <- function(classes, with.margins, with.shading, with.legend, 
                           center.plot)
{
    limits <- par("usr")
    xmin <- limits[1]
    xmax <- limits[2]
    ymin <- limits[3]
    ymax <- limits[4]

    # draw the fixed boundaries
    segments(0, ymin, 0, ymax)

    if (with.margins) {
        segments(-1, ymin, -1, ymax, lty=2)
        segments(1, ymin, 1, ymax, lty=2)
    }
    if (with.shading) {
        fill <- gensvm.fill.colors()
        polygon(c(xmin, -1, -1, xmin), c(ymin, ymin, ymax, ymax), 
                col=fill$blue, border=NA)
        polygon(c(1, xmax, xmax, 1), c(ymin, ymin, ymax, ymax), col=fill$orange,
                border=NA)
    }
    if (with.legend) {
        offset <- abs(xmax - xmin) * 0.05
        colors <- gensvm.plot.colors(2)
        markers <- gensvm.plot.markers(2)
        legend(xmax + offset, ymax, classes, col=colors, pch=markers, xpd=T)
    }
}

gensvm.plot.colors <- function(K=3)
{
    point.blue <- rgb(31, 119, 180, maxColorValue=255)
    point.orange <- rgb(255, 127, 14, maxColorValue=255)
    point.green <- rgb(44, 160, 44, maxColorValue=255)

    if (K == 3)
        colors <- as.matrix(c(point.green, point.blue, point.orange))
    else
        colors <- as.matrix(c(point.blue, point.orange))
    return(colors)
}

gensvm.fill.colors <- function()
{
    fill.blue <- rgb(31, 119, 180, 51, maxColorValue=255)
    fill.orange <- rgb(255, 127, 14, 51, maxColorValue=255)
    fill.green <- rgb(44, 160, 44, 51, maxColorValue=255)

    fills <- list(blue=fill.blue, orange=fill.orange, green=fill.green)
    return(fills)
}

gensvm.plot.markers <- function(K=3)
{
    markers <- as.vector(c(15, 16, 17))
    return(markers[1:K])
}
