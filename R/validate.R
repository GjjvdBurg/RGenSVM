#' @title [internal] Validate parameters
#'
#' @description Internal function used to validate the parameters passed to the 
#' gensvm() function.
#'
#' @return TRUE if all values pass their respective conditions, FALSE 
#' otherwise.
#'
#' @export
#' @keywords internal
#'
gensvm.validate.params <- function(p=NULL, kappa=NULL, lambda=NULL, 
                                   epsilon=NULL, gamma=NULL, weights=NULL, 
                                   kernel=NULL, ...)
{
    the.args <- as.list(match.call())
    conditions <- gensvm.param.conditions()
    for (param in names(the.args)) {
        if (is.null(the.args[[param]]))
            next
        if (!(param %in% names(conditions)))
            next
        func <- conditions[[param]]
        value <- eval(the.args[[param]])
        if (!func(value)) {
            cat(sprintf("Error: Parameter '%s' got invalid value: %s\n", param,
                        toString(value)))
            return(FALSE)
        }
    }
    return(TRUE)
}

#' @title [internal] Validate parameter grid
#'
#' @description Internal function to validate all parameters in a parameter 
#' grid.
#'
#' @return TRUE if all values pass their respective conditions, FALSE 
#' otherwise.
#'
#' @export
#' @keywords internal
#'
gensvm.validate.param.grid <- function(df)
{
    expected.colnames <- c("kernel", "coef", "degree", "gamma", "weights", 
                           "kappa", "lambda", "p", "epsilon", "max.iter")
    for (name in colnames(df)) {
        if (!(name %in% expected.colnames)) {
            cat(sprintf("Error: Invalid name supplied in parameter grid: %s\n",
                        name))
            return(FALSE)
        }
    }

    conditions <- gensvm.param.conditions()
    for (idx in 1:nrow(df)) {
        for (param in colnames(df)) {
            if (!(param %in% names(conditions)))
                next
            func <- conditions[[param]]
            value <- df[[param]][idx]
            if (!func(value)) {
                cat(sprintf("Invalid value in grid for parameter: %s\n", param))
                return(FALSE)
            }
        }
    }
    return(TRUE)
}

gensvm.param.conditions <- function()
{
    conditions <- list(
        p=function(x) { x >= 1.0 && x <= 2.0 },
        kappa=function(x) { x > -1.0 },
        lambda=function(x) {x > 0.0 },
        epsilon=function(x) { x > 0.0 },
        gamma=function(x) { x != 0.0 },
        weights=function(x) { x %in% c("raw", "unit", "group") },
        kernel=function(x) { x %in% c("linear", "poly", "rbf", "sigmoid") }
        )
}
