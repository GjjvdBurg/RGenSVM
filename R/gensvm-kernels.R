#' Kernels in GenSVM
#'
#' GenSVM can be used for both linear multiclass support vector machine 
#' classification and for nonlinear classification with kernels. In general, 
#' linear classification will be faster but depending on the dataset higher 
#' classification performance can be achieved using a nonlinear kernel.
#'
#' The following nonlinear kernels are implemented in the GenSVM package:
#' \describe{
#'  \item{RBF}{The Radial Basis Function kernel is a commonly used kernel. 
