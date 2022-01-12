#' @importFrom data.table data.table
NULL

#' Boy Baby names.
#'
#' Full baby name data provided by the SSA. This includes all names with at
#' least 5 uses.
#'
#' @format A data.table with 2 variables: \code{types}, \code{counts},
"elem_boys"

#' Girl Baby names.
#'
#' Full baby name data provided by the SSA. This includes all names with at
#' least 5 uses.
#'
#' @format A data.table with 2 variables: \code{types}, \code{counts},
"elem_girls"

#' @import utils 
utils::globalVariables(c("elem_boys", "elem_girls"))
