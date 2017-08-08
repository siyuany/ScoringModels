#' Information Value
#'
#' @description \code{iv} calculates the information value of a discrete
#' variable with respect to a (binomial) response.
#'
#' @param woe.tbl a \code{woe} object
#' @param ... other parameters in \code{\link{woe}}
#'
#' @return \code{iv}
#'
#' @seealso \code{\link{woe}}
#' @examples
#' \dontrun{
#' library(ISLR)
#' # approach 1
#' iv(Default, 'student', 'default')
#' # approach 2
#' w <- woe(Default, 'student', 'default', good = 'Yes')
#' iv(w)
#' }
#'
#' @export

iv <- function(...) {
    UseMethod("iv")
}

#' @describeIn iv Calculate IV based on a \code{woe} object
#' @export
iv.woe <- function(woe.tbl) {
    woe.tbl[woe.tbl$Levels == 'Total', 'IV']
}

#' @describeIn iv Implicitly call \code{woe} to calculate a woe object
#' @export
iv.default <- function(...) {
    tbl <- woe(...)
    iv(tbl)
}
