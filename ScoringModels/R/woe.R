#' Weight of Evidence
#'
#' @description \code{woe} calculates the weight of evidence of each level in a
#' discrete variable with respect to a response.
#'
#' @param predictor a vector representing the predictor
#' @param response  a vecotr representing the response
#' @param df        a \code{data.frame} containing the predictor and the
#' response
#' @param x         the name of the predictor in \code{df}
#' @param y         the name of the response in \code{df}
#' @param good      the value representing 'good' in \code{response}, other
#' values representing bad.
#' @return a \code{data.frame} showing the WoE of each level, and encapsulated
#' as a \code{woe} object
#'
#' @examples
#' \dontrun{
#' library(ISLR)
#' # data.frame generic
#' woe(Default, 'student', 'default', good = 'Yes')
#' # default generic
#' woe(Default$student, Default$default, good = 'Yes')
#' }
#'
#' @export

woe <- function(...) {
    UseMethod('woe')
}

#' @describeIn woe default generic
#' @export
woe.default <- function(predictor, response, good = NA, ...) {
    y_levels <- sort(unique(response))
    if (length(y_levels) <= 1)
        stop("y is a vector of a unique value.")
    if (is.na(good)) {
        good <- y_levels[1]
    }
    if (!good %in% y_levels)
        stop('good does not exist in response')
    if (length(predictor) != length(response))
        stop('predictor should have the same length as response')
    include <- !(is.na(predictor) | is.na(response))
    response <- response[include]
    predictor <- predictor[include]

    y <- ifelse(response == good, 'Good', 'Bad')

    tbl <- tapply(y, predictor, function(z) {
        c(Good = sum(z == 'Good'),
          Bad  = sum(z == 'Bad'))
    })
    tbl <- as.data.frame(do.call(rbind, tbl))
    tbl <- rbind(tbl, Total = apply(tbl, 2, sum))
    tbl <- cbind(Levels = rownames(tbl), tbl)

    tbl$GoodR <- tbl$Good / tbl['Total', 'Good']
    tbl$BadR <- tbl$Bad / tbl['Total', 'Bad']
    tbl$WoE <- log(tbl$GoodR / tbl$BadR)
    tbl$IV <- (tbl$GoodR - tbl$BadR) * tbl$WoE
    tbl$IV[is.infinite(tbl$IV)] <- NA
    tbl['Total', 'IV'] <- sum(tbl$IV, na.rm = TRUE)
    class(tbl) <- c('woe', class(tbl))
    tbl
}

#' @describeIn woe generic for data.frame
#' @export
woe.data.frame <- function(df, x, y, ...) {
    woe.default(df[, x], df[, y], ...)
}
