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
#' woe(Default, 'student', 'default', good = 'No')
#' # default generic
#' woe(Default$student, Default$default, good = 'No')
#' SummaryWoe(Default, 'default', good = 'No')
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
    tbl$Percentage <- (tbl$Good + tbl$Bad) / sum(tbl['Total', c('Good', 'Bad')])
    tbl['Total', 'IV'] <- sum(tbl$IV, na.rm = TRUE)
    class(tbl) <- c('woe', class(tbl))
    rownames(tbl) <- NULL
    tbl
}

#' @describeIn woe generic for data.frame
#' @export
woe.data.frame <- function(df, x, y, ...) {
    if (!x %in% colnames(df))
        stop("x does not exist in df")
    if (!y %in% colnames(df))
        stop("y does not exist in df")

    woe.default(df[, x], df[, y], ...)
}


#' @rdname woe
#' @export
SummaryWoe <- function(df, y, ...) {
    predictors <- colnames(df)
    if (!any(y == predictors)) {
        stop("'y' does not exist in df")
    }

    predictors <- predictors[predictors != y]
    summary_woe <- list()
    for (predictor in predictors) {
        if (is.factor(df[, predictor])) {
            .woe <- woe(df, predictor, y, ...)
            summary_woe[[predictor]] <- list(woe = .woe,
                                             iv  = .woe[.woe$Levels == "Total",
                                                        "WoE"])
        }

        if (is.numeric(df[, predictor])) {
            .cut <- bin(df, predictor, y, result = "cutpoints", ...)
            .woe <- woe(label.numeric(df[, predictor], .cut),
                        df[, y], ...)
            summary_woe[[predictor]] <- list(woe  = .woe,
                                             iv   = .woe[.woe$Levels == "Total",
                                                         "WoE"],
                                             cutp = .cut)
        }
    }

    class(summary_woe) <- c('Summary.WoE', class(summary_woe))
    summary_woe
}

#' @rdname woe
#' @param summary_woe result given by SummaryWoe
#' @importFrom stringr str_length
#' @export
ReplaceWoe <- function(df, y, summary_woe) {
    columns <- colnames(df)
    if (!any(y == columns)) {
        stop("'y' does not exist in df")
    }
    if (class(summary_woe)[1] != 'Summary.WoE') {
        stop('summary_woe should be an object of Summary.WoE')
    }

    new_df <- as.data.frame(df[[y]], stringsAsFactors = FALSE)
    colnames(new_df) <- y
    predictors <- names(summary_woe)
    for (predictor in predictors) {
        if (!any(predictor == columns)) {
            stop(sprintf('%s does not exist in df', predictor))
        }
        n_char <- str_length(predictor)
        if (substr(predictor, n_char - 3, n_char) == '_woe') {
            warning(sprintf("%s ends with '_woe'. Skip!", predictor))
            next
        }

        .woe <- summary_woe[[predictor]][['woe']]
        .woe <- .woe[-nrow(.woe), ]
        if (is.factor(df[, predictor])) {
            new_df[, paste0(predictor, '_woe')] <-
                .woe[match(df[, predictor], .woe[, 'Levels']), 'WoE']
        }

        if (is.numeric(df[, predictor])) {
            cutp <- summary_woe[[predictor]][['cutp']]
            x_disc <- label.numeric(df[, predictor], cutp)
            new_df[, paste0(predictor, '_woe')] <-
                .woe[match(x_disc, .woe[, 'Levels']), 'WoE']
        }
    }

    new_df
}
