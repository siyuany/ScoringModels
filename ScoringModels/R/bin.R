#' Binning based on Information Value
#'
#' @import foreach
#' @import doParallel
#' @import parallel
#' @export
bin <- function(...) {
    UseMethod("bin")
}

#' @describeIn bin for vector
#' @export
bin.default <- function(predictor,
                        response,
                        result = c("cutpoints", "iv", "woe"),
                        good = NA,
                        min_iv_incr = 0.05,
                        min_iv_inc = 0.001,
                        min_ratio = 0.05,
                        p = 0.05,
                        ...) {
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
    include <- !is.na(response)
    response <- response[include]
    predictor <- predictor[include]
    y <- ifelse(response == good, 'Good', 'Bad')
    x <- as.numeric(predictor)
    result <- match.arg(result)

    na_level <- c(Good = sum(y[is.na(x)] == 'Good'),
                  Bad  = sum(y[is.na(x)] == 'Bad'))
    include <- !is.na(x)
    y <- y[include]
    x <- x[include]
    size <- length(x)

    avail_cutp <-
        unique(quantile(
            x,
            probs = seq(from = 0.01, to = 0.99, by = 0.01),
            type = 1
        ))
    avail_cutp <- unique(round(avail_cutp, 3))

    bin.obj <- list(bands = c(min(x), max(x)),
                    iv  = c(0, 0, 0))

    total_good <- sum(y == "Good")
    total_bad  <- sum(y == "Bad")

    cutp_iv <- function(point) {
        if (point %in% bin.obj$bands)
            return(list(bin.obj))
        cut_interval <- max(which(bin.obj$bands < point))

        # ratio
        left <- x > bin.obj$bands[cut_interval] & x <= point
        left_good <- sum(left & y == "Good")
        left_bad  <- sum(left & y == "Bad")
        right <- x > point & x <= bin.obj$bands[cut_interval + 1]
        right_good <- sum(right & y == "Good")
        right_bad  <- sum(right & y == "Bad")
        if (min(sum(left), sum(right)) < min_ratio * size) {
            return(list(bin.obj))
        }
        # prop test
        res <-
            prop.test(left_good, sum(left), right_good, sum(right))
        if (res$p > p) {
            return(list(bin.obj))
        }
        # iv
        left_iv <- (left_good / total_good - left_bad / total_bad) *
            log(left_good / total_good / (left_bad / total_bad))
        right_iv <-
            (right_good / total_good - right_bad / total_bad) *
            log(right_good / total_good / (right_bad / total_bad))
        iv <- c(
            bin.obj$iv[1:cut_interval],
            ifelse(is.infinite(left_iv), 0, left_iv),
            ifelse(is.infinite(right_iv), 0, right_iv),
            bin.obj$iv[(cut_interval + 2):length(bin.obj$iv)]
        )
        if (sum(iv) > sum(bin.obj$iv)) {
            bin.obj$bands <- c(bin.obj$bands[1:cut_interval],
                               point,
                               bin.obj$bands[(cut_interval + 1):length(bin.obj$bands)])
            bin.obj$iv <- iv
        }

        return(list(bin.obj))
    }

    # Start binning
    while (TRUE) {
        # print(bin.obj)
        res <- NULL
        res <- foreach(point = avail_cutp, .combine = "c") %do% cutp_iv(point)

        ivs <- sapply(res, function(x)
            sum(x$iv))
        max_iv_index <- which(ivs == max(ivs))[1]
        best <- res[[max_iv_index]]
        inc <- sum(best$iv) - sum(bin.obj$iv)

        if ((inc > min_iv_inc) & (inc / sum(bin.obj$iv) > min_iv_incr)) {
            bin.obj <- best
            #next
        } else {
            break
        }
    }
    # End binning

    cutp <- bin.obj$bands[c(-1, -length(bin.obj$bands))]
    switch(
        result,
        iv = sum(bin.obj$iv),
        woe = {
            x_disc <- label.numeric(x, cutp)
            woe(x_disc, y, good = "Good")
        },
        cutpoints = cutp
    )
}

#' @describeIn bin for date.frame
#' @export
bin.data.frame <- function(df, x, y, ...) {
    bin(df[, x], df[, y], ...)
}
