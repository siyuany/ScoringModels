#' Binning based on Information Value
#'
#' @import foreach
#' @export
bin <- function(...) {
    UseMethod("bin")
}

#' @export
bin.default <- function(predictor,
                        response,
                        result = c("iv", "woe", "cutpoints"),
                        good = NA,
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

    bin.obj <- list(x_disc = rep(1, length(x)),
                    cutp_level = rep(1, length(avail_cutp)),
                    cutp = NULL,
                    iv = 0)

    cutp_iv <- function(point) {
        if (point %in% bin.obj$cutp)
            return(list(bin.obj))
        # ratio
        cutp <- c(bin.obj$cutp, point)
        cutp <- sort(unique(cutp))
        cutp_level <- bin.obj$cutp_level
        level <- max(cutp_level[avail_cutp <= point])
        x_disc <- bin.obj$x_disc
        x_disc[x_disc == level & x <= point] <- 2 * level
        x_disc[x_disc == level & x >  point] <- 2 * level + 1
        cutp_level[cutp_level == level & avail_cutp <= point] <- 2 * level
        cutp_level[cutp_level == level & avail_cutp >  point] <- 2 * level + 1

        min_size <- min(tapply(x_disc, x_disc, length))
        if (min_size < min_ratio * size) {
            return(list(bin.obj))
        }
        # statistical test
        to_test_x <- x_disc[!x_disc %in% bin.obj$x_disc]
        to_test_y <- y[!x_disc %in% bin.obj$x_disc]
        tbl <- tapply(to_test_y, to_test_x, function(seg_y) {
            c(Good = sum(seg_y == 'Good'),
              Bad  = sum(seg_y == 'Bad'))
        })
        test <- prop.test(tbl[[1]][1], sum(tbl[[1]]),
                          tbl[[2]][1], sum(tbl[[2]]))
        if (test$p > p) {
            return(list(bin.obj))
        }

        new_iv <- iv(x_disc, y, good = 'Good')
        list(list(x_disc = x_disc,
                  cutp_level = cutp_level,
                  cutp = cutp,
                  iv = new_iv))
    }

    # Start binning
    while (TRUE) {
        res <- NULL
        res <-
            foreach(point = avail_cutp, .combine = "c") %do% cutp_iv(point)

        ivs <- sapply(res, function(x) x$iv)
        max_iv_index <- which(ivs == max(ivs))[1]
        best <- res[[max_iv_index]]
        inc <- best$iv - bin.obj$iv

        if (inc > min_iv_inc) {
            bin.obj <- best
            next
        } else {
            break
        }
    }
    # End binning

    switch(result,
           cutpoints = bin.obj$cutp,
           woe = woe(bin.obj$x_disc, y, good = 'Good'),
           iv = bin.obj$iv)
}

#' @export
bin.data.frame <- function(df, x, y, ...) {
    bin(df[, x], df[, y])
}
