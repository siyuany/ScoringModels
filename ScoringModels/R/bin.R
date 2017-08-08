#' Binning based on Information Value
#' 
#' 
#' @import parallel
#' @import doParallel
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

    na_level <- c(Good = sum(y[is.na(x)] == 'Good'),
                  Bad  = sum(y[is.na(x)] == 'Bad'))
    include <- !is.na(x)
    y <- y[include]
    x <- x[include]

    avail_cutp <-
        unique(quantile(
            x,
            probs = seq(from = 0.01, to = 0.99, by = 0.01),
            type = 1
        ))

    bin.obj <- list(cutp = NULL, iv = 0)
    cl <- parallel::makeCluster(parallel::detectCores())
    doParallel::registerDoParallel(cl)
    
    cutp_iv <- function(point) {
        cutp <- c(bin.obj$cutp, point)
        x_disc <- label.numeric(x, cutp)
        new_iv <- iv(x_disc, y, good = 'Good')
        list(list(cutp = cutp, iv = new_iv))
    }
    
    # Start binning
    while (TRUE) {
        res <-
            foreach(point = avail_cutp, .combine = "c") %dopar% cutp_iv(point)
        break
    }
    # End binning
    parallel::stopCluster(cl)

    res
}

#' @export
bin.data.frame <- function(df, x, y, ...) {
    bin(df[, x], df[, y])
}