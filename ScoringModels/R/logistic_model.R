#' Stepwise Logistic Regression
#'
#' @import stringr
#' @export

logistic_model <- function(df,
                           y,
                           summary_woe,
                           max_nvars = NULL,
                           parallel = FALSE,
                           cv_nfold = 5,
                           ...) {
    predictors <- colnames(df)
    if (!any(y != predictors)) {
        stop("'y' does not exist in df")
    }
    predictors <- predictors[predictors != y]
    predictors_iv <- names(summary_woe)
    predictors <-
        intersect(predictors, paste0(predictors_iv, '_woe'))
    
    start <- sapply(predictors, function(predictor) {
        x <- substr(predictor, 1, str_length(predictor) - 4)
        summary_woe[[x]][['iv']]
    })
    iter_form <- paste0(y, ' ~ ',
                        names(start)[start == max(start, na.rm = TRUE)])
    
    step_record <- list()
    i <- 1
    
    if (!parallel) {
        while (TRUE) {
            round_name <- paste0('Round_', i)
            i <- i + 1
            
            auc
            
        }
    } else {
        
    }
}

cv.glm <- function(formual,
                   data,
                   eval = function(y, yhat) mean((y - yhat) ^ 2),
                   K = 5)
{
    call <- match.call()
    if (!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
        runif(1)
    seed <-
        get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    n <- nrow(data)
    if ((K > n) || (K <= 1))
        stop("'K' outside allowable range")
    K.o <- K
    K <- round(K)
    kvals <- unique(round(n / (1L:floor(n / 2))))
    temp <- abs(kvals - K)
    if (!any(temp == 0))
        K <- kvals[temp == min(temp)][1L]
    if (K != K.o)
        warning(gettextf("'K' has been set to %f", K), domain = NA)
    f <- ceiling(n / K)
    s <- sample0(rep(1L:K, f), n)
    n.s <- table(s)
    glm.y <- glmfit$y
    cost.0 <- cost(glm.y, fitted(glmfit))
    ms <- max(s)
    CV <- 0
    Call <- glmfit$call
    for (i in seq_len(ms)) {
        j.out <- seq_len(n)[(s == i)]
        j.in <- seq_len(n)[(s != i)]
        Call$data <- data[j.in, , drop = FALSE]
        d.glm <- eval.parent(Call)
        p.alpha <- n.s[i] / n
        cost.i <- cost(glm.y[j.out], predict(d.glm, data[j.out,
                                                         , drop = FALSE], type = "response"))
        CV <- CV + p.alpha * cost.i
        cost.0 <- cost.0 - p.alpha * cost(glm.y, predict(d.glm,
                                                         data, type = "response"))
    }
    list(
        call = call,
        K = K,
        eval = c(),
        seed = seed
    )
}