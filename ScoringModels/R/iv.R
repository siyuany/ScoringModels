iv <- function(...) {
    UseMethod("iv")
}

iv.default <- function(x, y, ...) {
    x <- as.factor(x)
    y <- as.factor(y)
    x_levels <- levels(x)
    y_levels <- levels(y)
    
    tbl <- tapply(x, list(x, y), length)
    tbl[is.na(tbl)] <- 0
    tbl <- as.data.frame(tbl)
    tbl <- rbind(tbl, apply(tbl, 2, sum))
    rownames(tbl) <- c(x_levels, 'Total')
    colnames(tbl) <- y_levels
    
    
    
    
}

iv.data.frame <- function(data_set, objective, predictor) {
    iv.factor(data_set[, predictor], data_set[, objective])
}