bin <- function(...) {
    UseMethod("bin")
}

bin.default <- function(y, x, ...) {
    if (length(y) != length(x))
        stop("The lengths of y and x should be equal")
    if (!is.numeric(x)) 
        stop("x should be numeric")
    
    both <- !(is.na(x) | is.na(y))
    y_complete <- y[both]
    x_complete <- x[both]
    
    if (length(unique(y_complete)) != 2)
        stop("y should be a bivariate variable")
    # all available cutpoints
    cutp <- unique(quantile(x, probs = seq(0, 1, 0.01), type = 1))
    
    
}
