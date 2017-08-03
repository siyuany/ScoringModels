prop.test <- function(y1, n1, y2, n2, sig.level = 0.05,
                      alternative = c("two.sided", "less", "greater")) {
    alternative <- match.arg(alternative)
    p1 <- y1 / n1
    p2 <- y2 / n2
    z <- (p1 - p2) / sqrt(p1 * (1 - p1) / n1 + p2 * (1 - p2) / n2)
    p <- switch(alternative,
                two.sided = (1 - pnorm(abs(z))) * 2,
                less = (1 - pnorm(-z)),
                greater = (1 - pnorm(z)))
    if (p > sig.level) {
        conclusion <- "Accept"
    } else {
        conclusion <- "Reject"
    }
    list(
        conclusion = conclusion,
        p = p,
        significane = sig.level,
        alternative = alternative,
        p1_estimate = p1,
        p2_estimate = p2
    )
}