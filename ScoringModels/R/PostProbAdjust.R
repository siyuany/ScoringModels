#' Posterior Probability Adjustment
#'
#' @description Adjust for posterior probability, considering the original
#' bad rate and the bad rate after oversampling.
#'
#' @param prob prob to be bad given by model prediction
#' @param r0   the original (designated) bad rate
#' @param r1   the bad rate in training set (after oversampling)
#'
#' @examples
#' \dontrun{
#' # Given the predicted probability 0.67, the original bad rate 0.05, and the
#' # bad rate after oversampling 0.5, then the adjusted posterior probability
#' # to be bad:l
#' PostProbAdjust(0.67, 0.05, 0.5)
#' }
#'
#' @export

PostProbAdjust <- function(prob, r0 = 0.05, r1 = 0.50) {
    k <- r1 * (1 - r0) / (r0 - r0 * r1)
    prob / (k * (1 - prob) + prob)
}
