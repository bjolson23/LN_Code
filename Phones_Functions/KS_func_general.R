#' KS Calculation
#'
#' Calculates KS value from a set of predictions, in logodds, and a set of labels.
#' @param preds Prediction values in logodds
#' @param labels Empirical performance labels
#' @return KS value from predictions and labels
#' @examples
#' ks_val <- KS_func_general(preds = logodds_score, labels = actual_outcome)
#' @export
#' @import ROCR

KS_func_general <- function(preds,labels){
  require(ROCR)
  p   <- prediction(as.numeric(preds),labels)
  perf <- performance(p, "tpr", "fpr")
  ks <- max(attr(perf, "y.values")[[1]] - (attr(perf, "x.values")[[1]]))
  return(ks)
}
