#' cram_v
#'
#' @param test chi square test
#'
#' @return cramer's V
#' @export
#'
#' @examples chisq.test(c(A = 20, B = 15, C = 25)) |> cram_v()
cram_v <- function(test){
  v = sqrt((test[['statistic']]) / (sum(test[['observed']]) * test[['parameter']]))[[1]]
  return(v)
}
