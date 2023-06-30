#' report_chi
#'
#' @param test chi square test
#'
#' @return output of chi square test
#' @export
#'
#' @examples chisq.test(c(A = 20, B = 15, C = 25)) |> report_chi()
report_chi <- function(test){
  n = sum(test[['observed']])
  df = test[['parameter']]
  p_val = test[['p.value']]
  stat = test[['statistic']]

  glue::glue("X2 ({df}, N = {n}) = {stat}, p {p_val}")
}
