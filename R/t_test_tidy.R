
#' Title
#'
#' @param x t-test data type
#'
#' @return a tidy df for the t-test
#' @export
#'
#' @examples t_test_tidy(t.test(extra ~ group, data = sleep))
t_test_tidy <- function(x){
  broom::tidy(x) |>
    # Calculate difference in means, since t.test() doesn't actually do that
    dplyr::mutate(estimate = estimate1 - estimate2) |>
    # Rearrange columns
    dplyr::select(dplyr::starts_with("estimate"), dplyr::everything())
}
