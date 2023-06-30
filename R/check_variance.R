# does a few different methods for checking if the variance between groups
# is roughly equal
#' check_variance
#'
#' @param formula t-test formula
#' @param data dataframe
#'
#' @return variances for different tests
#' @export
#'
#' @examples t.test(extra ~ group, data = sleep) |> check_variance()
check_variance <- function(formula, data) {
  # Bartlett test: Check homogeneity of variances based on the mean
  bartlett_result <- bartlett.test(formula, data = data) |> broom::tidy()

  # Levene test: Check homogeneity of variances based on the median, so it’s more robust to outliers
  levene_result <- car::leveneTest(formula, data = data) |>
    broom::tidy() |>
    dplyr::mutate(method = 'levene test')

  # Fligner-Killeen test: Check homogeneity of variances based on the median, so it’s more robust to outliers
  fligner_result <- stats::fligner.test(formula, data = data) |> broom::tidy()

  # Kruskal-Wallis test: Check homogeneity of distributions nonparametrically
  kruskal_result <- stats::kruskal.test(formula, data = data) |> broom::tidy()

  # Combine results into a data frame
  results_df <- dplyr::bind_rows(bartlett_result, levene_result, fligner_result, kruskal_result, .id = "test")

  return(results_df)
}
