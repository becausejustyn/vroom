#' subset_columns2
#'
#' @param df dataframe
#' @param cond_ranges data type that specifies the ranges for each condition. It is included in the package.
#'
#' @return df
#' @export
#'
#' @examples subset_columns2(df, cond_ranges)
subset_columns2 <- function(df, cond_ranges) {
  purrr::map(cond_ranges, ~ {
    df |> dplyr::select({{.x[1]}}:{{.x[2]}}) |> names()
  })
}
