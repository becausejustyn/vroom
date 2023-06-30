#' subset_columns1
#'
#' @param df dataframe
#' @param cond_ranges data type that specifies the ranges for each condition. It is included in the package.#'
#' @return df
#' @export
#'
#' @examples subset_columns1(df, cond_ranges)
subset_columns1 <- function(df, cond_ranges) {
  purrr::map(cond_ranges, ~ {
    start_col <- purrr::pluck(.x, 1)
    end_col <- purrr::pluck(.x, 2)
    df |> dplyr::select({{start_col}}:{{end_col}}) |> names()
  })
}
