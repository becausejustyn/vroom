#' calculate_cohen_d
#'
#' @param data dataframe
#' @param grouping_column column to group
#' @param y_column y column
#'
#' @return Cohen's D
#' @export
#'
#' @examples calculate_cohen_d(mtcars, vs, hp)
calculate_cohen_d <- function(data, grouping_column, y_column) {

  cohen_d <- function(mean_a, mean_b, sd_a, sd_b) {
    d <- (mean_a - mean_b) / sqrt((sd_a^2 + sd_b^2) / 2)
    return(d)
  }

  result <- data |>
    dplyr::group_by({{ grouping_column }}) |>
    dplyr::summarise(
      mean = mean({{ y_column }}),
      sd = sd({{ y_column }})
    )

  cohen_d_value <- cohen_d(result[['mean']][1], result[['mean']][2], result[['sd']][1], result[['sd']][2])

  return(cohen_d_value)
}
