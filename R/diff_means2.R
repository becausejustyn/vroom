#' diff_means2
#'
#' @param data dataframe
#' @param condition condition category
#' @param value value for the category
#'
#' @return dataframe of the difference in means
#' @export
#'
#' @examples mtcars |> diff_means2(vs, hp)
diff_means2 <- function(data, condition, value){
  data |>
    dplyr::group_by({{ condition }}) |>
    dplyr::summarise(avg_value = mean({{ value }}, na.rm = TRUE)) |>
    dplyr::mutate(diff_means = avg_value - dplyr::lead(avg_value))
}
