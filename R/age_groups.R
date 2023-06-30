#' age_groups
#'
#' @param data df
#' @param column age column
#'
#' @return column with age groups
#' @export
#'
#' @examples mtcars |> age_groups(mpg)
age_groups <- function(data, column){
  data <- data |>
    dplyr::mutate(
      age_x = dplyr::case_when(
        {{column}} <= 30 ~ '18 - 30',
        between({{column}}, 30, 45) ~ '30 - 45',
        between({{column}}, 45, 65) ~ '45 - 65',
        between({{column}}, 65, 99) ~ '65+')
    )
  return(data)
}
