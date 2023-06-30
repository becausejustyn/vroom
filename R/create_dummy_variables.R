# works similar to pd.get_dummies()
#' create_dummy_variables
#'
#' @param data dataframe
#' @param column column to create dummy variables with
#' @param prefix not used
#'
#' @return dataframe with dummy variables included
#' @export
#'
#' @examples create_dummy_variables(mtcars, vs)
create_dummy_variables <- function(data, column, prefix = "dummy") {
  data |>
    dplyr::mutate(row = dplyr::row_number()) |>
    tidyr::pivot_wider(names_from = {{ column }}, values_from = {{ column }}, values_fn = length, values_fill = 0) |>
    dplyr::select(-row) #%>% rename_all(~paste0(prefix, "_", .))
}
