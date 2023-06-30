# from the experiment block, get the models used, e.g.
# get_model_cond(df, condition) where condition == 'FD1'
# returns fair for cond1 and dark for cond2
#' get_model_cond
#'
#' @param data dataframe
#' @param column_name column name for the conditions
#'
#' @return dataframe
#' @export
#'
#' @examples get_model_cond(df, condition_column)
get_model_cond <- function(data, column_name) {
  column_name <- enquo(column_name)
  data <- data |>
    dplyr::mutate(
      cond1 = case_when(
        {{column_name}} == 'FD1' ~ 'fair',
        {{column_name}} == 'FD2' ~ 'dark',
        {{column_name}} == 'FL1' ~ 'fair',
        {{column_name}} == 'FL2' ~ 'light',
        {{column_name}} == 'B1' ~ 'light',
        {{column_name}} == 'B2' ~ 'dark') |> as.factor(),
      cond2 = dplyr::case_when(
        {{column_name}} == 'FD1' ~ 'dark',
        {{column_name}} == 'FD2' ~ 'fair',
        {{column_name}} == 'FL1' ~ 'light',
        {{column_name}} == 'FL2' ~ 'fair',
        {{column_name}} == 'B1' ~ 'dark',
        {{column_name}} == 'B2' ~ 'light') |> as.factor()
    )
  return(data)
}
