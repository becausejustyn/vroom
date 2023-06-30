# if you use this you will need to update it
#' get_col_names
#'
#' @param df dataframe
#' @param start_col first column to start at
#' @param end_col column to stop at
#'
#' @return column names between a range
#' @export
#'
#' @examples get_col_names(mtcars, cyl, wt)
get_col_names <- function(df, start_col, end_col){
  output <- df |> dplyr::select({{start_col}}:{{end_col}}) |> names()
  return(output)
}
