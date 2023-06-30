#' get_model_count
#'
#' @param df dataframe
#' @param model_var column to count
#'
#' @return dataframe
#' @export
#'
#' @examples get_model_count(df, model_column)
get_model_count <- function(df, model_var){
  df |> dplyr::count({{model_var}})
}
