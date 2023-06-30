#' get_cols
#'
#' @param df dataframe
#'
#' @return dataframe
#' @export
#'
#' @examples get_cols(df)
get_cols <- function(df){
  df |> dplyr::select(human_id, render_id)
}
