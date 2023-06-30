#' get_id_vals
#'
#' @param data dataframe
#' @param id id column
#'
#' @return dataframe of the render id
#' @export
#'
#' @examples get_id_vals(df, render_id)
get_id_vals <- function(data, id){
  data |>
    dplyr::filter(render_id %in% c(id))
}
