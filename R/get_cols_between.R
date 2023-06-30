#' get_cols_between
#'
#' @param df dataframe
#' @param col_start first column
#' @param col_end second column
#'
#' @return column names between range
#' @export
#'
#' @examples get_cols_between(mtcars, mpg, hp)
get_cols_between <- function(df, col_start, col_end) {
  col_idx <- match(col_start, names(df)):match(col_end, names(df))
  df |>
    # select(({{col_start}}:{{col_end}})) also works
    dplyr::select(dplyr::all_of(col_idx)) |>
    names()
}
