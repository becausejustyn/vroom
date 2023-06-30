#' rename_labs
#'
#' @param df dataframe
#' @param label label to rename
#'
#' @return renamed dataframe
#' @export
#'
#' @examples rename_labs(df, label)
rename_labs <- function(df, label) {
  df <- df |>
    dplyr::rename(
      pred_lab = pred_lab,
      pred_score = pred_score,
      img_lab = img_lab,
      !!glue::glue("pred_lab_{label}") := pred_lab,
      !!glue::glue("pred_score_{label}") := pred_score,
      !!glue::glue("img_lab_{label}") := img_lab
    )
  return(df)
}
