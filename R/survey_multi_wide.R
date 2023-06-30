# convert the multi-item survey questions to wide, e.g. survey_multi_wide(exp1_df)
#' survey_multi_wide
#'
#' @param df dataframe
#'
#' @return wide version of the input df
#' @export
#'
#' @examples survey_multi_wide(df)
survey_multi_wide <- function(df){
  df <- df |>
    dplyr::select(uuid, user_view_general:rely_info_face) |>
    tidyr::pivot_longer(
      cols = -c(uuid),
      names_to = c("variable", ".value"), names_pattern = "(.*)_(.*)"
    )
  return(df)
}
