# for the faces not used df
#' get_predictions_df
#'
#' @param dataf dataframe
#' @param model_names name of which model
#'
#' @return dataframe of predicted values
#' @export
#'
#' @examples get_predictions_df(df, model)
get_predictions_df <- function(dataf, model_names) {
  purrr::map(model_names, ~ {
    select_label <- base::sub("_.*$", "", .x)
    dataf |>
      dplyr::filter(model == .x) |>
      dplyr::select(
        human_id, render_id,
        !!glue::glue("pred_lab_{select_label}") := predicted_label,
        !!glue::glue("pred_score_{select_label}") := prediction_score,
      )
  }) |>
    purrr::reduce(dplyr::left_join, by = c("human_id", "render_id"))
}
