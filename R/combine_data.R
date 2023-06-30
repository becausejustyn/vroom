#' combine_data
#'
#' @param df dataframe
#' @param label_var label column
#' @param img_lab_var variable relating to the image
#' @param model_var model variable
#'
#' @return merged dataframe
#' @export
#'
#' @examples df |> combine_data(label_column, img_label_column, model_column)
combine_data <- function(df, label_var, img_lab_var, model_var){
  df |>
    dplyr::filter(model == {{model_var}}) |>
    dplyr::select(human_id, render_id, pred_lab = predicted_label, pred_score = prediction_score, -model) |>
    dplyr::mutate(img_lab = {{img_lab_var}}, label = {{label_var}})
}
