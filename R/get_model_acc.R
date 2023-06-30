#' get_model_acc
#'
#' @param df dataframe
#' @param label_var label variable
#' @param pred_lab the predicted label of model
#'
#' @return dataframe with model acc
#' @export
#'
#' @examples get_model_acc(df, label_column, predicted_label)
get_model_acc <- function(df, label_var, pred_lab){
  df |>
    dplyr::group_by({{label_var}}) |>
    dplyr::summarise(
      n = n(),
      correct = sum(human_id == {{pred_lab}}),
      incorrect = n - correct,
      accuracy = correct / n
    )
}
