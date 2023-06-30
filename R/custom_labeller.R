#' custom_labeller
#'
#' @param data dataframe
#' @param facet_var variable to facet
#' @param custom_labels list of labels to use
#'
#' @return labeller object for ggplot2
#' @export
#'
#' @examples custom_labeller(df, col1, lab_list)
custom_labeller <- function(data, facet_var, custom_labels) {
  labeller_fun <- ggplot2::labeller(!!as.name(facet_var) := ggplot2::as_labeller(setNames(custom_labels, unique(data[[facet_var]]))))
  return(labeller_fun)
}
