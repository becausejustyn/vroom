#' get_mode
#'
#' @param x vector
#'
#' @return mode of vector
#' @export
#'
#' @examples get_mode(mtcars[['carb']])
get_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
