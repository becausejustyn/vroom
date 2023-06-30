# this renames the columns so they are easier to read
# and relate to the related question
# for example, Q1_1...25 = FD1_1_1, Q1_2...26 = FD1_1_2
#' rename_questions
#'
#' @param x dataframe
#' @param cond condition
#'
#' @return renamed df
#' @export
#'
#' @examples rename_questions(df, 'FD1')
rename_questions <- function(x, cond){
  # FD = fair_dark, FL = fair_light, b = biased
  allowed_conds <- c('FD1', 'FD2', 'FL1', 'FL2', 'B1', 'B2')

  if(!(cond %in% allowed_conds)){
    stop("Invalid condition specified")
  }

  # return the question number for the condition, e.g. Q1_1...25 returns 1
  question_n <- stringr::str_extract(colnames(x), "(?<=Q)\\d+(?=_)")
  # which slider for the question, e.g. Q1_2...26 returns 2 since it is the second slider for the first question
  slider_n <- stringr::str_extract(colnames(x), "(?<=_)\\d+(?=\\.{3})")
  # output would be something like cond_questionn_slider
  output <- paste(cond, question_n, slider_n, sep = '_')
  return(output)
}
