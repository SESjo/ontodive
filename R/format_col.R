#' Format
#'
#' Format columns of a dataset by removing capital letters, bracket and
#' transform white space as underscore
#'
#' @param col_names A vector of character containing column names to be
#' formatted
#'
#' @return The input correctly formatted
#'
#' @export
#' @importFrom stringr str_replace_all str_to_lower str_replace_all
#' @examples
#' # format several vectors of characters
#' format_col(c("lkjez alzkj","kjfe[lkjz]","lkjee [lkj]"))
#'
format_col <- function(col_names) {
  # remove white space
  col_names_trim = str_replace_all(string = col_names,
                                   pattern = " ",
                                   replacement = "")
  # convert upper case to lower case
  col_names_trim_lower = str_to_lower(col_names_trim)
  # replace "[" by "_"
  col_names_trim_lower_bracket_inter = str_replace_all(col_names_trim_lower, "\\[", "_")
  # replace "]" by ""
  col_names_trim_lower_bracket = str_replace_all(col_names_trim_lower_bracket_inter, "\\]", "")
  # return
  return(col_names_trim_lower_bracket)
}
