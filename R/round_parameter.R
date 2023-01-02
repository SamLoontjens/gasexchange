#' A function that rounds one column or vector.
#'
#' @description
#'   A function that rounds one column or vector.
#'   It returns a new column or vector that is rounded.
#' @param column A column or vector of a dataframe
#' @export
#' @return Returns a new column or vector that is rounded.
#' @examples
#' mydata$Qin <- round_parameter(mydata$Qin)
#'
round_parameter <- function(column){
  rounded_column <- base::round(column/10)*10
}
