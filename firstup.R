#' First upper case letter
#'
#' @title firstup
#' @description Converts first letter to upper case
#' @param x character vector
#' @return character vector
#' @author Dirk
#' @export
#' 


firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}