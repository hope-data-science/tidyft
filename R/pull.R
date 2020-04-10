
#' @title Pull out a single variable
#' @description Analogous function for \code{pull} in \pkg{dplyr}
#' @param .data data.frame
#' @param col A name of column or index (should be positive).
#' @return A vector
#' @seealso \code{\link[dplyr]{pull}}
#' @examples
#' mtcars %>% pull(2)
#' mtcars %>% pull(cyl)
#' mtcars %>% pull("cyl")
#' @export


pull = function(.data,col) .data[[substitute(col)]]


