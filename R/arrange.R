
#' @title Arrange entries in data.frame
#' @description Analogous function for \code{arrange} in \pkg{dplyr}.
#' @param .data data.frame
#' @param ... Arrange by what group? Minus symbol means arrange by
#' descending order.
#' @param cols For \code{set_arrange} only.
#' A character vector of column names of \code{.data} by which to order.
#' If present, override \code{...}. Defaults to \code{NULL}.
#' @param order For \code{set_arrange} only. An integer vector with only possible
#' values of 1 and -1, corresponding to ascending and descending order.
#' Defaults to 1.
#' @details Once arranged, the order of entries would be changed forever.
#' @return A data.table
#' @seealso \code{\link[dplyr]{arrange}}, \code{\link[data.table]{setorder}}
#' @examples
#'
#' a = as.data.table(iris)
#' a %>% arrange(Sepal.Length)
#' a
#' a %>% arrange(cols = c("Sepal.Width","Petal.Length"))
#' a
#'

#' @export
arrange = function(.data,...,cols = NULL,order = 1L){
  if(is.null(cols)) setorder(.data,...)[]
  else setorderv(.data,cols = cols,order = order)[]
}
