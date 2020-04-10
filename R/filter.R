

#' @title Filter entries in data.frame
#' @description Analogous function for \code{filter} in \pkg{dplyr}.
#' @param .data data.frame
#' @param ... List of variables or name-value pairs of summary/modifications
#'   functions.
#' @return A data.table
#' @details Currently data.table is not able to delete rows by reference,
#' @references \url{https://github.com/Rdatatable/data.table/issues/635}
#' @references \url{https://stackoverflow.com/questions/10790204/how-to-delete-a-row-by-reference-in-data-table}
#' @seealso \code{\link[dplyr]{filter}}
#' @examples
#' iris = as.data.table(iris)
#' iris %>% filter(Sepal.Length > 7)
#' iris %>% filter(Sepal.Length > 7,Sepal.Width > 3)
#' iris %>% filter(Sepal.Length > 7 & Sepal.Width > 3)
#' iris %>% filter(Sepal.Length == max(Sepal.Length))

#' @export
filter = function(.data,...){
  substitute(list(...)) %>%
    lapply(deparse) %>%
    .[-1] %>%
    str_c(collapse = " & ") -> dot_string

  eval(parse(text = str_glue(".data[{dot_string}]")))
}


