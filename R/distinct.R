

#' @title Select distinct/unique rows in data.table
#' @description Analogous function for \code{distinct} in \pkg{dplyr}
#' @param .data data.table
#' @param ... Optional variables to use when determining uniqueness.
#'  If there are multiple rows for a given combination of inputs,
#'  only the first row will be preserved.
#'  If omitted, will use all variables.
#' @param .keep_all If \code{TRUE}, keep all variables in data.table. If a combination of ... is not distinct,
#' this keeps the first row of values.
#' @return data.table
#' @seealso \code{\link[dplyr]{distinct}}
#' @examples
#'
#'  a = as.data.table(iris)
#'  b = as.data.table(mtcars)
#'  a %>% distinct(Species)
#'  b %>% distinct(cyl,vs,.keep_all = TRUE)
#'
#'

#' @export

distinct = function (.data, ..., .keep_all = FALSE) {
  sel_name = select_dt(.data[0],...) %>% names()
  if(.keep_all) unique(.data,by = sel_name)
  else unique(select(.data,cols = sel_name))
}


