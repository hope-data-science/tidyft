
#' @title Fast value replacement in data frame
#' @description \code{replace_vars} could replace any value(s) or values
#' that match specific patterns to another specific value in a data.table.
#' @param .data A data.table
#' @param ... Colunms to be replaced. If not specified, use all columns.
#' @param from A value, a vector of values or a function returns a logical value.
#' Defaults to \code{NaN}.
#' @param to A value.
#' @return A data.table.
#' @seealso \code{\link[tidyfst]{replace_dt}}
#' @examples
#'  iris %>% as.data.table() %>%
#'    mutate(Species = as.character(Species))-> new_iris
#'
#'  new_iris %>%
#'    replace_vars(Species, from = "setosa",to = "SS")
#'  new_iris %>%
#'    replace_vars(Species,from = c("setosa","virginica"),to = "sv")
#'  new_iris %>%
#'    replace_vars(Petal.Width, from = .2,to = 2)
#'  new_iris %>%
#'    replace_vars(from = .2,to = NA)
#'  new_iris %>%
#'    replace_vars(is.numeric, from = function(x) x > 3, to = 9999 )
#'


#' @export
replace_vars = function (.data, ..., from = is.na,to) {

  dt = .data

  if(!is.function(from)) {
    if (setequal(from,to)) return(.data)
    if(length(from) == 1) .func = function(x) x == from
    else if(is.character(from)) .func = function(x) x %chin% from
    else .func = function(x) x %in% from
  } else .func = from

  if (substitute(list(...)) %>% deparse() == "list()")
    dot_string <- NULL
  else dot_string <- dt[0] %>% select_dt(...) %>% names()
  if (is.null(dot_string)) {
    for (j in seq_len(ncol(dt))) set(dt, which(.func(dt[[j]])),
                                     j, to)
  }
  else {
    for (j in dot_string) set(dt, which(.func(dt[[j]])),
                              j, to)
  }
  dt
}
