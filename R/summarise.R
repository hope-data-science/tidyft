
#' @title Summarise columns to single values
#' @description Create one or more scalar variables summarizing
#'  the variables of an existing data.table.
#' @param .data A data.table
#' @param ... List of variables or name-value pairs of summary/modifications
#'   functions for \code{summarise_dt}.Additional parameters to be passed to
#'    parameter '.func' in \code{summarise_vars}.
#' @param by Unquoted name of grouping variable of list of unquoted names of
#'   grouping variables. For details see \link[data.table]{data.table}
#' @param when An object which can be coerced to logical mode
#' @param .cols Columns to be summarised.
#' @param .func Function to be run within each column, should return a value or vectors with same length.

#' @return A data.table
#' @examples
#'
#' a = as.data.table(iris)
#' a %>% summarise(sum = sum(Sepal.Length),avg = mean(Sepal.Length))
#'
#'
#' a %>%
#'   summarise_when(Sepal.Length > 5, avg = mean(Sepal.Length), by = Species)
#'
#' a %>%
#'   summarise_vars(is.numeric, min, by = Species)
#'
#'

#' @rdname summarise
#' @export
summarise = function(.data,...,by = NULL){
  dt = .data
  eval(substitute(dt[,.(...),by = by]))
}


#' @rdname summarise
#' @export
summarise_when = function(.data,when,...,by = NULL){
  dt = .data
  eval(substitute(dt[when,.(...),by = by]))
}

#' @rdname summarise
#' @export
summarise_vars = function (.data, .cols = NULL, .func, ...,by) {
  dt = .data
  deparse(substitute(.cols)) -> .cols
  deparse(substitute(by)) -> .by
  if (.cols == "NULL")
    sel_name = names(dt[0])
  else{
    eval(
      parse(
        text =
          str_glue("select_dt(dt[0],{.cols}) %>% names() -> sel_name")))
  }

  eval(parse(text = str_glue(
    "res = dt[,lapply(.SD, .func, ...), by = {.by},.SDcols = sel_name]")))
  res[, unique(names(res)), with = FALSE]
}

globalVariables("res")
