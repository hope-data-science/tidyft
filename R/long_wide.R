
#' @title Pivot data between long and wide
#' @description Fast table pivoting from long to wide and from wide to long.
#' These functions are supported by \code{dcast.data.table} and \code{melt.data.table}
#' from \pkg{data.table}.
#' @param .data A data.table
#' @param ... Columns for unchanged group. Flexible, see examples.
#' @param name Name for the measured variable names column.
#' @param value Name for the data values column(s).
#' @param na.rm If \code{TRUE}, \code{NA} values will be removed from the molten data.
#' @param fun Should the data be aggregated before casting?
#' Defaults to \code{NULL}, which uses \code{length} for aggregation.
#' If a function is provided, with aggregated by this function.
#' @param fill Value with which to fill missing cells. Default uses \code{NA}.
#' @return A data.table
#' @seealso \code{\link[tidyfst]{longer_dt}},\code{\link[tidyfst]{wider_dt}}
#' @examples
#'
#' stocks <- data.table(
#'   time = as.Date('2009-01-01') + 0:9,
#'   X = rnorm(10, 0, 1),
#'   Y = rnorm(10, 0, 2),
#'   Z = rnorm(10, 0, 4)
#' )
#'
#' stocks %>% longer(time)
#' stocks %>% longer(-(2:4)) # same
#' stocks %>% longer(-"X|Y|Z") # same
#' long_stocks = longer(stocks,"ti") # same as above except for assignment
#'
#' long_stocks %>% wider(time,name = "name",value = "value")
#'
#' # the unchanged group could be missed if all the rest will be used
#' long_stocks %>% wider(name = "name",value = "value")
#'

#' @rdname long_wide
#' @export
longer = function(.data,...,
                     name = "name",
                     value = "value",
                     na.rm = FALSE){
  dt = .data
  group = dt[0] %>% select_vars(...) %>% names()
  melt(data = dt,
       id = group,
       variable.name = name,
       value.name = value,
       na.rm = na.rm)
}


#' @rdname long_wide
#' @export
wider = function(.data,
                    ...,
                    name,
                    value = NULL,
                    fun = NULL,
                    fill = NA){
  dt = .data
  group = dt[0] %>% select_vars(...) %>% names() %>% str_c(collapse = "+")
  if(group == "") group = "..."
  if(is.null(value)) value = "."

  substitute(fun) %>% deparse() -> fun
  if(fun == "list"){
    call_string = str_glue("dcast(dt,{group}~{name}, fun.aggregate = list,
                          value.var = value,fill = {fill}) %>%
                           unchop_dt(is.list)")
  }else{
    call_string = str_glue("dcast(dt,{group}~{name}, fun.aggregate = {fun},
                          value.var = value,fill = {fill})")
  }
  eval(parse(text = call_string))
}

