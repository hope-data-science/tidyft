
#' @title Change column order
#' @description Use `relocate()` to change column positions,
#'  using the same syntax as `select()`. Check similar function
#'  as `relocate()` in \pkg{dplyr}.
#' @param .data A data.table
#' @param ... Columns to move
#' @param how The mode of movement, including "first","last","after","before".
#' Default uses "first".
#' @param where Destination of columns selected by \code{...}.
#' Applicable for "after" and "before" mode.
#' @return A data.table with rearranged columns.
#' @details  Once you relocate the columns, the order changes forever.

#' @examples
#' df <- data.table(a = 1, b = 1, c = 1, d = "a", e = "a", f = "a")
#' df
#' df %>% relocate(f)
#' df %>% relocate(a,how = "last")
#'
#' df %>% relocate(is.character)
#' df %>% relocate(is.numeric, how = "last")
#' df %>% relocate("[aeiou]")
#'
#' df %>% relocate(a, how = "after",where = f)
#' df %>% relocate(f, how = "before",where = a)
#' df %>% relocate(f, how = "before",where = c)
#' df %>% relocate(f, how = "after",where = c)
#'
#' df2 <- data.table(a = 1, b = "a", c = 1, d = "a")
#' df2 %>% relocate(is.numeric,
#'                     how = "after",
#'                     where = is.character)
#' df2 %>% relocate(is.numeric,
#'                     how="before",
#'                     where = is.character)

#' @export

relocate = function(.data,...,
                       how= "first",
                       where = NULL){
  dt = .data

  dt[0] %>% select_dt(...) %>% names() -> sel_names
  names(dt[0]) %>% setdiff(sel_names) -> rest_names
  dt[0] %>% select_dt(cols = rest_names) -> rest_dt
  substitute(where) %>% deparse() -> where_n
  if(str_detect(where_n,"^\"|^'")) where_n = where

  if(how == "first") c(sel_names,rest_names) -> name_order
  else if(how == "last") c(rest_names,sel_names) -> name_order
  else if(where_n == "NULL") stop("The `where` parameter should be provided.")
  else
  {
    if(where_n %like% "^is\\."){
      eval(parse(text = str_glue("dt1 = select_dt(rest_dt,{where_n}) %>% names")))
      eval(parse(text = str_glue("dt2 = select_dt(rest_dt,-{where_n}) %>% names")))
      if(how == "after") c(dt1,sel_names,dt2) -> name_order
      else c(sel_names,dt1,dt2) -> name_order
    }
    else{
      chmatch(where_n,rest_names) -> position
      if(how == "after"){
        if(position < length(rest_names))
          c(rest_names[1:position], sel_names,
            rest_names[(position+1):length(rest_names)]) -> name_order
        else c(rest_names,sel_names) -> name_order
      }
      else if(how == "before"){
        if(position > 1)
          c(rest_names[1:(position-1)],sel_names,
            rest_names[position:length(rest_names)]) -> name_order
        else c(sel_names,rest_names) -> name_order
      }
      else stop("The `how` parameter could not be recognized.")
    }
  }
  setcolorder(dt,neworder = name_order)[]
}

globalVariables(c("dt1","dt2"))




