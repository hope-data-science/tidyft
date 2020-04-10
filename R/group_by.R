
#' @title Group by one or more variables
#' @description Most data operations are done on groups defined by variables.
#' \code{group_by} will group the data.table by selected variables (setting
#' them as keys), and arrange them in ascending order.
#' \code{group_exe} could do computations by group, it receives an object
#' returned by \code{group_by}.
#' @param .data A data.table
#' @param ... For \code{group_by}:Variables to group by.
#' For \code{group_exe}:Any data manipulation arguments that
#'  could be implemented on a data.table.
#' @param x A data.table
#' @return A data.table with keys
#' @details For \code{mutate} and \code{summarise}, it is recommended to
#'  use the innate "by" parameter, which is faster. Once the data.table is
#'  grouped, the order is changed forever.
#' @details \code{groups()} could return a character vector of specified groups.
#' @details \code{ungroup()} would delete the keys in data.table.
#' @examples
#' a = as.data.table(iris)
#' a
#' a %>%
#'   group_by(Species) %>%
#'   group_exe(
#'     head(3)
#'   )
#' groups(a)
#' ungroup(a)
#' groups(a)

#' @rdname group
#' @export
group_by = function(.data, ...){
  .data[0] %>% select_dt(...) %>% names() -> sel_name
  setkeyv(.data,cols = sel_name)[]
}

#' @rdname group
#' @export
group_exe = function(.data,...){
  by_deparse = deparse(substitute(by))
  if(!is.null(key(.data))) {
    dt_keys = str_c(key(.data), collapse = ",") %>%
      str_c(".(", ., ")")
    eval(parse(text = str_glue("group_dt(.data,by = {dt_keys},...)")))
  }
  else stop("Group(s) not specified.")
}

#' @rdname group
#' @export
groups = data.table::key

#' @rdname group
#' @export
ungroup = function(x){
  setkey(x,NULL)
}


group_dt = function(.data,by = NULL,...){
  dt = .data

  by = substitute(by)
  deparse(by) -> by_deparse
  if(by_deparse == "NULL") stop("Please provide the group(s).")
  else if(!str_detect(by_deparse,"^\\.|^list\\("))
    by_deparse %>%
    str_c(".(",.,")") -> by_deparse

  substitute(list(...)) %>%
    deparse() %>%
    str_c(collapse = "") %>%
    str_squish() %>%
    str_extract("\\(.+\\)") %>%
    str_sub(2,-2) -> dot_string

  dot_string %>%
    strsplit("%>%") %>%
    unlist() %>%
    str_squish() %>%
    lapply(dot_convert) %>%
    str_c("[,",.,",","by = {by_deparse}]") %>%
    str_c(collapse = "") %>%
    str_c("dt",.) -> to_eval


  eval(parse(text = str_glue(to_eval)))
}

dot_convert = function(string){
  if(str_detect(string,",\\s*\\.\\s*,"))
    str_replace(string,",\\s*\\.\\s*,",",.SD,") -> string
  else if(str_detect(string,",s*\\.s*\\)"))
    str_replace(string,",s*\\.s*\\)",",.SD\\)") -> string
  else str_replace(string,"\\(","\\(.SD,") -> string
  string
}


