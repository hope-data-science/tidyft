
#' @title Count observations by group
#' @description Analogous function for \code{count} and \code{add_count} in \pkg{dplyr}.
#'
#' @param .data data.table
#' @param ... variables to group by.
#' @param sort logical. If TRUE result will be sorted in desending order by resulting variable.
#' @param name character. Name of resulting variable. Default uses "n".
#'
#' @return data.table
#' @examples
#' a = as.data.table(mtcars)
#' count(a,cyl)
#' count(a,cyl,sort = TRUE)
#' a
#'
#' b = as.data.table(iris)
#' b %>% add_count(Species,name = "N")
#' b

#' @rdname count
#' @export

count = function(.data,...,sort = FALSE,name = "n"){
  select_dt(.data[0],...) %>% names() -> sel_name
  if(sort) setnames(.data[,.N,by = sel_name][order(-N)],old = "N",new = name)[]
  else setnames(.data[,.N,by = sel_name] ,old = "N",new = name)[]
}

#' @rdname count
#' @export

add_count = function(.data,...,name = "n"){
  select_dt(.data[0],...) %>% names() -> sel_name
  .data[,(name):=.N,by = sel_name][]
}

globalVariables("N")
