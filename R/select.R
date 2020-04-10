
#' @title Select/rename variables by name
#' @description  Choose or rename variables from a data.table.
#' \code{select()} keeps only the variables you mention;
#' \code{rename()} keeps all variables.
#' @param .data A data.table
#' @param ... One or more unquoted expressions separated by commas.
#'  Very flexible, same as \code{tidyfst::select_dt} and \code{tidyfst::select_mix}.
#'  details find \code{\link[tidyfst]{select_dt}}.
#' @param rm.dup Should duplicated columns be removed? Defaults to \code{TRUE}.
#' @param cols (Optional)A numeric or character vector.
#' @param negate Applicable when regular expression and "cols" is used.
#' If \code{TRUE}, return the non-matched pattern. Default uses \code{FALSE}.
#' @details No copy is made. Once you select or rename a data.table,
#' they would be changed forever. \code{select_vars} could select across
#' different data types, names and index. See examples.
#' @details \code{select_dt} and \code{select_mix} is the safe mode of
#' \code{select} and \code{select_vars}, they keey the original copy but
#' are not memory-efficient when dealing with large data sets.
#' @seealso \code{\link[tidyfst]{select_dt}}, \code{\link[tidyfst]{rename_dt}}
#' @return A data.table
#' @examples
#'
#'   a = as.data.table(iris)
#'   a %>% select(1:3)
#'   a
#'
#'   a = as.data.table(iris)
#'   a %>% select_vars(is.factor,"Se")
#'   a
#'
#'   a = as.data.table(iris)
#'   a %>% select("Se") %>%
#'     rename(sl = Sepal.Length,
#'     sw = Sepal.Width)
#'   a
#'
#'
#' DT = data.table(a=1:2,b=3:4,c=5:6)
#' DT
#' DT %>% rename(B=b)
#'

#' @rdname select
#' @export
select = function(.data,...){
  select_dt(.data[0],...) %>% names() -> sel_name
  setdiff(names(.data),sel_name) -> rm_name
  if(length(rm_name) == 0) .data
  else .data[,(rm_name):=NULL][]
}

#' @rdname select
#' @export
select_vars = function(.data,...,rm.dup = TRUE){
  dt = .data
  substitute(list(...)) %>%
    lapply(deparse) %>%
    .[-1] %>%
    lapply(function(col)
      eval(parse(text = str_glue("select_dt(dt[0],{col}) %>% names()"))))  %>%
    unlist() -> res

  if(rm.dup) res = unique(res)

  setdiff(names(.data),res) -> rm_name
  .data[,(rm_name):=NULL][]

}

#' @rdname select
#' @export
select_dt = function(.data,...,cols = NULL,negate =FALSE){
  dt = .data
  if(is.null(cols)){
    substitute(list(...)) %>%
      deparse() %>% paste0() %>%
      str_extract("\\(.+\\)") %>%
      str_sub(2,-2)-> dot_string
    if(is.na(dot_string)) dt
    else if(str_detect(dot_string,"^[-,0-9 ]+$"))
      eval(parse(text = str_glue("dt[,c({dot_string})]")))
    else if(str_detect(dot_string,"^[-!]?\"")){
      if(dot_string %like% "^[-!]") {
        dot_string = str_remove(dot_string,"^-|^!")
        negate = TRUE
      }
      str_remove_all(dot_string,"\"") %>%
        str_subset(names(dt),.,negate = negate) %>%
        str_c(collapse = ",") -> dot_string
      eval(parse(text = str_glue("dt[,.({dot_string})]")))
    }
    else if(str_detect(dot_string,"^[-!]?c\\(")|str_count(dot_string,":") == 1)
      eval(parse(text = str_glue("dt[,{dot_string}]")))
    else if(dot_string %like% "^[-!]?is\\.")
      eval(parse(text = str_glue("select_if_dt(dt,{dot_string})")))
    else if(str_detect(dot_string,"^-")){
      dot_string = str_remove_all(dot_string,"-") %>% str_squish()
      if(!str_detect(dot_string,","))
        eval(parse(text = str_glue("dt[,{dot_string} := NULL][]")))
      else{
        str_split(dot_string,",",simplify = TRUE) -> delete_names
        eval(parse(text = str_glue("dt[, {delete_names}:=NULL][]")))
      }
    }
    else eval(parse(text = str_glue("dt[,.({dot_string})]")))
  }
  else {
    if(!negate) dt[,.SD,.SDcols = cols]
    else eval(dt[, .SD,.SDcols = !cols])
  }
}

#' @rdname select
#' @export
select_mix = function(.data,...,rm.dup = TRUE){
  dt = .data

  substitute(list(...)) %>%
    lapply(deparse) %>%
    .[-1] %>%
    lapply(function(col) eval(parse(text = str_glue("select_dt(dt,{col})")))) %>%
    Reduce(f = cbind,x = .) -> res

  if(rm.dup) res = res[,unique(names(res)),with=FALSE]

  res

}

select_if_dt = function(dt,.if){
  if_name = substitute(.if) %>% deparse()
  if(str_detect(if_name,"^[-!]")){
    .if = str_remove(if_name,"[-!]")
    eval(parse(text = str_glue("sel_name = subset(sapply(dt,{.if}),
                        sapply(dt,{.if}) == FALSE) %>% names()")))
  } else
    sel_name = subset(sapply(dt,.if),sapply(dt,.if) == TRUE) %>% names()

  dt[,.SD, .SDcols = sel_name]
}

#' @rdname select
#' @export
rename = function(.data,...){

    substitute(list(...)) %>%
      lapply(deparse) %>%
      .[-1] -> dot_string
    new_names = names(dot_string)
    old_names = as.character(dot_string)

  setnames(.data,old = old_names,new = new_names)[]
}

