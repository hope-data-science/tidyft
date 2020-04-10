
#' @title Nest and unnest
#' @description Analogous function for \code{nest} and \code{unnest} in \pkg{tidyr}.
#' \code{unnest} will automatically remove other list-columns except for the
#' target list-columns (which would be unnested later). Also, \code{squeeze} is
#' designed to merge multiple columns into list column.
#' @param .data data.table, nested or unnested
#' @param ... The variables for nest group(for \code{nest}),
#' columns to be nested(for \code{squeeze} and \code{chop}),
#' or column(s) to be unnested(for \code{unnest}).
#' Could recieve anything that \code{\link[tidyfst]{select_dt}} could receive.
#' @param mcols Name-variable pairs in the list, form like
#' \code{list(petal="^Pe",sepal="^Se")}, see example.
#' @return data.table, nested or unnested
#' @details In the \code{nest}, the data would be nested to a column named `ndt`,
#'  which is short for nested data.table.
#' @details The \code{squeeze} would not remove the originial columns.
#' @details The \code{unchop} is the reverse operation of \code{chop}.
#' @details These functions are experiencing the experimental stage, especially
#' the \code{unnest}. If they don't work on some circumtances, try \pkg{tidyr}
#' package.
#' @seealso \code{\link[tidyr]{nest}}, \code{\link[tidyr]{chop}}
#' @references https://www.r-bloggers.com/much-faster-unnesting-with-data-table/
#' @references https://stackoverflow.com/questions/25430986/create-nested-data-tables-by-collapsing-rows-into-new-data-tables
#' @examples
#'
#' mtcars = as.data.table(mtcars)
#' iris = as.data.table(iris)
#'
#' # examples for nest
#'
#' # nest by which columns?
#'  mtcars %>% nest(cyl)
#'  mtcars %>% nest("cyl")
#'  mtcars %>% nest(cyl,vs)
#'  mtcars %>% nest(vs:am)
#'  mtcars %>% nest("cyl|vs")
#'  mtcars %>% nest(c("cyl","vs"))
#'
#' # nest two columns directly
#' iris %>% nest(mcols = list(petal="^Pe",sepal="^Se"))
#'
#' # nest more flexibly
#' iris %>% nest(mcols = list(ndt1 = 1:3,
#'   ndt2 = "Pe",
#'   ndt3 = Sepal.Length:Sepal.Width))
#'
#' # examples for unnest
#' # unnest which column?
#'  mtcars %>% nest("cyl|vs") %>%
#'    unnest(ndt)
#'  mtcars %>% nest("cyl|vs") %>%
#'    unnest("ndt")
#'
#' df <- data.table(
#'   a = list(c("a", "b"), "c"),
#'   b = list(c(TRUE,TRUE),FALSE),
#'   c = list(3,c(1,2)),
#'   d = c(11, 22)
#' )
#'
#' df
#' df %>% unnest(a)
#' df %>% unnest(2)
#' df %>% unnest("c")
#' df %>% unnest(cols = names(df)[3])
#'
#' # You can unnest multiple columns simultaneously
#' df %>% unnest(1:3)
#' df %>% unnest(a,b,c)
#' df %>% unnest("a|b|c")
#'
#' # examples for squeeze
#' # nest which columns?
#' iris %>% squeeze(1:2)
#' iris %>% squeeze("Se")
#' iris %>% squeeze(Sepal.Length:Petal.Width)
#'
#' # examples for chop
#' df <- data.table(x = c(1, 1, 1, 2, 2, 3), y = 1:6, z = 6:1)
#' df %>% chop(y,z)
#' df %>% chop(y,z) %>% unchop(y,z)

#' @rdname nest
#' @export

# nest by which columns?

nest = function(.data,...,mcols = NULL){

  dt = .data

  if(substitute(mcols) %>% deparse() == "NULL") nest_by(dt,...)
  else{
    name_list = substitute(mcols)%>%
      lapply(deparse) %>%
      .[-1] %>%
      lapply(function(x)
        eval(parse(text = str_glue("names(select_dt(dt[0],{x}))"))))
    group_names = setdiff(names(dt),unique(unlist(name_list)))
    lapply(name_list,function(x) c(group_names,x)) %>%
      lapply(function(x) nest_by(dt,cols = group_names)) -> list_table
    for(i in seq_along(list_table)){
      list_table[[i]] = setnames(list_table[[i]],
                                 old = "ndt",new = names(list_table[i]))
    }

    Reduce(f = merge, x = list_table)
  }

}



nest_by = function(.data,...){
  dt = .data
  dt[0] %>% select_dt(...) %>% names() %>%
    str_c(collapse = ",")-> group
  eval(parse(text = str_glue("dt[,.(ndt = list(.SD)),by = .({group})]")))
}

#' @rdname nest
#' @export

# unnest which column(s)?

unnest = function(.data,...){
  dt = .data
  col_names = dt[0] %>% select_dt(...) %>% names()
  if(length(col_names) == 1) unnest_col(dt,...)
  else
    lapply(col_names,function(x) unnest_col(dt,cols = x)) %>%
    Reduce(x = ., f = function(x,y) merge(x,y))
}

unnest_col = function(.data,...){
  dt = .data
  col_name = dt[0] %>% select_dt(...) %>% names()
  lapply(dt,class) -> dt_class
  names(subset(dt_class,dt_class != "list")) -> valid_col_names
  if(!col_name %chin% names(dt)) stop("The column does not exist.")
  valid_col_names %>%
    str_c(collapse = ",") %>%
    str_c("list(",.,")") -> group_name
  dt[[col_name]][[1]] -> first_element
  if(is.vector(first_element))
    eval(parse(text = str_glue("dt[,.({col_name} = unlist({col_name},recursive = FALSE)),by = {group_name}]")))
  else
    eval(parse(text = str_glue("dt[,unlist({col_name},recursive = FALSE),by = {group_name}]")))
}


#' @rdname nest
#' @export

# nest which columns?

squeeze = function(.data,...){
  dt = .data
  dt %>% select_dt(...) %>%
    setNames(NULL) %>%
    apply(1,list) %>%
    lapply(unlist)-> ndt
  dt[,ndt := ndt][]
}

#' @rdname nest
#' @export
chop = function(.data,...){
  dt = .data
  dt[0] %>% select_dt(...) %>% names() -> data_cols
  setdiff(names(dt),data_cols) -> group_cols
  group_cols %>%
    str_c(collapse = ",") %>%
    str_c("list(",.,")") -> group_names
  eval(parse(text = str_glue("dt[,lapply(.SD,list),
                             by = {group_names}]")))
}

#' @rdname nest
#' @export
unchop = function(.data,...){
  dt = .data
  col_names = dt[0] %>% select_dt(...) %>% names()
  group_names = setdiff(names(dt),col_names)
  if(length(col_names) == 1) unnest_col(dt,...)
  else
    lapply(col_names,function(x) unnest_col(dt,cols = x)) %>%
    Reduce(x = ., f = function(x,y) cbind(x,y)) %>%
    .[,unique(names(.)),with=FALSE]
}



