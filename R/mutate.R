
#' @title Create or transform variables
#' @description \code{mutate()} adds new variables and preserves existing ones;
#' \code{transmute()} adds new variables and drops existing ones.
#'  Both functions preserve the number of rows of the input.
#'   New variables overwrite existing variables of the same name.
#' @description \code{mutate_when} integrates \code{mutate} and \code{case_when}
#' in \pkg{dplyr} and make a new tidy verb for data.table. \code{mutate_vars} is
#'  a super function to do updates in specific columns according to conditions.
#' @param .data A data.table
#' @param ... Name-value pairs of expressions
#' @param by (Optional) Mutate by what group?
#' @param when An object which can be coerced to logical mode
#' @param .cols Any types that can be accepted by \code{\link[tidyfst]{select_dt}}.
#' @param .func Function to be run within each column, should return a value or
#' vectors with same length.
#' @return A data.table
#' @description If you mutate a data.table, it is forever changed.
#' No copies made, which is efficient, but should be used with caution.
#' If you still want the keep the original data.table, use
#'  \code{\link[data.table]{copy}} first.
#' @examples
#'   # Newly created variables are available immediately
#'   a = as.data.table(mtcars)
#'   copy(a) %>% mutate(cyl2 = cyl * 2)
#'   a
#'
#'   # change forever
#'   a %>% mutate(cyl2 = cyl * 2)
#'   a
#'
#'   # You can also use mutate() to remove variables and
#'   # modify existing variables
#'   a %>% mutate(
#'     mpg = NULL,
#'     disp = disp * 0.0163871 # convert to litres
#'   )
#'
#'   a %>% transmute(cyl,one = 1)
#'   a
#'
#'
#'  iris[3:8,] %>%
#'    as.data.table() %>%
#'    mutate_when(Petal.Width == .2,
#'                one = 1,Sepal.Length=2)
#'
#'  iris[3:8,] %>%
#'    as.data.table() %>%
#'    mutate_vars("Pe",scale)
#'

#' @rdname mutate
#' @export
mutate = function(.data,...,by){
  .data = .data
  eval(substitute(.data[, `:=`(...), by][]))
}

#' @rdname mutate
#' @export

transmute = function(.data,...,by){
  .data = .data
  eval(substitute(.data[,.(...),by][]))
}

#' @rdname mutate
#' @export

mutate_when = function(.data,when,...,by){
  dt = .data
  eval(substitute(dt[when,`:=`(...),by][]))
}

#' @rdname mutate
#' @export
mutate_vars = function(.data,.cols = NULL,.func,...,by){
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
    "dt[,(sel_name) := lapply(.SD,.func,...), by = {.by},.SDcols = sel_name][]")))

}





