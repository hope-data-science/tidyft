
#' @title Drop or delete data by rows or columns
#' @description \code{drop_na} drops entries by specified columns.
#' \code{delete_na} deletes rows or columns with too many NAs.
#' @param .data A data.table
#' @param ... Colunms to be dropped or deleted.
#' @param MARGIN 1 or 2. 1 for deleting rows, 2 for deleting columns.
#' @param n If number (proportion) of NAs is larger than or equal to "n",
#' the columns/rows would be deleted. When smaller than 1, use as proportion.
#' When larger or equal to 1, use as number.
#' @return A data.table
#'
#' @examples
#' x = data.table(x = c(1, 2, NA, 3), y = c(NA, NA, 4, 5),z = rep(NA,4))
#' x
#' x %>% delete_na(2,0.75)
#'
#' x = data.table(x = c(1, 2, NA, 3), y = c(NA, NA, 4, 5),z = rep(NA,4))
#' x %>% delete_na(2,0.5)
#'
#' x = data.table(x = c(1, 2, NA, 3), y = c(NA, NA, 4, 5),z = rep(NA,4))
#' x %>% delete_na(2,0.24)
#'
#' x = data.table(x = c(1, 2, NA, 3), y = c(NA, NA, 4, 5),z = rep(NA,4))
#' x %>% delete_na(2,2)
#'
#' x = data.table(x = c(1, 2, NA, 3), y = c(NA, NA, 4, 5),z = rep(NA,4))
#' x %>% delete_na(1,0.6)
#' x = data.table(x = c(1, 2, NA, 3), y = c(NA, NA, 4, 5),z = rep(NA,4))
#' x %>% delete_na(1,2)
#'
#'

#' @rdname drop_delete_na
#' @export
drop_na = function(.data,...){
  dt = .data
  if(substitute(list(...)) %>% deparse() == "list()")
    dot_string <- NULL
  else
    dt[0] %>%
      select_dt(...) %>%
      names() -> dot_string
  if(is.null(dot_string)) na.omit(dt)
  else {
    index = str_glue("!is.na({dot_string})") %>% paste0(collapse = " & ")
    eval(parse(text = str_glue("dt[{index}]")))
  }
}

#' @rdname drop_delete_na
#' @export
delete_na = function(.data,MARGIN,n){
  dt = .data
  if(MARGIN == 1){
    if(n > 0 & n < 1) n = ncol(.data) * n
    which(rowSums(is.na(dt)) < n) -> to_save
    .data[to_save]
  }else if(MARGIN == 2){
    if(n > 0 & n < 1) n = nrow(.data) * n
    which(colSums(is.na(dt)) < n) -> to_save
    select(.data,cols = to_save)
  }else stop("Inputs are invalid.")
}


