
#' @title Fill in missing values with previous or next value
#' @description Fills missing values in selected columns using the next or previous entry.
#' @param .data A data.table
#' @param ... 	A selection of columns.
#' @param direction Direction in which to fill missing values.
#'  Currently either "down" (the default), "up".
#' @param x A vector.
#' @return A filled data.table
#' @details \code{fill} is filling data.table's columns,
#'  \code{shift_fill} is filling any vectors.
#' @examples
#'
#' df <- data.table(Month = 1:12, Year = c(2000, rep(NA, 10),2001))
#' df
#' df %>% fill(Year)
#'
#' df <- data.table(Month = 1:12, Year = c(2000, rep(NA, 10),2001))
#' df %>% fill(Year,direction = "up")
#'

#' @rdname fill
#' @export
fill = function(.data,...,direction = "down"){
  dt = .data

  if(substitute(list(...)) %>% deparse() == "list()")
    update_cols <- names(dt)
  else
    dt[0] %>%
      select_dt(...) %>%
      names() -> update_cols

  dt[0] %>% select_dt(cols = update_cols) %>%
    select_dt(is.numeric) %>%
    names()-> num_cols
  setdiff(update_cols,num_cols) -> nnum_cols

  if(length(num_cols) > 0) {
    if(direction == "down") setnafill(dt,type = "locf",cols = num_cols)
    else if(direction == "up") setnafill(dt,type = "nocb",cols = num_cols)
  }
  if(length(nnum_cols) > 0){
    dt[,(nnum_cols) := lapply(.SD, shift_fill,direction = direction),
       .SDcols = nnum_cols]
  }

  dt[]
}

#' @rdname fill
#' @export
shift_fill = function(x,direction = "down"){
  if(direction == "down") type = "lag"
  else if(direction == "up") type = "lead"
  repeat{
    x = fcoalesce(x,shift(x,type = type))
    if(!anyNA(x)) break
  }
  x
}
