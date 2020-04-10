

#' @title Fast lead/lag for vectors
#' @description Analogous function for \code{lead} and \code{lag} in \pkg{dplyr} by
#' wrapping \pkg{data.table}'s \code{shift}.
#' @param x A vector
#' @param n a positive integer of length 1,
#' giving the number of positions to lead or lag by. Default uses 1
#' @param fill Value to use for padding when the window goes beyond the input length.
#' Default uses \code{NA}
#' @return A vector
#' @seealso \code{\link[dplyr]{lead}},\code{\link[data.table]{shift}}
#' @examples
#' lead(1:5)
#' lag(1:5)
#' lead(1:5,2)
#' lead(1:5,n = 2,fill = 0)

#' @rdname lag_lead
#' @export
lead = function(x,n = 1L,fill = NA){
  shift(x,n,fill,type = "lead")
}

#' @rdname lag_lead
#' @export
lag = function(x,n = 1L,fill = NA){
  shift(x,n,fill,type = "lag")
}
