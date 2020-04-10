#' @title "Uncount" a data frame
#' @description Performs the opposite operation to `dplyr::count()`,
#'  duplicating rows according to a weighting variable (or expression).
#'  Analogous to `tidyr::uncount`.
#' @param .data A data.frame
#' @param wt A vector of weights.
#' @param .remove Should the column for \code{weights} be removed?
#' Default uses \code{TRUE}.
#' @return A data.table
#' @seealso \code{\link[dplyr]{count}}, \code{\link[tidyr]{uncount}}
#' @examples
#'
#' df <- data.table(x = c("a", "b"), n = c(1, 2))
#' uncount(df, n)
#' uncount(df,n,FALSE)
#' @export

uncount = function(.data,wt,.remove = TRUE){
  dt = .data
  if(.remove) eval(substitute(dt[rep(1:.N,wt)][,wt:=NULL][]))
  else eval(substitute(dt[rep(1:.N,wt)]))
}

