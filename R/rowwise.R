
#' @title Computation by rows
#' @description Compute on a data frame a row-at-a-time.
#' This is most useful when a vectorised function doesn't exist.
#' Only mutate and summarise are supported so far.
#' @param .data A data.table
#' @param ... Name-value pairs of expressions
#' @return A data.table
#' @seealso \code{\link[dplyr]{rowwise}}
#' @examples
#' # without rowwise
#' df <- data.table(x = 1:2, y = 3:4, z = 4:5)
#' df %>% mutate(m = mean(c(x, y, z)))
#' # with rowwise
#' df <- data.table(x = 1:2, y = 3:4, z = 4:5)
#' df %>% rowwise_mutate(m = mean(c(x, y, z)))
#'
#'
#' # # rowwise is also useful when doing simulations
#' params = fread(" sim n mean sd
#'   1  1     1   1
#'   2  2     2   4
#'   3  3    -1   2")
#'
#' params %>%
#'   rowwise_summarise(sim,z = rnorm(n,mean,sd))
#'

#' @rdname rowwise
#' @export

rowwise_mutate = function(.data,...){
  .data = .data
  eval(substitute(.data[,`:=`(...),by = seq(nrow(.data))][]))

}

#' @rdname rowwise
#' @export

rowwise_summarise = function(.data,...){
  .data = .data
  eval(substitute(.data[,.(...),
                        by = .(ID_=seq(nrow(.data)))][,ID_:=NULL][]))
}








