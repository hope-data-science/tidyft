
#' @title Unite multiple columns into one by pasting strings together
#' @description Convenience function to paste together multiple columns into one.
#' Analogous to \code{tidyr::unite}.
#' @param .data A data frame.
#' @param united_colname The name of the new column, string only.
#' @param ... A selection of columns. If want to select all columns,
#' pass "" to the parameter. See example.
#' @param sep Separator to use between values.
#' @param remove If \code{TRUE}, remove input columns from output data frame.
#' @param na2char If \code{FALSE}, missing values would be merged into \code{NA},
#' otherwise \code{NA} is treated as character "NA". This is different from
#' \pkg{tidyr}.
#' @return A data.table
#' @seealso \code{\link[tidyr]{unite}},\code{\link[tidyft]{separate}}
#' @examples
#' df <- CJ(x = c("a", NA), y = c("b", NA))
#' df
#'
#' # Treat missing value as NA, default
#' df %>% unite("z", x:y, remove = FALSE)
#' # Treat missing value as character "NA"
#' df %>% unite("z", x:y, na2char = TRUE, remove = FALSE)
#' # the unite has memory, "z" would not be removed in new operations
#' # here we remove the original columns ("x" and "y")
#' df %>% unite("xy", x:y,remove = TRUE)
#'
#' # Select all columns
#' iris %>% as.data.table %>% unite("merged_name","")

#' @export
unite = function(.data,united_colname,...,
                    sep = "_",remove = FALSE,
                    na2char = FALSE){
  dt = .data

  dt %>%
    select_dt(...) -> selected_cols
  paste_fun = ifelse(na2char,paste,str_c)
  Reduce(function(x,y) paste_fun(x,y,sep = sep),selected_cols) -> new_col

  if(!remove) dt[,(united_colname):=new_col][]
  else
    dt[,names(selected_cols):=NULL][,(united_colname):=new_col][]

}




