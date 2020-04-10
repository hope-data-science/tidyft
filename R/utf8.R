


#' @title Use UTF-8 for character encoding in a data frame
#' @description \code{fread} from \pkg{data.table} could not recognize the encoding
#' and return the correct form, this could be unconvenient for text mining tasks. The
#' \code{utf8-encoding} could use "UTF-8" as the encoding to override the current
#' encoding of characters in a data frame.
#' @param .data A data.frame.
#' @param .cols The columns you want to convert, usually a character column.
#' @return A data.table with characters in UTF-8 encoding
#' @examples
#' iris %>%
#'   as.data.table() %>%
#'   utf8_encoding(Species)  # could also use `is.factor`

#' @export

utf8_encoding = function(.data,.cols){
  .data = .data

  eval(substitute(
    mutate_vars(.data,.cols,str_conv,encoding = "UTF-8")
  ))

}



