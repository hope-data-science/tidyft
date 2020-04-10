
#' @title Convenient file reader
#' @description A wrapper of \code{fread} in \pkg{data.table}.
#' Highlighting the encoding.
#' @param path File name in working directory, path to file.
#' @param utf8 Should "UTF-8" used as the encoding? (Defaults to \code{FALSE})
#' @param ... Other parameters passed to \code{data.table::fread}.
#' @return A data.table
#' @export

read_csv = function(path,utf8 = FALSE,...){
  fread(file = path,encoding = ifelse(utf8,"UTF-8","unknown"))
}

