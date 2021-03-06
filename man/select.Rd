% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/select.R
\name{select}
\alias{select}
\alias{select_vars}
\alias{select_dt}
\alias{select_mix}
\alias{rename}
\title{Select/rename variables by name}
\usage{
select(.data, ...)

select_vars(.data, ..., rm.dup = TRUE)

select_dt(.data, ..., cols = NULL, negate = FALSE)

select_mix(.data, ..., rm.dup = TRUE)

rename(.data, ...)
}
\arguments{
\item{.data}{A data.table}

\item{...}{One or more unquoted expressions separated by commas.
Very flexible, same as \code{tidyfst::select_dt} and \code{tidyfst::select_mix}.
details find \code{\link[tidyfst]{select_dt}}.}

\item{rm.dup}{Should duplicated columns be removed? Defaults to \code{TRUE}.}

\item{cols}{(Optional)A numeric or character vector.}

\item{negate}{Applicable when regular expression and "cols" is used.
If \code{TRUE}, return the non-matched pattern. Default uses \code{FALSE}.}
}
\value{
A data.table
}
\description{
Choose or rename variables from a data.table.
\code{select()} keeps only the variables you mention;
\code{rename()} keeps all variables.
}
\details{
No copy is made. Once you select or rename a data.table,
they would be changed forever. \code{select_vars} could select across
different data types, names and index. See examples.

\code{select_dt} and \code{select_mix} is the safe mode of
\code{select} and \code{select_vars}, they keey the original copy but
are not memory-efficient when dealing with large data sets.
}
\examples{

  a = as.data.table(iris)
  a \%>\% select(1:3)
  a

  a = as.data.table(iris)
  a \%>\% select_vars(is.factor,"Se")
  a

  a = as.data.table(iris)
  a \%>\% select("Se") \%>\%
    rename(sl = Sepal.Length,
    sw = Sepal.Width)
  a


DT = data.table(a=1:2,b=3:4,c=5:6)
DT
DT \%>\% rename(B=b)

}
\seealso{
\code{\link[tidyfst]{select_dt}}, \code{\link[tidyfst]{rename_dt}}
}
