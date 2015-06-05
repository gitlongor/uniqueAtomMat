\name{unique.matrix}
\alias{unique.matrix}
\alias{duplicated.matrix}
\alias{anyDuplicated.matrix}

\title{Finding Unique or Duplicated Rows or Columns for Atomic Matrices}
\description{
  \code{unique.matrix} returns a matrix with duplicated rows (or columns) removed. 
  
  \code{duplicated.matrix} returns a logical vector indicating which rows (or columns) are duplicated. 

  \code{anyDuplicated.matrix} is an efficient shortcut for \code{any(duplicated.matrix)}.
}

\usage{
\method{unique}{matrix}(x, incomparables = FALSE, MARGIN = 1,
       fromLast = FALSE, \dots)
\method{duplicated}{matrix}(x, incomparables = FALSE, MARGIN = 1,
           fromLast = FALSE, \dots)
\method{anyDuplicated}{matrix}(x, incomparables = FALSE,
           MARGIN = 1, fromLast = FALSE, \dots)
}


\arguments{
  \item{x}{a vector or a data frame or an array or \code{NULL}.}
  \item{incomparables}{a vector of values that cannot be compared.
    \code{FALSE} is a special value, meaning that all values can be
    compared, and may be the only value accepted for methods other than
    the default.  It will be coerced internally to the same type as
       \code{x}.}
  \item{fromLast}{logical indicating if duplication should be considered
    from the last, i.e., the last (or rightmost) of identical elements will
    be kept.  This only matters for \code{\link{names}} or
    \code{\link{dimnames}}.}
  \item{\dots}{arguments for particular methods.}
  \item{MARGIN}{the matrix margin to be held fixed, as in \code{\link{apply}}.}
}

\details{

  The array method calculates for each element of the dimension
  specified by \code{MARGIN} if the remaining dimensions are identical
  to those for an earlier element (in row-major order).  This would most
  commonly be used for matrices to find unique rows (the default) or columns
  (with \code{MARGIN = 2}).

  Note that unlike the Unix command \code{uniq} this omits
  \emph{duplicated} and not just \emph{repeated} elements/rows.  That
  is, an element is omitted if it is equal to any previous element and
  not just if it is equal the immediately previous one.  (For the
  latter, see \code{\link{rle}}).

  Missing values are regarded as equal, but \code{NaN} is not equal to
  \code{NA_real_}.  Character strings are regarded as equal if they are
  in different encodings but would agree when translated to UTF-8.

  Values in \code{incomparables} will never be marked as duplicated.
  This is intended to be used for a fairly small set of values and will
  not be efficient for a very large set.

  When used on a data frame with more than one column, or an array or
  matrix when comparing dimensions of length greater than one, this
  tests for identity of character representations.  This will
  catch people who unwisely rely on exact equality of floating-point
  numbers!

  Character strings will be compared as byte sequences if any input is
  marked as \code{"bytes"} (see \code{\link{Encoding}}).


 These are generic functions with methods for vectors (including
  lists), data frames and arrays (including matrices).

  For the default methods, and whenever there are equivalent method
  definitions for \code{duplicated} and \code{anyDuplicated},
  \code{anyDuplicated(x, ...)} is a \dQuote{generalized} shortcut for
  \code{any(duplicated(x, ...))}, in the sense that it returns the
  \emph{index} \code{i} of the first duplicated entry \code{x[i]} if
  there is one, and \code{0} otherwise.  Their behaviours may be
  different when at least one of \code{duplicated} and
  \code{anyDuplicated} has a relevant method.

  \code{duplicated(x, fromLast = TRUE)} is equivalent to but faster than
  \code{rev(duplicated(rev(x)))}.

  The data frame method works by pasting together a character
  representation of the rows separated by \code{\\r}, so may be imperfect
  if the data frame has characters with embedded carriage returns or
  columns which do not reliably map to characters.

  The array method calculates for each element of the sub-array
  specified by \code{MARGIN} if the remaining dimensions are identical
  to those for an earlier (or later, when \code{fromLast = TRUE}) element
  (in row-major order).  This would most commonly be used to find
  duplicated rows (the default) or columns (with \code{MARGIN = 2}).
  Note that \code{MARGIN = 0} returns an array of the same
  dimensionality attributes as \code{x}.

  Missing values are regarded as equal, but \code{NaN} is not equal to
  \code{NA_real_}.

  Values in \code{incomparables} will never be marked as duplicated.
  This is intended to be used for a fairly small set of values and will
  not be efficient for a very large set.

  When used on a data frame with more than one column, or an array or
  matrix when comparing dimensions of length greater than one, this
  tests for identity of character representations.  This will
  catch people who unwisely rely on exact equality of floating-point
  numbers!

  Character strings will be compared as byte sequences if any input is
  marked as \code{"bytes"} (see \code{\link{Encoding}}).

  Except for factors, logical and raw vectors the default \code{nmax = NA} is
  equivalent to \code{nmax = length(x)}.  Since a hash table of size
  \code{8*nmax} bytes is allocated, setting \code{nmax} suitably can
  save large amounts of memory.  For factors it is automatically set to
  the smaller of \code{length(x)} and the number of levels plus one (for
  \code{NA}).  If \code{nmax} is set too small there is liable to be an
  error: \code{nmax = 1} is silently ignored.

  \link{Long vectors} are supported for the default method of
  \code{duplicated}, but may only be usable if \code{nmax} is supplied.
  
}
\value{
  For a vector, an object of the same type of \code{x}, but with only
  one copy of each duplicated element.  No attributes are copied (so
  the result has no names).

  For a data frame, a data frame is returned with the same columns but
  possibly fewer rows (and with row names from the first occurrences of
  the unique rows).

  A matrix or array is subsetted by \code{[, drop = FALSE]}, so
  dimensions and dimnames are copied appropriately, and the result
  always has the same number of dimensions as \code{x}.



    \code{duplicated()}:
    For a vector input, a logical vector of the same length as
    \code{x}.  For a data frame, a logical vector with one element for
    each row.  For a matrix or array, and when \code{MARGIN = 0}, a
    logical array with the same dimensions and dimnames.

    \code{anyDuplicated()}: an integer or real vector of length one with
    value the 1-based index of the first duplicate if any, otherwise
    \code{0}.

}
\section{Warning}{
  Using this for lists is potentially slow, especially if the elements
  are not atomic vectors (see \code{\link{vector}}) or differ only
  in their attributes.  In the worst case it is \eqn{O(n^2)}.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\seealso{
  \code{\link[base]{duplicated}}, \code{\link[base]{unique}}
}
\examples{
x <- c(3:5, 11:8, 8 + 0:5)
(ux <- unique(x))
(u2 <- unique(x, fromLast = TRUE)) # different order
stopifnot(identical(sort(ux), sort(u2)))

length(unique(sample(100, 100, replace = TRUE)))
## approximately 100(1 - 1/e) = 63.21

unique(iris)




x <- c(9:20, 1:5, 3:7, 0:8)
## extract unique elements
(xu <- x[!duplicated(x)])
## similar, same elements but different order:
(xu2 <- x[!duplicated(x, fromLast = TRUE)])

## xu == unique(x) but unique(x) is more efficient
stopifnot(identical(xu,  unique(x)),
          identical(xu2, unique(x, fromLast = TRUE)))

duplicated(iris)[140:143]

duplicated(iris3, MARGIN = c(1, 3))
anyDuplicated(iris) ## 143
\dontshow{% array & data.frame methods:
stopifnot(identical(anyDuplicated(iris), 143L),
          identical(anyDuplicated(iris3, MARGIN = c(1, 3)), 143L))
}
anyDuplicated(x)
anyDuplicated(x, fromLast = TRUE)


}
\keyword{manip}
\keyword{logic}
