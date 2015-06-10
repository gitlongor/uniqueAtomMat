\name{grpDuplicated}
\alias{grpDuplicated}
\alias{grpDuplicated.default}
\alias{grpDuplicated.matrix}
\title{
Grouping by duplicated elelments
}
\description{
\code{grpDuplicated} is a generic function that outputs an integer vector such that input elements share the the output integer value if and only if they are identical to each other; in other words, duplicated elements are \emph{grouped} together with a common integer group number.  
}
\usage{
grpDuplicated(x, incomparables = FALSE, factor=FALSE, ...)
\method{grpDuplicated}{default}(x, incomparables = FALSE, factor=FALSE, fromLast=FALSE, \dots)
\method{grpDuplicated}{matrix}(x, incomparables = FALSE, factor=FALSE, MARGIN = 1, fromLast = FALSE,  \dots)
}
\arguments{
  \item{x}{a vector or matrix of atomic mode \code{"numeric"}, \code{"integer"}, \code{"logical"}, \code{"complex"}, \code{"character"} or \code{"raw"}. When \code{x} is not atomic or when it is not a matrix or vector, the 
  }
  
  \item{incomparables}{a vector of values that cannot be compared, as in \code{\link[base:unique]{base::unique.matrix}}.  Only \code{FALSE} is suppored.  }
  
  \item{factor}{a logical scalar, indicating if the result should be given as an integer vector (default) or a factor. }
  
  \item{fromLast}{a logical scalar indicating if duplication should be considered
    from the last, as in \code{\link[base:unique]{base::unique.matrix}}. }
    
  \item{\dots}{arguments for particular methods.}
  
  \item{MARGIN}{a numeric scalar, the matrix margin to be held fixed, as in \code{\link{apply}}. For \code{unique.matrix}, only \code{MARGIN=1} and \code{MARGIN=2} are allowed.  \code{MARGIN=0} is also allowed. }
}
\details{
For atomic \code{x}, the implementation is based on \code{std::map} in C++ standard template library. 
}
\value{
    If \code{factor = FALSE}, the result is an integer vector with all elements ranging from 1 to \code{k}, where \code{k} is the number of unique elements. 
    
    If \code{factor = TRUE}, the result is a factor, with levels being 1 through \code{k}. 
    
    In either case, the \code{nlevels} attribute of the result will be set to \code{k}. 
}

\author{
Long Qu
}

\seealso{
\code{\link[uniqueAtomMat:unique.matrix]{duplicated.matrix}}
}
\examples{
## prepare example data
set.seed(9992722L, kind="Mersenne-Twister")
trt.original=gl(5,8)[sample(40)]

## equivalent recoding: 
(trt.equivalent=grpDuplicated(trt.original, factor=TRUE))


## equivalent recoding based on a design matrix
x.double=model.matrix(~trt.original)
(trt.equivalent=grpDuplicated(x.double, factor=TRUE))

## check equivalence: should be a permutation matrix:
(table(trt.original, trt.equivalent)!=0)*1

}

\keyword{manip}
\keyword{logic}

