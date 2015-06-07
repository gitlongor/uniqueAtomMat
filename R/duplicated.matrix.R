duplicated.matrix = function (x, incomparables = FALSE, MARGIN = 1L, fromLast = FALSE, ...)
{
	if (!is.matrix(x) || !is.atomic(x) || !identical(incomparables, FALSE) || ((nzeroMarg <-MARGIN[1L]!=0L) && MARGIN[1L]!=1L && MARGIN[1L]!=2L) || length(MARGIN)!=1L )
		return(base::duplicated.matrix(x, incomparables, MARGIN, fromLast, ...))
	if (nzeroMarg) {
        .Call(C_dupAtomMat, x, as.integer(MARGIN), as.logical(fromLast))
	}else{
        att=attributes(x); dim(x)=c(as.integer(prod(att$dim)), 1L)
        res=.Call(C_dupAtomMat, x, MARGIN=1L, as.logical(fromLast))
        if(any(att$class=='factor')){
            att$class= setdiff(att$class, c('ordered','factor'))
            if(length(att$class)==0L) att$class=NULL
            att$levels=NULL
        }
        attributes(res)=att
        res
	}
}

unique.matrix=function (x, incomparables = FALSE, MARGIN = 1, fromLast = FALSE, ...)
{
	if (!is.matrix(x) || !is.atomic(x) || !identical(incomparables, FALSE) || (MARGIN[1L]!=1L && MARGIN[1L]!=2L) || length(MARGIN)!=1L )
		return(base::unique.matrix(x, incomparables, MARGIN, fromLast, ...))
	dups=.Call(C_dupAtomMat, x, as.integer(MARGIN), as.logical(fromLast))
	if(MARGIN==1L) x[!dups,,drop=FALSE] else x[,!dups,drop=FALSE]
}

anyDuplicated.matrix=function(x, incomparables = FALSE, MARGIN = 1, fromLast = FALSE, ...)
{
    if (!is.matrix(x) || !is.atomic(x) || !identical(incomparables, FALSE) || ((nzeroMarg <-MARGIN[1L]!=0L) && MARGIN[1L]!=1L && MARGIN[1L]!=2L) || length(MARGIN)!=1L )
        return(base::anyDuplicated.matrix(x, incomparables, MARGIN, fromLast, ...))
    if (nzeroMarg) {
        .Call(C_anyDupAtomMat, x, as.integer(MARGIN), as.logical(fromLast))
    }else{
        dx=dim(x); dim(x)=c(as.integer(prod(dx)), 1L)
        .Call(C_anyDupAtomMat, x, MARGIN=1L, as.logical(fromLast))
    }
}
