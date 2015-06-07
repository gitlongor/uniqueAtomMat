
## prepare test data: 
set.seed(9992722L, kind="Mersenne-Twister")
x.double=model.matrix(~gl(5,8))[sample(40), ]
x.integer=as.integer(x.double); attributes(x.integer)=attributes(x.double)
x.factor=as.factor(x.integer); dim(x.factor)=dim(x.integer); dimnames(x.factor)=dimnames(x.integer)
x.ordered=as.factor(x.integer); dim(x.ordered)=dim(x.integer); dimnames(x.ordered)=dimnames(x.integer)
x.logical=as.logical(x.double); attributes(x.logical)=attributes(x.double)
x.character=as.character(x.double); attributes(x.character)=attributes(x.double)
x.complex=as.complex(x.double); attributes(x.complex)=attributes(x.double)
x.raw=as.raw(x.double); attributes(x.raw)=attributes(x.double)


# further testing
nr=nrow(x.double) ; nc=ncol(x.double); n=nr*nc;
for(testi in 0:100){
    xna.double=x.double; 
    if(testi==0){
        xna.double[1,2]=xna.double[2,3]=xna.double[3,3]=NA_real_
        xna.double[4,1]=NaN
    }else{
        for(j in seq_len(max(2, round(n/4)))) xna.double[sample(nr,1L), sample(nc,1L)]=NA_real_
        for(j in seq_len(max(2, round(n/4))))  xna.double[sample(nr,1L), sample(nc,1L)]=NaN
    }
    xna.integer=as.integer(xna.double); attributes(xna.integer)=attributes(xna.double)
    xna.factor=as.factor(xna.integer); dim(xna.factor)=dim(xna.integer); dimnames(xna.factor)=dimnames(xna.integer)
    xna.ordered=as.ordered(xna.integer); dim(xna.ordered)=dim(xna.integer); dimnames(xna.ordered)=dimnames(xna.integer)
    xna.logical=as.logical(xna.double); attributes(xna.logical)=attributes(xna.double)
    xna.character=as.character(xna.double); attributes(xna.character)=attributes(xna.double)
    xna.complex=as.complex(xna.double); attributes(xna.complex)=attributes(xna.double)
    xna.raw=suppressWarnings(as.raw(xna.double)); attributes(xna.raw)=attributes(xna.double)
    
    x.objs = as.vector(outer(if(testi==0) c('x','xna') else 'xna', c('double','integer','factor', 'ordered','logical','character','complex','raw'),paste,sep='.'))
    test.cases=expand.grid(x = x.objs, MARGIN=0:2, fromLast=c(FALSE, TRUE), stringsAsFactors=FALSE)
    
    for(i in seq_len(nrow(test.cases))){
        this.case=as.list(test.cases[i,])
        this.case$x=get(this.case$x)
        if(this.case$MARGIN != 0)
            stopifnot(
                identical(do.call(base::unique.matrix, this.case), 
                          do.call(uniqueAtomMat::unique.matrix, this.case))
            )
        stopifnot(
            identical(do.call(base::duplicated.matrix, this.case), 
                      do.call(uniqueAtomMat::duplicated.matrix, this.case)),
            identical(do.call(base::anyDuplicated.matrix, this.case), 
                      do.call(uniqueAtomMat::anyDuplicated.matrix, this.case))
        )
    }
}
