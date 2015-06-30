#include "config.h"
#ifdef HAVE_CXX1X
#include "rcSetHash.h"

// instantiation of global objects:
vecSetHash<int> 			intVecSetHash;
vecSetHash<double> 			doubleVecSetHash;
vecSetHash<CharSEXP>		charsexpVecSetHash; 
vecSetHash<Rcomplex>		cmplxVecSetHash;
vecSetHash<unsigned char>	rawVecSetHash; 		// Rbyte is an alias of unsigned char

vecMapHash<int>     		intVecMapHash;
vecMapHash<double> 			doubleVecMapHash;
vecMapHash<CharSEXP>		charsexpVecMapHash; 
vecMapHash<Rcomplex>		cmplxVecMapHash;
vecMapHash<unsigned char>	rawVecMapHash; 		// Rbyte is an alias of unsigned char

extern "C" {

SEXP dupAtomMatHash(SEXP x, SEXP MARGIN, SEXP fromLast)
{/* returns a logical vector of duplicated rows of numeric matrix x */
	SEXP out;
	int* dim;
	dim=INTEGER(getAttrib(x, R_DimSymbol));
	out = PROTECT(allocVector(LGLSXP, dim[*INTEGER(MARGIN)-1]));
	
	switch (TYPEOF(x)) {
		case REALSXP:
			doubleVecSetHash.duplicatedMat	(REAL(x), dim, dim+1,  LOGICAL(out), *INTEGER(MARGIN)==1, (bool)(*(LOGICAL(fromLast))) );
			break;
		case INTSXP:  // factor type is also covered here
			// if(!inherits(x, "factor"))
				intVecSetHash.duplicatedMat	(INTEGER(x), dim, dim+1,  LOGICAL(out), *INTEGER(MARGIN)==1, (bool)(*(LOGICAL(fromLast))) );
			// else {;} 
			break;
		case LGLSXP:
			intVecSetHash.duplicatedMat	(LOGICAL(x), dim, dim+1,  LOGICAL(out), *INTEGER(MARGIN)==1, (bool)(*(LOGICAL(fromLast))) );
			break;
		case STRSXP: {
			CharSEXP* charSexpPtr = new CharSEXP [ dim[0]*dim[1] ];
			for(int i=dim[0]*dim[1]-1; i>=0; --i)
				charSexpPtr[i].sexp = STRING_ELT(x, i);
			
			charsexpVecSetHash.duplicatedMat	(charSexpPtr, dim, dim+1, LOGICAL(out), *INTEGER(MARGIN)==1, (bool)(*(LOGICAL(fromLast))) );
			
			delete[] charSexpPtr;
			break;
		}
		case CPLXSXP:
			cmplxVecSetHash.duplicatedMat	(COMPLEX(x), dim, dim+1,  LOGICAL(out), *INTEGER(MARGIN)==1, (bool)(*(LOGICAL(fromLast))) );
			break;
		case RAWSXP:
			rawVecSetHash.duplicatedMat	(RAW(x), dim, dim+1,  LOGICAL(out), *INTEGER(MARGIN)==1, (bool)(*(LOGICAL(fromLast))) );
			break;
		default:
			error("C function 'dumNumMat' only accepts REALSXP, LGLSXP, INTSXP and STRSXP");
	}
	
	UNPROTECT(1);
	return out;
}



SEXP anyDupAtomMatHash(SEXP x, SEXP MARGIN, SEXP fromLast)
{/* returns a logical vector of duplicated rows of numeric matrix x */
    SEXP out;
	int* dim;
	dim=INTEGER(getAttrib(x, R_DimSymbol));
	out = PROTECT(allocVector(INTSXP, 1));
	
	switch (TYPEOF(x)) {
		case REALSXP:
			doubleVecSetHash.anyDuplicatedMat	(REAL(x), dim, dim+1,  INTEGER(out), *INTEGER(MARGIN)==1, (bool)(*(LOGICAL(fromLast))) );
			break;
		case INTSXP:  // factor type is also covered here
			// if(!inherits(x, "factor"))
				intVecSetHash.anyDuplicatedMat	(INTEGER(x), dim, dim+1,  INTEGER(out), *INTEGER(MARGIN)==1, (bool)(*(LOGICAL(fromLast))) );
			// else {;} 
			break;
		case LGLSXP:
			intVecSetHash.anyDuplicatedMat	(LOGICAL(x), dim, dim+1,  INTEGER(out), *INTEGER(MARGIN)==1, (bool)(*(LOGICAL(fromLast))) );
			break;
		case STRSXP: {
			CharSEXP* charSexpPtr = new CharSEXP [ dim[0]*dim[1] ];
			for(int i=dim[0]*dim[1]-1; i>=0; --i)
				charSexpPtr[i].sexp = STRING_ELT(x, i);
			
			charsexpVecSetHash.anyDuplicatedMat	(charSexpPtr, dim, dim+1, INTEGER(out), *INTEGER(MARGIN)==1, (bool)(*(LOGICAL(fromLast))) );
			
			delete[] charSexpPtr;
			break;
		}
		case CPLXSXP:
			cmplxVecSetHash.anyDuplicatedMat	(COMPLEX(x), dim, dim+1,  INTEGER(out), *INTEGER(MARGIN)==1, (bool)(*(LOGICAL(fromLast))) );
			break;
		case RAWSXP:
			rawVecSetHash.anyDuplicatedMat	(RAW(x), dim, dim+1,  INTEGER(out), *INTEGER(MARGIN)==1, (bool)(*(LOGICAL(fromLast))) );
			break;
		default:
			error("C function 'anyDumNumMat' only accepts REALSXP, LGLSXP, INTSXP and STRSXP");
	}
	
	UNPROTECT(1);
	return out;
}

SEXP grpDupAtomMatHash(SEXP x, SEXP MARGIN, SEXP fromLast)
{/* returns an integer vector of duplicated rows of numeric matrix x */
    SEXP out;
	int* dim;
    int nGrps;
	dim=INTEGER(getAttrib(x, R_DimSymbol));
	out = PROTECT(allocVector(INTSXP, dim[*INTEGER(MARGIN)-1]));
	
	switch (TYPEOF(x)) {
		case REALSXP:
			nGrps = doubleVecMapHash.grpDuplicatedMat	(REAL(x), dim, dim+1,  INTEGER(out), *INTEGER(MARGIN)==1, (bool)(*(LOGICAL(fromLast))) );
			break;
		case INTSXP:  // factor type is also covered here
			// if(!inherits(x, "factor"))
				nGrps = intVecMapHash.grpDuplicatedMat	(INTEGER(x), dim, dim+1,  INTEGER(out), *INTEGER(MARGIN)==1, (bool)(*(LOGICAL(fromLast))) );
			// else {;} 
			break;
		case LGLSXP:
			nGrps = intVecMapHash.grpDuplicatedMat	(LOGICAL(x), dim, dim+1,  INTEGER(out), *INTEGER(MARGIN)==1, (bool)(*(LOGICAL(fromLast))) );
			break;
		case STRSXP: {
			CharSEXP* charSexpPtr = new CharSEXP [ dim[0]*dim[1] ];
			for(int i=dim[0]*dim[1]-1; i>=0; --i)
				charSexpPtr[i].sexp = STRING_ELT(x, i);
			
			nGrps = charsexpVecMapHash.grpDuplicatedMat	(charSexpPtr, dim, dim+1, INTEGER(out), *INTEGER(MARGIN)==1, (bool)(*(LOGICAL(fromLast))) );
			
			delete[] charSexpPtr;
			break;
		}
		case CPLXSXP:
			nGrps = cmplxVecMapHash.grpDuplicatedMat	(COMPLEX(x), dim, dim+1,  INTEGER(out), *INTEGER(MARGIN)==1, (bool)(*(LOGICAL(fromLast))) );
			break;
		case RAWSXP:
			nGrps = rawVecMapHash.grpDuplicatedMat	(RAW(x), dim, dim+1,  INTEGER(out), *INTEGER(MARGIN)==1, (bool)(*(LOGICAL(fromLast))) );
			break;
		default:
			error("C function 'grpDupAtomMat' only accepts REALSXP, LGLSXP, INTSXP and STRSXP");
	}
	
    SEXP nLevels;
    nLevels = PROTECT(allocVector(INTSXP, 1));
    INTEGER(nLevels)[0] = nGrps;
    setAttrib(out, install("nlevels"), nLevels);
    UNPROTECT(2);
	return out;
}

}

#endif
