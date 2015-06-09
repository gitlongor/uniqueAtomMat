#include <cfloat>   /* DBL_DIG */
#include "rcSet.h"

// instantiation of global objects:
vecSet<int> 			intVecSet;
vecSet<double> 			doubleVecSet;
vecSet<CharSEXP>		charsexpVecSet; 
vecSet<Rcomplex>		cmplxVecSet;
vecSet<unsigned char>	rawVecSet; 		// Rbyte is an alias of unsigned char

extern "C" {
SEXP dbl_dig()
{
    SEXP out = PROTECT(allocVector(INTSXP, 1));
    INTEGER(out)[0] = DBL_DIG;
    UNPROTECT(1);
    return out;
}

SEXP dupAtomMat(SEXP x, SEXP MARGIN, SEXP fromLast)
{/* returns a logical vector of duplicated rows of numeric matrix x */
	SEXP out;
	int* dim;
	dim=INTEGER(getAttrib(x, R_DimSymbol));
	out = PROTECT(allocVector(LGLSXP, dim[*INTEGER(MARGIN)-1]));
	
	switch (TYPEOF(x)) {
		case REALSXP:
    		doubleVecSet.duplicatedMat	(REAL(x), dim, dim+1,  LOGICAL(out), *INTEGER(MARGIN)==1, (bool)(*(LOGICAL(fromLast))) );
    		break;
		case INTSXP:  // factor type is also covered here
			// if(!inherits(x, "factor"))
				intVecSet.duplicatedMat	(INTEGER(x), dim, dim+1,  LOGICAL(out), *INTEGER(MARGIN)==1, (bool)(*(LOGICAL(fromLast))) );
			// else {;} 
			break;
		case LGLSXP:
			intVecSet.duplicatedMat	(LOGICAL(x), dim, dim+1,  LOGICAL(out), *INTEGER(MARGIN)==1, (bool)(*(LOGICAL(fromLast))) );
			break;
		case STRSXP: {
			CharSEXP* charSexpPtr = new CharSEXP [ dim[0]*dim[1] ];
			for(int i=dim[0]*dim[1]-1; i>=0; --i)
				charSexpPtr[i].sexp = STRING_ELT(x, i);
			
			charsexpVecSet.duplicatedMat	(charSexpPtr, dim, dim+1, LOGICAL(out), *INTEGER(MARGIN)==1, (bool)(*(LOGICAL(fromLast))) );
			
			delete[] charSexpPtr;
			break;
		}
		case CPLXSXP:
			cmplxVecSet.duplicatedMat	(COMPLEX(x), dim, dim+1,  LOGICAL(out), *INTEGER(MARGIN)==1, (bool)(*(LOGICAL(fromLast))) );
			break;
		case RAWSXP:
			rawVecSet.duplicatedMat	(RAW(x), dim, dim+1,  LOGICAL(out), *INTEGER(MARGIN)==1, (bool)(*(LOGICAL(fromLast))) );
			break;
		default:
			error("C function 'dumNumMat' only accepts REALSXP, LGLSXP, INTSXP and STRSXP");
	}
	
	UNPROTECT(1);
	return out;
}



SEXP anyDupAtomMat(SEXP x, SEXP MARGIN, SEXP fromLast)
{/* returns a logical vector of duplicated rows of numeric matrix x */
    SEXP out;
	int* dim;
	dim=INTEGER(getAttrib(x, R_DimSymbol));
	out = PROTECT(allocVector(INTSXP, 1));
	
	switch (TYPEOF(x)) {
		case REALSXP:
			doubleVecSet.anyDuplicatedMat	(REAL(x), dim, dim+1,  INTEGER(out), *INTEGER(MARGIN)==1, (bool)(*(LOGICAL(fromLast))) );
			break;
		case INTSXP:  // factor type is also covered here
			// if(!inherits(x, "factor"))
				intVecSet.anyDuplicatedMat	(INTEGER(x), dim, dim+1,  INTEGER(out), *INTEGER(MARGIN)==1, (bool)(*(LOGICAL(fromLast))) );
			// else {;} 
			break;
		case LGLSXP:
			intVecSet.anyDuplicatedMat	(LOGICAL(x), dim, dim+1,  INTEGER(out), *INTEGER(MARGIN)==1, (bool)(*(LOGICAL(fromLast))) );
			break;
		case STRSXP: {
			CharSEXP* charSexpPtr = new CharSEXP [ dim[0]*dim[1] ];
			for(int i=dim[0]*dim[1]-1; i>=0; --i)
				charSexpPtr[i].sexp = STRING_ELT(x, i);
			
			charsexpVecSet.anyDuplicatedMat	(charSexpPtr, dim, dim+1, INTEGER(out), *INTEGER(MARGIN)==1, (bool)(*(LOGICAL(fromLast))) );
			
			delete[] charSexpPtr;
			break;
		}
		case CPLXSXP:
			cmplxVecSet.anyDuplicatedMat	(COMPLEX(x), dim, dim+1,  INTEGER(out), *INTEGER(MARGIN)==1, (bool)(*(LOGICAL(fromLast))) );
			break;
		case RAWSXP:
			rawVecSet.anyDuplicatedMat	(RAW(x), dim, dim+1,  INTEGER(out), *INTEGER(MARGIN)==1, (bool)(*(LOGICAL(fromLast))) );
			break;
		default:
			error("C function 'anyDumNumMat' only accepts REALSXP, LGLSXP, INTSXP and STRSXP");
	}
	
	UNPROTECT(1);
	return out;
}
}
