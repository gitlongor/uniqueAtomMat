#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>



extern SEXP dupAtomMat(SEXP, SEXP, SEXP);

static R_CallMethodDef callMethods[]  = {
  {"dupAtomMat", (DL_FUNC) &dupAtomMat, 3},
  {NULL, NULL, 0}
};

void R_init_QuasiSeq(DllInfo *info)
{
   R_registerRoutines(info, NULL, callMethods, NULL, NULL);
}
