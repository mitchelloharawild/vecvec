#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <stdlib.h> // for NULL

/* .Call calls */
extern SEXP vecvec_is_altrep(SEXP x);

static const R_CallMethodDef CallEntries[] = {
  {"vecvec_is_altrep", (DL_FUNC) &vecvec_is_altrep, 1},
  {NULL, NULL, 0}
};

void R_init_vecvec(DllInfo *dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
