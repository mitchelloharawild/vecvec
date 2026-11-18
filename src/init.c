#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <stdlib.h> // for NULL

/* .Call calls */
extern SEXP vecvec_is_altrep(SEXP x);
extern SEXP vecvec_alt_new(SEXP index, SEXP slots);
extern SEXP vecvec_alt_index(SEXP x);
extern SEXP vecvec_alt_mirror(SEXP x);
extern SEXP vecvec_alt_slots(SEXP x);
extern SEXP vecvec_alt_rebuild(SEXP x, SEXP slots);
extern void vecvec_init_altrep(DllInfo *dll);

static const R_CallMethodDef CallEntries[] = {
  {"vecvec_is_altrep", (DL_FUNC) &vecvec_is_altrep, 1},
  {"vecvec_alt_new", (DL_FUNC) &vecvec_alt_new, 2},
  {"vecvec_alt_index", (DL_FUNC) &vecvec_alt_index, 1},
  {"vecvec_alt_mirror", (DL_FUNC) &vecvec_alt_mirror, 1},
  {"vecvec_alt_slots", (DL_FUNC) &vecvec_alt_slots, 1},
  {"vecvec_alt_rebuild", (DL_FUNC) &vecvec_alt_rebuild, 2},
  {NULL, NULL, 0}
};

void R_init_vecvec(DllInfo *dll) {
  vecvec_init_altrep(dll);
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
