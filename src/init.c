#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <stdlib.h> // for NULL

/* .Call calls */
extern SEXP vecvec_group_identity(SEXP x);
extern SEXP vecvec_rep_list(SEXP x, SEXP n);
extern SEXP vecvec_index_check(SEXP index, SEXP size);
extern SEXP vecvec_concat(SEXP x);
extern SEXP vecvec_materialise(SEXP x);
extern SEXP vecvec_group_keys(SEXP key, SEXP n_keys);
extern SEXP vecvec_index_has_dups(SEXP index, SEXP size);
extern SEXP vecvec_locate_c(SEXP pos, SEXP sizes);

static const R_CallMethodDef CallEntries[] = {
  {"vecvec_group_identity", (DL_FUNC) &vecvec_group_identity, 1},
  {"vecvec_rep_list", (DL_FUNC) &vecvec_rep_list, 2},
  {"vecvec_index_check", (DL_FUNC) &vecvec_index_check, 2},
  {"vecvec_concat", (DL_FUNC) &vecvec_concat, 1},
  {"vecvec_materialise", (DL_FUNC) &vecvec_materialise, 1},
  {"vecvec_group_keys", (DL_FUNC) &vecvec_group_keys, 2},
  {"vecvec_index_has_dups", (DL_FUNC) &vecvec_index_has_dups, 2},
  {"vecvec_locate_c", (DL_FUNC) &vecvec_locate_c, 2},
  {NULL, NULL, 0}
};

void R_init_vecvec(DllInfo *dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
