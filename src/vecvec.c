#include <R.h>
#include <Rinternals.h>

// Group the elements of a list by identity (the same R object, not equal
// values), numbering groups from 1 in order of first appearance.
//
// vctrs restores a vecvec from a data frame proxy whose rows each hold the
// (shared) list of stored vectors of the vecvec they came from. Grouping rows
// by that object tells which rows index into the same stored vectors. Rows
// from one vecvec are usually contiguous, so each element is first compared
// with the one before it, and only then with the groups found so far.
SEXP vecvec_group_identity(SEXP x) {
  if (TYPEOF(x) != VECSXP) Rf_error("`x` must be a list.");
  R_xlen_t n = XLENGTH(x);
  SEXP out = PROTECT(Rf_allocVector(INTSXP, n));
  int *grp = INTEGER(out);

  // Representative object of each group found so far
  SEXP uniq = PROTECT(Rf_allocVector(VECSXP, n));
  R_xlen_t n_uniq = 0;

  for (R_xlen_t i = 0; i < n; i++) {
    SEXP el = VECTOR_ELT(x, i);
    if (i > 0 && el == VECTOR_ELT(x, i - 1)) {
      grp[i] = grp[i - 1];
      continue;
    }
    R_xlen_t g = 0;
    while (g < n_uniq && VECTOR_ELT(uniq, g) != el) g++;
    if (g == n_uniq) SET_VECTOR_ELT(uniq, n_uniq++, el);
    grp[i] = (int) g + 1;
  }

  UNPROTECT(2);
  return out;
}
