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
//
// The 1-based position of the first element of each group is returned in the
// "first" attribute.
SEXP vecvec_group_identity(SEXP x) {
  if (TYPEOF(x) != VECSXP) Rf_error("`x` must be a list.");
  R_xlen_t n = XLENGTH(x);
  SEXP out = PROTECT(Rf_allocVector(INTSXP, n));
  int *grp = INTEGER(out);

  // Representative object and first position of each group found so far
  SEXP uniq = PROTECT(Rf_allocVector(VECSXP, n));
  SEXP first = PROTECT(Rf_allocVector(INTSXP, n));
  int *p_first = INTEGER(first);
  R_xlen_t n_uniq = 0;

  for (R_xlen_t i = 0; i < n; i++) {
    SEXP el = VECTOR_ELT(x, i);
    if (i > 0 && el == VECTOR_ELT(x, i - 1)) {
      grp[i] = grp[i - 1];
      continue;
    }
    R_xlen_t g = 0;
    while (g < n_uniq && VECTOR_ELT(uniq, g) != el) g++;
    if (g == n_uniq) {
      SET_VECTOR_ELT(uniq, n_uniq, el);
      p_first[n_uniq++] = (int) i + 1;
    }
    grp[i] = (int) g + 1;
  }

  first = PROTECT(Rf_xlengthgets(first, n_uniq));
  Rf_setAttrib(out, Rf_install("first"), first);
  UNPROTECT(4);
  return out;
}

// A list of length `n` whose elements are all `x` (as rep_len(list(x), n),
// without its overhead). vctrs computes the proxy of a vecvec several times
// per operation (e.g. to find its size), and this is the only part of it whose
// cost depends on the length.
SEXP vecvec_rep_list(SEXP x, SEXP n) {
  R_xlen_t len = (R_xlen_t) Rf_asReal(n);
  SEXP out = PROTECT(Rf_allocVector(VECSXP, len));
  for (R_xlen_t i = 0; i < len; i++) SET_VECTOR_ELT(out, i, x);
  UNPROTECT(1);
  return out;
}

// Check an integer index into `size` stored values in one pass, without
// allocating. Returns bit flags: 1 if some index is outside 1..size, 2 if
// every index is missing or outside (including when there are none).
SEXP vecvec_index_check(SEXP index, SEXP size) {
  if (TYPEOF(index) != INTSXP) Rf_error("`index` must be an integer vector.");
  R_xlen_t n = XLENGTH(index);
  int m = Rf_asInteger(size);
  const int *p = INTEGER_RO(index);
  int oob = 0, any_valid = 0;
  for (R_xlen_t i = 0; i < n; i++) {
    int v = p[i];
    if (v == NA_INTEGER) continue;
    if (v < 1 || v > m) oob = 1; else any_valid = 1;
  }
  return Rf_ScalarInteger(oob | (any_valid ? 0 : 2));
}
