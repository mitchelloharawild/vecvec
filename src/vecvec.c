#include <R.h>
#include <Rinternals.h>
#include <string.h>

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

// The data of a single-type vecvec is often an ALTREP wrapper (R wraps a long
// vector instead of copying it when its attributes change). Base c() and rep()
// read wrappers one element at a time, which is several times slower than
// copying their data, so vecvec copies the data directly (through the
// read-only data pointers, which wrappers give without copying).

static void copy_atomic(SEXP to, R_xlen_t at, SEXP from) {
  R_xlen_t n = XLENGTH(from);
  switch (TYPEOF(from)) {
  case LGLSXP: memcpy(LOGICAL(to) + at, LOGICAL_RO(from), n * sizeof(int)); break;
  case INTSXP: memcpy(INTEGER(to) + at, INTEGER_RO(from), n * sizeof(int)); break;
  case REALSXP: memcpy(REAL(to) + at, REAL_RO(from), n * sizeof(double)); break;
  case CPLXSXP: memcpy(COMPLEX(to) + at, COMPLEX_RO(from), n * sizeof(Rcomplex)); break;
  case RAWSXP: memcpy(RAW(to) + at, RAW_RO(from), n * sizeof(Rbyte)); break;
  case STRSXP:
    for (R_xlen_t i = 0; i < n; i++) SET_STRING_ELT(to, at + i, STRING_ELT(from, i));
    break;
  default: Rf_error("Can't copy a vector of type %s.", Rf_type2char(TYPEOF(from)));
  }
}

// Concatenate a list of atomic vectors of one type (without attributes).
SEXP vecvec_concat(SEXP x) {
  R_xlen_t k = XLENGTH(x), n = 0;
  if (k == 0) Rf_error("`x` must have at least one vector.");
  SEXPTYPE type = TYPEOF(VECTOR_ELT(x, 0));
  for (R_xlen_t j = 0; j < k; j++) {
    if (TYPEOF(VECTOR_ELT(x, j)) != type) Rf_error("The vectors must all have one type.");
    n += XLENGTH(VECTOR_ELT(x, j));
  }
  SEXP out = PROTECT(Rf_allocVector(type, n));
  R_xlen_t at = 0;
  for (R_xlen_t j = 0; j < k; j++) {
    copy_atomic(out, at, VECTOR_ELT(x, j));
    at += XLENGTH(VECTOR_ELT(x, j));
  }
  UNPROTECT(1);
  return out;
}

// A copy of the data of an atomic vector, without attributes.
SEXP vecvec_materialise(SEXP x) {
  SEXP out = PROTECT(Rf_allocVector(TYPEOF(x), XLENGTH(x)));
  copy_atomic(out, 0, x);
  UNPROTECT(1);
  return out;
}

// Group positions by an integer key in 1..n_keys (or NA), in order of first
// appearance, by counting. Returns a list of 1-based positions per group; all
// NA keys form one group.
SEXP vecvec_group_keys(SEXP key, SEXP n_keys) {
  if (TYPEOF(key) != INTSXP) Rf_error("`key` must be an integer vector.");
  R_xlen_t n = XLENGTH(key);
  int k = Rf_asInteger(n_keys);
  const int *p = INTEGER_RO(key);

  // Counts per key, with key 0 for NA
  R_xlen_t *count = (R_xlen_t *) R_alloc((size_t) k + 1, sizeof(R_xlen_t));
  int *group = (int *) R_alloc((size_t) k + 1, sizeof(int));
  for (int j = 0; j <= k; j++) {
    count[j] = 0;
    group[j] = -1;
  }
  int n_groups = 0;
  for (R_xlen_t i = 0; i < n; i++) {
    int v = p[i] == NA_INTEGER ? 0 : p[i];
    if (v < 0 || v > k) Rf_error("`key` must be in 1..n_keys.");
    if (group[v] < 0) group[v] = n_groups++;
    count[v]++;
  }

  SEXP out = PROTECT(Rf_allocVector(VECSXP, n_groups));
  int **fill = (int **) R_alloc((size_t) k + 1, sizeof(int *));
  for (int j = 0; j <= k; j++) {
    if (group[j] < 0) continue;
    SEXP pos = Rf_allocVector(INTSXP, count[j]);
    SET_VECTOR_ELT(out, group[j], pos);
    fill[j] = INTEGER(pos);
  }
  for (R_xlen_t i = 0; i < n; i++) {
    int v = p[i] == NA_INTEGER ? 0 : p[i];
    *(fill[v]++) = (int) i + 1;
  }
  UNPROTECT(1);
  return out;
}

// Does an integer index (into 1..size, or NA) use any position more than once?
SEXP vecvec_index_has_dups(SEXP index, SEXP size) {
  if (TYPEOF(index) != INTSXP) Rf_error("`index` must be an integer vector.");
  R_xlen_t n = XLENGTH(index);
  int m = Rf_asInteger(size);
  if (n > m) return Rf_ScalarLogical(TRUE);
  const int *p = INTEGER_RO(index);
  unsigned char *seen = (unsigned char *) R_alloc((size_t) m + 1, 1);
  memset(seen, 0, (size_t) m + 1);
  for (R_xlen_t i = 0; i < n; i++) {
    int v = p[i];
    if (v == NA_INTEGER || v < 1 || v > m) continue;
    if (seen[v]) return Rf_ScalarLogical(TRUE);
    seen[v] = 1;
  }
  return Rf_ScalarLogical(FALSE);
}

// For each position into the concatenation of vectors of the given sizes, the
// vector it falls in and its position within that vector (1-based; NA for
// missing positions or positions outside the vectors). As
// findInterval(pos, c(0, cumsum(sizes)), left.open = TRUE) in one pass.
SEXP vecvec_locate_c(SEXP pos, SEXP sizes) {
  if (TYPEOF(pos) != INTSXP || TYPEOF(sizes) != INTSXP) {
    Rf_error("`pos` and `sizes` must be integer vectors.");
  }
  R_xlen_t n = XLENGTH(pos);
  int k = LENGTH(sizes);
  const int *p = INTEGER_RO(pos), *sz = INTEGER_RO(sizes);

  // Start of each vector (0-based offsets)
  double *start = (double *) R_alloc((size_t) k + 1, sizeof(double));
  start[0] = 0;
  for (int j = 0; j < k; j++) start[j + 1] = start[j] + sz[j];

  SEXP slot = PROTECT(Rf_allocVector(INTSXP, n));
  SEXP within = PROTECT(Rf_allocVector(INTSXP, n));
  int *ps = INTEGER(slot), *pw = INTEGER(within);
  int last = 0;
  for (R_xlen_t i = 0; i < n; i++) {
    int v = p[i];
    if (v == NA_INTEGER || v < 1 || v > start[k]) {
      ps[i] = NA_INTEGER;
      pw[i] = NA_INTEGER;
      continue;
    }
    // Positions usually fall in the same vector as the one before
    int j = last;
    if (!(v > start[j] && v <= start[j + 1])) {
      int lo = 0, hi = k - 1;
      while (lo < hi) {
        int mid = lo + (hi - lo) / 2;
        if (v > start[mid + 1]) lo = mid + 1; else hi = mid;
      }
      j = lo;
      // Skip empty vectors
      while (j < k - 1 && v > start[j + 1]) j++;
    }
    last = j;
    ps[i] = j + 1;
    pw[i] = v - (int) start[j];
  }

  SEXP out = PROTECT(Rf_allocVector(VECSXP, 2));
  SET_VECTOR_ELT(out, 0, slot);
  SET_VECTOR_ELT(out, 1, within);
  SEXP nms = PROTECT(Rf_allocVector(STRSXP, 2));
  SET_STRING_ELT(nms, 0, Rf_mkChar("slot"));
  SET_STRING_ELT(nms, 1, Rf_mkChar("within"));
  Rf_setAttrib(out, R_NamesSymbol, nms);
  UNPROTECT(4);
  return out;
}
