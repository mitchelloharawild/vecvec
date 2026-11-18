#include <R.h>
#include <Rinternals.h>
#include <string.h>

// Returns TRUE if x is an ALTREP object (e.g. a compact seq()-generated
// sequence, a deferred string conversion, or any other ALTREP wrapper),
// and FALSE if x is a plain materialised vector. Used to decide whether
// c()-ing two adjacent vecvec slots together would force materialisation
// of an ALTREP vector, which defeats its lazy/compact representation.
SEXP vecvec_is_altrep(SEXP x) {
  return Rf_ScalarLogical(ALTREP(x));
}

// ---------------------------------------------------------------------------
// ALTREP base vector for vecvec objects
//
// A vecvec is an index into the concatenation of its stored vectors (the `x`
// property). Base R functions (and C code such as model.matrix()) that read the
// object directly, bypassing S3/S7 dispatch, should see the *values* the index
// refers to and never the index itself. This ALTREP double vector resolves the
// index to values on access:
//
//   data1: list(<integer index, 1-based into the concatenated slots, or NA>,
//               <list of stored vectors>)    -- immutable once created
//   data2: NULL, or the resolved doubles (a cache)
//
// The stored vectors are held in data1 (shared, not copied, with the `x`
// attribute) rather than looked up from attributes because base R functions
// routinely strip attributes from the copies they make.
//
// Stored values that have no numeric reading (character, factor, records, ...)
// resolve to NA.
// ---------------------------------------------------------------------------
#include <R_ext/Altrep.h>

static R_altrep_class_t vecvec_alt_class;

#define VV_INDEX(x) VECTOR_ELT(R_altrep_data1(x), 0)
#define VV_SLOTS(x) VECTOR_ELT(R_altrep_data1(x), 1)

// Classed numeric slots (e.g. mixtime's cyclical time, whose storage is a
// chronon count rather than the cyclical position) are read through their
// as.double() method, so the base agrees with as.numeric(). Plain vectors are
// read directly. Falls back to the raw storage if the method fails.
static SEXP slot_as_double(SEXP slot) {
  if (!Rf_isObject(slot) || Rf_isFactor(slot)) return slot;
  SEXPTYPE t = TYPEOF(slot);
  if (t != REALSXP && t != INTSXP && t != LGLSXP) return slot;
  SEXP call = PROTECT(Rf_lang2(Rf_install("as.double"), slot));
  int err = 0;
  SEXP v = R_tryEvalSilent(call, R_BaseEnv, &err);
  UNPROTECT(1);
  if (err || TYPEOF(v) != REALSXP || XLENGTH(v) != XLENGTH(slot)) return slot;
  return v;
}

static double slot_value(SEXP slot, R_xlen_t j) {
  switch (TYPEOF(slot)) {
  case REALSXP: return REAL(slot)[j];
  case INTSXP: {
    if (Rf_isFactor(slot)) return NA_REAL;
    int v = INTEGER(slot)[j];
    return v == NA_INTEGER ? NA_REAL : (double) v;
  }
  case LGLSXP: {
    int v = LOGICAL(slot)[j];
    return v == NA_LOGICAL ? NA_REAL : (double) v;
  }
  default: return NA_REAL;
  }
}

static SEXP vv_resolve(SEXP x) {
  SEXP d2 = R_altrep_data2(x);
  if (d2 != R_NilValue) return d2;

  SEXP index = VV_INDEX(x), slots = VV_SLOTS(x);
  R_xlen_t n = XLENGTH(index), nslots = Rf_xlength(slots), total = 0;
  const int *idx = INTEGER(index);

  for (R_xlen_t s = 0; s < nslots; s++) total += Rf_xlength(VECTOR_ELT(slots, s));
  SEXP flat = PROTECT(Rf_allocVector(REALSXP, total));
  double *fp = REAL(flat);
  R_xlen_t pos = 0;
  for (R_xlen_t s = 0; s < nslots; s++) {
    SEXP slot = PROTECT(slot_as_double(VECTOR_ELT(slots, s)));
    R_xlen_t m = Rf_xlength(slot);
    for (R_xlen_t j = 0; j < m; j++) fp[pos++] = slot_value(slot, j);
    UNPROTECT(1);
  }

  SEXP res = PROTECT(Rf_allocVector(REALSXP, n));
  double *rp = REAL(res);
  for (R_xlen_t i = 0; i < n; i++)
    rp[i] = (idx[i] == NA_INTEGER || idx[i] < 1 || idx[i] > total) ? NA_REAL : fp[idx[i] - 1];

  R_set_altrep_data2(x, res);
  UNPROTECT(2);
  return res;
}

static R_xlen_t vv_length(SEXP x) { return XLENGTH(VV_INDEX(x)); }
static double vv_elt(SEXP x, R_xlen_t i) { return REAL(vv_resolve(x))[i]; }
static R_xlen_t vv_region(SEXP x, R_xlen_t i, R_xlen_t n, double *buf) {
  R_xlen_t len = vv_length(x), m = len - i < n ? len - i : n;
  const double *r = REAL(vv_resolve(x));
  for (R_xlen_t j = 0; j < m; j++) buf[j] = r[i + j];
  return m;
}
static void *vv_dataptr(SEXP x, Rboolean writable) { return REAL(vv_resolve(x)); }
static const void *vv_dataptr_or_null(SEXP x) {
  SEXP d2 = R_altrep_data2(x);
  return d2 == R_NilValue ? NULL : REAL(d2);
}

// The index and stored vectors are mirrored in the `_vecvec` attribute, which
// is the very same list object as data1 (so costs no memory). `identical()`
// only looks at attributes and the base data, never at ALTREP data1, so without
// the mirror it can't tell vecvecs apart whose slots resolve to NA (character,
// factors, lists, records...) or that differ only in their index.
//
// When serializing, the mirror attribute already carries the data, so the
// ALTREP state is just a placeholder (so nothing is written twice). It is
// restored from the attribute by `vv_unserialize_ex()`. Without a mirror (e.g.
// when R's generic wrapper ALTREP encloses a vecvec base, or the attribute was
// removed) the state is the full data1.
static SEXP vv_mirror_sym;

static SEXP vv_serialized_state(SEXP x) {
  SEXP m = Rf_getAttrib(x, vv_mirror_sym);
  if (TYPEOF(m) == VECSXP && Rf_xlength(m) == 2) return Rf_allocVector(INTSXP, 0);
  return R_altrep_data1(x);
}
static SEXP vv_unserialize(SEXP cls, SEXP state) {
  return R_new_altrep(vecvec_alt_class, state, R_NilValue);
}
static SEXP vv_unserialize_ex(SEXP cls, SEXP state, SEXP attr, int objf, int levs) {
  SEXP data = state;
  if (TYPEOF(state) != VECSXP) {
    data = R_NilValue;
    for (SEXP a = attr; a != R_NilValue; a = CDR(a))
      if (TAG(a) == vv_mirror_sym) { data = CAR(a); break; }
    if (TYPEOF(data) != VECSXP || Rf_xlength(data) != 2)
      Rf_error("Can't restore a vecvec: its `_vecvec` attribute is missing.");
  }
  SEXP ans = PROTECT(R_new_altrep(vecvec_alt_class, data, R_NilValue));
  for (SEXP a = attr; a != R_NilValue; a = CDR(a)) Rf_setAttrib(ans, TAG(a), CAR(a));
  UNPROTECT(1);
  return ans;
}
static SEXP vv_duplicate(SEXP x, Rboolean deep) {
  // data1 is immutable, so is shared. The cache is rebuilt on demand.
  SEXP ans = PROTECT(R_new_altrep(vecvec_alt_class, R_altrep_data1(x), R_NilValue));
  Rf_copyMostAttrib(x, ans);
  SEXP a;
  if ((a = Rf_getAttrib(x, R_DimSymbol)) != R_NilValue) Rf_setAttrib(ans, R_DimSymbol, a);
  if ((a = Rf_getAttrib(x, R_DimNamesSymbol)) != R_NilValue) Rf_setAttrib(ans, R_DimNamesSymbol, a);
  if ((a = Rf_getAttrib(x, R_NamesSymbol)) != R_NilValue) Rf_setAttrib(ans, R_NamesSymbol, a);
  UNPROTECT(1);
  return ans;
}

SEXP vecvec_alt_new(SEXP index, SEXP slots) {
  if (TYPEOF(index) != INTSXP) Rf_error("`index` must be an integer vector.");
  if (TYPEOF(slots) != VECSXP) Rf_error("`slots` must be a list.");
  // Strip attributes so the index is bare and immutable.
  SEXP bare = PROTECT(Rf_allocVector(INTSXP, XLENGTH(index)));
  if (XLENGTH(index)) memcpy(INTEGER(bare), INTEGER(index), XLENGTH(index) * sizeof(int));
  SEXP d = PROTECT(Rf_allocVector(VECSXP, 2));
  SET_VECTOR_ELT(d, 0, bare);
  SET_VECTOR_ELT(d, 1, slots);
  MARK_NOT_MUTABLE(bare);
  MARK_NOT_MUTABLE(d);
  SEXP out = PROTECT(R_new_altrep(vecvec_alt_class, d, R_NilValue));
  Rf_setAttrib(out, vv_mirror_sym, d);
  UNPROTECT(3);
  return out;
}


// Setting attributes on a shared ALTREP vector (e.g. `dim(x) <- ...`) makes R
// wrap it in a generic wrapper ALTREP, with the original as data1. Find the
// vecvec ALTREP underneath.
static SEXP vv_unwrap(SEXP x) {
  while (ALTREP(x) && !R_altrep_inherits(x, vecvec_alt_class)) {
    SEXP inner = R_altrep_data1(x);
    if (!ALTREP(inner)) break;
    x = inner;
  }
  if (!ALTREP(x) || !R_altrep_inherits(x, vecvec_alt_class))
    Rf_error("`x` is not a vecvec base vector.");
  return x;
}

// Returns `x` as a plain (unwrapped) vecvec base vector that carries the
// attributes of `x` and the `_vecvec` mirror of its ALTREP data. Run after
// attributes were set wholesale. Setting attributes on a shared ALTREP vector
// wraps it in R's generic wrapper, which would be serialized together with its
// own copy of the attributes (so the mirror would be written twice). No data is
// copied, as the new vector shares data1.
SEXP vecvec_alt_mirror(SEXP x) {
  SEXP d = R_altrep_data1(vv_unwrap(x));
  SEXP ans = PROTECT(R_new_altrep(vecvec_alt_class, d, R_NilValue));
  Rf_copyMostAttrib(x, ans);
  SEXP a;
  if ((a = Rf_getAttrib(x, R_DimSymbol)) != R_NilValue) Rf_setAttrib(ans, R_DimSymbol, a);
  if ((a = Rf_getAttrib(x, R_DimNamesSymbol)) != R_NilValue) Rf_setAttrib(ans, R_DimNamesSymbol, a);
  if ((a = Rf_getAttrib(x, R_NamesSymbol)) != R_NilValue) Rf_setAttrib(ans, R_NamesSymbol, a);
  Rf_setAttrib(ans, vv_mirror_sym, d);
  UNPROTECT(1);
  return ans;
}

SEXP vecvec_alt_index(SEXP x) { return VV_INDEX(vv_unwrap(x)); }

SEXP vecvec_alt_slots(SEXP x) { return VV_SLOTS(vv_unwrap(x)); }

// A fresh vecvec ALTREP (without attributes) with the index of `x` and new
// stored vectors.
SEXP vecvec_alt_rebuild(SEXP x, SEXP slots) {
  return vecvec_alt_new(VV_INDEX(vv_unwrap(x)), slots);
}

void vecvec_init_altrep(DllInfo *dll) {
  vv_mirror_sym = Rf_install("_vecvec");
  vecvec_alt_class = R_make_altreal_class("vecvec_alt", "vecvec", dll);
  R_set_altrep_Length_method(vecvec_alt_class, vv_length);
  R_set_altreal_Elt_method(vecvec_alt_class, vv_elt);
  R_set_altreal_Get_region_method(vecvec_alt_class, vv_region);
  R_set_altvec_Dataptr_method(vecvec_alt_class, vv_dataptr);
  R_set_altvec_Dataptr_or_null_method(vecvec_alt_class, vv_dataptr_or_null);
  R_set_altrep_Serialized_state_method(vecvec_alt_class, vv_serialized_state);
  R_set_altrep_Unserialize_method(vecvec_alt_class, vv_unserialize);
  R_set_altrep_UnserializeEX_method(vecvec_alt_class, vv_unserialize_ex);
  R_set_altrep_Duplicate_method(vecvec_alt_class, vv_duplicate);
}
