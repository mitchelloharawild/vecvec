#include <R.h>
#include <Rinternals.h>

// Returns TRUE if x is an ALTREP object (e.g. a compact seq()-generated
// sequence, a deferred string conversion, or any other ALTREP wrapper),
// and FALSE if x is a plain materialised vector. Used to decide whether
// c()-ing two adjacent vecvec slots together would force materialisation
// of an ALTREP vector, which defeats its lazy/compact representation.
SEXP vecvec_is_altrep(SEXP x) {
  return Rf_ScalarLogical(ALTREP(x));
}
