collapse_vecvec <- function(x) {
  out <- attr(x, "vecs")
  ptypes <- lapply(out, vctrs::vec_ptype)
  loc <- vctrs::vec_group_loc(ptypes)
  # For now only handle dropping vecvec class
  if(nrow(loc) == 1L) return(unlist(mapply(function(i, x) out[[i]][[x]], field(x, "i"), field(x, "x"))))
  # Later handle collapsing like-types into the same attribute
  x
}
