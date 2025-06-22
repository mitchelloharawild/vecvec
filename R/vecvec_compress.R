vecvec_compress <- function(x) {
  vec_assert(x, new_vecvec())

  v <- attr(x, "v")
  v_common <- vec_group_loc(
    data_frame(type = lapply(v, typeof), attr = lapply(v, attributes))
  )
  # Already compressed, nothing to do
  if(identical(vec_size(v), vec_size(v_common))) return(x)

  v_compressed <- lapply(v_common$loc, function(i) unique(list_unchop(v[i])))
  i_mapping <- rep(vec_seq_along(v_common), lengths(v_common$loc))

  x_mapping <- list_unchop(.mapply(
    function(i, vec) vec_match(vec, v_compressed[[i_mapping[i]]]),
    list(i = seq_along(v), vec = v), NULL
  ))

  x_offset <- cumsum(c(0, lengths(v)[-length(v)]))
  field(x, "x") <- x_mapping[x_offset[field(x, "i")] + field(x, "x")]
  field(x, "i") <- i_mapping[field(x, "i")]
  attr(x, "v") <- v_compressed
  x
}
