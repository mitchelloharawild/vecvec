.onLoad <- function(...) {
  vctrs_exports <- getNamespaceExports(asNamespace("vctrs"))

  vec_cast_generics <- vctrs_exports[startsWith(vctrs_exports, "vec_cast.")]
  # Register vec_cast.*.vecvec methods
  lapply(vec_cast_generics, register_s3_method,
         pkg = "vctrs", class = "vecvec", fun = vec_cast_from_vecvec)
  # Register vec_cast.vecvec.* methods
  lapply(sub("^vec_cast", "vecvec", vec_cast_generics), register_s3_method,
         pkg = "vctrs", generic = "vec_cast", fun = vec_cast_to_vecvec)

  vec_ptype2_generics <- vctrs_exports[startsWith(vctrs_exports, "vec_ptype2.")]
  # Register vec_ptype2.*.vecvec methods
  lapply(vec_ptype2_generics, register_s3_method,
         pkg = "vctrs", class = "vecvec", fun = vec_ptype2.vecvec)
  # Register vec_ptype2.vecvec.* methods
  lapply(sub("^vec_ptype2", "vecvec", vec_ptype2_generics), register_s3_method,
         pkg = "vctrs", generic = "vec_ptype2", fun = vec_cast_to_vecvec)

  # Register vec_arith.*.vecvec methods
  vec_arith_generics <- vctrs_exports[startsWith(vctrs_exports, "vec_arith.")]
  lapply(vec_arith_generics, register_s3_method,
         pkg = "vctrs", class = "vecvec", fun = vec_default_arith_vecvec)

  # Register all methods
  # lapply(attr(methods(class = "default"), "info")$generic, register_s3_method,
  #        pkg = "vecvec", class = "vecvec", fun = dispatch_elements)
  invisible()
}
