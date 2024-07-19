.onLoad <- function(...) {
  vctrs_exports <- getNamespaceExports(asNamespace("vctrs"))
  # Register vec_cast methods
  vec_cast_generics <- vctrs_exports[startsWith(vctrs_exports, "vec_cast.")]
  lapply(vec_cast_generics, register_s3_method,
         pkg = "vctrs", class = "vecvec", fun = vec_cast_vecvec)

  # Register vec_ptype2 methods
  vec_ptype2_generics <- vctrs_exports[startsWith(vctrs_exports, "vec_ptype2.")]
  lapply(vec_ptype2_generics, register_s3_method,
         pkg = "vctrs", class = "vecvec", fun = vec_ptype2.vecvec)


  # Register all methods
  # lapply(attr(methods(class = "default"), "info")$generic, register_s3_method,
  #        pkg = "vecvec", class = "vecvec", fun = dispatch_elements)
  invisible()
}
