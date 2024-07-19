.onLoad <- function(...) {
  # Register vec_cast methods
  an <- getNamespaceExports(asNamespace("vctrs"))
  vec_cast_types <- an[startsWith(an, "vec_cast.")]
  lapply(vec_cast_types, register_s3_method,
         pkg = "vctrs", class = "vecvec", fun = vec_cast_vecvec)


  # Register all methods
  # lapply(attr(methods(class = "default"), "info")$generic, register_s3_method,
  #        pkg = "vecvec", class = "vecvec", fun = dispatch_elements)
  invisible()
}
