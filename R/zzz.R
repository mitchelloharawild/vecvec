.onLoad <- function(...) {
  if ("S7_on_load" %in% getNamespaceExports("S7")) {
    getExportedValue("S7", "S7_on_load")()
  } else {
    S7::methods_register()
  }

  # Register vecvec methods
  vecvec_register(class_vecvec)

  # Register all methods
  # lapply(
  #   setdiff(
  #     attr(methods(class = "default"), "info")$generic,
  #     attr(methods(class = "vecvec::vecvec"), "info")$generic
  #   ),
  #   register_s3_method,
  #   pkg = "vecvec", class = "vecvec::vecvec", 
  #   fun = vecvec_dispatch)
  invisible()
}

.onUnload <- function(...) {
  if ("S7_on_unload" %in% getNamespaceExports("S7")) {
    getExportedValue("S7", "S7_on_unload")()
  }
}

if ("S7_on_build" %in% getNamespaceExports("S7")) {
  getExportedValue("S7", "S7_on_build")()
}
