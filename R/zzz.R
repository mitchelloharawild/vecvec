# .onLoad <- function(...) {
#   lapply(attr(methods(class = "default"), "info")$generic, register_s3_method,
#          pkg = "vecvec", class = "vecvec", fun = dispatch_elements)
#   invisible()
# }
