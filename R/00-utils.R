`%||%` <- function(x, y) if (is.null(x)) y else x

pkg_file <- function(...) {
  system.file(..., package = "sprinkles", mustWork = TRUE)
}

glue_chr <- function(..., .envir = parent.frame(), collapse = "\n") {
  paste(as.character(glue::glue(..., .envir = .envir)), collapse = collapse)
}

pkg_lock_deps <- function(dep = NULL) {
  deps <- jsonlite::read_json(pkg_file("package-lock.json"))
  if (is.null(dep)) {
    deps
  } else {
    deps$dependencies[[dep]]
  }
}

src_href <- function(local, cdn = NULL, use_both = FALSE) {
  if (!is.null(cdn)) {
    x <- c(href = cdn, file = local)
    if (isTRUE(use_both)) return(x)
    x["href"]
  } else {
    c(file = local)
  }
}
