#' Alpine.js
#'
#' Alpine.js is a rugged, minimal framework for composing JavaScript behavior in
#' your markup. .
#'
#' @return An `htmltools::tagList()` with the Alpine.js dependencies, or an
#'   [htmltools::htmlDependency()].
#'
#' @references [Alpine.js](https://github.com/alpinejs/alpine/)
#' @name alpinejs
NULL

#' @describeIn alpinejs Adds Alpine.js dependencies to your document.
#' @param minified Use the minified Alpine.js file? Default is `TRUE`.
#' @export
use_alpinejs <- function(minified = TRUE) {
  htmltools::tagList(
    html_dependency_alpinejs(minified)
  )
}

#' @describeIn alpinejs Returns an [htmltools::htmlDependency()] with the
#'   alpinejs dependencies. Most users will want to use `use_alpinejs()`.
#' @param cdn The URL to an online location for Alpine.js, typically a CDN.
#'   Include the URL, without the CSS file, e.g. the full URL minus the
#'   `alpine.js` or `alpine.min.js`.
#' @param version The version of the Alpine.js resource hosted at the URL in
#'   `cdn`.
#' @export
html_dependency_alpinejs <- function(minified = TRUE, cdn = NULL, version = NULL) {
  if (!is.null(cdn) && is.null(version)) {
    stop("If using a CDN, you must specify the version of Alpine.js")
  }
  htmltools::htmlDependency(
    name = "alpinejs",
    version = version %||% pkg_lock_deps("alpinejs")$version,
    src = src_href(pkg_file("alpinejs"), cdn),
    script = paste0("alpine", if (minified) ".min", ".js"),
    all_files = FALSE
  )
}
