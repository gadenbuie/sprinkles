#' Tachyons
#'
#' Tachyons is a collection of CSS utility classes that works beautifully with
#' \pkg{xaringan} presentations using the `remarkjs`` class syntax.
#'
#' @return An `htmltools::tagList()` with the tachyons dependencies, or an
#'   [htmltools::htmlDependency()].
#' @section Usage: To add tachyons to your xaringan presentation, add the
#'   following code chunk to your slides' R Markdown file.
#'
#'   ````markdown
#'   ```{r xaringan-tachyons, echo=FALSE}
#'   xaringanExtra::use_tachyons()
#'   ```
#'   ````
#'
#'  Tachyons provides small, single-purpose CSS classes that are easily composed
#'  to achieve larger functionality and styles. In the [remarkjs content classes
#'  syntax](https://github.com/gnab/remark/wiki/Markdown#content-classes), you
#'  can compose classes by chaining them together. For example, the following
#'  markdown produces a box with a washed green background (`.bg-washed-green`),
#'  a dark green border (`.b--dark-green`) on all sides (`.ba`) with line width
#'  2 (`.bw2`) and border radius (`.br3`). The box has a shadow (`.shadow-5`)
#'  and medium-large horizontal padding (`.ph4`) with a large top margin
#'  (`.mt5`).
#'
#'  ```markdown
#'  .bg-washed-green.b--dark-green.ba.bw2.br3.shadow-5.ph4.mt5[
#'  The only way to write good code is to write tons of bad code first.
#'  Feeling shame about bad code stops you from getting to good code
#'
#'  .tr[
#'  — Hadley Wickham
#'  ]]
#'  ```
#'
#' @references [tachyons](http://tachyons.io/),
#'   [Tachyons Cheat Sheet](https://roperzh.github.io/tachyons-cheatsheet/)
#' @family tachyons
#' @name tachyons
NULL

#' @describeIn tachyons Adds tachyons to your xaringan slides.
#' @param minified Use the minified Tachyons css file? Default is `TRUE`.
#' @export
use_tachyons <- function(minified = TRUE) {
  htmltools::tagList(
    html_dependency_tachyons(minified)
  )
}

#' @describeIn tachyons Returns an [htmltools::htmlDependency()] with the tile
#'   view dependencies. Most users will want to use `use_tachyons()`.
#' @export
html_dependency_tachyons <- function(minified = TRUE, cdn = NULL, version = NULL) {
  if (!is.null(cdn) && is.null(version)) {
    stop("If using a CDN, you must specify the version of Tachyons")
  }
  tachyons_lock <- pkg_lock_deps("tachyons")
  htmltools::htmlDependency(
    name = "tachyons",
    version = version %||% tachyons_lock$version,
    src = src_href(pkg_file("tachyons"), cdn),
    stylesheet = paste0("tachyons", if (minified) ".min", ".css"),
    all_files = FALSE
  )
}


list_tachyons_vars <- function() {
  tc <- pkg_file("tachyons", "tachyons.css")
  tc <- readLines(tc)
  tc <- gsub(";", "&;", tc)
  tc <- unlist(strsplit(tc, ";"))
  m <- regexec("--([[:graph:]]+): ([^;]+)&$", tc)
  m <- regmatches(tc, m)
  m <- m[vapply(m, length, integer(1)) > 0]
  m <- do.call(rbind, m)[,2:3]
  vars <- as.list(m[, 2])
  names(vars) <- m[, 1]
  vars
}

args_tachyons_vars <- function() {
  vars <- lapply(list_tachyons_vars(), function(...) NULL)
  names(vars) <- gsub("-", "_", names(vars))
  vars
}

params_tachyons_vars <- function() {
  vars <- unlist(list_tachyons_vars())
  vars_names <- names(vars)
  vars_underscore <- gsub("-", "_", names(vars))
  glue_collapse(
    glue(
      "\\item{[vars_underscore]}{\\code{--[vars_names]}, default is \\code{[vars]}}",
      .open = "[",
      .close = "]"
    ),
    sep = "\n"
  )
}

#' Style Tachyons
#'
#' Creates a `<style>` tag to set CSS variables used by Tachyons.
#'
#' @evalRd paste("\\arguments{", params_tachyons_vars(), "}", collapse = "\n")
#'
#' @family tachyons
#' @export
style_tachyons <- function() {
  vars <- as.list(match.call())[-1]
  if (!length(vars)) {
    return()
  }

  css <- vector("character", length(vars))
  for (i in seq_along(vars)) {
    css_var <- gsub("_", "-", names(vars[i]))
    css[i] <- glue_chr("--{css_var}: {vars[i]};")
  }
  htmltools::tags$style(
    glue_chr(":root {{ {css} }}", css = paste(css, collapse = " "))
  )
}

formals(style_tachyons) <- args_tachyons_vars()

#' Add Tachyon-Inspired Utility Color Classes
#'
#' Adds additional utility classes for colors. For each named color, CSS classes
#' are added for the foreground color  as `.<color>`, background color as
#' `.bg-<color>`, hover classes for both as `.hover-<color>` and
#' `.hover-bg-<color>`, and border color as `.b--<color>`.
#'
#' @param ... Named colors. The color names need to be acceptable as valid CSS
#'   classes, in particular they cannot contain periods. Minimal validation is
#'   performed on the color names, and no validation is performed on the
#'   [color syntax](https://developer.mozilla.org/en-US/docs/Web/CSS/color_value).
#' @param .replace_underscores Replace underscores in argument names to turn
#'   color names like `"hot_pink"` to `"hot-pink"`.
#'
#' @family tachyons
#' @export
style_colors <- function(..., .replace_underscores = TRUE, .as_text = FALSE) {
  dots <- list(...)
  if (is.null(names(dots))) {
    stop("All arguments must be named, <color_name> = \"<color_spec>\"")
  }
  all_character <- all(vapply(dots, is.character, logical(1)))
  all_length_1 <- all(vapply(dots, length, integer(1)) == 1)
  if (!all_character || !all_length_1) {
    stop("All arguments must be length-1 characters")
  }

  colors <- unlist(dots)
  color_names <- names(colors)

  if (isTRUE(.replace_underscores)) {
    color_names <- gsub("_", "-", color_names, fixed = TRUE)
  }

  has_period <- grep(".", color_names, fixed = TRUE, value = TRUE)
  if (length(has_period)) {
    stop(
      "Periods are not allowed in color names: ",
      paste(has_period, collapse = ", ")
    )
  }

  color_css_vars <- glue_chr("  --{color_names}: {colors};", .trim = FALSE)

  color_styles <- glue_chr(
    ".[x] { color: var(--[x]) }",
    ".bg-[x] { background-color: var(--[x]) }",
    ".b--[x] { border-color: var(--[x]) }",
    ".hover-[x]:hover, .hover-[x]:focus: { var(--[x]) }",
    x = color_names,
    .open = "[",
    .close = "]",
    .sep = "\n"
  )

  css <- glue_chr(
    ":root {{\n{color_css_vars}\n}}\n{color_styles}"
  )

  if (isTRUE(.as_text)) return(css)

  htmltools::htmlDependency(
    name = "tachyons-extra-colors",
    version = glue_chr("0.0.{floor(runif(1, 1, 9999))}"),
    src = pkg_file(),
    head = format(htmltools::tags$style(css)),
    all_files = TRUE
  )
}
