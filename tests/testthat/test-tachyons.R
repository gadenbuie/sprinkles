test_that("these are tests", expect_true(TRUE))

describe("html_dependency_tachyons()", {
  it("returns an HTML dependency", {
    expect_s3_class(html_dependency_tachyons(), "html_dependency")
  })

  it("uses local dependency by default", {
    x <- html_dependency_tachyons()
    expect_true(file.exists(file.path(x$src$file, x$stylesheet)))
    expect_equal(x$version, pkg_lock_deps("tachyons")$version)

    x <- html_dependency_tachyons(minified = FALSE)
    expect_true(file.exists(file.path(x$src$file, x$stylesheet)))
    expect_equal(x$version, pkg_lock_deps("tachyons")$version)
  })

  it("error is CDN is used without version", {
    expect_error(html_dependency_tachyons(cdn = "https://unpkg.com"))
  })

  it("uses CDN version of library", {
    x <- html_dependency_tachyons(
      cdn = "https://unpkg.com/tachyons@4.12.0/css/",
      version = "4.12.0"
    )
    expect_equal(x$src$href, "https://unpkg.com/tachyons@4.12.0/css/")
    expect_equal(x$stylesheet, "tachyons.min.css")
  })

  it("powers use_tachyons()", {
    x_uses <- use_tachyons()
    x_dep <- html_dependency_tachyons()
    expect_s3_class(x_uses, "shiny.tag.list")
    expect_equal(x_uses[[1]], x_dep)
  })
})

describe("style_colors()", {
  it("errors if bad color name", {
    expect_error(style_colors("hot.pink" = "pink"), "Periods")
    expect_error(style_colors("pink"))
    expect_error(style_colors(pink = c("pink", "hotpink")))
  })

  it("fixes underscores and returns character when .as_text = TRUE", {
    x <- style_colors(HOT_PINK = "pink", .as_text = TRUE)
    expect_match(x, "--HOT-PINK")
    expect_type(x, "character")
    expect_snapshot(x)
  })

  it("returns an html_dependency", {
    x <- style_colors(HOT_PINK = "pink")
    x_txt <- style_colors(HOT_PINK = "pink", .as_text = TRUE)
    expect_s3_class(x, "html_dependency")
    expect_null(x$script)
    expect_null(x$stylesheet)
    expect_equal(x$head, paste0("<style>", x_txt, "</style>"))
  })
})
