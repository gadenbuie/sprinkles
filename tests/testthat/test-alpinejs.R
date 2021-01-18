test_that("these are tests", expect_true(TRUE))

describe("html_dependency_alpinejs()", {
  it ("returns an HTML dependency", {
    expect_s3_class(html_dependency_alpinejs(), "html_dependency")
  })

  it ("uses local dependency by default", {
    x <- html_dependency_alpinejs()
    expect_true(file.exists(file.path(x$src$file, x$script)))
    expect_equal(x$version, pkg_lock_deps("alpinejs")$version)
  })

  it ("error is CDN is used without version", {
    expect_error(html_dependency_alpinejs(cdn = "https://unpkg.com"))
  })

  it ("uses CDN version of library", {
    x <- html_dependency_alpinejs(
      cdn = "https://cdn.jsdelivr.net/gh/alpinejs/alpine@v2.8.0/dist/",
      version = "2.8.0"
    )
    expect_equal(x$src$href, "https://cdn.jsdelivr.net/gh/alpinejs/alpine@v2.8.0/dist/")
    expect_equal(x$script, "alpine.min.js")
  })
})
